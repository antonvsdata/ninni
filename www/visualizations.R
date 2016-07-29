

# Plots a heatmap: Variable labels on axes and colouring by effect. If effect type is "OR" or "FC", uses log2-transformation
# Input:  dataframe with variable_label1, variable_label2 and effect
#         string containing the effect type
make_heatmap <- function(dframe,effect_type){
  
  if (effect_type %in% c("OR","FC")){
    dframe$effect <- log2(dframe$effect)
  }
  dframe %>% filter(abs(log2(effect)) > 7)
  
  p <- ggplot(dframe, aes(var_label1,var_label2))
  
  p + geom_tile(aes(fill = effect) , colour = "white") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, space = "Lab") +
    theme( axis.text.x = element_text(angle = 90))
    
}

# Volcano plot (with double filtering?)
# Input:  data frame with effect, p_fdr and point labels
#         string containing the effect type

make_volcanoplot <- function(dframe,effect_type){
  
  if (effect_type %in% c("OR","FC")){
    dframe$effect <- log2(dframe$effect)
  }
  
  p <- ggplot(dframe, aes(x = effect, y = -log10(p_fdr)))
  
  p + geom_point()
}

# Q-Q plot
# Input:  data frame with effect
#         string containing effect_type
#         distribution, default: normal distribution

make_qqplot <- function(dframe,effect_type,dstr = stats::qnorm ){
  
  if (effect_type %in% c("OR","FC")){
    dframe$effect <- log2(dframe$effect)
  }
  
  x <- quantile(dframe$effect, c(0.25,0.75))
  y <- qnorm(c(0.25,0.75))
  slope <- diff(y) - diff(x)
  int <- y[1] - slope * x[1]
  
  p <- ggplot(dframe, aes(sample = effect))
  
  p + stat_qq(distribution = dstr) + geom_abline(slope = slope, intercept = int)
}

# Source: https://gist.github.com/rentrop/d39a8406ad8af2a1066c

gg_qq <- function(x, distribution = "norm", ..., line.estimate = NULL, conf = 0.95,
                  labels = names(x)){
  q.function <- eval(parse(text = paste0("q", distribution)))
  d.function <- eval(parse(text = paste0("d", distribution)))
  x <- na.omit(x)
  ord <- order(x)
  n <- length(x)
  P <- ppoints(length(x))
  df <- data.frame(ord.x = x[ord], z = q.function(P, ...))
  
  if(is.null(line.estimate)){
    Q.x <- quantile(df$ord.x, c(0.25, 0.75))
    Q.z <- q.function(c(0.25, 0.75), ...)
    b <- diff(Q.x)/diff(Q.z)
    coef <- c(Q.x[1] - b * Q.z[1], b)
  } else {
    coef <- coef(line.estimate(ord.x ~ z))
  }
  
  zz <- q.function(1 - (1 - conf)/2)
  SE <- (coef[2]/d.function(df$z)) * sqrt(P * (1 - P)/n)
  fit.value <- coef[1] + coef[2] * df$z
  df$upper <- fit.value + zz * SE
  df$lower <- fit.value - zz * SE
  
  if(!is.null(labels)){ 
    df$label <- ifelse(df$ord.x > df$upper | df$ord.x < df$lower, labels[ord],"")
  }
  
  p <- ggplot(df, aes(x=z, y=ord.x)) +
    geom_point() + 
    geom_abline(intercept = coef[1], slope = coef[2]) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) 
  if(!is.null(labels)) p <- p + geom_text( aes(label = label))
  print(p)
  coef
}
