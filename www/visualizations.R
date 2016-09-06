# Plots a heatmap: Variable labels on axes and colouring by effect. If effect type is "OR" or "FC", uses log2-transformation
# Input:  dataframe with variable_label1, variable_label2 and effect
#         string containing the effect type
make_heatmap <- function(dframe,effect_type){
  
  p <- ggplot(dframe, aes(var_label1,var_label2, label1 = effect, label2 = p_fdr, label3 = n))
  
  if (effect_type %in% c("OR","FC")){
    p <- p + geom_tile(aes(fill = log2(effect)) , colour = "white")
  }
  else{
    p <- p + geom_tile(aes(fill = effect) , colour = "white")
  }
  
  
  
  p <- p +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, space = "Lab") +
    theme( axis.text.x = element_text(angle = 90)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank())
  
  ggplotly(p, tooltip = c("x", "y","label1","label2","label3"))
}

dist2 <- function(x) {
  d <- dist(x)
  d <- as.matrix(d)
  g <- which(rowSums(!is.na(d)) > 2)
  f <- which(colSums(!is.na(d)) > 2)
  d <- d[g,f]
  c <- complete.cases(d)
  d <- d[c,c]
  d <- as.dist(d)
}

get_heatmap <- function(df){
  df <- df %>% select(var_label1,var_label2,effect) %>% filter(!is.na(var_label1))
  df$row <- 1:nrow(df)
  df$effect <- log2(df$effect)
  
  new_tbl <- spread(df, var_label2, effect) %>% select(-row) %>% group_by(var_label1) %>% summarise_each(funs(.[!is.na(.)][1]))
  row.names(new_tbl) <- new_tbl$var_label1
  new_tbl <- new_tbl %>% select(-var_label1)
  
  lampok <- heatmapr(new_tbl, distfun = dist2, dendrogram = "none")
  
  eaxis <- list(showticklabels = FALSE,
                showgrid = FALSE,
                zeroline = FALSE)
  
  heatmaply(lampok,
            scale_fill_gradient_fun = scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, space = "Lab"),
            column_text_angle = 90)
}

# Volcano plot with double filtering
# Input:  data frame with effect, p_fdr and point labels
#         string containing the effect type
#         varnum
#         logical value telling if double filtering is enabled (TRUE or FALSE)
#         limits for double filtering, p-value first

make_volcanoplot <- function(dframe,effect_type,varnum,double_filter,
                             df_p_lim = NULL, df_effect_lim = NULL){
  
  # The variable label(s) are added to tooltip info
  # The tooltip info is included in dummy aesthetics label1-5
  if (varnum == 1){
    p <- ggplot(dframe, aes(label1 = label))
  }
  if(varnum == 2){
    p <- ggplot(dframe, aes(label1 = var_label1, label2 = var_label2))
  }

  # OR and FC require log2 transformation before plotting
  # If double filtering is enabled, the points that pass the filtering will be colored red
  # Hover tooltip includes: Variable label(s), effect, p_fdr and n
  if (effect_type %in% c("OR","FC")){
    if (double_filter){
      dframe <- dframe %>% mutate(df = as.factor(p_fdr < df_p_lim & abs(log2(effect)) > df_effect_lim))
      
      # The ggplot object needs to be redone since dframe has been altered
      if (varnum == 1){
        p <- ggplot(dframe, aes(label1 = label))
      }
      if(varnum == 2){
        p <- ggplot(dframe, aes(label1 = var_label1, label2 = var_label2))
      }
      p <- p +
        geom_point(aes(x = log2(effect), y = -log10(p_fdr),color = df,
                       label3 = effect, label4 = p_fdr, label5 = n)) +
        scale_colour_manual(breaks = c("TRUE","FALSE"),values = c("TRUE" = "red", "FALSE" = "grey"),
                            guide = guide_legend(title = NULL)) +
        xlab(paste("log2(",effect_type,")",sep = ""))
    }
    else{
      p <- p +
        geom_point(aes(x = log2(effect), y = -log10(p_fdr), label3 = effect, label4 = p_fdr, label5 = n)) +
        xlab(paste("log2(",effect_type,")",sep = ""))
    }
  }
  # Identical to above, except x = effect instead of x = log2(effect)
  # This could be simplified
  else{
    if (double_filter){
      dframe <- dframe %>% mutate(doubfilt = as.factor(p_fdr < df_p_lim & abs(effect) > df_effect_lim))
      p <- ggplot(dframe, aes(x = effect, y = -log10(p_fdr))) +
        geom_point(aes(color = doubfilt)) +
        scale_color_manual(breaks = c("TRUE", "FALSE"), values = c("grey", "red")) +
        xlab("Correlation")
    }
    else{
      p <- ggplot(dframe, aes(x = effect, y = -log10(p_fdr))) +
        geom_point() +
        xlab("Correlation")
    }
  }
  
  p <- p + theme_minimal()
  
  if (varnum == 1){
    p <- ggplotly(p, tooltip = c("label1","label3","label4","label5"))
  }
  if(varnum == 2){
    p <- ggplotly(p, tooltip = c("label1","label2","label3","label4","label5"))
  }
  
  p
}

# Source: https://gist.github.com/rentrop/d39a8406ad8af2a1066c

gg_qq <- function(df,effect_type,varnum,ci = 0.95){
  
  x <- df$effect
  
  df$test <- 1
  
  if (effect_type %in% c("OR","FC")){
    x <- log2(x)
    ylabel <- paste("log2(",effect_type,")",sep = "")
  }
  else{
    ylabel <- "Correlation"
  }
  
  x <- na.omit(x)
  ord <- order(x)
  n <- length(x)
  P <- ppoints(length(x))
  df$ord.x <- x[ord]
  df$z <- qnorm(P)
  
  Q.x <- quantile(df$ord.x, c(0.25, 0.75))
  Q.z <- qnorm(c(0.25, 0.75))
  b <- diff(Q.x)/diff(Q.z)
  coef <- c(Q.x[1] - b * Q.z[1], b)
  
  
  zz <- qnorm(1 - (1 - ci)/2)
  SE <- (coef[2]/dnorm(df$z)) * sqrt(P * (1 - P)/n)
  fit.value <- coef[1] + coef[2] * df$z
  df$upper <- fit.value + zz * SE
  df$lower <- fit.value - zz * SE
  
  
  p <- ggplot(df, aes(x=z, y=ord.x)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    xlab("Normal quantiles") + ylab(ylabel) +
    geom_abline(intercept = coef[1], slope = coef[2]) +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) +
    scale_fill_gradient(low = "grey40", high = "grey40")
  
  if (varnum == 1){
    p <- p + geom_point(aes(label1 = label, label3 = effect, label4 = p_fdr, label5 = n))
    p <- ggplotly(p, tooltip = c("label1","label3","label4","label5"))
  }
  if(varnum == 2){
    p <- p + geom_point(aes(label1 = var_label1, label2 = var_label2, label3 = effect, label4 = p_fdr, label5 = n))
    p <- ggplotly(p, tooltip = c("label1","label2","label3","label4","label5"))
  }
  
  p
}

gg_qqplot <- function(df, varnum, ci = 0.95){
  ps <- df$p_fdr
  n <- length(ps)
  df$observed <- -log10(sort(ps))
  df$expected = -log10(1:n/n)
  df$cupper = -log10(qbeta(ci,     1:n, n - 1:n + 1))
  df$clower = -log10(qbeta(1- ci,  1:n, n - 1:n + 1))
  p <- ggplot(df) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    geom_abline(intercept = 0, slope = 1, alpha = 0.5) +
    geom_ribbon(aes(x = expected, ymin = clower, ymax = cupper), alpha = 0.2) +
    xlab(expression(paste("Expected -log"[10], plain(P)))) +
    ylab(expression(paste("Observed -log"[10], plain(P))))
  
  if(varnum == 2){
    p <- p + geom_point(aes(x=expected, y=observed,label1 = var_label1, label2 = var_label2, label3 = effect, label4 = p_fdr, label5 = n))
    p <- ggplotly(p, tooltip = c("label1","label2","label3","label4","label5"))
  }
  if (varnum == 1){
    p <- p + geom_point(aes(x=expected, y=observed,label1 = label, label3 = effect, label4 = p_fdr, label5 = n))
    p <- ggplotly(p, tooltip = c("label1","label3","label4","label5"))
  }
  
  p
}



