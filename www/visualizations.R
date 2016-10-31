
# Combines two matrices
coalesce<-function(...) {
  x<-lapply(list(...), function(z) {if (is.factor(z)) as.character(z) else z})
  m<-is.na(x[[1]])
  i<-2
  while(any(m) & i<=length(x)) {
    if ( length(x[[i]])==length(x[[1]])) {
      x[[1]][m]<-x[[i]][m]
    } else if (length(x[[i]])==1) {
      x[[1]][m]<-x[[i]]
    } else {
      stop(paste("length mismatch in argument",i," - found:", length( x[[i]] ),"expected:",length( x[[1]] ) ))
    }
    m<-is.na(x[[1]])
    i<-i+1
  }
  return(x[[1]])
}

get_heatmap_lowertri <- function(dframe,effect_type,clustering, interactive){
  
  dat <- dframe %>% select(Variable1,Variable2,Effect)
  dat$Variable1 <- as.character(dat$Variable1)
  dat$Variable2 <- as.character(dat$Variable2)
  
  # This makes sure all the variables will be included in both axes
  vars <- c(dat$Variable1,dat$Variable2) %>% unique()
  x <- setdiff(vars,dat$Variable1)
  y <- setdiff(vars,dat$Variable2)
  append_len <- max(length(x),length(y))
  append_df <- data.frame(Variable1 = c(x,rep(NA, append_len - length(x))),
                          Variable2 = c(y,rep(NA, append_len - length(y))),
                          Effect = NA)
  dat <- rbind(dat,append_df)
  
  # Converting data into wide format and tidying data
  dat_w <- dat %>% dcast(Variable1 ~ Variable2, value.var = "Effect") %>% filter(!is.na(Variable1))
  rownames(dat_w) <- dat_w$Variable1
  dat_w <- dat_w %>% select(-Variable1)
  dat_w <- dat_w[rownames(dat_w)!="NA",colnames(dat_w)!="NA"] #one of the columns or rows is named NA
  dat_w <- dat_w[rev(order(names(dat_w))),rev(order(names(dat_w)))] #this makes the image lie on the lower triangular
  
  # dat_w only has one-directional interactions
  # for example, metformine vs morphine = 1.09, but morphine vs metformine = NA
  # transpose values are added to make the matrix symmetrical
  dat_w_t <- data.frame(t(dat_w))
  dat_w_whole <- coalesce(dat_w,dat_w_t)
  
  if (clustering){
    dat_w_whole_zeros <- dat_w_whole
    if (effect_type %in% c("OR","FC")){
      dat_w_whole_zeros[is.na(dat_w_whole_zeros)] <- 1
    }
    else{
      dat_w_whole_zeros[is.na(dat_w_whole_zeros)] <- 0
    }
    hc <- hclust(dist(dat_w_whole_zeros))
    dat_w_whole <- dat_w_whole[hc$order,hc$order]
  }
  
  # Only half of the associations are needed for plotting
  dat_w_whole[upper.tri(dat_w_whole)] <- NA
  # Diagonal should be included in the plot
  diag(dat_w_whole) <- 0
  
  # Melt back to long format for ggplot2
  dat_w_whole$Variable1 <- rownames(dat_w_whole)
  dat_l <- na.omit(melt(dat_w_whole, "Variable1", variable.name = "Variable2")) %>% rename(Effect = value)
  
  # Setting the factor levels to correctly draw the heatmap
  # This ensures the tiles are plotted in correct order to make a lower triangular heat map
  if (clustering){
    dat_l$Variable1 <- dat_l$Variable1 %>% as.character() %>% 
      factor(levels = rev(colnames(dat_w_whole)))
    dat_l$Variable2 <- dat_l$Variable2 %>% as.character() %>% 
      factor(levels = rownames(dat_w_whole))
  }
  
  # Creating the ggplot object
  p <- ggplot(dat_l, aes(x = Variable1, y = Variable2)) +
    scale_fill_gradient2(low = "steelblue", mid = "white", high = "red", midpoint = 0, space = "Lab") +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    xlab("") + ylab("")
  
  if (effect_type %in% c("OR","FC")){
    p <- p + geom_tile(aes(fill = log2(Effect)))
  }
  else{
    p <- p + geom_tile(aes(fill = Effect))
  }
  
  if (interactive){
    
    #!!!!Add other information HERE !!!!#
    p <- p +
      theme(axis.text.x = element_text(angle = 90))
    ggplotly(p)
  }
  else{
    p <- p +
      theme(axis.text.x = element_blank(),
            axis.text.y = element_blank(),
            axis.ticks = element_blank())
    p
  }
}


# Volcano plot with double filtering
# Input:  data frame with effect, p_fdr and point labels
#         string containing the effect type
#         varnum
#         boolean telling if double filtering is enabled (TRUE or FALSE)
#         limits for double filtering, p-value first
#         fdr: boolean, TRUE: p-limit is for P_FDR FALSE: p-limit is for P

make_volcanoplotly <- function(dframe,effect_type,varnum,double_filter,
                             df_p_lim = NULL, fdr = NULL, df_effect_lim = NULL){
  # The points with p_fdr = 0 would not be plotted,
  # so they are replaced with 1e-300
  dframe$P <- lapply(dframe$P, function(x){if(x == 0) x = 1e-300 else x}) %>% unlist()
  # The variable label(s) are added to tooltip info
  # Other tooltip info is included in dummy aesthetics label*
  if (varnum == 1){
    p <- ggplot(dframe, aes(label0 = Variable, label1 = Description))
  }
  if(varnum == 2){
    p <- ggplot(dframe, aes(label00 = Variable1, label0 = Variable2, label1 = Description1, label2 = Description2))
  }
  # OR and FC require log2 transformation before plotting
  
  # If double filtering is enabled, the points that pass the filtering will be colored red
  # Hover tooltip includes: Variable label(s), effect, p_fdr and n
  if (effect_type %in% c("OR","FC")){
    if (double_filter){
      if (fdr){
        dframe <- dframe %>% mutate(df = as.factor(P_FDR < df_p_lim & abs(log2(Effect)) > df_effect_lim))
      }
      else{
        dframe <- dframe %>% mutate(df = as.factor(P < df_p_lim & abs(log2(Effect)) > df_effect_lim))
      }
      # The ggplot object needs to be redone since dframe has been altered
      if (varnum == 1){
        p <- ggplot(dframe, aes(label0 = Variable,label1 = Description))
      }
      if(varnum == 2){
        p <- ggplot(dframe, aes(label00 = Variable1, label0 = Variable2, label1 = Description1, label2 = Description2))
      }
      p <- p +
        geom_point(aes(x = log2(Effect), y = -log10(P),color = df,
                       label3 = Effect, label4 = P_FDR, label5 = N)) +
        scale_colour_manual(breaks = c("TRUE","FALSE"),values = c("TRUE" = "red", "FALSE" = "grey"),
                            guide = guide_legend(title = NULL))
    }
    else{
      p <- p +
        geom_point(aes(x = log2(Effect), y = -log10(P), label3 = Effect, label4 = P_FDR, label5 = N))
    }
    p <- p +
      xlab(paste("log2(",effect_type,")",sep = "")) +
      xlim(-max(abs(log2(dframe$Effect))),max(abs(log2(dframe$Effect))))
  }
  # Identical to above, except x = effect instead of x = log2(effect)
  # This could be simplified
  else{
    p <- ggplot(dframe, aes(x = Effect, y = -log10(P))) +
      xlab("Correlation") +
      xlim(-max(dframe$Effect),max(dframe$Effect))
    if (double_filter){
      dframe <- dframe %>% mutate(df = as.factor(P_FDR < df_p_lim & abs(Effect) > df_effect_lim))
      p <- p +
        geom_point(aes(color = df)) +
        scale_color_manual(breaks = c("TRUE", "FALSE"), values = c("grey", "red"))
    }
    else{
      p <- p +
        geom_point()
    }
  }
  # Supresses excess background, speeds up the function
  p <- p + theme_minimal()
  # Plotly makes the figure interactive
  if (varnum == 1){
    p <- ggplotly(p, tooltip = c("label0","label1","label3","label4","label5"))
  }
  if(varnum == 2){
    p <- ggplotly(p, tooltip = c("label00","label0","label1","label2","label3","label4","label5"))
  }
  p
}

volcano_static <- function(dframe,effect_type,varnum,double_filter,
                           df_p_lim = NULL, fdr = NULL, df_effect_lim = NULL){
  dframe$P <- lapply(dframe$P, function(x){if(x == 0) x = 1e-300 else x}) %>% unlist()
  
  
  # OR and FC require log2 transformation before plotting
  # If double filtering is enabled, the points that pass the filtering will be colored red
  # Hover tooltip includes: Variable label(s), effect, p_fdr and n
  if (effect_type %in% c("OR","FC")){
    if (double_filter){
      if (fdr){
        dframe <- dframe %>% mutate(df = as.factor(P_FDR < df_p_lim & abs(log2(Effect)) > df_effect_lim))
      }
      else{
        dframe <- dframe %>% mutate(df = as.factor(P < df_p_lim & abs(log2(Effect)) > df_effect_lim))
      }
      
      p <- ggplot(dframe) +
        geom_point(aes(x = log2(Effect), y = -log10(P),color = df)) +
        scale_colour_manual(breaks = c("TRUE","FALSE"),values = c("TRUE" = "red", "FALSE" = "grey"),
                            guide = guide_legend(title = NULL)) +
        xlab(paste("log2(",effect_type,")",sep = ""))
    }
    else{
      p <- ggplot(dframe) +
        geom_point(aes(x = log2(Effect), y = -log10(P))) +
        xlab(paste("log2(",effect_type,")",sep = ""))
    }
  }
  
  # Supresses excess background, speeds up the function
  p <- p + theme_minimal()
  p
}

# This function is a modified version of the one presented in
# https://gist.github.com/rentrop/d39a8406ad8af2a1066c
qq_normal <- function(dframe,effect_type,varnum,ci = 0.95,interactive = TRUE){
  
  x <- dframe$Effect
  dframe <- dframe %>% arrange(Effect)
  
  if (effect_type %in% c("OR","FC")){
    x <- log2(x)
    ylabel <- paste("log2(",effect_type,")",sep = "")
  }
  else{
    ylabel <- "Correlation"
  }
  #Missing values are removed and the effect vector is ordered
  x <- na.omit(x)
  n <- length(x)
  P <- ppoints(length(x))
  dframe$ord.x <- sort(x, decreasing = FALSE)
  dframe$z <- qnorm(P)
  # The coefficients for the line
  Q.x <- quantile(dframe$ord.x, c(0.25, 0.75))
  Q.z <- qnorm(c(0.25, 0.75))
  b <- diff(Q.x)/diff(Q.z)
  coef <- c(Q.x[1] - b * Q.z[1], b)
  # The values for the confidence band
  zz <- qnorm(1 - (1 - ci)/2)
  SE <- (coef[2]/dnorm(dframe$z)) * sqrt(P * (1 - P)/n)
  fit.value <- coef[1] + coef[2] * dframe$z
  dframe$upper <- fit.value + zz * SE
  dframe$lower <- fit.value - zz * SE
  # Grid and background color is omited to boost performance,
  # the line and the confidence band is added
  p <- ggplot(dframe, aes(x=z, y=ord.x)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    xlab("Normal quantiles") + ylab(ylabel) +
    geom_abline(intercept = coef[1], slope = coef[2], color = "red") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) +
    scale_fill_gradient(low = "grey40", high = "grey40")
  # Interactivity can be disabled
  if (interactive){
    if (varnum == 1){
      p <- p + geom_point(aes(label0 = Variable, label1 = Description, label3 = Effect, label4 = P_FDR, label5 = N))
      p <- ggplotly(p, tooltip = c("label0","label1","label3","label4","label5"))
    }
    if(varnum == 2){
      p <- p + geom_point(aes(label00 = Variable1, label0 = Variable2, label1 = Description1, label2 = Description2, label3 = Effect, label4 = P_FDR, label5 = N))
      p <- ggplotly(p, tooltip = c("label00","label0","label1","label2","label3","label4","label5"))
    } 
  }
  else{
    p <- p + geom_point()
  }
  p
}

qq_pvalues <- function(dframe, varnum, ci = 0.95, interactive = TRUE){
  dframe <- dframe %>% arrange(P)
  # The points with p_fdr = 0 would not be plotted,
  # so they are replaced with 1e-300
  dframe$P <- lapply(dframe$P, function(x){if(x == 0) x = 1e-300 else x}) %>% unlist()
  

  n <- nrow(dframe)
  dframe$observed <- -log10(dframe$P)
  dframe$expected <- -log10(1:n/n)
  dframe$cupper <- -log10(qbeta(ci,     1:n, n - 1:n + 1))
  dframe$clower <- -log10(qbeta(1- ci,  1:n, n - 1:n + 1))
  p <- ggplot(dframe) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    geom_ribbon(aes(x = expected, ymin = clower, ymax = cupper), alpha = 0.2) +
    xlab("Expected - log10(P)") +
    ylab("Observed - log10(P)")
  
  if (interactive){
    if(varnum == 2){
      p <- p + geom_point(aes(x=expected, y=observed,label00 = Variable1, label0 = Variable2, label1 = Description1, label2 = Description2, label3 = Effect, label4 = P_FDR, label5 = N))
      p <- ggplotly(p, tooltip = c("label00","label0","label1","label2","label3","label4","label5"))
    }
    if (varnum == 1){
      p <- p + geom_point(aes(x=expected, y=observed,label0 = Variable, label1 = Description, label3 = Effect, label4 = P_FDR, label5 = N))
      p <- ggplotly(p, tooltip = c("label0","label1","label3","label4","label5"))
    }
  }
  else{
    p <- p + geom_point(aes(x=expected, y=observed))
  }
  p
}