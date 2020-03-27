
### COMMON INFORMATION FOR ALL THE VISUALIZATION FUNCTIONS ####
# -------------------------------------------------------------------------------------------------------
# interactive argument is used to toggle between interactive plotly plots and normal static ggplots
# the information showed in the tooltip box on mouse hover is included as dummy aesthetics label1, label2 ..
# the above causes new versions of ggplot2 to throw warnings, which can be ignored
# shiny also displays warnings about excplicit widget IDs when working with plotly, but these can be ignored as well
#-------------------------------------------------------------------------------------------------------

# Volcano plot with double filtering
# Input:  data frame with effect, p_fdr and point labels
#         string containing the effect type
#         varnum
#         double_filter: boolean telling if double filtering is enabled (TRUE or FALSE)
#         df_p_lim: double filtering limit for p-value
#         fdr: boolean, TRUE: p-limit is for P_FDR FALSE: p-limit is for P
#         df_effect_lim: double filtering limit for effect
plot_volcano <- function(dframe, log2_effect, effect_type, varnum, double_filter,
                             df_p_lim = NULL, fdr = NULL, df_effect_lim = NULL, eff_limit_log2 = FALSE,
                             shape){
  
  if (log2_effect && any(dframe$Effect < 0)) {
    stop("Negative values can't be log-transformed")
  }
  # The points with p_fdr = 0 would not be plotted,
  # so they are replaced with 1e-300
  dframe$P <- lapply(dframe$P, function(x){if(x == 0) x = 1e-300 else x}) %>% unlist()
  # Create column for double filtering coloring
  if (double_filter){
    if (eff_limit_log2){
      df_effect_lim <- as.numeric(df_effect_lim)
    }
    else{
      df_effect_lim <- log2(as.numeric(df_effect_lim))
    }
    if (log2_effect) {
      
      if (fdr){
        dframe <- dframe %>% mutate(df = ifelse(P_FDR < df_p_lim & abs(log2(Effect)) > df_effect_lim, "Pass", "Fail"))
      }
      else{
        dframe <- dframe %>% mutate(df = ifelse(P < df_p_lim & abs(log2(Effect)) > df_effect_lim, "Pass", "Fail"))
      }
    } else {
      if (fdr){
        dframe <- dframe %>% mutate(df = ifelse(P_FDR < df_p_lim & abs(Effect) > df_effect_lim, "Pass", "Fail"))
      }
      else{
        dframe <- dframe %>% mutate(df = ifelse(P < df_p_lim & abs(Effect) > df_effect_lim, "Pass", "Fail"))
      }
    }
    dframe$df <- factor(dframe$df, levels = c("Pass", "Fail"))
    coloring <- "df"
  }
  else{
    coloring <- NULL
  }
  # OR and FC require log2 transformation before plotting
  # Set x axis labels and limits for symmetrical plot in terms of zero
  if (log2_effect) {
    
    x_axis <- "log2(Effect)"
    x_label <- paste("log2(",effect_type,")",sep = "")
    x_lims <- c(-max(abs(log2(dframe$Effect))),max(abs(log2(dframe$Effect))))
  }
  else{
    x_axis <- "Effect"
    x_label <- effect_type
    x_lims <- c(-max(abs(dframe$Effect)),max(abs(dframe$Effect)))
  }
  
  if (varnum == 1){
    p <- ggplot(dframe, aes(label1 = Dataset, label2 = Variable1, label3 = Description1,
                            label4 = Effect, label5 = P_FDR, label6 = N))
  }
  if(varnum == 2){
    p <- ggplot(dframe, aes(label1 = Dataset, label2 = Variable1, label3 = Variable2, label4 = Description1,
                            label5 = Description2, label6 = Effect, label7 = P_FDR, label8 = N))
  }
  if(shape){
    point_shape <- "Dataset"
  }
  else{
    point_shape <- NULL
  }
  p <- p +
    geom_point(aes_string(x = x_axis, y = "-log10(P)", color = coloring, shape = point_shape)) +
    scale_colour_manual(breaks = c("TRUE","FALSE"),values = c("Pass" = "red", "Fail" = "grey"),
                        guide = guide_legend(title = NULL)) +
    xlim(x_lims[1],x_lims[2]) +
    xlab(x_label) +
    theme_minimal()
  
  p
}

# Normal Q_Q plot with confidence bands
# This function is a modified version of the one presented in
# https://gist.github.com/rentrop/d39a8406ad8af2a1066c
qq_normal <- function(dframe, log2_effect, effect_type,varnum,ci = 0.95, color_col = NULL, color_type = NULL, interactive = TRUE){
  
  x <- dframe$Effect
  dframe <- dframe %>% arrange(Effect)
  
  if (log2_effect){
    if (any(x < 0)) {
      stop("Negative values can't be log-transformed")
    }
    x <- log2(x)
    ylabel <- paste("log2(",effect_type,")",sep = "")
  }
  else{
    ylabel <- effect_type
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
  if(!is.null(color_col)){
    if(color_type == "Discrete"){
      dframe[,color_col] <- as.factor(dframe[,color_col])
    }
  }
  p <- ggplot(dframe, aes(x=z, y=ord.x)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    xlab("Normal quantiles") + ylab(ylabel) +
    geom_abline(intercept = coef[1], slope = coef[2], color = "red") +
    geom_ribbon(aes(ymin = lower, ymax = upper), alpha=0.2) +
    scale_fill_gradient(low = "grey40", high = "grey40")
  
  if(!is.null(color_col) & class(dframe[, color_col]) %in% c("character", "factor") & length(unique(dframe[, color_col])) <= 12){
    p <- p +
      scale_color_brewer(type = "qual", palette = "Paired")
  }
  # Interactivity can be disabled
  if (interactive){
    if (varnum == 1){
      p <- p + geom_point(aes_string(color = color_col, label1 = "Dataset", label2 = "Variable1", label3 = "Description1",
                              label4 = "Effect", label5 = "P_FDR", label6 = "N"))
      p <- ggplotly(p, tooltip = paste("label",1:6,sep=""))
    }
    if(varnum == 2){
      p <- p + geom_point(aes_string(color = color_col, label1 = "Dataset", label2 = "Variable1", label3 = "Variable2", label4 = "Description1",
                              label5 = "Description2", label6 = "Effect", label7 = "P_FDR", label8 = "N"))
      p <- ggplotly(p, tooltip = paste("label",1:8,sep=""))
    } 
  }
  else{
    p <- p + geom_point()
  }
  p
}

# Q-Q plot of p-values versus expected p-values with confidence bands
qq_pvalues <- function(dframe, varnum, ci = 0.95, color_col = NULL, color_type = NULL, interactive = TRUE){
  dframe <- dframe %>% arrange(P)
  # The points with p_fdr = 0 would not be plotted,
  # so they are replaced with 1e-300
  dframe$P <- lapply(dframe$P, function(x){if(x == 0) x = 1e-300 else x}) %>% unlist()
  
  # Calculate expected p-values
  n <- nrow(dframe)
  dframe$observed <- -log10(dframe$P)
  dframe$expected <- -log10(1:n/n)
  dframe$cupper <- -log10(qbeta(ci,     1:n, n - 1:n + 1))
  dframe$clower <- -log10(qbeta(1- ci,  1:n, n - 1:n + 1))
  
  if(!is.null(color_col)){
    if(color_type == "Discrete"){
      dframe[,color_col] <- as.factor(dframe[,color_col])
    }
  }
  
  # Create ggplot object
  p <- ggplot(dframe, aes(x=expected, y=observed)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    geom_abline(intercept = 0, slope = 1, color = "red") +
    geom_ribbon(aes(x = expected, ymin = clower, ymax = cupper), alpha = 0.2) +
    xlab("Expected - log10(P) from uniform distribution") +
    ylab("Observed - log10(P)")
  
  if(!is.null(color_col) & class(dframe[, color_col]) %in% c("character", "factor") & length(unique(dframe[, color_col])) <= 12){
    p <- p +
      scale_color_brewer(type = "qual", palette = "Paired")
  }
  
  if (interactive){
    if (varnum == 1){
      p <- p + geom_point(aes_string(color = color_col, label1 = "Dataset", label2 = "Variable1", label3 = "Description1",
                                     label4 = "Effect", label5 = "P_FDR", label6 = "N"))
      p <- ggplotly(p, tooltip = paste("label",1:6,sep=""))
    }
    if(varnum == 2){
      p <- p + geom_point(aes_string(color = color_col, label1 = "Dataset", label2 = "Variable1", label3 = "Variable2", label4 = "Description1",
                                     label5 = "Description2", label6 = "Effect", label7 = "P_FDR", label8 = "N"))
      p <- ggplotly(p, tooltip = paste("label",1:8,sep=""))
    } 
  }
  else{
    p <- p + geom_point()
  }
  p
}

# Lady Manhattan plot
# The y-axis of a traditional Manhattan plot, -log10(p) is multiplied by the sign of the effect
# The plot can be colored by chosen column
lady_manhattan_plot <- function(dframe, log2_effect, effect_type,varnum, color_col = NULL, color_type = NULL){
  # For OR and FC, use log2 effect
  if(log2_effect){
    dframe <- dframe %>% mutate(Y = -log10(P) * sign(log2(Effect)))
    y_label <- paste("-log10(P) * sign(log2(", effect_type ,"))",sep="")
  }
  else{
    dframe <- dframe %>% mutate(Y = -log10(P) * sign(Effect))
    y_label <- paste("-log10(P) * sign(", effect_type ,")",sep="")
  }
  # For datasets with interactions, the combinations of variables are used as x-axis
  if(varnum == 1){
    x_axis <- "Variable1"
    x_label <- "Variable"
    x_breaks <- sort(dframe$Variable1)[seq(1,nrow(dframe),length.out = 40)]
  }
  if(varnum == 2){
    dframe <- dframe %>% mutate(X = paste(Variable1,Variable2,sep="_x_"))
    x_axis <- "X"
    x_label <- "Variables"
    x_breaks <- sort(dframe$X)[seq(1,nrow(dframe),length.out = 40)]
  }
  # Color is discretised by changing the coloring column to factor
  if(!is.null(color_col)){
    if(color_type == "Discrete"){
      dframe[,color_col] <- as.factor(dframe[,color_col])
    }
  }
  
  p <- ggplot(dframe, aes_string(x = x_axis,y = "Y", color = color_col)) +
    scale_x_discrete(breaks = x_breaks) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank(),
          axis.text.x = element_text(angle = 90)) +
    labs(x = x_label, y = y_label)
  
  # Use color scale from colorbrewer when possible
  if(!is.null(color_col) & class(dframe[, color_col]) %in% c("character", "factor") & length(unique(dframe[, color_col])) <= 12){
    p <- p +
      scale_color_brewer(type = "qual", palette = "Paired")
  }
  
  if (varnum == 1){
    p <- p + geom_point(aes(label1 = Dataset, label2 = Variable1, label3 = Description1,
                            label4 = Effect, label5 = P_FDR, label6 = N))
  }
  if(varnum == 2){
    p <- p + geom_point(aes(label1 = Dataset, label2 = Variable1, label3 = Variable2, label4 = Description1,
                            label5 = Description2, label6 = Effect, label7 = P_FDR, label8 = N))
  } 

  p
}