
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
  # so they are replaced with minimum observed p-value
  dframe$P <- zero_to_min(dframe$P)
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

# helper function for dealing with zero p-values
zero_to_min <- function(x) {
  ifelse(x == 0, min(x[x != 0], na.rm = TRUE), x)
}

gg_qq <- function(dframe, variable, log2_effect, effect_type, varnum, ci = 0.95,
                    color_col = NULL, color_type = NULL) {
  
  if (color_col == "") color_col <- NULL
  
  if (variable == "P") {
    dframe <- dframe %>%
      filter(!is.na(P)) %>%
      arrange(P)
    # The points with p = 0 would not be plotted,
    # so they are replaced with the minimum of p-values
    n <- nrow(dframe)
    dframe$observed <- -log10(zero_to_min(dframe$P))
    # Expected p-values from uniform distribution
    dframe$expected <- -log10(1:n/n)
    # Confidence intervals
    dframe$cupper <- -log10(qbeta(ci,     1:n, n - 1:n + 1))
    dframe$clower <- -log10(qbeta(1- ci,  1:n, n - 1:n + 1))
    
    # The coefficients for the line
    coef <- c(0, 1)
    
    # Axis labels
    xlabel <- "Expected -log10(P)"
    ylabel <- "Observed - log10(P)"
  } else {  # Effect
    dframe <- dframe %>%
      filter(!is.na(Effect)) %>%
      arrange(Effect)
    if (log2_effect){
      if (any(dframe$Effect < 0)) {
        stop("Negative values can't be log-transformed")
      }
      dframe$observed <- log2(dframe$Effect)
      ylabel <- paste("log2(",effect_type,")",sep = "")
    }
    else{
      dframe$observed <- dframe$Effect
      ylabel <- effect_type
    }
    xlabel <- "Expected normal quantiles"
    
    n <- nrow(dframe)
    P <- ppoints(n)
    dframe$expected <- qnorm(P)
    
    # The coefficients for the line
    Qx <- quantile(dframe$observed, c(0.25, 0.75))
    Qz <- qnorm(c(0.25, 0.75))
    b <- diff(Qx)/diff(Qz)
    coef <- c(Qx[1] - b * Qz[1], b)
    
    
    # The values for the confidence band
    zz <- qnorm(1 - (1 - ci)/2)
    SE <- (coef[2]/dnorm(dframe$expected)) * sqrt(P * (1 - P)/n)
    fit.value <- coef[1] + coef[2] * dframe$expected
    dframe$cupper <- fit.value + zz * SE
    dframe$clower <- fit.value - zz * SE
    
    # Axis labels
    xlabel <- "Standard normal quantiles"
    ylabel <- "Observed values"
  }
  
  if(!is.null(color_col)){
    if(color_type == "Discrete"){
      dframe[,color_col] <- as.factor(dframe[,color_col])
    }
  }
  # Create ggplot object
  p <- ggplot(dframe, aes(x = expected, y = observed)) +
    theme(panel.grid.major = element_blank(), panel.grid.minor = element_blank(),
          panel.background = element_blank(), axis.line = element_line(colour = "black")) +
    geom_abline(intercept = coef[1], slope = coef[2], color = "red") +
    geom_ribbon(aes(x = expected, ymin = clower, ymax = cupper), alpha = 0.2) +
    labs(x = xlabel, y = ylabel)
  
  
  if(!is.null(color_col) && class(dframe[, color_col]) %in% c("character", "factor") &&
     length(unique(dframe[, color_col])) <= 12){
    p <- p +
      scale_color_brewer(type = "qual", palette = "Paired")
  }
  
  if (varnum == 1){
    p <- p + geom_point(aes_string(color = color_col, label1 = "Dataset", label2 = "Variable1",
                                   label3 = "Description1",
                                   label4 = "Effect", label5 = "P_FDR", label6 = "N"))
  }
  if(varnum == 2){
    print("2 triggered")
    print(colnames(dframe))
    p <- p + geom_point(aes_string(color = color_col, label1 = "Dataset", label2 = "Variable1",
                                   label3 = "Variable2", label4 = "Description1",
                                   label5 = "Description2", label6 = "Effect",
                                   label7 = "P_FDR", label8 = "N"))
    print("2 over")
  } 
  print("yep")
  p
}


# Lady Manhattan plot
# The y-axis of a traditional Manhattan plot, -log10(p) is multiplied by the sign of the effect
# The plot can be colored by chosen column
lady_manhattan_plot <- function(dframe, log2_effect, effect_type,varnum, color_col = NULL, color_type = NULL){
  
  dframe$P <- zero_to_min(dframe$P)
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