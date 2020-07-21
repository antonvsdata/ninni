
### COMMON INFORMATION FOR ALL THE VISUALIZATION FUNCTIONS ####
# -------------------------------------------------------------------------------------------------------
# the information showed in the tooltip box on mouse hover is included as dummy aesthetics label1, label2 ..
# the above causes new versions of ggplot2 to throw warnings, which can be ignored
#-------------------------------------------------------------------------------------------------------

# Volcano plot with double filtering
# Input:  data frame with effect, p_fdr and point labels
#         string containing the effect type
#         varnum
#         double_filter: boolean telling if double filtering is enabled (TRUE or FALSE)
#         df_p_lim: double filtering limit for p-value
#         fdr: boolean, TRUE: p-limit is for P_FDR FALSE: p-limit is for P
#         df_effect_lim: double filtering limit for effect
plot_volcano <- function(dframe, log2_effect, effect_type, varnum, double_filter, df_p_lim = NULL,
                         fdr = NULL, df_effect_lim = NULL, eff_limit_log2 = FALSE, shape){
  
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
        dframe <- dframe %>% mutate(df = ifelse(P_FDR < df_p_lim & abs(log2(Effect)) > df_effect_lim,
                                                "Pass", "Fail"))
      }
      else{
        dframe <- dframe %>% mutate(df = ifelse(P < df_p_lim & abs(log2(Effect)) > df_effect_lim,
                                                "Pass", "Fail"))
      }
    } else {
      if (fdr){
        dframe <- dframe %>% mutate(df = ifelse(P_FDR < df_p_lim & abs(Effect) > df_effect_lim,
                                                "Pass", "Fail"))
      }
      else{
        dframe <- dframe %>% mutate(df = ifelse(P < df_p_lim & abs(Effect) > df_effect_lim,
                                                "Pass", "Fail"))
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
    x_label <- paste0("log2(", effect_type, ")")
    x_lims <- c(-max(abs(log2(dframe$Effect))),max(abs(log2(dframe$Effect))))
  }
  else{
    x_axis <- "Effect"
    x_label <- effect_type
    x_lims <- c(-max(abs(dframe$Effect)),max(abs(dframe$Effect)))
  }
  
  if (varnum == 1){
    p <- ggplot(dframe, aes(label1 = Dataset, label2 = Variable1, label3 = Description1,
                            label4 = Effect, label5 = P, label6 = P_FDR, label7 = N))
  }
  if(varnum == 2){
    p <- ggplot(dframe, aes(label1 = Dataset, label2 = Variable1, label3 = Variable2, label4 = Description1,
                            label5 = Description2, label6 = Effect, label7 = P, label8 = P_FDR, label9 = N))
  }
  if(shape){
    point_shape <- "Dataset"
  }
  else{
    point_shape <- NULL
  }
  p <- p +
    geom_point(aes_string(x = x_axis, y = "-log10(P)", color = coloring, shape = point_shape)) +
    scale_colour_manual(breaks = c("TRUE","FALSE"), values = c("Pass" = "red", "Fail" = "grey"),
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
      ylabel <- paste0("log2(", effect_type, ")")
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
    b <- diff(Qx) / diff(Qz)
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
      dframe[,color_col] <- as.factor(dframe[, color_col])
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
                                   label4 = "Effect", label5 = "P", label6 = "P_FDR", label7 = "N"))
  }
  if(varnum == 2){
    p <- p + geom_point(aes_string(color = color_col, label1 = "Dataset", label2 = "Variable1",
                                   label3 = "Variable2", label4 = "Description1",
                                   label5 = "Description2", label6 = "Effect", label7 = "P",
                                   label8 = "P_FDR", label9 = "N"))
  }
  p
}


# Lady Manhattan plot
# The y-axis of a traditional Manhattan plot, -log10(p) is multiplied by the sign of the effect
# The plot can be colored by chosen column
lady_manhattan_plot <- function(dframe, x_axis, log2_effect, effect_type, varnum,
                                color_col = NULL, color_type = NULL){
  
  dframe$P <- zero_to_min(dframe$P)
  # For OR and FC, use log2 effect
  if(log2_effect){
    dframe <- dframe %>% mutate(Y = -log10(P) * sign(log2(Effect)))
    y_label <- paste0("-log10(P) * sign(log2(", effect_type, "))")
  }
  else{
    dframe <- dframe %>% mutate(Y = -log10(P) * sign(Effect))
    y_label <- paste0("-log10(P) * sign(", effect_type, ")")
  }
  if (x_axis == "Variables together") {
    # For datasets with interactions, the combinations of variables are used as x-axis
    if(varnum == 1){
      x_axis <- "Variable1"
      x_label <- "Variable"
    }
    if(varnum == 2){
      dframe <- dframe %>% mutate(X = paste(Variable1, Variable2, sep = "_x_"))
      x_axis <- "X"
      x_label <- "Variables"
    }
    
  } else {
    x_label <- x_axis
  }
  
  # Color is discretised by changing the coloring column to factor
  if(!is.null(color_col)){
    if(color_type == "Discrete"){
      dframe[, color_col] <- as.factor(dframe[, color_col])
    }
  }
  
  p <- ggplot(dframe, aes_string(x = x_axis,y = "Y", color = color_col)) +
    theme_minimal() +
    theme(panel.grid.major = element_blank(),
          panel.grid.minor = element_blank()) +
    labs(x = x_label, y = y_label)
  
  if (class(dframe[,x_axis]) %in% c("character", "factor")) {
    n_unique <-length(unique(dframe[, x_axis]))
    if (n_unique > 40) {
      x_breaks <- sort(dframe[, x_axis])[seq(1, nrow(dframe), length.out = 40)]
      p <- p +
        scale_x_discrete(breaks = x_breaks)
    }
    p <- p +
      theme(axis.text.x = element_text(angle = 90))
  }
  
  # Use color scale from colorbrewer when possible
  if(!is.null(color_col) && class(dframe[, color_col]) %in% c("character", "factor") &&
     length(unique(dframe[, color_col])) <= 12){
    p <- p +
      scale_color_brewer(type = "qual", palette = "Paired")
  }
  
  if (varnum == 1){
    p <- p + geom_point(aes(label1 = Dataset, label2 = Variable1, label3 = Description1,
                            label4 = Effect, label5 = P, label6 = P_FDR, label7 = N))
  }
  if(varnum == 2){
    p <- p + geom_point(aes(label1 = Dataset, label2 = Variable1, label3 = Variable2, label4 = Description1,
                            label5 = Description2, label6 = Effect, label7 = P,
                            label8 = P_FDR, label9 = N))
  } 

  p
}


lollipop_plot <- function(dframe, main_col, n_top,
                          color_col = NULL, color_type = NULL) {
  
  if (main_col == "Variables together") {
    c1 <- dframe %>%
      group_by(Variable1) %>%
      summarize(n1 = n())
    c2 <- dframe %>%
      filter(!is.na(Variable2)) %>%
      group_by(Variable2) %>%
      summarize(n2 = n())
    counts <- full_join(c1, c2, by = c("Variable1" = "Variable2"))
    counts$n1[is.na(counts$n1)] <- 0
    counts$n2[is.na(counts$n2)] <- 0
    counts$count <- counts$n1 + counts$n2
    main_col <- "Variable1"
  } else {
    counts <- dframe[!is.na(dframe[, main_col]), ] %>%
      group_by(!!sym(main_col)) %>%
      summarize(count = n())
  }
  
  counts <- counts[order(counts$count, decreasing = TRUE), ][seq(min(n_top, nrow(counts))), ]
  counts <- as.data.frame(counts)
  counts[, main_col] <- factor(counts[, main_col], levels = rev(unique(counts[, main_col])))
  
  p <- ggplot(counts, aes_string(y = "count", x = main_col)) +
    geom_segment(aes_string(x = main_col, xend = main_col, y = 0, yend="count"), color="grey") +
    geom_point(aes_string(label1 = main_col, label2 = "count"), size=3, color="#69b3a2") +
    coord_flip() +
    labs(x = "", y = "Count") +
    theme_minimal()
  
  p
  
}


upset_plot <- function(dframe, group, main_col, n_top,
                       order_by, text_scale, show_empty) {
  
  groups <- unique(dframe[, group])
  if (length(groups) < 2) {
    return(NULL)
  }
  
  if (main_col == "Variables together") {
    main_col <- "Variable"
    df1 <- dframe[!is.na(dframe$Variable1), c(group, "Variable1")]
    df2 <- dframe[!is.na(dframe$Variable2), c(group, "Variable2")]
    colnames(df1) <- colnames(df2) <- c(group, "Variable")
    dframe <- rbind(df1, df2)
  }
  dframe <- dframe[!is.na(dframe[, main_col]), ]
  
  list_input <- lapply(groups, function(g){
    unique(dframe[dframe[, group] == g, main_col])
  })
  names(list_input) <- groups
  
  order_by <- list("Degree & Frequency" = c("freq", "degree"),
                   "Frequency" = "freq")[[order_by]]
  if (show_empty) {
    show_empty <- "on"
  } else {
    show_empty <- NULL
  }
  
  upset(fromList(list_input), nintersects = n_top,
        text.scale = text_scale,
        order.by = order_by,
        empty.intersections = show_empty)
}


p_histogram <- function(dframe, facet = NULL) {
  # Custom breaks for the x-axis
  breaks <- seq(0, 1, by = 0.05)
  
  finite_count <- sum(is.finite(dframe$P))
  h_line <- finite_count/(length(breaks)-1)
  
  
  p <- ggplot(dframe, aes(P)) +
    geom_histogram(breaks = breaks, col = "grey50", fill = "grey80", size = 1) +
    labs(x = "p-value", y = "Frequency") +
    theme_minimal() +
    theme(plot.title = element_text(face="bold", hjust=0.5)) +
    geom_hline(yintercept = h_line, color="red", linetype = "dashed", size = 1)
  
  if (!is.null(facet) && facet != "") {
    p <- p + facet_wrap(facet, ncol = 1)
  }
  p
}
