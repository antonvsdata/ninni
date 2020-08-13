
### COMMON INFORMATION FOR ALL THE VISUALIZATION FUNCTIONS ####
# -------------------------------------------------------------------------------------------------------
# the information showed in the tooltip box on mouse hover is included as dummy aesthetics label1, label2 ..
# the above causes new versions of ggplot2 to throw warnings, which can be ignored
#-------------------------------------------------------------------------------------------------------

#' Volcano plot with double filtering
#' 
#' Plot a volcano plot with optional double filtering, i.e. setting a limit to both p-value and effect
#' size and highlighting points that satisfy the criteria.
#' 
#' @param dframe association data frame
#' @param log2_effect logical, should the effect size be plotted on a log2 axis?
#' @param effect_type character, the effect type (used as x-axis label)
#' @param varnum numeric, number of variables in the dataset
#' @param double_filter logical, whether to apply filters on p-value and effect size
#' @param df_p_lim numeric, limit for p-value in double filtering
#' @param p_adj logical, should p-value limit be applied on adjusted p-values instead of raw p-values
#' @param df_effectlim numeric, limit for effect in double filtering
#' @param eff_limit_log2 logical, is the effect limit given in the log2 space?
#' @param shape logical, should the points be shaped by dataset?
#' 
#' @return ggplot object
plot_volcano <- function(dframe, log2_effect, effect_type, varnum, double_filter, df_p_lim = NULL,
                         p_adj = NULL, df_effect_lim = NULL, eff_limit_log2 = FALSE, shape){
  
  if (log2_effect && any(dframe$Effect < 0)) {
    stop("Negative values can't be log-transformed")
  }
  # The points with p_adj = 0 would not be plotted,
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
      
      if (p_adj){
        dframe <- dframe %>% mutate(df = ifelse(P_adj < df_p_lim & abs(log2(Effect)) > df_effect_lim,
                                                "Pass", "Fail"))
      }
      else{
        dframe <- dframe %>% mutate(df = ifelse(P < df_p_lim & abs(log2(Effect)) > df_effect_lim,
                                                "Pass", "Fail"))
      }
    } else {
      if (p_adj){
        dframe <- dframe %>% mutate(df = ifelse(P_adj < df_p_lim & abs(Effect) > df_effect_lim,
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
                            label4 = Effect, label5 = P, label6 = P_adj, label7 = N))
  }
  if(varnum == 2){
    p <- ggplot(dframe, aes(label1 = Dataset, label2 = Variable1, label3 = Variable2, label4 = Description1,
                            label5 = Description2, label6 = Effect, label7 = P, label8 = P_adj, label9 = N))
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

#' Q-Q plot
#' 
#' Plot a Q-Q plot of -log10 p-values or normal Q-Q plot of effect sizes with confidence intervals
#' 
#' @param dframe association data frame
#' @param variable either "P" or "Effect", Q-Q plot of p-values of effect sizes
#' @param log2_effect logical, should the effect size be log2 transformed
#' @param effect_type character, the effect type (used as x-axis label)
#' @param varnum numeric, number of variables in the dataset
#' @param ci confidence level for the confidence intervals
#' @param color_col column for point color
#' @param color_type "Continuous" or "Discrete". If Discrete, color column is converted to factor
#' for a discrete color scale
#' 
#' @return ggplot object
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
     length(unique(dframe[, color_col])) <= 9){
    p <- p +
      scale_color_brewer(type = "qual", palette = "Set1")
  }
  
  if (varnum == 1){
    p <- p + geom_point(aes_string(color = color_col, label1 = "Dataset", label2 = "Variable1",
                                   label3 = "Description1",
                                   label4 = "Effect", label5 = "P", label6 = "P_adj", label7 = "N"))
  }
  if(varnum == 2){
    p <- p + geom_point(aes_string(color = color_col, label1 = "Dataset", label2 = "Variable1",
                                   label3 = "Variable2", label4 = "Description1",
                                   label5 = "Description2", label6 = "Effect", label7 = "P",
                                   label8 = "P_adj", label9 = "N"))
  }
  p
}

#' Lady Manhattan plot
#' 
#' The y-axis of a traditional Manhattan plot, -log10(p) is multiplied by the sign of the effect
#' The plot can be colored by chosen column.
#' 
#' @param dframe association data frame
#' @param x_axis the column name for x-axis of the plot
#' @param log2_effect logical, should the effect size be log2 transformed
#' @param effect_type character, the effect type (used as x-axis label)
#' @param varnum numeric, number of variables in the dataset
#' @param color_col column for point color
#' @param color_type "Continuous" or "Discrete". If Discrete, color column is converted to factor
#' for a discrete color scale
#' 
#' @return ggplot object
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
  
  color_col <- empty_to_null(color_col)
  
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
  
  if (class(dframe[, x_axis]) %in% c("character", "factor")) {
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
  if(!is.null(color_col) && class(dframe[, color_col]) %in% c("character", "factor")) {
    if (length(unique(dframe[, color_col])) <= 9){
      p <- p +
        scale_color_brewer(type = "qual", palette = "Set1")
    }
  }
  
  if (varnum == 1){
    p <- p + geom_point(aes(label1 = Dataset, label2 = Variable1, label3 = Description1,
                            label4 = Effect, label5 = P, label6 = P_adj, label7 = N))
  }
  if(varnum == 2){
    p <- p + geom_point(aes(label1 = Dataset, label2 = Variable1, label3 = Variable2, label4 = Description1,
                            label5 = Description2, label6 = Effect, label7 = P,
                            label8 = P_adj, label9 = N))
  }

  p
}

#' Lollipop plot
#' 
#' A lollipop plot of most common observations in chosen column(s). E.g. for variables
#' finds the most common variables.
#' 
#' @param dframe association data frame
#' @param x_axis the column name for x-axis of the plot,
#' or "Variables together" to combine Variable1 and Variable2
#' @param n_top the number of top observations to show
#' 
#' @return ggplot object
lollipop_plot <- function(dframe, main_col, n_top) {
  
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

#' Lollipop plot
#' 
#' A lollipop plot of most common observations in chosen column(s). E.g. for variables
#' finds the most common variables.
#' 
#' @param dframe association data frame
#' @param group column name giving the sets
#' @param main_col the column name for observations,
#' or "Variables together" to combine Variable1 and Variable2
#' @param n_top the number of top intersections/sets to show
#' @param order_by either "Degree & Frequeny" or "Frequency". Former first shows
#' individual sets, then 2-way intersections etc. Latter only sorts by frequency
#' @param text_scale numeric, the scale for al lthe text in the plot
#' @param show_empty logical, whether to show empty sets & intersections
#' 
#' @return ggplot object
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

#' P-value histograms
#' 
#' Draws p-value histograms
#' 
#' @param dframe association data frame
#' @param facet column name to facet by, e.g. dataset
#' 
#' @return ggplot object
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
  
  facet <- empty_to_null(facet)
  if (!is.null(facet)) {
    p <- p + facet_wrap(facet, ncol = 1)
  }
  p
}

#' Density Ridge plot
#' 
#' Draws a density ridge plot of any column, separated by a factor
#' 
#' @param dframe association data frame
#' @param x,y x and y axis column names for the plot
#' @param x-log2 logical, apply log2-transformation to x-axis?
#' @param scale numeric, height of individual plots
#' @param style character, style of the plot
#' 
#' @return ggplot object
ridge_plot <- function(dframe, x, y, x_log2, scale, style) {
  
  params <- list(bw = list(size = 1.2, fill = NA),
                 grey = list(color = NA, fill = "grey50", alpha = 0.5),
                 colours = list(mapping = aes_string(color = y, fill = y), alpha = 0.5))
  
  p <- ggplot(dframe, aes_string(x, y)) +
    do.call(geom_density_ridges, c(list(scale = scale), params[[style]])) +
    theme_minimal()
  
  if (style == "colours") {
    if (length(unique(dframe[, y])) <= 9){
      p <- p +
        scale_color_brewer(type = "qual", palette = "Set1") +
        scale_fill_brewer(type = "qual", palette = "Set1")
    }
    p <- p +
      theme(legend.position = "none")
  }
  
  if (x_log2) {
    p <- p +
      scale_x_continuous(trans = "log2")
  }
  p
}

# Helper function
empty_to_null <- function(x) {
  if (is.null(x) || x %in% c("", "none")) {
    NULL
  } else {
    x
  }
}

#' Network plot
#' 
#' Creates a graph of the variables/outcomes and raws a static network plot.
#' 
#' @param dframe association data frame
#' @param type either "var_to_var" or "var_to_outcome"
#' @param layout layout method
#' @param node_names,names_repel logicals, draw and repel node names?
#' @param edge_color,edge_width,edge_weight column names to use as edge parameters
#' @param edge_width_range range of edge width
#' @param edge_color_log2,edge_width_log2,edge_weight_log2 logicals, apply log2 transformation to edge parameters
#' @param edge_color_scale edge color scale type, "Discrete", "Continuous" or "Diverging"
#' @param edge_color_midpoint midpoint for diverging scale
#' 
#' @return a list with graph = igraph object, plot = ggplot object made with ggraph
network_plot <- function(dframe, type, layout,
                         node_names, names_repel,
                         edge_color, edge_width, edge_weight,
                         edge_width_range,
                         edge_color_log2, edge_width_log2, edge_weight_log2,
                         edge_color_scale, edge_color_midpoint) {
  
  
  edge_color <- empty_to_null(edge_color)
  edge_width <- empty_to_null(edge_width)
  edge_weight <- empty_to_null(edge_weight)
  
  dframe$weight <- dframe[, edge_weight]
  dframe$width <- dframe[, edge_width]
  
  if (edge_width_log2) {
    dframe$width <- log2(dframe$width)
  }
  if (edge_weight_log2) {
    dframe$weight <- log2(dframe$weight)
    dframe[is.na(dframe$weight), "weight"] <- 0
  }
  
  
  if (type == "var_to_var") {
    graph_data <- dframe[!is.na(dframe$Variable2), c("Variable1", "Variable2", edge_color, edge_width)]
    
    g <- graph_from_data_frame(graph_data)
  } else if (type == "var_to_outcome") {
    graph_data <- dframe[, c("Variable1", "Dataset", edge_color, edge_width) ]
    if ("Variable2" %in% colnames(dframe)) {
      graph_data2 <- dframe[!is.na(dframe$Variable2), c("Variable2", "Dataset", edge_color, edge_width)]
      colnames(graph_data2)[1] <- "Variable1" 
      graph_data <- rbind(graph_data,
                          graph_data2)
    }
    
    vert <- data.frame(var = union(graph_data$Variable1, graph_data$Dataset))
    vert$outcome <- vert$var %in% dframe$Dataset
    
    g <- graph_from_data_frame(graph_data, vertices = vert)
  }
  
  set.seed(38)
  p <- ggraph(g, layout = layout) +
    theme_graph() +
    scale_edge_width(range = edge_width_range)
  
  # Get default fill scales
  
    
  if (!is.null(edge_color)) {
    p <- p +
      geom_edge_link0(aes_string(edge_color = edge_color, edge_width = edge_width))
    if (edge_color_scale == "Discrete") {
      p <- p + 
        scale_edge_color_brewer(palette = "Set1")
    } else if (edge_color_scale == "Continuous") {
      p <- p +
        scale_edge_color_viridis(trans = ifelse(edge_color_log2, "log2", "identity"))
    } else if (edge_color_scale == "Diverging") {
      p <- p + 
        scale_edge_color_gradient2(low = "#0571B0", mid = "#f0f0f0", high = "#CA0020",
                                   midpoint = edge_color_midpoint,
                                   trans = ifelse(edge_color_log2, "log2", "identity"))
    }
  } else {
    
    p <- p +
      geom_edge_link0(aes_string(edge_width = edge_width), edge_color = "grey70")
  }
  
  if (type == "var_to_var") {
    p <- p + 
      geom_node_point()
  } else if (type == "var_to_outcome") {
    p <- p + 
      geom_node_point(aes(size = outcome, color = outcome)) +
      scale_size_discrete(range = c(2, 32), guide = NULL) +
      scale_color_manual(values = c("black", "#FFA500"), guide = NULL)
  }
  if (node_names) {
      p <- p +
        geom_node_text(aes(label = name), repel = names_repel, point.padding = unit(0.2, "lines"))
  }
  
  return(list(graph = g, plot = p))
  
}

#' Network plot
#' 
#' Creates a graph of the variables/outcomes and raws a static network plot.
#' 
#' @param g igraph object
#' @param type either "var_to_var" or "var_to_outcome"
#' @param node_size,link_distance,font_size numerics, plot attributes
#' 
#' @return JS network plot
interactive_network <- function(g, type, node_size, link_distance, font_size) {
  if (type == "var_to_var") {
    group <- rep("A", length(V(g)))
    d3g <- igraph_to_networkD3(g, group = group)
    d3g$nodes$size <- node_size
    col_scale <- JS("d3.scaleOrdinal(d3.schemeCategory20);")
  } else if (type == "var_to_outcome") {
    group <- ifelse(V(g)$outcome, "outcome", "variable")
    d3g <- igraph_to_networkD3(g, group = group)
    d3g$nodes$size <- ifelse(d3g$nodes$group == "outcome", node_size * 3, node_size)
    col_scale <- JS("d3.scaleOrdinal(['000000', '#FFA500']);")
  }
  
  forceNetwork(Links = d3g$links, Nodes = d3g$nodes, 
               Source = 'source', Target = 'target', 
               NodeID = 'name', Group = 'group',
               Nodesize = "size", zoom = TRUE,
               linkDistance = link_distance, fontSize = font_size,
               colourScale = col_scale)
}
