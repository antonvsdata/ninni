library(dplyr)
library(ggplot2)

# Plots a heatmap: Variable labels on axes and colouring by effect. If effect type is "OR" or "FC", uses log2-transformation
# Input:  dataframe with variable_label1, variable_label2 and effect
#         string containing the effect type
make_heatmap <- function(dframe,effect_type){
  
  if (effect_type %in% c("OR","FC")){
    dframe$effect <- log2(dframe$effect)
  }
  p <- ggplot(dframe, aes(variable_label1,variable_label2))
  
  p + geom_tile(aes(fill = effect) , colour = "white") +
    scale_fill_gradient2(low = "blue", mid = "white", high = "red", midpoint = 0, space = "Lab")+
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
  
  p <- ggplot(dframe, aes(sample = effect))
  
  p + geom_qq(distribution = dstr)
}

