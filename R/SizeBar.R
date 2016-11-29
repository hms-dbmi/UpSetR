## Find frequency of each set for set size bar plot
FindSetFreqs <- function(data, start_col, num_sets, set_names, keep_order){
  end_col <- as.numeric(((start_col + num_sets) -1))
  temp_data <- data[ ,start_col:end_col]
  temp_data <- temp_data[set_names]
  temp_data <- as.data.frame(colSums(temp_data))
  colnames(temp_data) <- c("y")
  if(keep_order == FALSE){
  temp_data <- temp_data[order(temp_data$y, decreasing = T), ]
  }
  else{
    temp_data <- temp_data$y
  }
  x <- seq(1:num_sets)
  temp_data <- cbind(temp_data, x)
  colnames(temp_data) <- c("y", "x")
  return(as.data.frame(temp_data))
}

log10_reverse_trans <- function(){
  trans <- function(x) -log(x, 10)
  inv <- function(x) (10 ^ -x)
  trans_new(paste0("reverselog2-", format(2), "reverse"), trans, inv,
            log_breaks(base = 10), domain = c(1e-100, Inf))
}

log2_reverse_trans <- function(){
  trans <- function(x) -log(x, 2)
  inv <- function(x) (2 ^ -x)
  trans_new(paste0("reverselog2-", format(2), "reverse"), trans, inv,
            log_breaks(base = 2), domain = c(1e-100, Inf))
}

## Generate set size plot
Make_size_plot <- function(Set_size_data, sbar_color, ratios, ylabel, scale_sets, text_scale, set_size_angle){
#   if(ratios[1] < 0.4){
#     m <- (-0.05)
#   }
#   else if((ratios[1] > 0.4) & (ratios[1] < 0.46)){
#     m <- (-0.03)
#   }
#   else{
#     m <- 0
#   }
  
  if(length(text_scale) > 1 && length(text_scale) <= 6){
    x_axis_title_scale <- text_scale[3]
    x_axis_tick_label_scale <- text_scale[4]
  }
  else{
    x_axis_title_scale <- text_scale
    x_axis_tick_label_scale <- text_scale
  }
  
  if(ylabel == "Set Size" && scale_sets != "identity"){
    ylabel <- paste("Set Size", paste0("( ", scale_sets, " )"))
    if(scale_sets == "log2"){
      Set_size_data$y <- log2(Set_size_data$y)
    }
    if(scale_sets == "log10"){
      Set_size_data$y <- log10(Set_size_data$y)
    }
  }
  
  Size_plot <- (ggplot(data = Set_size_data, aes_string(x ="x", y = "y"))
                + geom_bar(stat = "identity",colour = sbar_color, width = 0.4,
                           fill = sbar_color, position = "identity")
                + scale_x_continuous(limits = c(0.5, (nrow(Set_size_data)+0.5)),
                                     breaks = c(0, max(Set_size_data)),
                                     expand = c(0,0))
                + theme(panel.background = element_rect(fill = "white"),
                        plot.margin=unit(c(-0.11,-1.3,0.5,0.5), "lines"),
                        axis.title.x = element_text(size = 8.3*x_axis_title_scale),
                        axis.text.x = element_text(size = 7*x_axis_tick_label_scale, angle = set_size_angle,
                                                   vjust = 1, hjust = 0.5),
                        axis.line = element_line(colour = "gray0"),
                        axis.line.y = element_blank(),
                        axis.line.x = element_line(colour = "gray0", size = 0.3),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank())
                + xlab(NULL) + ylab(ylabel)
                + coord_flip())
  
  if(scale_sets == "log10"){
    Size_plot <- (Size_plot + scale_y_continuous(trans = log10_reverse_trans()))
  }
  else if (scale_sets == "log2"){
    Size_plot <- (Size_plot + scale_y_continuous(trans = log2_reverse_trans()))
  }
  else{
    Size_plot <- (Size_plot + scale_y_continuous(trans = "reverse"))
  }
  
  Size_plot <- ggplot_gtable(ggplot_build(Size_plot))
  return(Size_plot)
}

