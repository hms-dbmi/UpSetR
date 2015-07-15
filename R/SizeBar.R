## Find frequency of each set for set size bar plot
FindSetFreqs <- function(data, start_col, num_sets, set_names){
  end_col <- as.numeric(((start_col + num_sets) -1))
  temp_data <- data[ ,start_col:end_col]
  temp_data <- temp_data[set_names]
  temp_data <- as.data.frame(colSums(temp_data))
  x <- seq(1:num_sets)
  temp_data <- cbind(temp_data, x)
  colnames(temp_data) <- c("y", "x")
  return(as.data.frame(temp_data))
}

## Generate set size plot
Make_size_plot <- function(Set_size_data, sbar_color, ratios){
  if(ratios[2] < 0.46){
    m <- 0.4
  }
  else if((ratios[2] > 0.45) & (ratios[2] < 0.66)){
    m <- 0.35
  }
  else{
    m <- 0.3
  }
  Size_plot <- (ggplot(data = Set_size_data, aes_string(x ="x", y = "y"))
                + geom_bar(stat = "identity",colour = sbar_color, width = 0.4,
                           fill = sbar_color, position = "identity")
                + scale_x_continuous(limits = c(0.5, (nrow(Set_size_data)+0.5)),
                                     breaks = c(0, max(Set_size_data)),
                                     expand = c(0,0))
                + theme(panel.background = element_rect(fill = "white"),
                        plot.margin=unit(c(-m,-1.3,0.5,0.5), "lines"),
                        axis.title.x = element_text(size = 10, face = "bold"),
                        axis.line = element_line(colour = "gray0"),
                        axis.line.y = element_blank(),
                        axis.line.x = element_line(colour = "gray0", size = 0.3),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank())
                + xlab(NULL) + ylab("Set Size")
                + coord_flip() 
                + scale_y_reverse())
  
  Size_plot <- ggplot_gtable(ggplot_build(Size_plot))
  return(Size_plot)
}

