#' Base plot for UpSetR
#' 
#' Takes intersections from most abundant sets in a data set and produces a matrix
#' layout of the intersections, along with plotting their corresponding sizes and 
#' the size of each set used.
#' @param data Data set
#' @param first_col First column in data set that represents a set
#' @param last_col Last column in data set that represents a set
#' @param nsets Number of sets to look at
#' @param nintersects Number of intersections to plot
#' @export
Upset_Base <- function(data, first_col, last_col, nsets, nintersects){
  My_data <- data
  start_col <- first_col
  end_col <- last_col
  num_sets <- nsets
  num_intersections <- nintersects
  TopFreq <- FindMostFreq(My_data, start_col, end_col, num_sets)
  Sets_to_remove <- Remove(My_data, start_col, end_col, TopFreq)
  New_data <- Wanted(My_data, Sets_to_remove)
  Num_of_set <- Number_of_sets(TopFreq)
  All_Freqs <- Counter(New_data, Num_of_set, start_col, TopFreq, num_intersections)
  Matrix_setup <- Create_matrix(All_Freqs)
  labels <- Make_labels(Matrix_setup)
  Matrix_layout <- Create_layout(Matrix_setup)
  Set_sizes <- FindSetFreqs(New_data, start_col, Num_of_set)
  Make_base_plot(All_Freqs, Matrix_layout, Set_sizes, labels)
}

FindMostFreq <- function(data, start_col, end_col, n_sets){  
  temp_data <- data[ ,start_col:end_col]
  temp_data <- colSums(temp_data)
  temp_data <- as.data.frame(temp_data)
  temp_data <- tail(temp_data[order(temp_data[ ,"temp_data"]), , drop = F], as.integer(n_sets))
  temp_data <- rev(row.names(temp_data))
  return(temp_data)
}

Remove <- function(data, start_col, end_col, sets){
  temp_data <- as.data.frame(data[ , start_col:end_col])
  Unwanted_sets <- colnames(temp_data[ ,!(colnames(temp_data) %in% sets), drop = F])
}

Wanted <- function(data, unwanted_sets){
  temp_data <- (data[ ,!(colnames(data) %in% unwanted_sets), drop = F])
}

Number_of_sets <- function(sets){
  temp <- length(sets)
  return(temp)
}

Counter <- function(data, num_sets, start_col, name_of_sets, nintersections){
  temp_data <- list()
  Freqs <- data.frame()
  end_col <- as.numeric(((start_col + num_sets) -1))
  for( i in 1:num_sets){
    temp_data[i] <- match(name_of_sets[i], colnames(data))
  }
  Freqs <- count(data[ ,as.integer(temp_data)])
  Freqs <- Freqs[!(rowSums(Freqs[ ,1:num_sets]) == 0), ]
  Freqs <- Freqs[order(Freqs$freq, decreasing = T), ]
  for( i in 1:nrow(Freqs)){
    Freqs$x[i] <- i
  }
  Freqs <- Freqs[1:nintersections, ]
  Freqs <- na.omit(Freqs)
  return(Freqs)
}

Create_matrix <- function(data){
  Matrix_setup <- as.matrix(t(data[ , 1:(length(data) -2)]))
  names <- rownames(Matrix_setup)
  max <- max(nchar(names))
  if( max < 7)
  {
    Spaces <- list()
    for(i in 1:nrow(Matrix_setup)){
      Name_length <- nchar(names[i])
      Spaces_needed <- (6 - (Name_length))
      Spaces[i] <- paste(replicate(Spaces_needed, " "), collapse = "")
      rownames(Matrix_setup)[i] <- paste(as.character(Spaces[i]), names[i], collapse = "")
    }
    rownames(Matrix_setup) <- gsub(x = rownames(Matrix_setup), pattern = "\\.", replacement = " ")
  }
  return(Matrix_setup)
}

Make_labels <- function(setup){
  names <- rownames(setup)
  return(names)
}

Create_layout <- function(setup){
  Matrix_layout <- expand.grid(y=seq(nrow(setup)), x=seq(ncol(setup)))
  Matrix_layout <- data.frame(Matrix_layout, value = as.vector(setup))
  for(i in 1:nrow(Matrix_layout)){
    if(Matrix_layout$value[i] == as.integer(1)){
      Matrix_layout$color[i] <- "gray23"
      Matrix_layout$Intersection[i] <- paste(Matrix_layout$x[i], "yes", sep ="")
    }
    else{
      Matrix_layout$color[i] <- "gray92"
      Matrix_layout$Intersection[i] <- paste(i, "No", sep = "")
    } 
  }
  return(Matrix_layout)
}

FindSetFreqs <- function(data, start_col, num_sets){
  end_col <- as.numeric(((start_col + num_sets) -1))
  temp_data <- data[ ,start_col:end_col]
  temp_data <- as.data.frame(colSums(temp_data))
  temp_data <- temp_data[order(temp_data, decreasing = T), ]
  x <- seq(1:num_sets)
  temp_data <- cbind(temp_data, x)
  colnames(temp_data) <- c("y", "x")
  return(as.data.frame(temp_data))
}

Make_base_plot <- function(Main_bar_data, Mat_data, Set_size_data, labels){
  Main_bar_plot <- (ggplot(data = Main_bar_data, aes(x = x, y = freq)) 
                    + geom_bar(stat = "identity", colour = "gray23", width = 0.4)
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0),
                                         breaks = NULL)
                    + xlab(NULL) + ylab("Intersection Size")
                    + theme(panel.background = element_rect(fill = "white"),  
                            plot.margin=unit(c(0.2,0.2,0.2,0.2), "cm"),
                            panel.border = element_blank(),
                            axis.title.y = element_text(vjust = 0.5))
                    + geom_vline(xintercept = 0, size = 1, colour = "gray0")
                    + geom_hline( yintercept = 0, colour = "gray0")
                    + geom_text(aes(label = freq), size = 3, vjust = -0.4))
  
  Matrix_plot <- (ggplot(data=Mat_data, aes(x= x, y= y)) 
                  + geom_point(colour = Mat_data$color, size=4) 
                  + geom_line(aes(group = Intersection), size = 1, colour = "gray23")
                  + theme(panel.background = element_rect(fill = "white"),
                          plot.margin=unit(c(-0.1,0.2,0.1,0.2), "cm"),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.text.y = element_text(colour = "gray0"),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank())
                  + xlab(NULL) + ylab("   ")
                  + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                       limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                       labels = labels)
                  + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0)))
  
  Main_bar_plot <- ggplotGrob(Main_bar_plot)
  Matrix_plot <- ggplotGrob(Matrix_plot)
  Main_bar_plot$widths <- Matrix_plot$widths
  
  Size_plot <- (ggplot(data = Set_size_data, aes(x =x, y = y))
                + geom_bar(stat = "identity",colour = "dodgerblue", width = 0.4,
                           fill = "dodgerblue", position = "identity")
                + scale_x_continuous(limits = c(0.5, (nrow(Set_size_data)+0.5)),
                                     breaks = c(0, max(Set_size_data)))
                + theme(panel.background = element_rect(fill = "white"),
                        plot.margin=unit(c(0,0.2,0.1,0.2), "cm"),
                        axis.title.x = element_text(size = 10, face = "bold"),
                        axis.line = element_line(colour = "gray0"),
                        axis.line.y = element_line(colour = "white"),
                        axis.text.y = element_blank(),
                        axis.ticks.y = element_blank(),
                        panel.grid.minor = element_blank(),
                        panel.grid.major = element_blank())
                + xlab(NULL) + ylab("Set Size")
                + coord_flip()
                + scale_y_reverse())
  
  
  Size_plot <- ggplot_gtable(ggplot_build(Size_plot))
  Matrix_plot$heights <- Size_plot$heights 
  grid.newpage()
  pushViewport(viewport(layout = grid.layout(100,100)))
  vplayout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = c(0.7, 0.3)), vp = vplayout(1:100, 21:100))
  print(arrangeGrob(Size_plot), vp = vplayout(71:100, 1:20))
}