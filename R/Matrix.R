## Sets up data for matrix layout and adjusts names of sets if they are too small
## Essentially strips uneeded columns, converts data to matrix, and adjusts the labels to appropriate length
## i.e. if the labels were one letter each, appropriate space is added to make it fit and look neat
Create_matrix <- function(data){
  Matrix_setup <- as.matrix(t(data[ , 1:(length(data) -3)]))
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

## Take adjusted (if they needed to be) names of sets for matrix y-axis tick labels
## Used to add set names to matrix ggplot
Make_labels <- function(setup){
  names <- rownames(setup)
  return(names)
}

## Takes matrix setup data and turns it into grid format (binary)
## 1's represent dark circles, 0's light, and if x-value has multiple 1's they are connected.
Create_layout <- function(setup, mat_color, mat_col, matrix_dot_alpha){
  Matrix_layout <- expand.grid(y=seq(nrow(setup)), x=seq(ncol(setup)))
  Matrix_layout <- data.frame(Matrix_layout, value = as.vector(setup))
  for(i in 1:nrow(Matrix_layout)){
    if(Matrix_layout$value[i] > as.integer(0)){
      Matrix_layout$color[i] <- mat_color
      Matrix_layout$alpha[i] <- 1
      Matrix_layout$Intersection[i] <- paste(Matrix_layout$x[i], "yes", sep ="")
    }
    else{
      
      Matrix_layout$color[i] <- "gray83"
      Matrix_layout$alpha[i] <- matrix_dot_alpha
      Matrix_layout$Intersection[i] <- paste(i, "No", sep = "")
    } 
  }
  if(is.null(mat_col) == F){
    for(i in 1:nrow(mat_col)){
      mat_x <- mat_col$x[i]
      mat_color <- as.character(mat_col$color[i])
      for(i in 1:nrow(Matrix_layout)){
        if((Matrix_layout$x[i] == mat_x) && (Matrix_layout$value[i] != 0)){
          Matrix_layout$color[i] <- mat_color
        }
      }
    }
  }
  return(Matrix_layout)
}

## Create data set to shade matrix 
MakeShading <- function(Mat_data, color){
  y <- unique(Mat_data$y)
  y <- (y[which(y %% 2 != 0)])
  data <- data.frame(cbind(y))
  data$min <- 0
  data$max <- (max(Mat_data$x) + 1)
  for( i in 1:length(y)){
    data$y_min[i] <- ((y[i]) - 0.5)
    data$y_max[i] <- ((y[i]) + 0.5)
  }
  data$shade_color <- color
  return(data)
}

## Generate matrix plot
Make_matrix_plot <- function(Mat_data,Set_size_data, Main_bar_data, point_size, line_size, name_size, labels,
                             shading_data, shade_alpha){
  
  Matrix_plot <- (ggplot() 
                  + theme(panel.background = element_rect(fill = "white"),
                          plot.margin=unit(c(-0.2,0.5,0.5,0.5), "lines"),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.ticks.y = element_blank(),
                          axis.text.y = element_text(colour = "gray0", 
                                                     size = name_size, hjust = 0.4),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank())
                  + xlab(NULL) + ylab("   ")
                  + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                       limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                       labels = labels, expand = c(0,0))
                  + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0))
                  + geom_rect(data = shading_data, aes_string(xmin = "min", xmax = "max",
                                                              ymin = "y_min", ymax = "y_max"),
                              fill = shading_data$shade_color, alpha = shade_alpha)
                  + geom_point(data= Mat_data, aes_string(x= "x", y= "y"), colour = Mat_data$color,
                               size= point_size, alpha = Mat_data$alpha)
                  + geom_line(data= Mat_data, aes_string(group = "Intersection", x="x", y="y",
                                                         colour = "color"), size = line_size)
                  + scale_color_identity())
  Matrix_plot <- ggplot_gtable(ggplot_build(Matrix_plot))
  return(Matrix_plot)
}