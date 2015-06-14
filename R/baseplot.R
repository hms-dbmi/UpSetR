#' Base plot for UpSetR
#' 
#' Takes intersections from most abundant sets in a data set and produces a matrix
#' layout of the intersections, along with plotting their corresponding sizes and 
#' the size of each set used.
#' @param data Data set
#' @param first.col First column in data set that represents a set
#' @param last.col Last column in data set that represents a set
#' @param nsets Number of sets to look at
#' @param nintersects Number of intersections to plot
#' @param sets Specific sets to look at (Include as combinations. Ex: c("Name1", "Name2"))
#' @param matrix.color Color of the intersection points
#' @param main.bar.color Color of the main bar plot
#' @param sets.bar.color Color of set size bar plot
#' @param point.size Size of points in matrix plot
#' @param line.size Width of lines in matrix plot
#' @param name.size Size of set names in matrix plot
#' @param mb.ratio Ratio between matrix plot and main bar plot (Keep in terms of hundreths)
#' @param att.x Attribute entered as a string. If att.y is NULL a histogram will be produced. Best if data attached
#' @param att.y Attribute entered as a string. Produces a scatter plot vs. att.x. Best if data attached
#' @param expression Expression to subset attribute data entered as string (Ex: "ColName > 3"). Best if data attached
#' @param att.pos Position of attribute plot. If NULL plot will be at bottom. If "top" it will be above other plots
#' @param att.color Color of attribute plot bars or points
#' @param elements Specific elements to plot entered as a list. First element in list is name of attribute (string) followed by specific elements of attribute.
#' @param elements.color Color of element data on plot
#' @param order.matrix What the intersections in the matrix should be ordered by
#' @param show.numbers Show numbers of intersection sizes above bars 
#' @param aggregate.by How the data should be aggregated ("degree" or "sets")
#' @param cutoff The number of intersections from each set (to cut off at) when aggregating by sets
#' @param queries Unified querie of intersections and elements
#' @param query.title.plot Title of query plot
#' @param shade.color Color of row shading in matrix
#' @param shade.alpha Transparency of shading in matrix
#' @export
upset_base <- function(data, first.col, last.col, nsets = 5, nintersects = 40, sets = NULL,
                       matrix.color = "gray23",main.bar.color = "gray23", sets.bar.color = "dodgerblue",
                       point.size = 4, line.size = 1, name.size = 10, mb.ratio = c(0.70,0.30), att.x = NULL, 
                       att.y = NULL, expression = NULL, att.pos = NULL, att.color = "dodgerblue",
                       order.matrix = c("degree", "freq"), show.numbers = "yes", aggregate.by = "degree",
                       cutoff = NULL, queries = NULL, query.plot.title = "My Query Plot Title", 
                       shade.color = "skyblue", shade.alpha = 0.25){
  require(ggplot2);
  require(gridExtra);
  require(plyr);
  Set_names <- sets
  if(is.null(Set_names) == T){
    Set_names <- FindMostFreq(data, first.col, last.col, nsets)
  }
  
  Sets_to_remove <- Remove(data, first.col, last.col, Set_names)
  New_data <- Wanted(data, Sets_to_remove)
  Num_of_set <- Number_of_sets(Set_names)
  All_Freqs <- Counter(New_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                       rev(order.matrix), aggregate.by, cutoff)
  Matrix_setup <- Create_matrix(All_Freqs)
  labels <- Make_labels(Matrix_setup)
  
  # IntersectionBoxPlot(All_Freqs, Matrix_setup)
  
  
  if(is.null(queries) == F){
    Matrix_col <-  QuerieInterData(queries, New_data, first.col, Num_of_set, All_Freqs, expression, Set_names)
  }
  else{
    Matrix_col <- NULL
  }
  Matrix_layout <- Create_layout(Matrix_setup, matrix.color, Matrix_col)
  Set_sizes <- FindSetFreqs(New_data, first.col, Num_of_set)
  Bar_Q <- NULL
  if(is.null(queries) == F){
    Bar_Q <- QuerieInterBar(queries, New_data, first.col, Num_of_set, All_Freqs, expression, Set_names)
  }
  QInter_att_data <- NULL
  QElem_att_data <- NULL
  if(is.null(queries) == F){
    QInter_att_data <- QuerieInterAtt(New_data, first.col, queries, Num_of_set, att.x, att.y, expression, Set_names)
    QElem_att_data <- QuerieElemAtt(New_data, queries, first.col, expression, Set_names, att.x, att.y)
  }
  ShadingData <- MakeShading(Matrix_layout)
  Main_bar <- Make_main_bar(All_Freqs, Bar_Q, show.numbers, att.x, mb.ratio)
  Matrix <- Make_matrix_plot(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             name.size, labels, ShadingData, shade.color, shade.alpha)
  Sizes <- Make_size_plot(Set_sizes, sets.bar.color)
  Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
                 expression, att.pos, first.col, att.color, QElem_att_data, QInter_att_data, queries,
                 query.plot.title)
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

Subset_att <- function(data, exp){
  attach(data)
  express <- paste("data$", exp, sep = "")
  data <- data[which(eval(parse(text = express))), ]
  detach(data)
  return(data)
}

Number_of_sets <- function(sets){
  temp <- length(sets)
  return(temp)
}

Counter <- function(data, num_sets, start_col, name_of_sets, nintersections, mbar_color, order_mat,
                    aggregate, cut){
  temp_data <- list()
  Freqs <- data.frame()
  end_col <- as.numeric(((start_col + num_sets) -1))
  for( i in 1:num_sets){
    temp_data[i] <- match(name_of_sets[i], colnames(data))
  }
  Freqs <- count(data[ ,as.integer(temp_data)])
  Freqs <- Freqs[!(rowSums(Freqs[ ,1:num_sets]) == 0), ]
  if(tolower(aggregate) == "degree"){
    for(i in 1:nrow(Freqs)){
      Freqs$degree[i] <- rowSums(Freqs[ i ,1:num_sets])
    }
    order_cols <- list()
    for(i in 1:length(order_mat)){
      order_cols[i] <- match(order_mat[i], colnames(Freqs))
    }
    for(i in order_cols){
      if(i == (num_sets + 1)){
        logic <- T
      }
      else{
        logic <- F
      }
      Freqs <- Freqs[order(Freqs[ , i], decreasing = logic), ]
    }
  }
  else if(tolower(aggregate) == "sets")
  {
    Freqs <- Get_aggregates(Freqs, num_sets, order_mat, cut)
  }
  delete_row <- (num_sets + 2)
  Freqs <- Freqs[ , -delete_row]
  for( i in 1:nrow(Freqs)){
    Freqs$x[i] <- i
    Freqs$color <- mbar_color
  }
  Freqs <- Freqs[1:nintersections, ]
  Freqs <- na.omit(Freqs)
  return(Freqs)
}

Get_aggregates <- function(data, num_sets, order_mat, cut){
  temp_data <- list()
  set_agg <- list()
  for(i in 1:num_sets){
    temp_data <- data[which(data[ , i] == 1), ]
    for(i in 1:nrow(temp_data)){
      temp_data$degree[i] <- rowSums(temp_data[ i ,1:num_sets])
    }
    order_cols <- list()
    for(i in 1:length(order_mat)){
      order_cols[i] <- match(order_mat[i], colnames(temp_data))
    }
    for(i in order_cols){
      if(i == (num_sets + 1)){
        logic <- T
      }
      else{
        logic <- F
      }
      temp_data <- temp_data[order(temp_data[ , i], decreasing = logic), ]
    }
    if(is.null(cut) == F){
      temp_data <- temp_data[1:cut, ]
    }
    set_agg <- rbind(set_agg, temp_data)
  }
  return(set_agg)
}

OverlayEdit <- function(data1, data2, start_col, num_sets, intersects, exp, inter_color){
  end_col <- as.numeric(((start_col + num_sets) -1))
  set_cols <- data1[ ,start_col:end_col]
  temp_data <- data1[which(rowSums(data1[ ,start_col:end_col]) == length(intersects)), ]
  unwanted <- colnames(set_cols[ ,!(colnames(set_cols) %in% intersects), drop = F])
  temp_data <- (temp_data[ ,!(colnames(data1) %in% unwanted), drop = F])
  new_end <- ((start_col + length(intersects)) -1)
  if(new_end == start_col){
    temp_data <- temp_data[ which(temp_data[ ,start_col] == 1), ]
  }
  else{
    temp_data <- temp_data[which(rowSums(temp_data[ ,start_col:new_end]) == length(intersects)), ]
  }
  if(is.null(exp) == F){
    temp_data <- Subset_att(temp_data, exp)
  }
  temp_data <- na.omit(temp_data)
  other_data <- data2[which(rowSums(data2[ ,1:num_sets]) == length(intersects)), ]
  other_data <- (other_data[ ,!(colnames(data2) %in% unwanted), drop = F])
  if(new_end == start_col){
    
    other_data <- other_data[ which(other_data[ ,1] == 1), ]
  }
  else{
    other_data <- other_data[which(rowSums(other_data[ ,1:length(intersects)]) == length(intersects)), ]
  }
  row_num <- as.integer(other_data$x)
  overlay_row <- data2[row_num, ]
  new_freq <- nrow(temp_data)
  overlay_row$freq <- new_freq
  overlay_row$color <- inter_color
  return(overlay_row)
}

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

Make_labels <- function(setup){
  names <- rownames(setup)
  return(names)
}

Create_layout <- function(setup, mat_color, mat_col){
  Matrix_layout <- expand.grid(y=seq(nrow(setup)), x=seq(ncol(setup)))
  Matrix_layout <- data.frame(Matrix_layout, value = as.vector(setup))
  for(i in 1:nrow(Matrix_layout)){
    if(Matrix_layout$value[i] > as.integer(0)){
      Matrix_layout$color[i] <- mat_color
      Matrix_layout$Intersection[i] <- paste(Matrix_layout$x[i], "yes", sep ="")
    }
    else{
      Matrix_layout$color[i] <- "gray92"
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

GetElements <- function(data, elements){
  col_num <- match(elements[1], colnames(data))
  num_elem <- length(elements)
  elems <- as.character(elements[2:num_elem])
  temp_data <- data[data[ ,col_num] %in% elems, ]
  return(temp_data)
}

GetIntersects <- function(data, start_col, sets, num_sets){
  end_col <- as.numeric(((start_col + num_sets) -1))
  set_cols <- data[ ,start_col:end_col]
  temp_data <- data[which(rowSums(data[ ,start_col:end_col]) == length(sets)), ]
  unwanted <- colnames(set_cols[ ,!(colnames(set_cols) %in% sets), drop = F])
  temp_data <- (temp_data[ ,!(colnames(data) %in% unwanted), drop = F])
  new_end <- ((start_col + length(sets)) -1 )
  if(new_end == start_col){
    temp_data <- temp_data[ which(temp_data[ ,start_col] == 1), ]
    return(temp_data)
  }
  else{
    temp_data <- temp_data[ which(rowSums(temp_data[ ,start_col:new_end]) == length(sets)) , ]
    return(temp_data)
  }
}

Make_main_bar <- function(Main_bar_data, Q, show_num, att_x, ratios){
  if(is.null(Q) == F){
    inter_data <- Q
    if(nrow(inter_data) != 0){
      inter_data <- inter_data[order(inter_data$x), ]
    }
    else{
      inter_data <- NULL
    }
  }
  else{
    inter_data <- NULL
  }
  if(is.null(att_x) == T){
    b <- 0.71
  }
  else{
    inc <- (ratios[2] - 0.3)
    b <- (0.59 - (0.00735 * (inc *100)))
  }
  Main_bar_plot <- (ggplot(data = Main_bar_data, aes(x = x, y = freq)) 
                    + geom_bar(stat = "identity", colour = Main_bar_data$color, width = 0.6, 
                               fill = Main_bar_data$color)
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0),
                                         breaks = NULL)
                    + xlab(NULL) + ylab("Intersection Size")
                    + theme(panel.background = element_rect(fill = "white"),
                            plot.margin=unit(c(0.35,0.2,-b,0.2), "cm"), panel.border = element_blank(),
                            axis.title.y = element_text(vjust = 0.5)))
  if((show_num == "yes") || (show_num == "Yes")){
    Main_bar_plot <- (Main_bar_plot + geom_text(aes(label = freq), size = 3.0, vjust = -0.4, colour = Main_bar_data$color))
  }
  if(is.null(inter_data) == F){
    Main_bar_plot <- Main_bar_plot + geom_bar(data = inter_data,
                                              aes(x=x, y = freq), colour = inter_data$color, fill = inter_data$color,
                                              stat = "identity", width = 0.4)
    if((show_num == "yes") || (show_num == "Yes")){
      Main_bar_plot <- (Main_bar_plot + geom_text(data = inter_data, aes(label = freq), size = 3.0, 
                                                  vjust = -0.4, colour = inter_data$color))
    }
  }
  Main_bar_plot <- (Main_bar_plot 
                    + geom_vline(xintercept = 0, size = 1, colour = "gray0")
                    + geom_hline( yintercept = 0, colour = "gray0"))
  Main_bar_plot <- ggplotGrob(Main_bar_plot)
  return(Main_bar_plot)
}

MakeShading <- function(Mat_data){
  y <- unique(Mat_data$y)
  y <- (y[which(y %% 2 != 0)])
  data <- data.frame(cbind(y))
  data$min <- 0.5
  data$max <- (max(Mat_data$x) + 0.5)
  for( i in 1:length(y)){
    data$y_min[i] <- ((y[i]) - 0.5)
    data$y_max[i] <- ((y[i]) + 0.5)
  }
  return(data)
}

Make_matrix_plot <- function(Mat_data,Set_size_data, Main_bar_data, point_size, line_size, name_size, labels,
                             shading_data, shade_color, shade_alpha){
  Matrix_plot <- (ggplot() 
                  + theme(panel.background = element_rect(fill = "white"),
                          plot.margin=unit(c(-0.1,0.2,0.1,0.2), "cm"),
                          axis.text.x = element_blank(),
                          axis.ticks.x = element_blank(),
                          axis.text.y = element_text(colour = "gray0", size = name_size),
                          panel.grid.major = element_blank(), 
                          panel.grid.minor = element_blank())
                  + xlab(NULL) + ylab("   ")
                  + scale_y_continuous(breaks = c(1:nrow(Set_size_data)),
                                       limits = c(0.5,(nrow(Set_size_data) +0.5)),
                                       labels = labels)
                  + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0))
                  + geom_point(data=Mat_data, aes(x= x, y= y), colour = Mat_data$color, size= point_size) 
                  + geom_line(data = Mat_data, aes(group = Intersection, x=x, y=y), 
                              size = line_size, colour = Mat_data$color)
                  + geom_rect(data = shading_data, aes(xmin = min, xmax = max, ymin = y_min, ymax = y_max ),
                              fill = shade_color, alpha = shade_alpha))
  Matrix_plot <- ggplotGrob(Matrix_plot)
  return(Matrix_plot)
}

Make_size_plot <- function(Set_size_data, sbar_color){
  Size_plot <- (ggplot(data = Set_size_data, aes(x =x, y = y))
                + geom_bar(stat = "identity",colour = sbar_color, width = 0.4,
                           fill = sbar_color, position = "identity")
                + scale_x_continuous(limits = c(0.5, (nrow(Set_size_data)+0.5)),
                                     breaks = c(0, max(Set_size_data)))
                + theme(panel.background = element_rect(fill = "white"),
                        plot.margin=unit(c(0,-0.6,0.1,0.2), "cm"),
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
  return(Size_plot)
}

IntersectionBoxPlot <- function(data1, data2){
  View(data)
  View(data2)
}

QuerieInterData <- function(q, data1, first_col, num_sets, data2, exp, names){
  rows <- data.frame()
  for(i in 1:length(q)){
    index_q <- unlist(q[i])
    inter_color <- index_q[length(index_q)]
    index_q <- index_q[1:(length(index_q) - 1)]
    test <- as.character(index_q[1])
    check <- match(test, names)
    if(is.na(check) == T){
      inter_data <- NULL
    }
    else{
      for( i in 1:length(index_q)){
        double_check <- match(index_q[i], names)
        if(is.na(double_check) == T){
          warning("Intersection or set may not be present in data set. Please refer to matrix.")
        }
      }
      inter_data <- OverlayEdit(data1, data2, first_col, num_sets, index_q, exp, inter_color)
    }
    rows <- rbind(rows, inter_data)
  }
  
  if(nrow(rows) != 0){
    rows <- cbind(rows$x, rows$color)
    rows <- as.data.frame(rows)
    colnames(rows) <- c("x", "color")
  }
  else{
    rows <- NULL
  }
  return(rows)
}


QuerieInterBar  <- function(q, data1, first_col, num_sets, data2, exp, names){
  rows <- data.frame()
  for(i in 1:length(q)){
    index_q <- unlist(q[i])
    inter_color <- index_q[length(index_q)]
    index_q <- index_q[1:(length(index_q) - 1)]
    test <- as.character(index_q[1])
    check <- match(test, names)
    if(is.na(check) == T){
      inter_data <- NULL
    }
    else{
      inter_data <- OverlayEdit(data1, data2, first_col, num_sets, index_q, exp, inter_color)
    }
    rows <- rbind(rows, inter_data)
  }
  return(rows)
}

QuerieInterAtt <- function(data, first_col, q, num_sets, att_x, att_y, exp, names){
  rows <- data.frame()
  for(i in 1:length(q)){
    index_q <- unlist(q[i])
    inter_color <- index_q[length(index_q)]
    index_q <- index_q[1:(length(index_q) - 1)]
    test <- as.character(index_q[1])
    check <- match(test, names)
    if(is.na(check) == T){
      intersect <- NULL
    }
    else{
      intersect <- GetIntersects(data, first_col, index_q, num_sets)
      if(is.null(att_y) == T){
        c1 <- match(att_x, colnames(intersect))
        v1 <- intersect[ , c1]
        intersect <- cbind(intersect, v1)
        if(is.null(exp) == F){
          intersect <- Subset_att(intersect, exp)
        }
        if(nrow(intersect) != 0){
          intersect$color <- inter_color
          attx_col <- match("v1", colnames(intersect))
          color_col <- match("color", colnames(intersect))
          colnames(intersect)[color_col] <- "IColor"
          intersect <- intersect[ , c(attx_col, color_col)]
        }
      }
      else if(is.null(att_y) == F){
        c1 <- match(att_x, colnames(intersect))
        c2 <- match(att_y, colnames(intersect))
        v1 <- intersect[ , c1]
        v2 <- intersect[ , c2]
        intersect <- cbind(intersect, v1, v2)
        if(is.null(exp) == F){
          intersect <- Subset_att(intersect, exp)
        }
        intersect$color <- inter_color
        attx_col <- match("v1", colnames(intersect))
        atty_col <- match("v2", colnames(intersect))
        color_col <- match("color", colnames(intersect))
        intersect <- intersect[ , c(attx_col, atty_col, color_col)]
      }
    }
    rows <- rbind(rows, intersect)
  }
  return(rows)
}

QuerieElemAtt <- function(data, q, start_col, exp, names, att_x, att_y){
  rows <- data.frame()
  for(i in 1:length(q)){
    index_q <- unlist(q[i])
    elem_color <- index_q[length(index_q)]
    index_q <- index_q[1:(length(index_q) - 1)]
    test <- as.character(index_q[1])
    check <- match(test, names)
    if(is.na(check) == T){
      if(is.null(att_y) == F){
        elems <- GetElements(data, index_q)
        end_col <- ((start_col + as.integer(length(names))) - 1)
        col1 <- match(att_x, colnames(elems))
        col2 <- match(att_y, colnames(elems))
        val1 <- elems[ , col1]
        val2 <- elems[ , col2]
        elems <- cbind(elems, val1, val2)
        elems <- elems[which(rowSums(elems[ ,start_col:end_col]) != 0), ]
        if(is.null(exp) == F){
          elems <- Subset_att(elems, exp)
        }
        if(nrow(elems) != 0){
          elems$color <- elem_color
        }
        else{
          elems <- NULL
        }
      }
      else if(is.null(att_y) == T){
        elems <- GetElements(data, index_q)
        end_col <- ((start_col + as.integer(length(names))) - 1)
        col1 <- match(att_x, colnames(elems))
        val1 <- elems[ , col1]
        elems <- cbind(elems, val1)
        elems <- elems[which(rowSums(elems[ ,start_col:end_col]) != 0), ]
        if(is.null(exp) == F){
          elems <- Subset_att(elems, exp)
        }
        if(nrow(elems) != 0){
          elems$color <- elem_color
        }
        else{
          elems <- NULL
        }
      }
    }
    else{
      elems <- NULL
    }
    rows <- rbind(rows, elems)
  }
  return(rows)
}

Make_base_plot <- function(Main_bar_plot, Matrix_plot, Size_plot, labels, hratios, att_x, att_y,
                           Set_data, exp, position, start_col, att_color, elems_att, q_att, q,
                           Q_Title){
  
  Main_bar_plot$widths <- Matrix_plot$widths
  Matrix_plot$heights <- Size_plot$heights
  
  size_plot_height <- (((hratios[1])+0.01)*100) 
  if((hratios[1] > 0.7 || hratios[1] < 0.3) || 
       (hratios[2] > 0.7 || hratios[2] < 0.3)) warning("Plot might be out of range if ratio > 0.7 or < 0.3")
  vplayout <- function(x,y){
    viewport(layout.pos.row = x, layout.pos.col = y)
  }
  if((is.null(att_x) == T) && (is.null(att_y) == F)){
    warning("Please place lone attribute in att.x")
    if(is.null(exp) == F) warning("No attribute selected to subset")
  }
  
  else if((is.null(att_x) == T) && (is.null(att_y) == T)){
    if(is.null(exp) == F) warning("No attribute selected to subset")
    grid.newpage()
    pushViewport(viewport(layout = grid.layout(100,100)))
    print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(1:100, 21:100))
    print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:100, 1:20))
  }
  
  else if((is.null(att_x) == F) && (is.null(att_y) == T)){
    col_to_switch <- match(att_x, colnames(Set_data))
    end_col <- ((start_col + as.integer(length(labels))) - 1)
    Set_data <- Set_data[which(rowSums(Set_data[ ,start_col:end_col]) != 0), ]
    if(is.null(exp) == F){
      Set_data <- Subset_att(Set_data, exp)
    }
    colnames(Set_data)[col_to_switch] <- "values"
    if(is.null(q) == F){
      elems <- elems_att
      if(nrow(elems) != 0){
        elems <- elems[order(elems$val1), ]
        EColors <- unique(elems$color)
      }
      else{
        elems <- NULL
      }
    }
    else{
      elems <- NULL
    }
    if(is.null(q) == F){
      intersect <- q_att
      if(nrow(intersect) != 0){
        intersect <- intersect[order(intersect$v1), ]
        IColors <- unique(intersect$IColor)
      }
      else{
        intersect <- NULL
      }
    }
    else{
      intersect <- NULL
    }
    att_plot <- (ggplot(data = Set_data, aes(x = values)) 
                 + geom_histogram(binwidth = 1, colour = "black", fill = att_color)
                 + xlab(att_x) + ylab("Frequency") + labs(title = Q_Title)
                 + theme(panel.background = element_rect(fill = "white"),
                         plot.title = element_text(vjust = 1.5),
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         plot.margin=unit(c(-0.7,0.2,0.1,0.2), "cm")))
    if(is.null(elems) == F){
      for(i in 1:length(EColors)){
        Color <- EColors[i]
        elems_data <- elems[which(elems$color == Color), ]
        att_plot <- att_plot + geom_histogram(data = elems_data, aes(x = val1), 
                                              binwidth = 1, colour = "black", fill = Color)
      }
    }
    if(is.null(intersect) == F){
      for(i in 1:length(IColors)){
        Color <- IColors[i]
        intersect_data <- intersect[which(intersect$IColor == Color ), ]
        att_plot <- att_plot + geom_histogram(data = intersect_data, aes(x = v1), binwidth = 1,
                                              colour = "black", fill = Color)
      }
    }
    
    att_plot <- ggplot_gtable(ggplot_build(att_plot))
    att_plot$widths <-  Matrix_plot$widths
    if((hratios[1] < 0.4) || 
         (hratios[2] > 0.6)) warning("Plot might be out of range if mb.ratio[1] < 0.4 or mb.ratio[2] >  0.6")
    if(is.null(position) == T){
      size_plot_height <- (((hratios[1])+0.01)*100) 
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(130, 100)))
      print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(1:100, 21:100))
      print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:100, 1:20))
      print(arrangeGrob(att_plot), vp = vplayout(101:130, 21:100))
    }
    else{
      size_plot_height <- ((((hratios[1])+0.01)*100) + 30) 
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(130, 100)))
      print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(31:130, 21:100))
      print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:130, 1:20))
      print(arrangeGrob(att_plot), vp = vplayout(1:30, 21:100))
    }
  }
  else if((is.null(att_x) == F) && (is.null(att_y) == F)){
    col_switch1 <- match(att_x, colnames(Set_data))
    col_switch2 <- match(att_y, colnames(Set_data))
    end_col <- ((start_col + as.integer(length(labels))) - 1)
    Set_data <- Set_data[which(rowSums(Set_data[ ,start_col:end_col]) != 0), ]
    if(is.null(exp) == F){
      Set_data <- Subset_att(Set_data, exp)
    }
    colnames(Set_data)[col_switch1] <- "values1"
    colnames(Set_data)[col_switch2] <- "values2"
    if(is.null(q) == F){
      elems <- elems_att
      if(nrow(elems) == 0){
        elems <- NULL
      } 
    }
    else{
      elems <- NULL
    }
    if(is.null(q) == F){
      intersect <- q_att
      if(nrow(intersect) != 0){
        intersect <- intersect[order(intersect$v1, intersect$v2), ]
      }
      else{
        intersect <- NULL
      }
    }
    else{
      intersect <- NULL
    }
    att_plot <- (ggplot(data = Set_data, aes(x = values1, y = values2)) 
                 + geom_point(colour = att_color)
                 + xlab(att_x) + ylab(att_y) + labs(title = Q_Title)
                 + theme(panel.background = element_rect(fill = "white"),
                         plot.title = element_text(vjust = 1.3),
                         panel.grid.minor = element_blank(),
                         panel.grid.major = element_blank(),
                         plot.margin=unit(c(-0.7,0.2,0.1,0.2), "cm")))
    if(is.null(elems) == F){
      att_plot <- att_plot + geom_point(data = elems, aes(x = val1, y = val2), colour = elems$color)
    }
    if(is.null(intersect) == F){
      att_plot <- (att_plot + geom_point(data = intersect, aes(x = v1, y = v2), color = intersect$color))
    }
    
    att_plot <- ggplot_gtable(ggplot_build(att_plot))
    att_plot$widths <-  Matrix_plot$widths
    if((hratios[1] < 0.4) || 
         (hratios[2] > 0.6)) warning("Plot might be out of range if mb.ratio[1] < 0.4 or mb.ratio[2] >  0.6")
    if(is.null(position) == T){
      size_plot_height <- (((hratios[1])+0.01)*100) 
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(130, 100)))
      print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(1:100, 21:100))
      print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:100, 1:20))
      print(arrangeGrob(att_plot), vp = vplayout(101:130, 21:100))
    }
    else{
      size_plot_height <- ((((hratios[1])+0.01)*100) + 30) 
      grid.newpage()
      pushViewport(viewport(layout = grid.layout(130, 100)))
      print(arrangeGrob(Main_bar_plot, Matrix_plot, heights = hratios), vp = vplayout(31:130, 21:100))
      print(arrangeGrob(Size_plot), vp = vplayout(size_plot_height:130, 1:20))
      print(arrangeGrob(att_plot), vp = vplayout(1:30, 21:100))
    }
  }
}