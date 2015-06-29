#' Base Plot for UpSetR
#' 
#' @description Visualization of set intersections using novel UpSet matrix design.
#' @param data Data set
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
#' @param att.x Attribute entered as a string. If att.y is NULL a histogram will be produced
#' @param att.y Attribute entered as a string. Produces a scatter plot vs. att.x
#' @param expression Expression to subset attributes of intersection or element query data. Enter as string (Ex: "ColName > 3")
#' @param att.pos Position of attribute plot. If NULL or "bottom" the plot will be at below UpSet plot. If "top" it will be above UpSert plot
#' @param att.color Color of attribute histogram bins or scatterplot points for unqueried data represented by main bars. Default set to color of main bars.
#' @param order.matrix How the intersections in the matrix should be ordered by. Options include frequency (entered as "freq"), degree, or both in any order.
#' @param show.numbers Show numbers of intersection sizes above bars 
#' @param aggregate.by How the data should be aggregated ("degree" or "sets")
#' @param cutoff The number of intersections from each set (to cut off at) when aggregating by sets
#' @param queries Unified querie of intersections, elements, and custom row functions. Entered as a list that contains a list of
#'        queries. query is the type of query being conducted. params are the parameters of the query (if any). color is the color of the points on the 
#'        plot that will represent the query. If no color is selected one will be provided automatically. active takes TRUE or FALSE, and if 
#'        TRUE, it will overlay the bars present  with the results from the query. If FALSE a tick mark will indicate the intersection size.
#'        See examples section on how to do this.
#' @param query.legend Position query legend on top or bottom of UpSet plot
#' @param query.title.plot Title of query plot
#' @param shade.color Color of row shading in matrix
#' @param shade.alpha Transparency of shading in matrix
#' @param color.pal Color palette for attribute plots
#' @param boxplot.summary Boxplots representing the distribution of a selected attribute for each intersection. Change param from NULL to "on" for this option. 
#' @param custom.plot Create custom ggplot using intersection data represented in the main bar plot. Prior to adding custom plots, the UpSet plot is set up in a 100 by 100 grid.
#'        The custom.plot parameter takes a list that contains the number of rows that should be allocated for the custom plot, and a list of plots with specified positions.
#'        nrows is the number of rows the custom plots should take up. There is already 100 allocated for the custom plot. plots takes a list that contains a function that returns
#'        a custom ggplots and the x and y aesthetics for the function. ncols is the number of columns that your ggplots should take up. See examples for how to add custom ggplots. 
#' @details Visualization of set data in the layout described by Lex and Gehlenborg in \url{<http://www.nature.com/nmeth/journal/v11/n8/full/nmeth.3033.html>}. 
#' UpSet also allows for visualization of queries on intersections and elements, along with custom queries queries implemented using 
#' Hadley Wickhams apply function. To further analyze the data contained in the intersections, the user may select additional attribute plots
#' to be displayed alongside the UpSet plot. The user also has the the ability to pass their own plots into the function to further analyze data
#' data that has already been generated. Most aspects of the UpSet plot are customizable, allowing the user to select the plot that looks best to them.
#' Depending on how the featuers are selected UpSet can display between 25-65 sets, and between 40-100 intersections. 
#' @note Data set must be formatted as described on the orginal UpSet github page: \url{<https://github.com/VCG/upset/wiki>}.
#' @references Lex and Gehlenborg (2014). Points of view: Sets and intersections. Nature Methods 11, 779 (2014). \url{http://www.nature.com/nmeth/journal/v11/n8/full/nmeth.3033.html}
#' @references Original UpSet Website: \url{http://vcg.github.io/upset/about/}
#' @seealso Movies data from example can be found here: \url{https://github.com/hms-dbmi/UpSetR}
#' @examples #Link to correctly formatted movies data set provided in see also section.
#' movies <- read.csv( system.file("extdata", "movies.csv", package = "UpSetR"), header=TRUE, sep=";" )
#'
#' between <- function(row, min, max){
#'   newData <- (row["ReleaseDate"] < max) & (row["ReleaseDate"] > min)
#' }
#'
#' plot1 <- function(mydata, x, color){
#'   myplot <- (ggplot(mydata, aes_string(x= x, fill = "color"))
#'             + geom_histogram() + scale_fill_identity()
#'             + theme(plot.margin = unit(c(0,0,0,0), "cm")))
#' }
#'
#' plot2 <- function(mydata, x, y){
#'   myplot <- (ggplot(data = mydata, aes_string(x=x, y=y, colour = "color"), alpha = 0.5)
#'             + geom_point() + scale_color_identity()
#'             + theme_bw() + theme(plot.margin = unit(c(0,0,0,0), "cm")))
#' }
#'
#' customplot <- list(nrows = 55,
#'                   plots = list(list(plot = plot1, x= "ReleaseDate",  queries = FALSE),
#'                                list(plot = plot1, x= "ReleaseDate", queries = TRUE),
#'                                list(plot = plot2, x = "ReleaseDate", y = "AvgRating", queries = FALSE),
#'                                list(plot = plot2, x = "ReleaseDate", y = "AvgRating", queries = TRUE)),
#'                    ncols = 3)
#'
#' upset(movies, nsets = 7, nintersects = 30, mb.ratio = c(0.5, 0.5),
#'      att.x = "ReleaseDate", att.y = "AvgRating", expression = "ReleaseDate > 1970 & AvgRating < 4.2",
#'      order.matrix = c("freq", "degree"))
#'
#' upset(movies, sets = c("Drama", "Comedy", "Action", "Thriller", "Western", "Documentary"),
#'      att.x = "ReleaseDate", att.y = "AvgRating",
#'       queries = list(list(query = "Intersection",params = list("Drama", "Action")),
#'                 list(query = between, params = list(1970, 1980), color = "red", active = TRUE)))
#'
#' upset(movies, custom.plot = customplot, 
#'      queries = list(list(query = between, params = list(1920, 1940)),
#'                     list(query = "Intersection", params = list("Drama"), color= "red")),
#'      att.x = "ReleaseDate", att.y = "AvgRating", main.bar.color = "yellow")
#' @export
upset <- function(data, nsets = 5, nintersects = 40, sets = NULL, matrix.color = "gray23",
                       main.bar.color = "gray23", sets.bar.color = "dodgerblue",point.size = 4, line.size = 1, 
                       name.size = 10, mb.ratio = c(0.70,0.30), att.x = NULL, att.y = NULL, expression = NULL, 
                       att.pos = NULL, att.color = main.bar.color, order.matrix = c("degree", "freq"), 
                       show.numbers = "yes", aggregate.by = "degree",cutoff = NULL, queries = NULL, query.legend = "none", 
                       query.plot.title = "My Query Plot Title", shade.color = "skyblue", shade.alpha = 0.25, 
                       color.pal = 1, boxplot.summary = NULL, custom.plot = NULL){
  require(ggplot2);
  require(gridExtra);
  require(plyr);
  
  startend <-FindStartEnd(data) 
  first.col <- startend[1]
  last.col <- startend[2]

  if(color.pal == 1){
    palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2",
                 "#7F7F7F", "#BCBD22", "#17BECF")
  }
  else{
    palette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7")
  }
  
  Set_names <- sets
  if(is.null(Set_names) == T || length(Set_names) == 0 ){
    Set_names <- FindMostFreq(data, first.col, last.col, nsets)
  }
  Sets_to_remove <- Remove(data, first.col, last.col, Set_names)
  New_data <- Wanted(data, Sets_to_remove)
  Num_of_set <- Number_of_sets(Set_names)
  All_Freqs <- Counter(New_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                       rev(order.matrix), aggregate.by, cutoff)
  Matrix_setup <- Create_matrix(All_Freqs)
  labels <- Make_labels(Matrix_setup)
  
  BoxPlots <- NULL
  if(is.null(boxplot.summary) == F && boxplot.summary != tolower("off")){
  BoxData <- IntersectionBoxPlot(All_Freqs, New_data, first.col, Set_names)
  if(is.null(att.x) == F & is.null(att.y) == T){ warning("Please use att.y for boxplot summary.")
                           BoxPlots <- NULL}
  if(is.null(att.y) == T & is.null(att.x) == T){warning("Please select att.y for the boxplot summary.")
                          BoxPlots <- NULL}
  else{
    BoxPlots <- list()
    for(i in seq_along(att.y)){
  BoxPlots[[i]] <- BoxPlotsPlot(BoxData, att.y[i], att.color)
    }
  }
  }
  
  customAttDat <- NULL
  customQBar <- NULL
  Intersection <- NULL
  Element <- NULL
  legend <- NULL
  if(is.null(queries) == F){
    custom.queries <- SeperateQueries(queries, 2, palette)
    customDat <- customQueries(New_data, custom.queries, Set_names)
    legend <- GuideGenerator(queries, palette)
    legend <- Make_legend(legend)
    if(is.null(att.x) == F && is.null(customDat) == F){
        customAttDat <- CustomAttData(customDat, att.x, att.y)
    }
    customQBar <- customQueriesBar(customDat, Set_names, All_Freqs, custom.queries)
  }
  if(is.null(queries) == F){
    Intersection <- SeperateQueries(queries, 1, palette)
    Matrix_col <-  QuerieInterData(Intersection, New_data, first.col, Num_of_set, All_Freqs, 
                                   expression, Set_names, palette)
  }
  else{
    Matrix_col <- NULL
  }
  Matrix_layout <- Create_layout(Matrix_setup, matrix.color, Matrix_col)
  Set_sizes <- FindSetFreqs(New_data, first.col, Num_of_set, Set_names)
  Bar_Q <- NULL
  if(is.null(queries) == F){
    Bar_Q <- QuerieInterBar(Intersection, New_data, first.col, Num_of_set, All_Freqs, expression, Set_names, palette)
  }
  QInter_att_data <- NULL
  QElem_att_data <- NULL
  if((is.null(queries) == F) & (is.null(att.x) == F)){
    QInter_att_data <- QuerieInterAtt(New_data, first.col, Intersection, Num_of_set, att.x, att.y, 
                                      expression, Set_names, palette)
    Element <- SeperateQueries(queries, 1, palette)
    QElem_att_data <- QuerieElemAtt(New_data, Element, first.col, expression, Set_names, att.x, att.y,
                                    palette)
  }
  AllQueryData <- combineQueriesData(QInter_att_data, QElem_att_data, customAttDat, att.x, att.y)
  ShadingData <- MakeShading(Matrix_layout)
  Main_bar <- Make_main_bar(All_Freqs, Bar_Q, show.numbers, mb.ratio, customQBar)
  Matrix <- Make_matrix_plot(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             name.size, labels, ShadingData, shade.color, shade.alpha)
  Sizes <- Make_size_plot(Set_sizes, sets.bar.color, mb.ratio)
  Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
                 expression, att.pos, first.col, att.color, AllQueryData,
                 query.plot.title, custom.plot, legend, query.legend, BoxPlots)
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

FindStartEnd <- function(data){
  startend <- c()
  for(i in 1:ncol(data)){
    column <- data[, i]
      column <- (levels(factor(column)))
      if((column[1] == "0") && (column[2] == "1" && (length(column) == 2))){
        startend[1] <- i
        break
      }
      else{
        next
      }
    }
  for(i in ncol(data):1){
    column <- data[ ,i]
    column <- (levels(factor(column)))
      if((column[1] == "0") && (column[2] == "1") && (length(column) == 2)){
        startend[2] <- i
        break
      }
      else{
        next
      }
    }
  return(startend)
}