#' UpSetR Plot
#'
#' @description Visualization of set intersections using novel UpSet matrix design.
#' @param data Data set
#' @param nsets Number of sets to look at
#' @param nintersects Number of intersections to plot
#' @param sets Specific sets to look at (Include as combinations. Ex: c("Name1", "Name2"))
#' @param set.metadata Metadata that offers insight to an attribute of the sets. Input should be a data frame where the first column is set names, and the 
#'        remaining columns are attributes of those sets. To learn how to use this parameter it is highly suggested to view the set metadata vignette. The link
#'        can be found on the package's GitHub page.
#' @param intersections Specific intersections to include in plot entered as a list of lists.
#'        Ex: list(list("Set name1", "Set name2"), list("Set name1", "Set name3")). If data is entered into this parameter the only data shown on the UpSet plot
#'        will be the specific intersections listed.
#' @param matrix.color Color of the intersection points
#' @param main.bar.color Color of the main bar plot
#' @param mainbar.y.label The y-axis label of the intersection size bar plot
#' @param mainbar.y.max The maximum y value of the intersection size bar plot scale. May be useful when aligning multiple UpSet plots horizontally.
#' @param sets.bar.color Color of set size bar plot
#' @param sets.x.label The x-axis label of the set size bar plot
#' @param point.size Size of points in matrix plot
#' @param line.size Width of lines in matrix plot
#' @param name.size Size of set names in matrix plot
#' @param mb.ratio Ratio between matrix plot and main bar plot (Keep in terms of hundreths)
#' @param expression Expression to subset attributes of intersection or element query data. Enter as string (Ex: "ColName > 3")
#' @param att.pos Position of attribute plot. If NULL or "bottom" the plot will be at below UpSet plot. If "top" it will be above UpSert plot
#' @param att.color Color of attribute histogram bins or scatterplot points for unqueried data represented by main bars. Default set to color of main bars.
#' @param order.by How the intersections in the matrix should be ordered by. Options include frequency (entered as "freq"), degree, or both in any order.
#' @param decreasing How the variables in order.by should be ordered. "freq" is decreasing (greatest to least) and "degree" is increasing (least to greatest)
#' @param show.numbers Show numbers of intersection sizes above bars
#' @param number.angles The angle of the numbers atop the intersection size bars
#' @param group.by How the data should be grouped ("degree" or "sets")
#' @param cutoff The number of intersections from each set (to cut off at) when aggregating by sets
#' @param queries Unified querie of intersections, elements, and custom row functions. Entered as a list that contains a list of
#'        queries. query is the type of query being conducted. params are the parameters of the query (if any). color is the color of the points on the
#'        plot that will represent the query. If no color is selected one will be provided automatically. active takes TRUE or FALSE, and if
#'        TRUE, it will overlay the bars present  with the results from the query. If FALSE a tick mark will indicate the intersection size.
#'        See examples section on how to do this.
#' @param query.legend Position query legend on top or bottom of UpSet plot
#' @param shade.color Color of row shading in matrix
#' @param shade.alpha Transparency of shading in matrix
#' @param matrix.dot.alpha Transparency of the empty intersections points in the matrix
#' @param empty.intersections Additionally display empty sets up to nintersects
#' @param color.pal Color palette for attribute plots
#' @param boxplot.summary Boxplots representing the distribution of a selected attribute for each intersection. Select attributes by entering a character vector of attribute names (e.g. c("Name1", "Name2")).
#'        The maximum number of attributes that can be entered is 2. 
#' @param attribute.plots Create custom ggplot using intersection data represented in the main bar plot. Prior to adding custom plots, the UpSet plot is set up in a 100 by 100 grid.
#'        The attribute.plots parameter takes a list that contains the number of rows that should be allocated for the custom plot, and a list of plots with specified positions.
#'        nrows is the number of rows the custom plots should take up. There is already 100 allocated for the custom plot. plots takes a list that contains a function that returns
#'        a custom ggplots and the x and y aesthetics for the function. ncols is the number of columns that your ggplots should take up. See examples for how to add custom ggplots.
#' @details Visualization of set data in the layout described by Lex and Gehlenborg in \url{http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html}.
#' UpSet also allows for visualization of queries on intersections and elements, along with custom queries queries implemented using
#' Hadley Wickhams apply function. To further analyze the data contained in the intersections, the user may select additional attribute plots
#' to be displayed alongside the UpSet plot. The user also has the the ability to pass their own plots into the function to further analyze
#' data belonging to queries of interest. Most aspects of the UpSet plot are customizable, allowing the user to select the plot that best suits their style.
#' Depending on how the featuers are selected, UpSet can display between 25-65 sets and between 40-100 intersections.
#' @note Data set must be formatted as described on the orginal UpSet github page: \url{http://github.com/VCG/upset/wiki}.
#' @references Lex et al. (2014). UpSet: Visualization of Intersecting Sets
#' IEEE Transactions on Visualization and Computer Graphics (Proceedings of InfoVis 2014), vol 20, pp. 1983-1992, (2014). \url{http://people.seas.harvard.edu/~alex/papers/2014_infovis_upset.pdf}
#' @references Lex and Gehlenborg (2014). Points of view: Sets and intersections. Nature Methods 11, 779 (2014). \url{http://www.nature.com/nmeth/journal/v11/n8/abs/nmeth.3033.html}
#' @seealso Original UpSet Website: \url{http://vcg.github.io/upset/about/}
#' @seealso UpSetR github for additional examples: \url{http://github.com/hms-dbmi/UpSetR}
#' @examples movies <- read.csv( system.file("extdata", "movies.csv", package = "UpSetR"), header=TRUE, sep=";" )
#'
#'require(ggplot2); require(plyr); require(gridExtra); require(grid);
#'
#' between <- function(row, min, max){
#'   newData <- (row["ReleaseDate"] < max) & (row["ReleaseDate"] > min)
#' }
#'
#' plot1 <- function(mydata, x){
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
#' attributeplots <- list(gridrows = 55,
#'                   plots = list(list(plot = plot1, x= "ReleaseDate",  queries = FALSE),
#'                          list(plot = plot1, x= "ReleaseDate", queries = TRUE),
#'                          list(plot = plot2, x = "ReleaseDate", y = "AvgRating", queries = FALSE),
#'                          list(plot = plot2, x = "ReleaseDate", y = "AvgRating", queries = TRUE)),
#'                    ncols = 3)
#'
#' upset(movies, nsets = 7, nintersects = 30, mb.ratio = c(0.5, 0.5),
#'       order.by = c("freq", "degree"), decreasing = c(TRUE,FALSE))
#'
#' upset(movies, sets = c("Drama", "Comedy", "Action", "Thriller", "Western", "Documentary"),
#'       queries = list(list(query = intersects, params = list("Drama", "Action")),
#'                 list(query = between, params = list(1970, 1980), color = "red", active = TRUE)))
#'
#' upset(movies, attribute.plots = attributeplots,
#'      queries = list(list(query = between, params = list(1920, 1940)),
#'                     list(query = intersects, params = list("Drama"), color= "red"),
#'                     list(query = elements, params = list("ReleaseDate", 1990, 1991, 1992))),
#'       main.bar.color = "yellow")
#'       
#' @import gridExtra
#' @import ggplot2
#' @import utils
#' @import stats
#' @import methods
#' @import grDevices       
#' @export
upset <- function(data, nsets = 5, nintersects = 40, sets = NULL, set.metadata = NULL, intersections = NULL, matrix.color = "gray23",
                  main.bar.color = "gray23", mainbar.y.label = "Intersection Size", mainbar.y.max = NULL, sets.bar.color = "gray23",
                  sets.x.label = "Set Size", point.size = 2.2, line.size = 0.7, name.size = 7, mb.ratio = c(0.70,0.30),
                  expression = NULL, att.pos = NULL, att.color = main.bar.color, order.by = c("freq", "degree"),
                  decreasing = c(T, F), show.numbers = "yes", number.angles = 0, group.by = "degree",cutoff = NULL,
                  queries = NULL, query.legend = "none", shade.color = "gray88", shade.alpha = 0.25, matrix.dot.alpha =0.5,
                  empty.intersections = NULL, color.pal = 1, boxplot.summary = NULL, attribute.plots = NULL){
  
  startend <-FindStartEnd(data)
  first.col <- startend[1]
  last.col <- startend[2]
  
  if(color.pal == 1){
    palette <- c("#1F77B4", "#FF7F0E", "#2CA02C", "#D62728", "#9467BD", "#8C564B", "#E377C2",
                 "#7F7F7F", "#BCBD22", "#17BECF")
  }
  else{
    palette <- c("#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00",
                 "#CC79A7")
  }
  
  if(is.null(intersections) == F){
    Set_names <- unique((unlist(intersections)))
    Sets_to_remove <- Remove(data, first.col, last.col, Set_names)
    New_data <- Wanted(data, Sets_to_remove)
    Num_of_set <- Number_of_sets(Set_names)
    Set_names <- order_sets(New_data, Set_names)
    All_Freqs <- specific_intersections(data, first.col, last.col, intersections, order.by, group.by, decreasing,
                                        cutoff, main.bar.color, Set_names)
  }
  else if(is.null(intersections) == T){
    Set_names <- sets
    if(is.null(Set_names) == T || length(Set_names) == 0 ){
      Set_names <- FindMostFreq(data, first.col, last.col, nsets)
    }
    Sets_to_remove <- Remove(data, first.col, last.col, Set_names)
    New_data <- Wanted(data, Sets_to_remove)
    Num_of_set <- Number_of_sets(Set_names)
    Set_names <- order_sets(New_data, Set_names)
    All_Freqs <- Counter(New_data, Num_of_set, first.col, Set_names, nintersects, main.bar.color,
                         order.by, group.by, cutoff, empty.intersections, decreasing)
  }
  Matrix_setup <- Create_matrix(All_Freqs)
  labels <- Make_labels(Matrix_setup)
  #Chose NA to represent NULL case as result of NA being inserted when at least one contained both x and y
  #i.e. if one custom plot had both x and y, and others had only x, the y's for the other plots were NA
  #if I decided to make the NULL case (all x and no y, or vice versa), there would have been alot more if/else statements
  #NA can be indexed so that we still get the non NA y aesthetics on correct plot. NULL cant be indexed.
  att.x <- c(); att.y <- c();
  if(is.null(attribute.plots) == F){
    for(i in seq_along(attribute.plots$plots)){
      if(length(attribute.plots$plots[[i]]$x) != 0){
        att.x[i] <- attribute.plots$plots[[i]]$x
      }
      else if(length(attribute.plots$plots[[i]]$x) == 0){
        att.x[i] <- NA
      }
      if(length(attribute.plots$plots[[i]]$y) != 0){
        att.y[i] <- attribute.plots$plots[[i]]$y
      }
      else if(length(attribute.plots$plots[[i]]$y) == 0){
        att.y[i] <- NA
      }
    }
  }
  
  BoxPlots <- NULL
  if(is.null(boxplot.summary) == F){
    BoxData <- IntersectionBoxPlot(All_Freqs, New_data, first.col, Set_names)
    BoxPlots <- list()
    for(i in seq_along(boxplot.summary)){
      BoxPlots[[i]] <- BoxPlotsPlot(BoxData, boxplot.summary[i], att.color)
    }
  }
  
  customAttDat <- NULL
  customQBar <- NULL
  Intersection <- NULL
  Element <- NULL
  legend <- NULL
  EBar_data <- NULL
  if(is.null(queries) == F){
    custom.queries <- SeperateQueries(queries, 2, palette)
    customDat <- customQueries(New_data, custom.queries, Set_names)
    legend <- GuideGenerator(queries, palette)
    legend <- Make_legend(legend)
    if(is.null(att.x) == F && is.null(customDat) == F){
      customAttDat <- CustomAttData(customDat, Set_names)
    }
    customQBar <- customQueriesBar(customDat, Set_names, All_Freqs, custom.queries)
  }
  if(is.null(queries) == F){
    Intersection <- SeperateQueries(queries, 1, palette)
    Matrix_col <- intersects(QuerieInterData, Intersection, New_data, first.col, Num_of_set,
                             All_Freqs, expression, Set_names, palette)
    Element <- SeperateQueries(queries, 1, palette)
    EBar_data <-ElemBarDat(Element, New_data, first.col, expression, Set_names,palette, All_Freqs)
  }
  else{
    Matrix_col <- NULL
  }
  
  Matrix_layout <- Create_layout(Matrix_setup, matrix.color, Matrix_col, matrix.dot.alpha)
  Set_sizes <- FindSetFreqs(New_data, first.col, Num_of_set, Set_names)
  Bar_Q <- NULL
  if(is.null(queries) == F){
    Bar_Q <- intersects(QuerieInterBar, Intersection, New_data, first.col, Num_of_set, All_Freqs, expression, Set_names, palette)
  }
  QInter_att_data <- NULL
  QElem_att_data <- NULL
  if((is.null(queries) == F) & (is.null(att.x) == F)){
    QInter_att_data <- intersects(QuerieInterAtt, Intersection, New_data, first.col, Num_of_set, att.x, att.y,
                                  expression, Set_names, palette)
    QElem_att_data <- elements(QuerieElemAtt, Element, New_data, first.col, expression, Set_names, att.x, att.y,
                               palette)
  }
  AllQueryData <- combineQueriesData(QInter_att_data, QElem_att_data, customAttDat, att.x, att.y)
  
  ShadingData <- NULL
  
  if(is.null(set.metadata) == F){
    set.metadata.plots <- Make_set_metadata_plot(set.metadata, Set_names)
    ShadingData <- get_shade_groups(set.metadata, Set_names, Matrix_layout, shade.alpha)
    if(is.null(ShadingData) == FALSE){
    shade.alpha <- unique(ShadingData$alpha)
    }
  }
  if(is.null(ShadingData) == TRUE){
  ShadingData <- MakeShading(Matrix_layout, shade.color)
  }
  Main_bar <- Make_main_bar(All_Freqs, Bar_Q, show.numbers, mb.ratio, customQBar, number.angles, EBar_data, mainbar.y.label,
                            mainbar.y.max)
  Matrix <- Make_matrix_plot(Matrix_layout, Set_sizes, All_Freqs, point.size, line.size,
                             name.size, labels, ShadingData, shade.alpha)
  Sizes <- Make_size_plot(Set_sizes, sets.bar.color, mb.ratio, sets.x.label)
  
  Make_base_plot(Main_bar, Matrix, Sizes, labels, mb.ratio, att.x, att.y, New_data,
                 expression, att.pos, first.col, att.color, AllQueryData, attribute.plots,
                 legend, query.legend, BoxPlots, Set_names, set.metadata, set.metadata.plots)
  
}