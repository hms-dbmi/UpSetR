#' @importFrom plyr count

## Counts the frequency of each intersection being looked at and sets up data for main bar plot.
## Also orders the data for the bar plot and matrix plot
Counter <- function(data, num_sets, start_col, name_of_sets, nintersections, mbar_color, order_mat,
                    aggregate, cut, empty_intersects, decrease){
  temp_data <- list()
  Freqs <- data.frame()
  end_col <- as.numeric(((start_col + num_sets) -1))
  #gets indices of columns containing sets used
  for( i in 1:num_sets){
    temp_data[i] <- match(name_of_sets[i], colnames(data))
  }
  Freqs <- data.frame(count(data[ ,as.integer(temp_data)]))
  colnames(Freqs)[1:num_sets] <- name_of_sets
  #Adds on empty intersections if option is selected
  if(is.null(empty_intersects) == F){
    empty <- rep(list(c(0,1)), times = num_sets)
    empty <- data.frame(expand.grid(empty))
    colnames(empty) <- name_of_sets
    empty$freq <- 0
    all <- rbind(Freqs, empty)
    Freqs <- data.frame(all[!duplicated(all[1:num_sets]), ])
  }
  #Remove universal empty set
  Freqs <- Freqs[!(rowSums(Freqs[ ,1:num_sets]) == 0), ]
  #Aggregation by degree
  if(tolower(aggregate) == "degree"){
    for(i in 1:nrow(Freqs)){
      Freqs$degree[i] <- rowSums(Freqs[ i ,1:num_sets])
    }
    order_cols <- c()
    for(i in 1:length(order_mat)){
      order_cols[i] <- match(order_mat[i], colnames(Freqs))
    }
    # if(length(order_cols)==2 && order_cols[1]>order_cols[2]){decrease <- rev(decrease)}
    for(i in 1:length(order_cols)){
      logic <- decrease[i]
      Freqs <- Freqs[order(Freqs[ , order_cols[i]], decreasing = logic), ]
    }
  }
  #Aggregation by sets
  else if(tolower(aggregate) == "sets")
  {
    Freqs <- Get_aggregates(Freqs, num_sets, order_mat, cut)
  }
  #delete rows used to order data correctly. Not needed to set up bars.
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

## Generate main bar plot
Make_main_bar <- function(Main_bar_data, Q, show_num, ratios, customQ, number_angles,
                          ebar, ylabel, ymax){
  if(is.null(Q) == F){
    inter_data <- Q
    if(nrow(inter_data) != 0){
      inter_data <- inter_data[order(inter_data$x), ]
    }
    else{inter_data <- NULL}
  }
  else{inter_data <- NULL}
  
  if(is.null(ebar) == F){
    elem_data <- ebar
    if(nrow(elem_data) != 0){
      elem_data <- elem_data[order(elem_data$x), ]
    }
    else{elem_data <- NULL}
  }
  else{elem_data <- NULL}
  
  #ten_perc creates appropriate space above highest bar so number doesnt get cut off
  if(is.null(ymax) == T){
  ten_perc <- ((max(Main_bar_data$freq)) * 0.1)
  ymax <- max(Main_bar_data$freq) + ten_perc
  }
  Main_bar_plot <- (ggplot(data = Main_bar_data, aes_string(x = "x", y = "freq")) 
                    + geom_bar(stat = "identity", width = 0.6, 
                               fill = Main_bar_data$color)
                    + scale_x_continuous(limits = c(0,(nrow(Main_bar_data)+1 )), expand = c(0,0),
                                         breaks = NULL)
                    + scale_y_continuous(limits = c(0, ymax), 
                                         expand = c(c(0,0), c(0,0)))
                    + xlab(NULL) + ylab(ylabel) +labs(title = NULL)
                    + theme(panel.background = element_rect(fill = "white"),
                            plot.margin = unit(c(0.5,0.5,0.19,0.5), "lines"), panel.border = element_blank(),
                            axis.title.y = element_text(vjust = -0.8, size = 8.3), axis.text.y = element_text(vjust=0.3,
                                                                                                            size=7 )))
  if((show_num == "yes") || (show_num == "Yes")){
    Main_bar_plot <- (Main_bar_plot + geom_text(aes_string(label = "freq"), size = 2.2, vjust = -1,
                                                angle = number_angles, colour = Main_bar_data$color))
  }
  bInterDat <- NULL
  pInterDat <- NULL
  bCustomDat <- NULL
  pCustomDat <- NULL
  bElemDat <- NULL
  pElemDat <- NULL
  if(is.null(elem_data) == F){
    bElemDat <- elem_data[which(elem_data$act == T), ]
    bElemDat <- bElemDat[order(bElemDat$x), ]
    pElemDat <- elem_data[which(elem_data$act == F), ]
  }
  if(is.null(inter_data) == F){
    bInterDat <- inter_data[which(inter_data$act == T), ]
    bInterDat <- bInterDat[order(bInterDat$x), ]
    pInterDat <- inter_data[which(inter_data$act == F), ]
  }
  if(length(customQ) != 0){
    pCustomDat <- customQ[which(customQ$act == F), ]
    bCustomDat <- customQ[which(customQ$act == T), ]
    bCustomDat <- bCustomDat[order(bCustomDat$x), ]
  }
  
  if(length(bInterDat) != 0){
    Main_bar_plot <- Main_bar_plot + geom_bar(data = bInterDat,
                                              aes_string(x="x", y = "freq"), colour = bInterDat$color,
                                              fill = bInterDat$color,
                                              stat = "identity", position = "identity", width = 0.6)
  }
  if(length(bElemDat) != 0){
    Main_bar_plot <- Main_bar_plot + geom_bar(data = bElemDat,
                                              aes_string(x="x", y = "freq"), colour = bElemDat$color,
                                              fill = bElemDat$color,
                                              stat = "identity", position = "identity", width = 0.6)
  }
  if(length(bCustomDat) != 0){
    Main_bar_plot <- (Main_bar_plot + geom_bar(data = bCustomDat, aes_string(x="x", y = "freq2"),
                                               fill = bCustomDat$color2,
                                               stat = "identity", position ="identity", width = 0.6))
  }
  if(length(pCustomDat) != 0){
    Main_bar_plot <- (Main_bar_plot + geom_point(data = pCustomDat, aes_string(x="x", y = "freq2"), colour = pCustomDat$color2,
                                                 size = 2, shape = 17, position = position_jitter(width = 0.2, height = 0.2)))
  }
  if(length(pInterDat) != 0){
    Main_bar_plot <- (Main_bar_plot + geom_point(data = pInterDat, aes_string(x="x", y = "freq"),
                                                 position = position_jitter(width = 0.2, height = 0.2),
                                                 colour = pInterDat$color, size = 2, shape = 17))
  }
  if(length(pElemDat) != 0){
    Main_bar_plot <- (Main_bar_plot + geom_point(data = pElemDat, aes_string(x="x", y = "freq"),
                                                 position = position_jitter(width = 0.2, height = 0.2),
                                                 colour = pElemDat$color, size = 2, shape = 17))
  }
  
  Main_bar_plot <- (Main_bar_plot 
                    + geom_vline(xintercept = 0, color = "gray0")
                    + geom_hline(yintercept = 0, color = "gray0"))
  
  Main_bar_plot <- ggplotGrob(Main_bar_plot)
  return(Main_bar_plot)
}