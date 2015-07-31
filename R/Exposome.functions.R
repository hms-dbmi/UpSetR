## Counts the frequency of each intersection being looked at and sets up data for main bar plot
EWAS.Bar.Data <- function(data, num_sets, start_col, name_of_sets, nintersections, mbar_color, order_mat,
                          aggregate, cut, empty_intersects){
  temp_data <- list()
  Freqs <- data.frame()
  end_col <- as.numeric(((start_col + num_sets) -1))
  #gets indices of columns containing sets used
  for( i in 1:num_sets){
    temp_data[i] <- match(name_of_sets[i], colnames(data))
  }
  Freqs <- data.frame(count(data[ ,as.integer(temp_data)]))
  positive <- data[which(data$Association.Size >= 0), ]
  negative <- data[which(data$Association.Size < 0), ]
  positive <- data.frame(count(positive[ ,as.integer(temp_data)]))
  negative <- data.frame(count(negative[ ,as.integer(temp_data)]))
  negative$freq <- (-(negative$freq))
  correlation <- rbind(positive, negative)
  colnames(Freqs)[1:num_sets] <- name_of_sets
  colnames(correlation)[1:num_sets] <- name_of_sets
  #Adds on empty intersections if option is selected
  if(is.null(empty_intersects) == F){
    empty <- rep(list(c(0,1)), times = num_sets)
    empty <- data.frame(expand.grid(empty))
    colnames(empty) <- name_of_sets
    empty$freq <- 0
    all <- rbind(Freqs, empty)
    allCorr <- rbind(correlation, empty)
    Freqs <- data.frame(all[!duplicated(all[1:num_sets]), ])
    correlation <- data.frame(all[!duplicated(all[1:numsets]), ])
  }
  #Remove univeral empty set
  Freqs <- Freqs[!(rowSums(Freqs[ ,1:num_sets]) == 0), ]
  correlation <- correlation[!(rowSums(correlation[ ,1:num_sets]) == 0), ]
  #Aggregation by degree
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
  #Aggregation by sets
  else if(tolower(aggregate) == "sets")
  {
    Freqs <- Get_aggregates(Freqs, num_sets, order_mat, cut)
  }
  #delete rows used to order data correctly. Not needed to set up bars.
  delete_row <- (num_sets + 2)
  Freqs <- Freqs[ , -delete_row]
  correlation <- correlation[ ,-delete_row]
  for( i in 1:nrow(Freqs)){
    Freqs$x[i] <- i
    Freqs$color <- mbar_color
  }
  Freqs <- Freqs[1:nintersections, ]
  correlation <- correlation[1:nintersections, ]
  Freqs <- na.omit(Freqs)
  correlation <- na.omit(correlation)
  correlation <- correlationBarData(Freqs, correlation, num_sets)
  return(correlation)
}

correlationBarData <- function(Freqs, corrFreqs, num_sets){
  matching <- list()
  bars <- list()
  corrBars <- list()
  for(i in seq(nrow(Freqs))){bars[[i]] <- as.integer(Freqs[1:num_sets][i,])}
  for(i in seq(nrow(corrFreqs))){corrBars[[i]] <- as.integer(corrFreqs[1:num_sets][i,])}
  for(i in seq(length(corrBars))){
    matching[[i]] <- which(bars %in% corrBars[i])
  }
  matching <- unlist(matching)
  corrFreqs$x <- matching
  corrFreqs$color <- NA
  corrFreqs$color[which(corrFreqs$freq < 0)] <- "blueviolet"
  corrFreqs$color[which(corrFreqs$freq >= 0)] <- "cyan3"
  return(corrFreqs)
}