specific_intersections <- function(data, first.col, last.col, intersections, order_mat,
                                   aggregate, decrease, cut, mbar_color, set_names){
  sets <- names(data[c(first.col:last.col)])
  keep <- unique(unlist(intersections))
  remove <- sets[which(!sets %in% keep)]
  remove <- which(names(data) %in% remove)
  if(length(remove) != 0){
    data <- data[-remove]
  }
  data <- plyr_count(data[keep])
  sets <- names(data[seq_along(keep)])
  data <- lapply(intersections, function(x){
    temp_sets <- unlist(x)
    x <- data[which(rowSums(data[seq_along(keep)]) == length(temp_sets)), ]
    x <- x[which(rowSums(x[temp_sets]) == length(temp_sets)), ]
    if(nrow(x) == 0){
      names <- names(x[seq_along(keep)])
      x <- rbind(x, rep(0, ncol(x)))
      colnames(x) <- c(names, "freq")
      x[ ,which(names %in% temp_sets)] <- 1
    }
    x <- x
  })
  
  Freqs <- data.frame()
  
  for(i in seq_along(data)){
    Freqs <- rbind(Freqs, data[[i]])
  }
  
  Freqs <- Freqs[c(set_names, "freq")]
  
  num_sets <- length(keep)
  
  if(tolower(aggregate) == "degree"){
    for(i in seq_len(nrow(Freqs))){
      Freqs$degree[i] <- rowSums(Freqs[ i ,seq_len(num_sets)])
    }
    order_cols <- integer(length(order_mat))
    for(i in seq_along(order_mat)){
      order_cols[i] <- match(order_mat[i], colnames(Freqs))
    }
    
    for(i in seq_along(order_cols)){
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
  for( i in seq_len(nrow(Freqs))){
    Freqs$x[i] <- i
    Freqs$color <- mbar_color
  }
  Freqs <- na.omit(Freqs)
  return(Freqs)
}