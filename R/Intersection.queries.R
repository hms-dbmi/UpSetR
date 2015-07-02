## Creates data set for intersection queries to be plotted in attribute plots
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

## Generates data needed to represent selected intersections on matrix
QuerieInterData <- function(query, data1, first_col, num_sets, data2, exp, names, palette){
  rows <- data.frame()
  if(length(query) == 0){
    return(NULL)
  }
  for(i in 1:length(query)){
    index_q <- unlist(query[[i]]$params)
    inter_color <- query[[i]]$color
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

## Generates intersection bar data to overlay main bars
QuerieInterBar  <- function(q, data1, first_col, num_sets, data2, exp, names, palette){
  rows <- data.frame()
  act <- c()
  if(length(q) == 0){
    return(NULL)
  }
  for(i in 1:length(q)){
    index_q <- unlist(q[[i]]$params)
    inter_color <- q[[i]]$color
    test <- as.character(index_q[1])
    check <- match(test, names)
    if(is.na(check) == T){
      inter_data <- NULL
    }
    else{
      inter_data <- OverlayEdit(data1, data2, first_col, num_sets, index_q, exp, inter_color)
    }
    if((isTRUE(q[[i]]$active) == T) && (is.null(inter_data) == F)){
      act[i] <- T
    }
    else if((isTRUE(q[[i]]$active) == F) && (is.null(inter_data) == F)){
      act[i] <- F
    }
    rows <- rbind(rows, inter_data)
  }
  rows <- cbind(rows, act)
  return(rows)
}

## Generate attribute data for intersection queries
QuerieInterAtt <- function(q, data, first_col, num_sets, att_x, att_y, exp, names, palette){
  rows <- data.frame()
  if(length(q) == 0){
    return(NULL)
  }
  for(i in 1:length(q)){
    index_q <- unlist(q[[i]]$params)
    inter_color <- unlist(q[[i]]$color)
    test <- as.character(index_q[1])
    check <- match(test, names)
    if(is.na(check) == T){
      intersect <- NULL
    }
    else{
      intersect <- GetIntersects(data, first_col, index_q, num_sets)
      if(is.na(att_y[i]) == T){
        if(is.null(exp) == F){
          intersect <- Subset_att(intersect, exp)
        }
        if(nrow(intersect) != 0){
          intersect$color <- inter_color
        }
      }
      else if(is.na(att_y[i]) == F){
        if(is.null(exp) == F){
          intersect <- Subset_att(intersect, exp)
        }
        intersect$color <- inter_color        
      }
    }
    intersect <- intersect[ ,-which(names(intersect) %in% index_q)]
    rows <- rbind(rows, intersect)
  }
  return(rows)
}