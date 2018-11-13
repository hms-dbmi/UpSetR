## Finds the columns that represent the sets 
FindStartEnd <- function(data){
  startend <- c()
  for(i in 1:ncol(data)){
    column <- data[[i]]
    column <- (levels(factor(column)))
    if((column[1] == "0") && (column[2] == "1" && (length(column) == 2))){
      startend[1] <- i
      break
    }
    else if((column[1] == "1") && (length(column) == 1)){
      startend[1] <- i
      break
    }
    else if((column[1] == "-1") && (column[2] == "0") && (column[3] == "1") && (length(column) == 3)){
      startend[1] <- i
      break
    }
    else{
      next
    }
  }
  for(i in ncol(data):1){
    column <- data[[i]]
    column <- (levels(factor(column)))
    if((column[1] == "0") && (column[2] == "1") && (length(column) == 2)){
      startend[2] <- i
      break
    }
    else if((column[1] == "1") && (length(column) == 1)){
      startend[2] <- i
      break
    }
    else if((column[1] == "-1") && (column[2] == "0") && (column[3] == "1") && (length(column) == 3)){
      startend[2] <- i
      break
    }
    else{
      next
    }
  }
  return(startend)
}

## Finds the n largest sets if the user hasn't specified any sets
FindMostFreq <- function(data, start_col, end_col, n_sets){  
  temp_data <- data[ ,start_col:end_col]
  temp_data <- colSums(temp_data)
  temp_data <- as.data.frame(temp_data)
  temp_data <- tail(temp_data[order(temp_data[ ,"temp_data"]), , drop = F], as.integer(n_sets))
  temp_data <- rev(row.names(temp_data))
  return(temp_data)
}

## Finds the names of the sets that aren't being used
Remove <- function(data, start_col, end_col, sets){
  temp_data <- as.data.frame(data[ , start_col:end_col])
  Unwanted_sets <- colnames(temp_data[ ,!(colnames(temp_data) %in% sets), drop = F])
}

## Removes unwanted sets from data
Wanted <- function(data, unwanted_sets){
  temp_data <- (data[ ,!(colnames(data) %in% unwanted_sets), drop = F])
}

order_sets <- function(data, sets){
  sets <- colSums(data[sets])
  sets <- names(sets[order(sets, decreasing = T)])
  return(sets)
}

## Subsets intersection and element queries using expression parameter
Subset_att <- function(data, exp){
  express <- unlist(strsplit(exp, " "))
  for(i in seq_along(express)){
    if(is.na(match(express[i], colnames(data))) == F){
      express[i] <- paste("data$",express[i], sep = "")
    }
    else{
      next;
    }
  }
  express <- paste(express, sep = "", collapse = " ")
  data <- data[which(eval(parse(text = express))), ]
  return(data)
}

## Number of sets being looked at
Number_of_sets <- function(sets){
  temp <- length(sets)
  return(temp)
}


## Creates data set if data is aggregated by sets
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

## Creates data set to overlay main bar plot and matrix plot with intersection queries
OverlayEdit <- function(data1, data2, start_col, num_sets, intersects, exp, inter_color){
  end_col <- as.numeric(((start_col + num_sets) -1))
  set_cols <- data1[ ,start_col:end_col]
  temp_data <- data1[which(rowSums(data1[ ,start_col:end_col]) == length(intersects)), ]
  unwanted <- colnames(set_cols[ ,!(colnames(set_cols) %in% intersects), drop = F])
  temp_data <- (temp_data[ ,!(colnames(data1) %in% unwanted), drop = F])
  new_end <- ((start_col + length(intersects)) -1)
  if(new_end == start_col){
    n <- names(temp_data)
    temp_data <- temp_data[which(temp_data[ ,start_col] == 1), ]
    temp_data <- as.data.frame(temp_data)
    names(temp_data) <- n
  }
  else{
    temp_data <- temp_data[which(rowSums(temp_data[ ,start_col:new_end]) == length(intersects)), ]
  }
  if(is.null(exp) == F){
    temp_data <- Subset_att(temp_data, exp)
  }
  temp_data <- temp_data[intersects]
  temp_data <- na.omit(temp_data)
  
  other_data <- data2[which(rowSums(data2[ ,1:num_sets]) == length(intersects)), ]
  other_data <- (other_data[ ,!(colnames(data2) %in% unwanted), drop = F])
  if(new_end == start_col){
    other_data <- other_data[ which(other_data[intersects] == 1), ]
  }
  else{
    other_data <- other_data[which(rowSums(other_data[intersects]) == length(intersects)), ]
  }
  row_num <- as.integer(other_data$x)
  overlay_row <- data2[row_num, ]
  new_freq <- nrow(temp_data)
  overlay_row$freq <- new_freq
  overlay_row$color <- inter_color
  return(overlay_row)
}