## Creates data set for element query to be plotted in attribute plots
GetElements <- function(data, elements){
  col_num <- match(elements[1], colnames(data))
  num_elem <- length(elements)
  elems <- as.character(elements[2:num_elem])
  temp_data <- data[data[ ,col_num] %in% elems, ]
  return(temp_data)
}

## Generate attribute data from element queries
QuerieElemAtt <- function(q, data, start_col, exp, names, att_x, att_y, palette){
  rows <- data.frame()
  if(length(q) == 0){
    return(NULL)
  }
  for(i in 1:length(q)){
    index_q <- unlist(q[[i]]$params)
    elem_color <- unlist(q[[i]]$color)
    test <- as.character(index_q[1])
    check <- match(test, names)
    if(length(check) != 0){
      if(is.na(att_y[i]) == F){
        elems <- GetElements(data, index_q)
        end_col <- ((start_col + as.integer(length(names))) - 1)
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
      else if(is.na(att_y[i]) == T){
        elems <- GetElements(data, index_q)
        end_col <- ((start_col + as.integer(length(names))) - 1)
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
    elems <- elems[ ,-which(names(elems) %in% names)]
    rows <- rbind(rows, elems)
  }
  rows <- as.data.frame(rows)
  if(length(rows) == 0){
    return(NULL)
  }
  else{
    return(rows)
  }
}


ElemBarDat <- function(q, data1, first_col, exp, names, palette, mbdata){
  data1 <- data.frame(data1, check.names = F)
  bar <- count(data1)
  bar$x <- 1:nrow(bar)
  rows <- data.frame()
  act <- c()
  if(length(q) == 0){
    return(NULL)
  }
  for(i in 1:length(q)){
    index_q <- unlist(q[[i]]$params)
    test <- as.character(index_q[1])
    check <- match(test, names)
    elem_color <- q[[i]]$color
    if(is.na(check) != T){
      elem_data <- NULL
    }
    else{
      elem_data <- data1[which(as.character(data1[ ,test]) %in% c(index_q[2:length(index_q)])), ]
      if(!is.null(exp)){
      elem_data <- Subset_att(elem_data, exp)
      }
      elem_data <- as.data.frame(count(elem_data[names]))
      names(elem_data) <- c(names, "freq")
      elem_data <- elem_data[which(rowSums(elem_data[names]) != 0), ]
      x <- merge(mbdata, elem_data[names], by = names)
      elem_data <- merge(x[names], elem_data, by = names)
      x <- x$x
      elem_data$x <- x
      if((isTRUE(q[[i]]$active) == T) && (is.null(elem_data) == F)){
        act <- T
      }
      else if((isTRUE(q[[i]]$active) == F || is.null(q[[i]]$active) == T) && (is.null(elem_data) == F)){
        act <- F
      }
      elem_data$color <- elem_color
      elem_data$act <- act
    }
    rows <- rbind(rows, elem_data)
  }
  return(rows)
}