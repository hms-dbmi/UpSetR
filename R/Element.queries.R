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
      if(is.null(att_y) == F){
        elems <- GetElements(data, index_q)
        end_col <- ((start_col + as.integer(length(names))) - 1)
        col1 <- match(att_x, colnames(elems))
        col2 <- match(att_y, colnames(elems))
        colnames(elems)[col1] <- "val1"
        colnames(elems)[col2] <- "val2"
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
        colnames(elems)[col1] <- "val1"
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
  rows <- as.data.frame(rows)
  if(length(rows) == 0){
    return(NULL)
  }
  else if(is.null(att_y) == F && length(rows) != 0){
    rows <- rows[c("val1","val2","color")]
    rows <- rows[order(rows$val1, rows$val2), ]
    return(rows)
  }
  else if(is.null(att_y) == T && length(rows) != 0){
    rows <- rows[c("val1", "color")]
    rows <- rows[order(rows$val1), ]
    return(rows)
  }
}