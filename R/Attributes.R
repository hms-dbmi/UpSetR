QuerieInterData <- function(q, data1, first_col, num_sets, data2, exp, names, palette){
  rows <- data.frame()
  for(i in 1:length(q)){
    index_q <- unlist(q[i])
    inter_color <- palette[i]
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


QuerieInterBar  <- function(q, data1, first_col, num_sets, data2, exp, names, palette){
  rows <- data.frame()
  for(i in 1:length(q)){
    index_q <- unlist(q[i])
    inter_color <- palette[i]
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

QuerieInterAtt <- function(data, first_col, q, num_sets, att_x, att_y, exp, names, palette){
  rows <- data.frame()
  for(i in 1:length(q)){
    index_q <- unlist(q[i])
    inter_color <- palette[i]
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

QuerieElemAtt <- function(data, q, start_col, exp, names, att_x, att_y, palette){
  rows <- data.frame()
  for(i in 1:length(q)){
    index_q <- unlist(q[i])
    elem_color <- palette[i]
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

CustomFunctions <- function(data, remove,...){
  FUNS <- list(...)
  if(length(FUNS) == 0){
    return(NULL)
  }
  else{
  num_of_fun <- length(FUNS)
  data_sets <- list()
  for( i in 1:num_of_fun){
    data_sets[[i]] <- FUNS[[i]](data)
    data_sets[[i]] <- Wanted(data_sets[[i]], remove)
  }
  }
  return(data_sets)
}