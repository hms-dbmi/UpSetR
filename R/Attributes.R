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

QuerieInterAtt <- function(data, first_col, q, num_sets, att_x, att_y, exp, names, palette){
  rows <- data.frame()
  if(length(q) == 0){
    return(NULL)
  }
  for(i in 1:length(q)){
    index_q <- unlist(q[[i]]$params)
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
  if(length(q) == 0){
    return(NULL)
  }
  for(i in 1:length(q)){
    index_q <- unlist(q[[i]]$params)
    elem_color <- palette[i]
    test <- as.character(index_q[1])
    check <- match(test, names)
    if(length(check) != 0){
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

customQueries <- function(data, custom, names){
  data_sets <- list()
  dataAndrow <- list(data, 1)
  if(length(custom) == 0){
    return(NULL)
  }
  for(i in 1:length(custom)){
    x <- c(dataAndrow, custom[[i]]$query, (custom[[i]]$params))
    x <- do.call(apply, x)
    x <- data[which(x), ]
    data_sets[[i]] <- x
    first <- min(match(names, colnames(data_sets[[i]])))
    last <- max(match(names, colnames(data_sets[[i]])))
    data_sets[[i]] <- data_sets[[i]][!(rowSums(data_sets[[i]][ ,first:last]) == 0), ]
    data_sets[[i]]$color2 <- custom[[i]]$color
  }
  return(data_sets)
}

customQueriesBar <- function(cust_data, sets,bar_data,custom){
  setup <- list()
  final_data <- list()
  num <- (length(sets) + 1)
  if(length(cust_data) == 0){
    return(NULL)
  }
  for(i in 1:length(cust_data)){
    cust_data[[i]] <- count(cust_data[[i]][sets]) 
    colnames(cust_data[[i]])[num] <- "freq2"
    cust_data[[i]] <- cust_data[[i]][!(rowSums(cust_data[[i]][ ,1:length(sets)]) == 0), ]
    setup[[i]] <- merge(cust_data[[i]], bar_data, by = sets)
    color2 <- rep(custom[[i]]$color, times = nrow(setup[[i]]))
    if(isTRUE(custom[[i]]$active) == T){
      act <- rep(T, nrow(setup[[i]]))
    }
    else{
      act <- rep(F, nrow(setup[[i]]))
    }
    setup[[i]] <- cbind(setup[[i]], color2, act)
  }
  for(i in 1:length(setup)){
    final_data <- rbind(final_data, setup[[i]])
  }
  return(final_data)
}

SeperateQueries <- function(queries, choice, palette){
  seperated <- list()
  for(i in 1:length(queries)){
    if(is.null(queries[[i]]$color) == T){
      queries[[i]]$color <- palette[1]
      palette <- palette[-1]
    }
    else if(is.null(queries[[i]]$color) == F){
      next
    }
  }
  if(choice == 1){
    for(i in 1:length(queries)){
      if(is.function(queries[[i]]$query) == F){
        seperated <- c(seperated, list(queries[[i]]))
      }
      else{
        next
      }
    }
  }
  else if(choice == 2){
    for(i in 1:length(queries)){
      if(is.function(queries[[i]]$query) == T){
        seperated <- c(seperated, list(queries[[i]]))
      }
      else{
        next
      }
    }
  }
  return(seperated)
}

GuideGenerator <- function(queries, palette){
  numbers <- c()
  colors <- c()
  if(length(queries) == 0){
    return(NULL)
  }
  for(i in 1:length(queries)){
    if(is.null(queries[[i]]$color) == T){
      queries[[i]]$color <- palette[1]
      palette <- palette[-1]
    }
    else if(is.null(queries[[i]]$color) == F){
      queries[[i]]$color <- queries[[i]]$color
    }
    colors[i] <- queries[[i]]$color
    numbers[i] <- paste("Query", as.character(i), sep = "")
  }
guide <- cbind(numbers, colors)
return(as.data.frame(guide))
}

Make_legend <- function(legend){
  colors <- as.character(legend$colors)
  labels <- as.character(legend$numbers)
  
  leg <- legendGrob(labels =labels, pch=22,
                     gp=gpar(col = colors, fill = colors, fontsize = 8),
                     ncol = length(labels), hgap = unit(0, "lines"))
  return(leg)
}


