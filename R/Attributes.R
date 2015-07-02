#Generates data needed to represent selected intersections on matrix
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

#Generates intersection bar data to overlay main bars 
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

#Generate attribute data for intersection queries 
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
      if(is.null(att_y) == T){
        c1 <- match(att_x, colnames(intersect))
        colnames(intersect)[c1] <- "val1"
        if(is.null(exp) == F){
          intersect <- Subset_att(intersect, exp)
        }
        if(nrow(intersect) != 0){
          intersect$color <- inter_color
          intersect <- intersect[ , c("val1", "color")]
          intersect <- as.data.frame(intersect[order(intersect$val1), ])
        }
      }
      else if(is.null(att_y) == F){
        c1 <- match(att_x, colnames(intersect))
        c2 <- match(att_y, colnames(intersect))
        colnames(intersect)[c1] <- "val1"
        colnames(intersect)[c2] <- "val2"
        if(is.null(exp) == F){
          intersect <- Subset_att(intersect, exp)
        }
        intersect$color <- inter_color
        intersect <- intersect[ , c("val1", "val2", "color")]
        intersect <- as.data.frame(intersect[order(intersect$val1, intersect$val2), ])
        
      }
    }
    rows <- rbind(rows, intersect)
  }
  return(rows)
}

#Generate attribute data from element queries 
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

#Apply custom functions passed into queries and generate list of data sets from these functions
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
    data_sets[[i]]$color <- custom[[i]]$color
  }
  return(data_sets)
}

#Generate list of data sets to overlay main bars
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

#Seperate the queries between and paramters by built in(intersection and element), and custom functions
#Apply colors if not specified
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
      if(identical(intersects, queries[[i]]$query) == T || identical(elements, queries[[i]]$query) == T){
        seperated <- c(seperated, list(queries[[i]]))
      }
      else{
        next
      }
    }
  }
  else if(choice == 2){
    for(i in 1:length(queries)){
      if(identical(intersects, queries[[i]]$query) == F && identical(elements, queries[[i]]$query) == F){
        seperated <- c(seperated, list(queries[[i]]))
      }
      else{
        next
      }
    }
  }
  return(seperated)
}

#Create legend data for queries
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

#Make plot of that legend using legend data 
Make_legend <- function(legend){
  colors <- as.character(legend$colors)
  labels <- as.character(legend$numbers)
  
  leg <- legendGrob(labels =labels, pch=22,
                     gp=gpar(col = colors, fill = colors, fontsize = 8),
                     ncol = length(labels), hgap = unit(0, "lines"))
  return(leg)
}

#Generate attribute data for queries of custom functions
CustomAttData <- function(custom_data, att_x, att_y){
  if(is.null(att_y) == F && is.null(att_x) == T){
    warning("Please insert lone attribute to att.x paramter")
  }
  customAttDat <- data.frame()
  for(i in 1:length(custom_data)){
    customAttDat <- rbind(customAttDat, custom_data[[i]])
  }
  if(is.null(att_x) == F && is.null(att_y) == T){
    col1 <- match(att_x, colnames(customAttDat))
    colnames(customAttDat)[col1] <- "val1"
    customAttDat <- customAttDat[c("val1", "color")]
    customAttDat <- customAttDat[order(customAttDat$val1), ]
  }
  if(is.null(att_y) == F && is.null(att_y) == F){
    col1 <- match(att_x, colnames(customAttDat))
    col2 <- match(att_y, colnames(customAttDat))
    colnames(customAttDat)[col1] <- "val1"
    colnames(customAttDat)[col2] <- "val2"
    customAttDat <- customAttDat[c("val1", "val2", "color")]
    customAttDat <- customAttDat[order(customAttDat$val1, customAttDat$val2), ]
  }
  return(customAttDat)
}

#Combine all attribute data generated from queries 
combineQueriesData <- function(Intersection, Elements, Custom, att_x, att_y){
  all_data <- data.frame()
  all_data <- rbind(Intersection, Elements, Custom)
  if(length(all_data) == 0){
    return(NULL)
  }
  if(is.null(att_x) == F && is.null(att_y) == T){
    all_data <- all_data[order(all_data$val1), ]
  }
  else if(is.null(att_x) == F && is.null(att_y) == F){
    all_data <- all_data[order(all_data$val1, all_data$val2), ]
  }
  return(all_data)
}