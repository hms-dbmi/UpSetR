## Apply custom functions passed into queries and generate list of data sets from these functions
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

## Generate list of data sets to overlay main bars
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

#Generate attribute data for queries of custom functions
CustomAttData <- function(custom_data, names){
  customAttDat <- data.frame()
  for(i in 1:length(custom_data)){
    customAttDat <- rbind(customAttDat, custom_data[[i]])
  }
  customAttDat <- customAttDat[ ,-which(names(customAttDat) %in% names)]
  return(customAttDat)
}


