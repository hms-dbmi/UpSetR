Make_set_metadata_plot <- function(set.metadata, set_names){
  metadata <- as.data.frame(set.metadata$data)
  metadata_columns <- colnames(metadata)
  metadata_columns[1] <- "sets"
  names(metadata) <- metadata_columns
  metadata <- metadata[which(metadata$sets %in% set_names), ]
  metadata <- metadata[match(set_names, metadata$sets), ]
  metadata$sets <- seq(1,nrow(metadata))
  rownames(metadata) <- set_names
  
  num_of_att <- 0
  num_of_plots <- length(set.metadata$plots)
  
  for(i in 1:num_of_plots){
    if(set.metadata$plots[[i]]$type != "matrix_rows"){
      num_of_att <- num_of_att + 1
    }
    else{
       set.metadata$plots <- set.metadata$plots[-i]
       num_of_att <- length(set.metadata$plots)
       break
    }
  }
  
  metadata_plot <- list()

  if(num_of_att != 0){
  for(i in 1:num_of_att){
    if(is.null(set.metadata$plots[[i]]$colors) == FALSE){
      colors <- set.metadata$plots[[i]]$colors
    }
    else{
      colors <- NULL
    }
    if(set.metadata$plots[[i]]$type == "hist"){
      metadata_plot[[i]] <- metadataHist(metadata, set.metadata$plots[[i]]$column, colors)
      metadata_plot[[i]] <- ggplot_gtable(ggplot_build(metadata_plot[[i]]))
    }
    if(set.metadata$plots[[i]]$type == "heat"){
      metadata_plot[[i]] <- metadataHeat(metadata, set.metadata$plots[[i]]$column, set.metadata$plots[[i]]$type, colors)
      metadata_plot[[i]] <- ggplot_gtable(ggplot_build(metadata_plot[[i]]))
    }
    if(set.metadata$plots[[i]]$type == "bool"){
      metadata_plot[[i]] <- metadataHeat(metadata, set.metadata$plots[[i]]$column, set.metadata$plots[[i]]$type, colors)
      metadata_plot[[i]] <- ggplot_gtable(ggplot_build(metadata_plot[[i]]))
    }
    if(set.metadata$plots[[i]]$type == "text"){
      if(is.null(set.metadata$plots[[i]]$alignment)){
        alignment <- NULL
      }
      else{
        alignment <- set.metadata$plots[[i]]$alignment
      }
      metadata_plot[[i]] <- metadataText(metadata, set.metadata$plots[[i]]$column, colors, alignment)
      metadata_plot[[i]] <- ggplot_gtable(ggplot_build(metadata_plot[[i]]))
    }
    if(set.metadata$plots[[i]]$type == "matrix_rows"){
      next
    }
  }

  return(list(metadata_plot, set.metadata))
  }
  else{
    return(c(NULL, NULL))
  }
}