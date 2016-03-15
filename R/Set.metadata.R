Make_set_metadata_plot <- function(set.metadata, set_names){
  metadata <- set.metadata$data
  num_of_att <- ncol(metadata)-1
  metadata_columns <- colnames(metadata)
  metadata_columns[1] <- "sets"
  names(metadata) <- metadata_columns
  metadata <- metadata[which(metadata$sets %in% set_names), ]
  metadata <- metadata[order(set_names), ]
  metadata$sets <- seq(1,nrow(metadata))
  rownames(metadata) <- set_names
  
  y_data <- c()
  plot_type <- c()
  
    for(i in 1:num_of_att){
      y_data[i] <- names(metadata[i+1])
      plot_type[i] <- set.metadata$type[i]
    }
  
  for(i in 1:num_of_att){
    if(plot_type[i] == "hist"){
      metadata_plot <- metadataHist(metadata, y_data[i])
      metadata_plot <- ggplot_gtable(ggplot_build(metadata_plot))
    }
    if(plot_type[i] == "heat"){
      metadata_plot <- metadataHeat(metadata, y_data[i], plot_type[i])
      metadata_plot <- ggplot_gtable(ggplot_build(metadata_plot))
    }
    if(plot_type[i] == "bool"){
      metadata_plot <- metadataHeat(metadata, y_data[i], plot_type[i])
      metadata_plot <- ggplot_gtable(ggplot_build(metadata_plot))
    }
    if(plot_type[i] == "text"){
      metadata_plot <- metadataText(metadata, y_data[i])
      metadata_plot <- ggplot_gtable(ggplot_build(metadata_plot))
    }
  }
  
return(metadata_plot)
}