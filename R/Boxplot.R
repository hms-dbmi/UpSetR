## Create data for boxplots of all intersections
IntersectionBoxPlot <- function(data1, data2, start_col, names){
  end_col <- ((start_col + length(names)) - 1)
  data2 <- data2[which(rowSums(data2[ ,start_col:end_col]) != 0), ]
  #tagging because x axis values need to be 1:number of sets so they line up with their intersections
  data2$tag <- 1:nrow(data2)
  sets <- list()
  intersections <- list()
  box_plot_data <- data.frame()
  for(i in 1:nrow(data1)){
    sets[[i]] <- colnames(data1)[which(data1[i, 1:length(names)] == 0)]
  }
  for(i in 1:length(sets)){
    intersections[[i]] <- data2[(rowSums(data2[ ,start_col:end_col]) == (length(names) - length(as.character(sets[[i]])))), ]
    intersections[[i]] <- Wanted(intersections[[i]], as.character(sets[[i]]))
    end <- ((start_col + (length(names) - length(as.character(sets[[i]]))))-1)
    if(start_col == end){
      intersections[[i]] <- intersections[[i]][(intersections[[i]][ ,start_col]) == 1, ]
      intersections[[i]] <- intersections[[i]]$tag
    }
    else{
      num <- length(names) - length(as.character(sets[[i]]))
      intersections[[i]] <- intersections[[i]][(rowSums(intersections[[i]][ ,start_col:end]) == num), ]
      intersections[[i]] <- intersections[[i]]$tag
    }
    intersections[[i]] <- data2[data2$tag %in% as.numeric(intersections[[i]]), ]
    intersections[[i]]$x <- i
  }
  for(i in 1:length(intersections)){
    box_plot_data <- rbind(box_plot_data, intersections[[i]])
  }
  return(box_plot_data)
}

## Generate boxplot summary plots
BoxPlotsPlot <- function(bdat, att, att_color){
  yaxis <- as.character(att)
  col <- match(att, colnames(bdat))
  colnames(bdat)[col] <- "attribute"
  upper_xlim <- as.numeric((max(bdat$x) + 1))
  plot_lims <- as.numeric(0:upper_xlim)
  bdat$x <- as.factor(bdat$x)
  boxplots <- ggplotGrob(ggplot()
                         + theme_bw() +ylab(yaxis)
                         + scale_x_discrete(limits = plot_lims, expand = c(0,0))
                         + theme(plot.margin = unit(c(-0.7,0,0,0), "cm"),
                                 axis.title.y = element_text(vjust = -0.8),
                                 axis.ticks.x = element_blank(),
                                 axis.text.x = element_blank(),
                                 panel.border = element_blank(),
                                 panel.grid.minor = element_blank(),
                                 panel.grid.major = element_blank(),
                                 axis.title.x = element_blank())
                         + geom_boxplot(data = bdat, aes_string(x="x", y="attribute"),
                                        fill = att_color, colour = "gray80"))
  return(boxplots)
}