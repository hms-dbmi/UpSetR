## ---- tidy=TRUE----------------------------------------------------------
library(UpSetR); library(ggplot2); library(grid)
movies <- read.csv( system.file("extdata", "movies.csv", package = "UpSetR"), header=T, sep=";" )

## ---- fig.width=12, fig.height=7, out.width="850px",tidy=TRUE, fig.align='center'----
upset(movies, main.bar.color = "black", queries = list(list(query = intersects, params = list("Drama"), active = T)), attribute.plots = list(gridrows = 50, plots = list(list(plot = histogram, x = "ReleaseDate", queries = F), list(plot = histogram, x = "AvgRating", queries = T)), ncols = 2))

## ---- fig.width=12, fig.height=7,out.width="850px",tidy=TRUE, fig.align='center'----
upset(movies, main.bar.color = "black", queries = list(list(query = intersects, params = list("Drama"), color = "red", active = F), list(query = intersects, params = list("Action", "Drama"), active = T), list(query = intersects, params = list("Drama", "Comedy", "Action"), color = "orange", active = T)), attribute.plots = list(gridrows = 45, plots = list(list(plot = scatter_plot, x = "ReleaseDate", y = "AvgRating", queries = T), list(plot = scatter_plot, x = "AvgRating", y = "Watches", queries = F)), ncols = 2), query.legend = "bottom")

## ---- tidy=TRUE----------------------------------------------------------
myplot <- function(mydata,x,y){
    plot <- (ggplot(data = mydata, aes_string(x=x, y=y, colour = "color")) + geom_point() +    scale_color_identity() + theme(plot.margin = unit(c(0,0,0,0), "cm")))
}

## ---- fig.width=12, fig.height=7, out.width="850px",tidy=TRUE, fig.align='center'----
upset(movies, main.bar.color = "black", queries = list(list(query = intersects, params = list("Drama"), color = "red", active = F), list(query = intersects, params = list("Action", "Drama"), active = T), list(query = intersects, params = list("Drama", "Comedy", "Action"), color = "orange", active = T)), attribute.plots = list(gridrows = 45, plots = list(list(plot = myplot, x = "ReleaseDate", y = "AvgRating", queries = T), list(plot = myplot, x = "AvgRating", y = "Watches", queries = F)), ncols = 2))

## ---- fig.width=12, fig.height=7,out.width="850px",tidy=TRUE, fig.align='center'----
upset(movies, main.bar.color = "black", mb.ratio = c(0.5,0.5), queries = list(list(query = intersects, params = list("Drama"), color = "red", active = F), list(query = intersects, params = list("Action", "Drama"), active = T), list(query = intersects, params = list("Drama", "Comedy", "Action"), color = "orange", active = T)), attribute.plots = list(gridrows=50, plots = list(list(plot = histogram, x = "ReleaseDate", queries = F), list(plot = scatter_plot, x = "ReleaseDate", y = "AvgRating", queries = T),list(plot = myplot, x = "AvgRating", y = "Watches", queries = F)), ncols = 3))

