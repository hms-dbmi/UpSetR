## ---- tidy =TRUE---------------------------------------------------------
library(UpSetR)
movies <- read.csv( system.file("extdata", "movies.csv", package = "UpSetR"), header=T, sep=";" )

## ---- out.width="850px", fig.width=13, fig.height =7, tidy =TRUE, fig.align='center'----
upset(movies, nsets = 6, number.angles = 30, point.size = 5, name.size = 12, line.size = 2)

## ---- out.width="850px", fig.width=13, fig.height =7, tidy=TRUE, fig.align='center'----
upset(movies, sets = c("Action", "Adventure", "Comedy", "Drama", "Mystery", "Thriller", "Romance", "War", "Western"), mb.ratio = c(0.55,0.45), order.by = "freq")

## ---- out.width="850px", fig.width=13, fig.height =7,tidy=TRUE, fig.align='center'----
upset(movies, sets = c("Action", "Adventure", "Comedy", "Drama", "Mystery", "Thriller", "Romance", "War", "Western"), mb.ratio = c(0.55,0.45), order.by = "degree")

## ---- out.width="850px", fig.width=13, fig.height =7,tidy=TRUE, fig.align='center'----
upset(movies, sets = c("Action", "Adventure", "Comedy", "Drama", "Mystery", "Thriller", "Romance", "War", "Western"), mb.ratio = c(0.55,0.45), order.by = c("degree", "freq"))

## ---- out.width="850px", fig.width=13, fig.height =7,tidy=TRUE, fig.align='center'----
upset(movies, nintersects = 70, group.by = "sets", cutoff = 7)

## ---- out.width="850px", fig.width=13, fig.height =7,tidy=TRUE, fig.align='center'----
upset(movies, empty.intersections = "on", order.by = "freq")

