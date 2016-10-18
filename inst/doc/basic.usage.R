## ---- tidy =TRUE---------------------------------------------------------
library(UpSetR)
movies <- read.csv( system.file("extdata", "movies.csv", package = "UpSetR"), header=T, sep=";" )

## ---- tidy=TRUE----------------------------------------------------------
#example of list input (list of named vectors)
listInput <-list(one = c(1,2,3,5,7,8,11,12,13), two = c(1,2,4,5,10), three = c(1,5,6,7,8,9,10,12,13))

#example of expression input
expressionInput <- c("one" = 2, "two" = 1, "three" = 2, "one&two" = 1, "one&three" = 4, "two&three" = 1, "one&two&three" = 2)

## ---- out.width="850px", fig.width=9, fig.height =5,tidy=TRUE, fig.align='center'----
upset(fromList(listInput), order.by = "freq")

## ---- out.width="850px", fig.width=9, fig.height =5,tidy=TRUE, fig.align='center'----
upset(fromExpression(expressionInput), order.by = "freq")

## ---- out.width="850px", fig.width=9, fig.height =5, tidy =TRUE, fig.align='center'----
upset(movies, nsets = 6, number.angles = 30, point.size = 3.5, line.size = 2, mainbar.y.label = "Genre Intersections", sets.x.label = "Movies Per Genre", text.scale=c(1.3, 1.3, 1, 1, 2, 0.75))

## ---- out.width="850px", fig.width=9, fig.height =5, tidy=TRUE, fig.align='center'----
upset(movies, sets = c("Action", "Adventure", "Comedy", "Drama", "Mystery", "Thriller", "Romance", "War", "Western"), mb.ratio = c(0.55,0.45), order.by = "freq")

## ---- out.width="850px", fig.width=9, fig.height =5,tidy=TRUE, fig.align='center'----
upset(movies, sets = c("Action", "Adventure", "Comedy", "Drama", "Mystery", "Thriller", "Romance", "War", "Western"), mb.ratio = c(0.55,0.45), order.by = "degree")

## ---- out.width="850px", fig.width=9, fig.height =5,tidy=TRUE, fig.align='center'----
upset(movies, sets = c("Action", "Adventure", "Comedy", "Drama", "Mystery", "Thriller", "Romance", "War", "Western"), mb.ratio = c(0.55,0.45), order.by = c("degree", "freq"))

## ---- out.width="850px", fig.width=9, fig.height =5, tidy=TRUE, fig.align='center'----
upset(movies, sets = c("Action", "Adventure", "Comedy", "Drama", "Mystery", "Thriller", "Romance", "War", "Western"), mb.ratio = c(0.55,0.45), order.by = "freq", keep.order = TRUE)

## ---- out.width="850px", fig.width=9, fig.height =5,tidy=TRUE, fig.align='center'----
upset(movies, nintersects = 70, group.by = "sets", cutoff = 7)

## ---- out.width="850px", fig.width=9, fig.height =5,tidy=TRUE, fig.align='center'----
upset(movies, empty.intersections = "on", order.by = "freq")

