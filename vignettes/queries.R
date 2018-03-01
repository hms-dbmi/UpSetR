## ---- tidy=TRUE----------------------------------------------------------
library(UpSetR)
movies <- read.csv( system.file("extdata", "movies.csv", package = "UpSetR"), header=T, sep=";" )

## ---- out.width="850px", fig.width=9, fig.height =5, tidy=TRUE, fig.align='center'----
upset(movies, queries = list(list(query = intersects, params = list("Drama", "Comedy", "Action"), color = "orange", active = T), list(query = intersects, params = list("Drama"), color = "red", active = F), list(query = intersects, params = list("Action", "Drama"), active = T)))

## ---- out.width="850px", fig.width=9, fig.height =5, tidy=TRUE, fig.align='center'----
upset(movies, queries = list(list(query = elements, params = list("AvgRating", 3.5, 4.1), color = "blue", active = T), list(query = elements, params = list("ReleaseDate", 1980, 1990, 2000), color = "red", active = F)))

## ---- out.width="850px", fig.width=9, fig.height =5, tidy=TRUE, fig.align='center'----
upset(movies, queries = list(list(query = intersects, params = list("Action", "Drama"), active = T), list(query = elements, params = list("ReleaseDate", 1980, 1990, 2000), color = "red", active = F)), expression = "AvgRating > 3 & Watches > 100")

## ---- tidy=TRUE----------------------------------------------------------
Myfunc <- function(row, release, rating){
    data <- (row["ReleaseDate"] %in% release) & (row["AvgRating"] > rating)
}

## ---- out.width="850px", fig.width=9, fig.height =5, tidy=TRUE, fig.align='center'----
upset(movies, queries = list(list(query = Myfunc, params = list(c(1970,1980, 1990, 1999, 2000), 2.5), color = "blue", active =T)))

## ---- out.width="850px", fig.width=9, fig.height =5, tidy=TRUE, fig.align='center'----
upset(movies, query.legend = "top", queries = list(list(query = intersects, params = list("Drama", "Comedy", "Action"), color = "orange", active = T, query.name="Funny action"), list(query = intersects, params = list("Drama"), color = "red", active = F), list(query = intersects, params = list("Action", "Drama"), active = T, query.name="Emotional action")))

## ---- out.width="850px", fig.width=9, fig.height =5, tidy=TRUE, fig.align='center'----
upset(movies, query.legend = "bottom", queries = list(list(query = Myfunc, params = list(c(1970,1980, 1990, 1999, 2000), 2.5), color = "orange", active =T), list(query = intersects, params = list("Action", "Drama"), active = F), list(query = elements, params = list("ReleaseDate", 1980, 1990, 2000), color = "red", active = F, query.name="Decades")), expression = "AvgRating > 3 & Watches > 100")

