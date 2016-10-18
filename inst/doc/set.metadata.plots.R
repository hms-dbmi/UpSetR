## ---- tidy=TRUE----------------------------------------------------------
library(UpSetR)
movies <- read.csv( system.file("extdata", "movies.csv", package = "UpSetR"), header=T, sep=";" )

## ---- tidy=TRUE----------------------------------------------------------
sets <- names(movies[3:19])
avgRottenTomatoesScore <- round(runif(17, min=0, max = 90))
metadata <- as.data.frame(cbind(sets, avgRottenTomatoesScore))
names(metadata) <- c("sets", "avgRottenTomatoesScore")

## ---- tidy=TRUE----------------------------------------------------------
is.numeric(metadata$avgRottenTomatoesScore)

## ---- tidy=TRUE----------------------------------------------------------
metadata$avgRottenTomatoesScore <- as.numeric(as.character(metadata$avgRottenTomatoesScore))

## ---- fig.width=9, fig.height=5,out.width="850px", tidy=TRUE, fig.align='center'----
upset(movies, set.metadata = list(data = metadata, plots = list(list(type="hist", column="avgRottenTomatoesScore", assign=20))))

## ---- tidy=TRUE----------------------------------------------------------
Cities <- sample(c("Boston","NYC","LA"), 17, replace = T)
metadata <- cbind(metadata, Cities)
metadata$Cities <- as.character(metadata$Cities)
metadata[which(metadata$sets %in% c("Drama", "Comedy", "Action", "Thriller", "Romance")), ]

## ---- fig.width=9, fig.height=5,out.width="850px", tidy=TRUE, fig.align='center'----
upset(movies, set.metadata = list(data = metadata, plots = list(list(type = "heat", column = "Cities", assign = 10, colors = c("Boston" = "green", "NYC" = "navy", "LA" = "purple")))))

## ---- fig.width=9, fig.height=5,out.width="850px", tidy=TRUE, fig.align='center'----
upset(movies, set.metadata = list(data = metadata, plots = list(list(type = "heat", column = "Cities", assign = 10, colors = c("Boston" = "green", "NYC" = "navy", "LA" = "purple")), list(type = "heat", column = "avgRottenTomatoesScore", assign = 10))))

## ---- fig.width=9, fig.height=5,out.width="850px", tidy=TRUE, fig.align='center'----
accepted <- round(runif(17, min = 0, max = 1))
metadata <- cbind(metadata, accepted)
metadata[which(metadata$sets %in% c("Drama", "Comedy", "Action", "Thriller", "Romance")), ]
upset(movies, set.metadata = list(data = metadata, plots = list(list(type="bool", column= "accepted", assign = 5, colors = c("#FF3333", "#006400")))))

## ---- fig.width=9, fig.height=5,out.width="850px", tidy=TRUE, fig.align='center'----
upset(movies, set.metadata = list(data = metadata, plots = list(list(type="heat", column= "accepted", assign = 5, colors = c("red", "green")))))

## ---- fig.width=9, fig.height=5,out.width="850px", tidy=TRUE, fig.align='center'----
upset(movies, set.metadata = list(data = metadata, plots = list(list(type = "text", column = "Cities", assign = 10, colors = c("Boston" = "green", "NYC" = "navy", "LA" = "purple")))))

## ---- fig.width=9, fig.height=5,out.width="850px", tidy=TRUE, fig.align='center'----
upset(movies, set.metadata = list(data = metadata, plots = list(list(type="hist", column="avgRottenTomatoesScore", assign=20),list(type="matrix_rows", column = "Cities", colors = c("Boston" = "green", "NYC" = "navy", "LA" = "purple"), alpha = 0.5))))

## ---- fig.width=9, fig.height=5,out.width="850px", tidy=TRUE, fig.align='center'----
upset(movies, set.metadata = list(data = metadata, plots = list(list(type="hist", column="avgRottenTomatoesScore", assign=20),list(type="bool", column= "accepted", assign = 5, colors = c("#FF3333", "#006400")), list(type = "text", column = "Cities", assign = 5, colors = c("Boston" = "green", "NYC" = "navy", "LA" = "purple")))))

## ---- fig.width=9, fig.height=5,out.width="850px", tidy=TRUE, fig.align='center'----
upset(movies, set.metadata = list(data = metadata, plots = list(list(type="hist", column="avgRottenTomatoesScore", assign=20), list(type="bool", column= "accepted", assign = 5, colors = c("#FF3333", "#006400")), list(type="text", column="Cities", assign=5, colors=c("Boston"="green","NYC"="navy","LA"="purple")), list(type="matrix_rows", column="Cities", colors=c("Boston"="green", "NYC"="navy", "LA"="purple"), alpha=0.5))), queries=list(list(query=intersects, params=list("Drama"), color="red", active=F), list(query=intersects, params=list("Action", "Drama"), active = T), list(query=intersects, params=list("Drama", "Comedy", "Action"), color="orange", active=T)), attribute.plots = list(gridrows=45, plots = list(list(plot=scatter_plot, x="ReleaseDate", y="AvgRating", queries=T), list(plot=scatter_plot, x="AvgRating", y="Watches", queries=F)), ncols=2), query.legend="bottom")

