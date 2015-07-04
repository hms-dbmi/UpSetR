# UpSetR [![Travis-CI Build Status](https://travis-ci.org/hms-dbmi/UpSetR.svg?branch=master)](https://travis-ci.org/hms-dbmi/UpSetR)

## Sample Data

A sample data set for UpSet is included in the package and can be loaded like this:

```R
> movies <- read.csv( system.file("extdata", "movies.csv", package = "UpSetR"), header=T, sep=";" )
> mutations <- read.csv( system.file("extdata", "mutations.csv", package = "UpSetR"), header=T, sep = ",")
```

The sample data set is a movie data set created by the [GroupLens Lab](http://grouplens.org/datasets/movielens) and curated by [Bilal Alsallakh](https://github.com/bilalsal).

## Examples
A view of the UpSet plot with additional plots based on elements in the intersections.

![Image](https://cloud.githubusercontent.com/assets/12614369/8464958/2af1008c-2014-11e5-93d8-8d8442ec5631.png)

```R
upset(movies,attribute.plots=list(gridrows=60,plots=list(list(plot=scatter_plot, x="ReleaseDate", y="AvgRating"),
list(plot=scatter_plot, x="ReleaseDate", y="Watches"),list(plot=scatter_plot, x="Watches", y="AvgRating"),
list(plot=histogram, x="ReleaseDate")), ncols = 2))
```
A view of UpSetR mimicing the plot published by Lex & Gehlenborg
http://www.nature.com/nmeth/journal/v11/n8/full/nmeth.3033.html

![image](https://cloud.githubusercontent.com/assets/12614369/8468576/18d5ef52-203c-11e5-9f5d-e034ec41c538.png)

```R
upset(mutations, sets = c("PTEN", "TP53", "EGFR", "PIK3R1", "RB1"), sets.bar.color = "#56B4E9",
order.matrix = "freq", empty.intersections = "on")
```

An example using two set queries (war movies and noir movies) along with attribute plots comparing the average rating (top) and average rating vs the number of times the movies have been watched (bottom).

![image](https://cloud.githubusercontent.com/assets/1216518/8486663/2bc2bf44-20d4-11e5-9651-4b660a652b05.png)

```R
upset(movies, attribute.plots=list(gridrows = 100, ncols = 1, 
plots = list(list(plot=histogram, x="AvgRating",queries=T),
list(plot = scatter_plot, y = "AvgRating", x = "Watches", queries = T))), 
sets = c("Action", "Adventure", "Children", "War", "Noir"),
queries = list(list(query = intersects, params = list("War"), active = T),
list(query = intersects, params = list("Noir"))))
```


## Download
Download the latest version of UpSetR from GitHub with

```R
devtools::install_github("hms-dbmi/UpSetR")
```
