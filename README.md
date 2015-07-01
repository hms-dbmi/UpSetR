# UpSetR

## Sample Data

A sample data set for UpSet is included in the package and can be loaded like this:

```R
> movies <- read.csv( system.file("extdata", "movies.csv", package = "UpSetR"), header=T, sep=";" )
> attach( movies )
```

The sample data set is a movie data set created by the [GroupLens Lab](http://grouplens.org/datasets/movielens) and curated by [Bilal Alsallakh](https://github.com/bilalsal).

## Example
A view of the UpSet plot with additional plots based on elements in the intersections.

[Image](https://cloud.githubusercontent.com/assets/12614369/8464958/2af1008c-2014-11e5-93d8-8d8442ec5631.png)

## Download
Download the latest version of UpSetR from github with

```R
devtools::install_github("hms-dbmi/UpSetR")
```
