\name{treesplits}
\alias{cooccurExtra}
\title{
A function to detect break points of species occurance in different ingradients.
}
\description{
This is a function of the package "cooccurExtra". The main idea of this function is detecting break points of species occurance in different ingradients with the information of species and ingradients from different regions. It provides two kinds of plots to show the breaks points in different ingradients of each specie. Also it provides an output of break points (splits) of each ingradient and for each specie.
}
\usage{
treesplits = function(mydata, species, ingradients,
                      show.plots = c("both", "splits", "dendrogram", "none"))
}
\arguments{
  \item{mydata}{
     A data frame, which contains species and ingradients information from different regions.
}
  \item{species}{
     A list of charaters with names of the needed species.
}
  \item{ingradients}{
     A list of charaters with names of the needed ingradients where to be deteted break points.
}
  \item{show.plots}{
     A choice of the showing plots. (show.plots = "splits") shows only the break points in the scatter plots. (show.plots = "dendrgram") shows only the tree dendrograms from the "rpart" models. (show.plots = "none") if no plot is needed. The default is (show.plots = "both"), which shows both the splits plots and the dendrograms.
}
}
\value{
  Returns a list of break points in different ingradients and for different species.
}
\author{
  Yingjia Jot He
}
\examples{
  # create a vector of charaters with all the needed species' names
  Ten.Needed <- c("alpheus", "theora.lubrica", "aricidea",
                 "paradonidae.other", "austrovenus.stutchburyi", "macomona.liliana",
                 "maldanidae", "aonides", "prionospio.aucklandica", "orbiniidae")

  # create a vector of charaters with all the needed ingradients' names
  Ing.Needed <- c("Log.mud.1.", "SST")

  # ask for the data frame to be analized
  data(macrofauna)

  # the default with two kinds of plots
  mytest <- treesplits(macrofauna, species = Ten.Needed, ingradients = Ing.Needed)

  # show the output
  mytest

  # case with one specie that does not exist and show only the splits plots
  mytest <- treesplits(macrofauna, species = c(Ten.Needed, "fake.specie"),
                      ingradients = Ing.Needed, show.plots = "splits")

  # case with one ingradient that does not exist and show only the dendrograms
  mytest <- treesplits(macrofauna, species = Ten.Needed,
                      ingradients = c(Ing.Needed, "fake.specie"),
                      show.plots = "dendrogram")
}
