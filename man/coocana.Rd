\name{coocana}
\alias{cooccurExtra}
\title{
A function for a further analysis into the cooccurence between species base on function "cooccur".
}
\description{
This is the main function of the package "cooccurExtra". The main idea of this function is to do a further analysis into the cooccurence between species base on function "cooccur". It helps reproduce the analysis of cooccurences between different species pairs in different ingradients. It can be used in diagnostic pairs, creating splits plots, creating matrix for the use of "cooccur" function and displaying fancy output tables.
}
\usage{
coocana(mydata, ID.Col = "es", species = NA, ingradients = NA,
        type = c("byrow", "bycol", "all"), with.splits = FALSE,
        show.tables = FALSE, pair.scatter = c("scatter", "fit", "none"),
        any.output = c("lite", "full", "none"),
        show.split.plot = c("none", "both", "splits", "dendrogram"))
}
\arguments{
  \item{mydata}{
     A data frame, which contains species and ingradients information from different regions.
}
  \item{ID.Col}{
     Charater string, gives the name of a column in the data frame for the use of identifier. With a default value "es".
}
  \item{species}{
     A list of charaters with names of the needed species.
}
  \item{ingradients}{
     A list of charaters with names of the needed ingradients where to be shown in the tables.
}
  \item{type}{
     Charaters string, choices of species pairing types. (type = "byrow") by default. Pairing all possible pairs of species if (type = "all").
}
  \item{with.splits}{
     Logical. "TRUE" if the tables with splits information are preferred. Cannot be "TRUE" when the input model without any split information.
}
  \item{show.tables}{
     Logical. Display fancy tables if "TRUE".
}
  \item{pair.scatter}{
     Charaters string, choices of displaying scatter plots types. By default, display only the scatter plots. (pair.scatter = "fit") if the scatter plot with fitted curve is preferred. (pair.scatter = "none") if no scatter plot is needed.
}
  \item{any.output}{
     Charaters string, choices of output model types. By default, output the "lite" results. (any.output = "full") if the full result is preferred. (any.output = "none") if no result is needed.
}
  \item{show.split.plot}{
     Charaters string,choices of the showing plots. (show.plots = "splits") shows only the break points in the scatter plots. (show.plots = "dendrgram") shows only the tree dendrograms from the "rpart" models. (show.plots = "none") if no plot is needed. The default is (show.plots = "both"), which shows both the splits plots and the dendrograms.
}
}
\value{
  Returns a list of objects inculding models, splits and tables and etc.
  \item{Model}{
     A list of "cooccur" models in different splited ranges for different species pairs. Or one "cooccur" model if (with.splits = FALSE).
}
  \item{Splits}{
     A list of break points in different ingradients and for different species. Or "NA" if (with.splits = FALSE).
}
  \item{Stats}{
     Statistic table containing all analyzed species pairs and probabilities for rejecting classifying positive and negative associations.
}
  \item{Pairs}{
     The used pairs of species.
}
  \item{Cooc.Mat}{
     A matrix in the form for the usage of "cooccur" function.
}
  \item{Used.Ingradients}{
     The used ingradients.
}
}
\author{
  Yingjia Jot He
}
\examples{
  # create a vector of charaters with all the needed species' names
  Four.Needed <- c("alpheus", "theora.lubrica",
                   "austrovenus.stutchburyi", "macomona.liliana")

  # create a vector of charaters with all the needed ingradients' names
  Ing.Needed <- c("Log.mud.1.", "SST")

  # ask for the data frame to be analized
  data(macrofauna)

  # show the scatter plot with fitted curve
  mytest = coocana(mydata = macrofauna, species = Four.Needed,
                   pair.scatter = "fit")

  # show the split plots and the dendrograms
  mytest = coocana(mydata = macrofauna, species = Four.Needed,
                   ingradients = Ing.Needed, with.splits = TRUE,
                   pair.scatter = "none", show.split.plot = "both")

  # show fancy tables
  mytest = coocana(mydata = macrofauna, species = Four.Needed,
                   ingradients = Ing.Needed, with.splits = TRUE,
                   pair.scatter = "none", show.tables = TRUE)

}
