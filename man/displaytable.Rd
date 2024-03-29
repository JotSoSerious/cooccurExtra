\name{displaytable}
\alias{cooccurExtra}
\title{
A function for display the result tables.
}
\description{
This is a function of the package "cooccurExtra". The main idea of this function is displaying the result tables in fancy format. It provides both significant cooccurence table and a table with statistic information.
}
\usage{
displaytable(mymod, ingradients = mymod$Used.Ingradients, with.splits = FALSE,
             table.out = c("both", "stats", "significants"),
             full_stats = c("full", "prob", "obs"),
             collapse_row = c("no", "top", "bottom"))
}
\arguments{
  \item{mymod}{
     A list object generated by "coocana".
}
  \item{ingradients}{
     A list of charaters with names of the needed ingradients where to be shown in the tables.
}
  \item{with.splits}{
     Logical. "TRUE" if the tables with splits information are preferred. Cannot be "TRUE" when the input model without any split information.
}
  \item{table.out}{
     Charater string, a choice of preferance output tables. (table.out = "stats") if only the table with statistics is needed. (table.out = "significants") if only the table with cooccurence association is needed. The default value is (table.out = "both"), which returns and shows both kinds of tables.
}
  \item{full_stats}{
     Charater string, a choice of preferance output of statistic tables. (full_stats = "prob") if probability values are needed in statistics table. (full_stats = "obs") shows the observed number of species occurrence or co-occurrence. The default value is (full_stats = "full"), which returns all the statistic from coocana() output.
}
  \item{collapse_row}{
     Character string, options of preferrance of the aligning form. (collapse_row = "top") gives the tables with species names and ingradients aligned to the top. (collapse_row = "bottom") gives the tables with species names and ingradients aligned to the bottom. The default value is (collapse_row = "no"), which returns and shows no aligned tables.
}
}
\value{
  Returns a list of tables in the form of data frames and tables in form of htmlwidgets.
\item{STATSTAB}{
     Statistic table in the form of data frame.
}
  \item{SIGNTAB}{
     Significant table in the form of data frame.
}
  \item{STATSFT}{
     Statistic table in the form of htmlwidget.
}
  \item{SIGNFT}{
     Significant table in the form of htmlwidget.
}
}
\author{
  Yingjia Jot He
}
\examples{
  # create a vector of charaters with all the needed ingradients' names
  Ing.Needed <- c("Log.mud.1.", "SST")

  # ask for the model generated by "coocana" (with splits)
  data(modelca)

  # default coocana with splits
  mytest <- displaytable(mymod = modelca)
  # set "with splits" as true
  mytest <- displaytable(mymod = modelca,
                         with.splits = TRUE,
                         table.out = "both")
  # see different row collapse options
  mytest <- displaytable(mymod = modelca,
                         with.splits = TRUE,
                         collapse_row = "top")
  mytest <- displaytable(mymod = modelca,
                         with.splits = TRUE,
                         collapse_row = "bottom")

  # case with more ingradients
  mytest <- displaytable(mymod = modelca,
                         ingradients = c(Ing.Needed , "more"),
                         with.splits = TRUE,
                         table.out = "significants")
  # case with less ingradients
  mytest <- displaytable(mymod = modelca,
                         ingradients = Ing.Needed[-1],
                         with.splits = TRUE,
                         table.out = "stats")
}
