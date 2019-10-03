\name{ModelTables}
\alias{cooccurExtra}
\docType{data}
\title{
Output list of "DisplayTable" function.
}
\description{
Output list of "DisplayTable" function, which includes a summary of statistic table, a summary of table with significant cooccurence ("positive", "negative", or "random") of species pairs. It also include two fancy formatted tables based on "formattable" package in types of htmlwidget.
}
\usage{data("ModelTables")}
\format{
  An output list of "DisplayTable" function, which contains the following moduels.
  \describe{
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
}
\examples{
## import data 
data("ModelTables")

## see the fancy tables in type of htmlwidget
ModelTables$STATSFT
ModelTables$SIGNFT
}