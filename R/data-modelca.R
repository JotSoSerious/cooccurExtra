#' Output list of "coocana" function (with splits of ingradients)
#'
#' Output list of "coocana" function (with splits of ingradients), which includes cooccur models, matrix for using "cooccur" function, splits informations, statistic tables, the used species pairs and the used ingradients.
#'
#' @format An output list of "coocana" function (with splits of ingradients), which contains the following moduels.
#' \describe{
#' \item{Model}{Output models of the "cooccur" function.}
#' \item{Splits}{Splits of different ingradients that detected by tree based method.}
#' \item{Stats}{Summary statistic tables form the cooccur models.}
#' \item{Pairs}{The used species pairs in the analysis.}
#' \item{Cooc.Mat}{A matrix for the usage of "cooccur" model.}
#' \item{Used.Ingradients}{The used ingradients in the analysis.}
#' }
#' @examples
#' data(modelca)
"modelca"
