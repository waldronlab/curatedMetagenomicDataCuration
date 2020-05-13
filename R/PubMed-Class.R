#' @title Define a PubMed class object for containing a PubMed article citation
#'
#' @slot pubinfo a data.frame containing publication title, journal, DOI, month, year
#' @slot authors a data.frame containing authors' first names, last names, and initials
#' @slot affiliations a data.frame containing author affiliations
#'
#' @return A PubMed-class object with a list of three data frames 
#' @export checkCuration
#' @details A helper class for \code{\link{PubMed}}

setClass("PubMed", list(pubinfo="data.frame", authors="data.frame", affiliations="data.frame"))

setMethod("show", "PubMed", function(object){
  cat(
    paste(
      paste(
        object@pubinfo$title,
        object@pubinfo$journal,
        object@pubinfo$DOI,
        object@pubinfo$month,
        object@pubinfo$year,
        sep=", "
      )
      , paste(
        paste(
          object@authors$first_init,
          ". ",
          object@authors$last_name, sep=""),
        collapse=", "),
      paste(unique(object@affiliations[,1]), collapse=" "),
      cat(object@pubinfo$abstract)
      , sep="\n\n")
  )
})
