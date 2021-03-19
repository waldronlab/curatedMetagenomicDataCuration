#' Title Retrieve article details from PubMed
#'
#' @param pmid A valid PMID as a string
#'
#' @return a PubMed-class object
#' @importFrom RISmed EUtilsSummary EUtilsGet PMID ArticleTitle MedlineTA Affiliation 
#' Volume MedlinePgn DayPubmed MonthPubmed YearPubmed ELocationID AbstractText Author
#' @export PubMed
#' @details Retrieves article details for a PMID and returns a PubMed class object.
#' The object is a list with three data.frames:
#' 
#' 1. pubinfo contains details of the article and its publication
#' 2. authors contains the author list
#' 3. affiliations contains the unique author affiliations

PubMed <- function(PMID) {
  res <- EUtilsSummary(PMID, type="esearch")
  article <- EUtilsGet(res)
  
  dfpub <- data.frame(PMID=PMID(article),
                      title=ArticleTitle(article),
                      journal=MedlineTA(article),
                      volume=Volume(article),
                      pages=MedlinePgn(article),
                      day=DayPubmed(article),
                      month=MonthPubmed(article),
                      year=YearPubmed(article),
                      DOI=ELocationID(article),
                      abstract=AbstractText(article))
  
  dfauth <- data.frame(order=Author(article)[[1]]$order,
                       first_name=Author(article)[[1]]$ForeName,
                       last_name=Author(article)[[1]]$LastName,
                       first_init=Author(article)[[1]]$Initials)
  dfaffil <- data.frame(affiliation=Affiliation(article))
  
  return(new("PubMed", pubinfo=dfpub, authors=dfauth, affiliations=dfaffil))
}
