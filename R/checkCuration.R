#' Title Check the curation of per-participant metadata against a template.
#'
#' @param curated a data.frame containing the curated per-participant metadata to be checked
#' @param template a data.frame defining valid syntax
#'
#' @return a list with two elements, [["colnames"]] and [["values]]
#' @export checkCuration
#' @details See Examples for the template used for curatedMetagenomicData. 
#' The template has five columns:
#'
#' 1. "col.name" specifies the name of the column in the curated data.frame.
#' 2. "multiplevalues": multiple semicolon-separated values are allowed
#' 3. "uniqueness": unique means each value must be unique, non-unique means repeated values are allowed
#' 4. "requiredness": if "required", there must be no missing (NA) values. If "optional", missing values are allowed.
#' 5. "allowedvalues": a regex defining allowable values for the column
#' 6. "description": a free-form description of the variable
#' 
#' @examples 
#' allfiles <- dir(
#'   system.file("curated", package = "curatedMetagenomicDataCuration"),
#'               recursive = TRUE,
#'               pattern = "_metadata.txt$",
#'               full.names = TRUE)
#' for (i in seq_along(allfiles)) {
#'   dsname <- allfiles[i]
#'   dat <- read.delim(allfiles[i], sep = "\t", stringsAsFactors = FALSE)
#'   res <- checkCuration(dat)
#'   if(!identical(res, list(missingcols = NULL, invalidcols = NULL, values = NULL)))
#'     warning(paste0("Curation problems in ", allfiles[i]))
#' }
checkCuration <- function(curated,
      template = read.csv(system.file("extdata/template.csv", package = "curatedMetagenomicDataCuration"),
                          as.is = TRUE)) {
  problems <- list(missingcols = NULL, invalidcols = NULL, values = NULL)
  ##-------------------------------------------------
  ##check that the column names match the template
  ##-------------------------------------------------
  requiredcols <- template[template$requiredness=="required", "col.name"]
  missingcols <- requiredcols[!requiredcols %in% colnames(curated)]
  if (length(missingcols) > 0)
    problems$missingcols <- missingcols
  invalidcols <- colnames(curated)[!colnames(curated) %in% template$col.name]
  if (length(invalidcols) > 0)
    problems$invalidcols <- invalidcols
  ##-------------------------------------------------
  ##construct the regexes from template$allowedvalues
  ##-------------------------------------------------
  template <- template[template$col.name %in% colnames(curated), , drop=FALSE]
  curated <- curated[, match(template$col.name, colnames(curated)), drop=FALSE]
  regexes <- template$allowedvalues
  regexes[template$multiplevalues] <- 
    paste0(regexes[template$multiplevalues],
           "(;",
           regexes[template$multiplevalues],
           ")*")
  regexes <- paste("^", regexes, "$", sep = "")
  regexes[!template$multiplevalues] <- 
    gsub("|", "$|^", regexes[!template$multiplevalues], fixed = TRUE)
  names(regexes) <- template$col.name
  ##-------------------------------------------------
  ##Check the data entries in each column for regex
  ## matching, uniqueness, and missingness
  ##-------------------------------------------------
  all.ok <- TRUE
  for (j in seq_along(colnames(curated))) {
    doesmatch <- grepl(regexes[j], curated[, j])
    if (template[j, "requiredness"] == "optional") {
      doesmatch[is.na(curated[, j])] <- TRUE
    }
    ## if field must be unique, add non-unique values to doesnotmatch
    if (template[j, "uniqueness"] == "unique") {
      doesmatch[!isUnique( curated[, j] )] <- FALSE
    }
    curated[!doesmatch, j] <- paste("!!!", curated[!doesmatch, j], "!!!", sep = "")
    if(!all(doesmatch)) all.ok <- FALSE
  }
  if(!all.ok)
    problems$values <- curated
  if(!identical(problems, list(missingcols = NULL, invalidcols = NULL, values = NULL)))
    warning("Curation problems found")
  return(problems)
}

isUnique <- function (x) {
  rv = rep(TRUE, length(x))
  if (length(x) >= 2) {
    ord = order(x)
    ox = x[ord]
    neq = (ox[-length(ox)] != ox[-1])
    rv[ord] = c(neq, TRUE) & c(TRUE, neq)
  }
  return(rv)
}