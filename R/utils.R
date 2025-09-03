.PATTERN <- c(
    doi = "[0-9.]+/[A-Za-z0-9.-]+",
    bioproject = "PRJ[A-Z]{2}[0-9]+"
)

.CROSSREF <- function(doi) {
    paste("https://api.crossref.org/works", doi, sep = "/")
}

.EUTILS <- function(doi) {
    paste0("https://eutils.ncbi.nlm.nih.gov/entrez/eutils/",
           "esearch.fcgi?db=pubmed&term=",
           doi,
           "[DOI]&retmode=json")
}

#' Replace quotes with HTML code, remove HTML and line breaks
#'
#' @param s a string possibly with HTML, quotes, and line breaks
#' @return string with HTML and line breaks removed, quotes encoded
#' @importFrom stringr str_replace_all str_squish
#' @details See https://stackoverflow.com/questions/17227294/removing-html-tags-from-a-string-in-r#answer-17227415
#' @export
#'
#' @examples
#' encodeAndClean("There's two lines.\nThere's also <b>HTML</b>.")
encodeAndClean <- function(s) {
    s <- str_replace_all(s, "'", "&apos;")
    s <- str_replace_all(s, '"', "&quot;")
    str_squish(gsub("(<.*?>|\n)", "", s))
}

#' Get pattern
#'
#' @param ss a string
#' @param patternName a string matching a name in .PATTERN
#' @return a string matching the pattern
#' @importFrom stringr str_split str_squish str_extract
#' @export
#'
#' @examples
#' ss <- "The DOI: 10.1128/mSystems.00164-16\nBioProject ID: PRJEB1234"
#' getPattern(ss, "doi")
#' getPattern(ss, "bioproject")
getPattern <- function(ss, patternName) {
    value <- ""
    for (s in str_split(ss, "\n", simplify = TRUE)) {
        value <- str_squish(s) |> str_extract(.PATTERN[patternName])
        if (!is.na(value))
            break
    }
  value
}

#' Get request
#'
#' @param url a string representing a URL
#' @return a string representing JSON response
#' @importFrom httr2 request req_perform resp_body_json
#'
#' @examples
#' getRequest(.CROSSREF("10.1128/mSystems.00164-16"))
.getRequest <- function(url) {
    request(url) |>
        req_perform() |>
        resp_body_json(simplifyVector = TRUE)
}

#' Get data from Crossref
#'
#' @param doi A string representing a DOI
#' @return list with data associated with the DOI
#' @export
#'
#' @examples
#' getCrossref("10.1128/mSystems.00164-16")
getCrossref <- function(doi) {
    values <- list("title" = "",
                   "published" = "",
                   "container-title" = "",
                   "author" = "")
    resp <- .getRequest(.CROSSREF(doi))

    for (value in names(values)) {
        if (value %in% names(resp$message)) {
            if (value == "title")
                values$title <- encodeAndClean(resp$message$title)
            else if (value == "published")
                values$published <- resp$message$published$`date-parts`[1]
            else if (value == "container-title")
                values$`container-title` <- encodeAndClean(resp$message$`container-title`[[1]])
            else if (value == "author") {
                author <- paste(resp$message$author$given[1],
                                resp$message$author$family[1],
                                sep = " ")
                values$author <- encodeAndClean(author)
            }
        }
    }
    names(values) <- c("title", "year", "journal", "author")
    values
}

#' Get PMID from Eutils
#'
#' @param doi a string representing a DOI
#' @return PMID or ""
#' @export
#'
#' @examples
#' getPmid("10.1128/mSystems.00164-16")
getPmid <- function(doi) {
    pmid <- ""
    resp <- .getRequest(.EUTILS(doi))
    if (length(resp$esearchresult$idlist) != 0)
        pmid <- resp$esearchresult$idlist
    pmid
}

#' Create template name
#'
#' @param author a string representing the author's first and last name
#' @param year a string representing the publication year
#' @return PMID or ""
#' @details The name is the author's last name plus the first initial
#' @importFrom stringr str_to_title
#' @export
#'
#' @examples
#' baseName("Grace Brewster Hopper", "2025")
baseName <- function(author, year) {
    first_last <- unlist(strsplit(str_to_title(author), " "))
    first_name <- first_last[1]
    last_name <- first_last[length(first_last)]
    paste0(last_name, substr(first_name, 1, 1), "_", year)
}

#' Create template
#'
#' @details Note: the provided path must already exist otherwise the function
#' will fail.
#'
#' @param author a string representing the author's first and last name
#' @param year a string representing the publication year
#' @param path a string
#' @return invisible NULL
#' @details The name is the author's last name plus the first initial
#' @importFrom stringr str_to_title
#' @export
#'
#' @examples
#' createTemplate("HopperG_2025", tempdir())
createTemplate <- function(base, path) {
    full_path <- file.path(path, base)
    if (!dir.exists(full_path)) {
        dir.create(full_path, recursive = TRUE)
        study_df <- data.frame("Study Title" = character(),
                               target_condition = character(),
                               check.names = FALSE)
        study_df_created <- tryCatch({
            write.table(study_df, sep = "\t", quote = FALSE,
                        file.path(full_path, paste0(base, "_study.tsv")))
            TRUE
            }, error = function(e) FALSE)
        sample_df <- data.frame(study_name = character(),
                                target_condition = character(),
                                pmid = numeric(),
                                doi = character(),
                                check.names = FALSE)
        sample_df_created <- tryCatch({
            write.table(sample_df, sep = "\t", quote = FALSE,
                        file.path(full_path, paste0(base, "_sample.tsv")))
            TRUE
            }, error = function(e) FALSE)
        study_df_created & sample_df_created
    } else
        FALSE
}
