# Download and cache dictionary files from cloud storage

#' @title Retrieve and cache dictionary files
#' @description 'cache_dictionaries' downloads the dictionary files found in the
#' 'odm_template' Google Cloud Bucket and stores them in a local
#' curatedMetagenomicDataCuration cache. If the same files are requested again
#' through this function, they will not be re-downloaded unless explicitly
#' specified, in order to reduce excessive downloads.
#' @param redownload String: "yes", "no", or "ask"; should the function
#' re-download a file that is already present in the cache, Default: 'no'
#' @param custom_cache BiocFileCache object: a custom cache object may be
#' specified instead of the default created by cMDC_get_cache(), Default: NULL
#' @return A tibble with information on the cached files, Google Cloud Bucket
#' object name, local cache ID, and cached file path
#' @examples
#' \dontrun{
#' if(interactive()){
#'  cache_dictionaries(redownload = "ask")
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_split}}
#'  \code{\link[tibble]{tibble}}
#' @rdname cache_dictionaries
#' @export
#' @importFrom stringr str_split
#' @importFrom tibble tibble
#' @importFrom stats setNames
cache_dictionaries <- function(redownload = "no",
                               custom_cache = NULL) {
  
  ## Check redownload value
  allowed_redown <- c("y", "n", "a")
  p_redown <- substr(tolower(redownload), 1, 1)
  if (!p_redown %in% allowed_redown) {
    stop("'", redownload, "' is not an allowed value for 'redownload'. Please enter 'yes', 'no', or 'ask'")
  }
  
  ## Check custom_cache
  if (!is.null(custom_cache)) {
    stopifnot(methods::is(custom_cache, "BiocFileCache"))
  }
  
  ## Get locators of all full dictionary files
  objs <- googleCloudStorageR::gcs_list_objects()
  locators <- objs$name[grep("^cmd_[a-z_\\.]+$", objs$name)]
  
  ## Download and cache requested files
  cache_paths <- vector("list", length(locators))
  errors <- character(0)
  
  for (i in seq_along(locators)) {
    res <- tryCatch(
      {
        list(
          file = cache_gcb(
            locators[i],
            redownload = p_redown,
            custom_cache = custom_cache
          ),
          error = NULL
        )
      },
      error = function(e) {
        list(
          file = NA |> stats::setNames(NA),
          error = paste0("Unable to cache ", locators[i], ": ",
                         conditionMessage(e))
        )
      }
    )
    
    cache_paths[[i]] <- res$file
    if (!is.null(res$error)) {
      errors <- c(errors, res$error)
    }
  }
  
  cache_paths <- unlist(cache_paths)
  
  ## Format cache information for user
  cache_tbl <- tibble::tibble(gcb_object = locators,
                              cache_id = names(cache_paths),
                              cache_path = cache_paths)
  
  ## Print any errors and return cache information
  if (length(errors) > 0) {
    warning(errors)
  }
  
  return(cache_tbl)
}

#' @title Get location of dedicated file cache
#' @description 'cMDC_get_cache' returns the location of the dedicated
#' curatedMetagenomicDataCuration file cache or creates it if it does not exist.
#' @return BiocFileCache cache object
#' @examples
#' \dontrun{
#' if(interactive()){
#'  cMDC_get_cache()
#'  }
#' }
#' @seealso
#'  \code{\link[tools]{userdir}}
#'  \code{\link[BiocFileCache]{BiocFileCache-class}}, \code{\link[BiocFileCache]{BiocFileCache}}
#' @rdname cMDC_get_cache
#' @export
#' @importFrom tools R_user_dir
#' @importFrom BiocFileCache BiocFileCache
cMDC_get_cache <- function() {
  ## Create a directory for cached data
  cache <- tools::R_user_dir("curatedMetagenomicDataCuration", "cache")
  if (!dir.exists(cache)) {dir.create(cache)}
  
  ## Directory path of cache
  BiocFileCache::BiocFileCache(cache = cache)
}

#' @title Return all "extensions" from a file path
#' @description 'get_exts' returns the extension of a file name or path,
#' including pseudo-extensions such as ".tsv" in "file.tsv.gz".
#' @param file_path String: file name or path to get extension(s) from
#' @return String: file extension, including pseudo-extensions
#' @examples
#' \dontrun{
#' if(interactive()){
#'  get_exts("path/file.tsv.gz")
#'  }
#' }
#' @seealso
#'  \code{\link[stringr]{str_split}}
#' @rdname get_exts
#' @export
#' @importFrom stringr str_split_fixed
get_exts <- function(file_path) {
  bname <- basename(file_path)
  exts <- paste0(".", stringr::str_split_fixed(bname, "\\.", 2)[2])
  return(exts)
}

#' @title Cache Google Bucket object
#' @description 'cache_gcb' checks the parkinsonsMetagenomicData cache for
#' the presence of a Google Bucket object, downloads or updates the file as
#' needed, and returns the path of the cached file.
#' @param locator String: the name of a Google Bucket object
#' @param redownload String: "yes", "no", or "ask"; should the function
#' re-download a file that is already present in the cache, Default: 'no'
#' @param custom_cache BiocFileCache object: a custom cache object may be
#' specified instead of the default created by cMDC_get_cache(), Default: NULL
#' @return Named vector of strings: Names are the rids of cached files, values
#' are the paths to the cached files
#' @examples
#' \dontrun{
#' if(interactive()){
#'  cache_gcb(locator = "cMD_curator.csv",
#'            redownload = "ask")
#'  }
#' }
#' @seealso
#'  \code{\link[googleCloudStorageR]{gcs_download_url}}, \code{\link[googleCloudStorageR]{gcs_get_object}}
#'  \code{\link[BiocFileCache]{BiocFileCache-class}}
#' @rdname cache_gcb
#' @export
#' @importFrom googleCloudStorageR gcs_get_object
#' @importFrom BiocFileCache bfcquery bfcnew bfcremove bfcrpath
cache_gcb <- function(locator, redownload = "no", custom_cache = NULL) {
  ## Check redownload value
  allowed_redown <- c("y", "n", "a")
  p_redown <- substr(tolower(redownload), 1, 1)
  if (!p_redown %in% allowed_redown) {
    stop("'", redownload, "' is not an allowed value for 'redownload'. Please enter 'yes', 'no', or 'ask'")
  }
  
  ## Get cache
  if (!is.null(custom_cache)) {
    stopifnot(methods::is(custom_cache, "BiocFileCache"))
    bfc <- custom_cache
  } else {
    bfc <- cMDC_get_cache()
  }
  
  ## Check if file already cached
  bfcres <- BiocFileCache::bfcquery(x = bfc,
                                    query = locator,
                                    field = "rname")
  
  if (nrow(bfcres) > 0) {
    rid <- bfcres$rid[bfcres$create_time == max(bfcres$create_time)]
  } else {
    rid <- bfcres$rid
  }
  
  ## Cached file not found
  if (!length(rid)) {
    ## Create cache location
    newpath <- BiocFileCache::bfcnew(x = bfc,
                                     rname = locator,
                                     ext = get_exts(locator),
                                     fname = "exact")
    rid <- names(newpath)
    
    ## Download file
    tryCatch({
      googleCloudStorageR::gcs_get_object(locator, saveToDisk = newpath)
    }, error = function(e) {
      ## Remove cache location if download fails
      BiocFileCache::bfcremove(bfc, rid)
      stop("The file was not able to be downloaded: ",
           conditionMessage(e))
    })
    
    ## Cached file found, follow "redownload" instructions
  } else if (length(rid)) {
    
    if (p_redown == "a" & interactive()) {
      over <- readline(prompt = paste0("Resource with rname = '", locator, "' found in cache. Redownload and overwrite? (yes/no): "))
      response <- substr(tolower(over), 1, 1)
      doit <- switch(response, y = TRUE, n = FALSE, NA)
    } else if (p_redown == "y") {
      doit <- TRUE
      message("Resource with rname = '", locator, "' found in cache, redownloading.")
    } else if (p_redown == "n") {
      doit <- FALSE
      message("Resource with rname = '", locator, "' found in cache, proceeding with most recent version.")
    }
    
    if (doit) {
      rpath <- BiocFileCache::bfcrpath(bfc, rids = rid)
      googleCloudStorageR::gcs_get_object(locator, saveToDisk = rpath, overwrite = TRUE)
    }
  }
  
  ## Return path of cached resource
  res <- BiocFileCache::bfcrpath(bfc, rids = rid)
  return(res)
}
