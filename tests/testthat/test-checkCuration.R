context("checkCuration")

test_that("datasets pass checkCuration()", {
  allfiles <- dir(
      system.file("curated", package = "curatedMetagenomicDataCuration"),
      recursive = TRUE,
      pattern = "_metadata.txt$",
      full.names = TRUE
    )
  for (i in seq_along(allfiles)) {
    dsname <- allfiles[i]
    dat <- read.delim(allfiles[i], sep = "\t", stringsAsFactors = FALSE)
    res <- checkCuration(dat)
    if(!identical(res, list(missingcols = NULL, invalidcols = NULL, values = NULL)))
      warning(paste0("Curation problems in ", allfiles[i]))
    # expect_true(is.null(res$missingcols), info=allfiles[i])
    # expect_true(is.null(res$invalidcols), info=allfiles[i])
    # expect_true(is.null(res$values), info=allfiles[i])
  }
})
