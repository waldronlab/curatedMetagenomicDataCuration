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
    expect_equal(res$colnames, NULL)
    expect_equal(res$values, NULL)
    warnings()
    res
  }
})
