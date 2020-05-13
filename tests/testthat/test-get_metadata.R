context("get_metadata")

test_that("get_metadata returns metadata data.frame",
          expect_s3_class(get_metadata("SRP082656"), "data.frame"))