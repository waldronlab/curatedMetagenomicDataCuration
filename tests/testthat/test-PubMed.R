context("PubMed")

test_that("PubMed returns PubMed class s4 object",
          expect_s4_class(PubMed("29088129"), "PubMed"))