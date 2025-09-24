original <- "There's two lines.\nThere's also <b>HTML</b>."
cleaned <- "There&apos;s two lines.There&apos;s also HTML."

test_that("encodeAndClean removes html and replaces quotes",
    expect_equal(encodeAndClean(original), cleaned))

text <- "The DOI: 10.1128/mSystems.00164-16\nBioProject ID: PRJEB1234"

test_that("getPattern extracts a DOI",
    expect_equal(getPattern(text, "doi"), "10.1128/mSystems.00164-16"))

test_that("getPattern extracts a bioproject id",
    expect_equal(getPattern(text, "bioproject"), "PRJEB1234"))

values <- getCrossref("10.1128/mSystems.00164-16")

test_that("getCrossref returns a title",
    expect_equal(values$title,
                 paste("Studying Vertical Microbiome Transmission from",
                       "Mothers to Infants by Strain-Level Metagenomic",
                       "Profiling")))

test_that("getCrossref returns a year",
    expect_equal(values$year, 2017))

test_that("getCrossref returns a journal",
    expect_equal(values$journal, "mSystems"))

test_that("getCrossref returns an author",
    expect_equal(values$author,"Francesco Asnicar"))

test_that("baseName generates a name given a middle name",
    expect_equal(baseName("Grace Brewster Hopper", "2025"), "HopperG_2025"))

test_that("baseName generates a name given a first initial",
    expect_equal(baseName("G Hopper", "2025"), "HopperG_2025"))

test_that("baseName generates a name given initials",
    expect_equal(baseName("G. B. Hopper", "2025"), "HopperG_2025"))

path <- tempdir()
template_name <- baseName("Grace Brewster Hopper", "2025")
if (dir.exists(file.path(path, template_name))) {
    unlink(file.path(path, template_name), recursive = TRUE)
} else if (!dir.exists(path)) {
    dir.create(path)
}

test_that("createTemplate returns TRUE if template created",
    expect_true(createTemplate(template_name, path)))

test_that("createTemplate creates a template along path",
    expect_true(dir.exists(file.path(path, template_name))))
