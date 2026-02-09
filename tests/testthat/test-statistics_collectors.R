test_that("normalize_values removes NA and empty strings", {
  result <- normalize_values(c("  value1  ", NA, "", "value2"))
  expect_equal(result, c("value1", "value2"))
})

test_that("split_values handles multiple separators", {
  result <- split_values(c("a;b", "c,d", "e|f"))
  expect_equal(result, c("a", "b", "c", "d", "e", "f"))
})

test_that("split_values handles spaces around separators", {
  result <- split_values("a ; b , c")
  expect_equal(result, c("a", "b", "c"))
})

test_that("find_col finds column case-insensitively", {
  cols <- c("Age_Group", "Body_Site", "Country")
  result <- find_col(cols, c("age_group", "agegroup"))
  expect_equal(result, "Age_Group")
})

test_that("find_col returns NA when no match", {
  cols <- c("Age_Group", "Body_Site", "Country")
  result <- find_col(cols, c("nonexistent", "missing"))
  expect_true(is.na(result))
})

test_that("update_counts adds new values", {
  counts <- integer(0)
  counts <- update_counts(counts, c("a", "b", "a"))
  expect_equal(as.integer(counts["a"]), 2L)
  expect_equal(as.integer(counts["b"]), 1L)
})

test_that("update_counts increments existing values", {
  counts <- c(a = 5, b = 3)
  counts <- update_counts(counts, c("a", "c"))
  expect_equal(as.integer(counts["a"]), 6L)
  expect_equal(as.integer(counts["b"]), 3L)
  expect_equal(as.integer(counts["c"]), 1L)
})

test_that("build_distribution handles empty counts", {
  result <- build_distribution(integer(0))
  expect_equal(result$total_distinct, 0)
  expect_equal(result$total_count, 0)
  expect_equal(length(result$top), 0)
})

test_that("build_distribution returns top N items", {
  counts <- c(a = 10, b = 5, c = 3, d = 1)
  result <- build_distribution(counts, top_n = 2)

  expect_equal(result$total_distinct, 4)
  expect_equal(result$total_count, 19)
  expect_equal(length(result$top), 2)
  expect_equal(result$top[[1]]$name, "a")
  expect_equal(result$top[[1]]$count, 10)
})
