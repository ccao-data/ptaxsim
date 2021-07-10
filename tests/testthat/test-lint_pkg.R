context("lintr coverage")

test_that("no lintr errors", {
  lintr::expect_lint_free()
})
