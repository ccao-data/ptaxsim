context("lintr coverage")

test_that("no lintr errors", {
  lintr::expect_lint_free(
    linters = lintr::with_defaults(
      object_name_linter = NULL,
      object_length_linter = NULL,
      object_usage_linter = NULL
    )
  )
})
