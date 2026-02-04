library(testthat)

test_that("getClusteredHeatmap works on small matrix and saves file", {
  skip_if_not_installed("pheatmap")

  m <- matrix(rnorm(20), nrow = 5)
  rownames(m) <- paste0("g", seq_len(nrow(m)))

  tmp <- tempfile(fileext = ".png")
  on.exit({ if (file.exists(tmp)) file.remove(tmp) }, add = TRUE)

  res <- getClusteredHeatmap(m, figureName = "testhm", filename = tmp)

  expect_true(is.list(res) || inherits(res, "pheatmap"))
  expect_true(file.exists(tmp))
})
