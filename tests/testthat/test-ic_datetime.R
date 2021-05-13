context("test-ic_datetime.R")

# test_that("multiplication works", {
#   expect_equal(2 * 2, 4)
# })

test_that("ic_datetime works for 20180809T160000Z:", {
  expect_equal(
    class(ic_datetime("20180809T160000Z")), class(Sys.time())
  )
})

test_that("ic_datetime is NA for empty:", {
  expect_output(suppressWarnings(ic_datetime("")), NA)
})

test_that("ic_datetime warns about input format:", {
  expect_warning(ic_datetime(""))
})
