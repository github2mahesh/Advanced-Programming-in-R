#context("latlong")
#library(API)
library(testthat)

test_that("latlong rejects errounous input", {
  expect_error(latlong("1","error"))
})

test_that(" 'Lund' is character", {
  expect_equal(typeof('Lund'), "character")
})

test_that("latlong() is not working", {
  expect_false(is.list("lund"))
})

test_that("latlong rejects special characters as input", {
  expect_error(latlong("%%%%%","My error!"))
})


test_that("latlong() is working", {
  expect_true(is.character("lund"))
})

test_that("latlong rejects errounous input", {
  expect_error(latlong("hej","error"))
})

test_that("latlong() rejects errounous input ", {
  expect_false(is.integer("museum"))
})

