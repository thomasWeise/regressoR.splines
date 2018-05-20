library("regressoR.splines")
context("FittedSplineModel")

test_that("Test FittedSplineModel constructor", {
  f <- function(x) x
  quality <- 12;
  size <- 7L;
  instance <- new("FittedSplineModel", f=f, quality=quality, size=size, name="bla");
  methods::validObject(instance);
  expect_identical(instance@quality, quality);
  expect_identical(instance@f, f);
  expect_identical(instance@size, size);
  expect_identical(instance@name, "bla");

  str <- as.character(instance);
  expect_identical(str, "bla");
})

test_that("Test FittedSplineModel constructor error", {
  f <- function(y) y
  quality <- 12;
  size <- 7L;
  expect_error(new("FittedSplineModel", f=f, quality=quality, size=size, name="bla"));

  f <- function(x) x
  quality <- -12;
  size <- 7L;
  expect_error(new("FittedSplineModel", f=f, quality=quality, size=size, name="bla"));

  f <- function(x) x
  quality <- 12;
  size <- -7L;
  expect_error(new("FittedSplineModel", f=f, quality=quality, size=size, name="bla"));

  f <- function(x) x
  quality <- 12;
  size <- 7L;
  expect_error(new("FittedSplineModel", f=f, quality=quality, size=size));
  expect_error(new("FittedSplineModel", f=f, size=size, name="bla"));
  expect_error(new("FittedSplineModel", quality=quality, size=size, name="bla"));
  expect_error(new("FittedSplineModel", f=f, name="bla"));
  expect_error(new("FittedSplineModel", quality=quality, name="bla"));
  expect_error(new("FittedSplineModel", size=size, name="bla"));
  instance <- new("FittedSplineModel");
  expect_error(validObject(instance));
})


test_that("Test FittedSplineModel.new", {
  f <- function(x) x
  quality <- 12;
  size <- 7L;
  instance <- FittedSplineModel.new(f=f, quality=quality, size=size, name="bla");
  methods::validObject(instance);
  expect_identical(instance@quality, quality);
  expect_identical(instance@f, f);
  expect_identical(instance@size, size);
  expect_identical(instance@name, "bla");

  str <- as.character(instance);
  expect_gt(nchar(str), 0);
  expect_identical(str, "bla");
})

test_that("Test FittedSplineModel.new error", {
  f <- function(y) y
  quality <- 12;
  size <- 7L;
  expect_error(FittedSplineModel.new(f=f, quality=quality, size=size, name="bla"));

  f <- function(x) x
  quality <- -12;
  size <- 7L;
  expect_error(FittedSplineModel.new(f=f, quality=quality, size=size, name="bla"));

  f <- function(x) x
  quality <- 12;
  size <- -7L;
  expect_error(FittedSplineModel.new(f=f, quality=quality, size=size, name="bla"));

  f <- function(x) x
  quality <- 12;
  size <- 7L;
  expect_error(FittedSplineModel.new(f=f, quality=quality, size=size));
  expect_error(FittedSplineModel.new(f=f, size=size, name="bla"));
  expect_error(FittedSplineModel.new(quality=quality, size=size, name="bla"));
  expect_error(FittedSplineModel.new(f=f, name="bla"));
  expect_error(FittedSplineModel.new(quality=quality, name="bla"));
  expect_error(FittedSplineModel.new(f=f, quality=quality, name="bla"));
  expect_error(FittedSplineModel.new(size=size, name="bla"));
  expect_error(FittedSplineModel.new());
})
