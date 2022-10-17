test_that("reading from a connection works",
{
  filename <- tempfile()
  cat("foo: 123", file=filename, sep="\n")
  foo <- file(filename, 'r')
  x <- yaml.load_file(foo)
  close(foo)
  unlink(filename)
  expect_equal(123L, x$foo)
})

test_that("reading from specified filename works",
{
  filename <- tempfile()
  cat("foo: 123", file=filename, sep="\n")
  x <- yaml.load_file(filename)
  unlink(filename)
  expect_equal(123L, x$foo)
})

test_that("reading a complicated document works",
{
  filename <- system.file(file.path("tests", "files", "test.yml"), package = "yaml")
  x <- yaml.load_file(filename)
  expected <- list(
    foo = list(one = 1, two = 2),
    bar = list(three = 3, four = 4),
    baz = list(list(one = 1, two = 2), list(three = 3, four = 4)),
    quux = list(one = 1, two = 2, three = 3, four = 4, five = 5, six = 6),
    corge = list(
      list(one = 1, two = 2, three = 3, four = 4, five = 5, six = 6),
      list(xyzzy = list(one = 1, two = 2, three = 3, four = 4, five = 5, six = 6))
    )
  )
  expect_equal(expected, x)
})

test_that("expressions are not implicitly converted with warning",
{
  warnings <- captureWarnings({
    filename <- tempfile()
    cat("!expr 123 + 456", file=filename, sep="\n")
    foo <- file(filename, 'r')
    x <- yaml.load_file(foo)
    close(foo)
    unlink(filename)
  })
  expect_equal("character", class(x))
  expect_equal("123 + 456", x)
  expect_equal("Evaluating R expressions (!expr) requires explicit `eval.expr=TRUE` option (see yaml.load help)", warnings)
})

test_that("expressions are explicitly converted without warning",
{
  warnings <- captureWarnings({
    filename <- tempfile()
    cat("!expr 123 + 456", file=filename, sep="\n")
    foo <- file(filename, 'r')
    x <- yaml.load_file(foo, eval.expr = TRUE)
    close(foo)
    unlink(filename)
  })
  expect_equal("numeric", class(x))
  expect_equal(579, x)
  expect_equal(0, length(warnings))
})

test_that("expressions are unconverted",
{
  filename <- tempfile()
  cat("!expr 123 + 456", file=filename, sep="\n")
  foo <- file(filename, 'r')
  x <- yaml.load_file(foo, eval.expr = FALSE)
  close(foo)
  unlink(filename)

  expect_equal("character", class(x))
  expect_equal("123 + 456", x)
})

test_that("merge specification example with merge override",
{
  filename <- system.file(file.path("tests", "files", "merge.yml"), package = "yaml")
  x <- yaml.load_file(filename, merge.precedence = "override")
  expected <- list(x = 1, y = 2, r = 10, label = "center/big")
  expect_equal(expected, x[[5]])
  expect_equal(expected, x[[6]])
  expect_equal(expected, x[[7]])
  expect_equal(expected, x[[8]])
})
