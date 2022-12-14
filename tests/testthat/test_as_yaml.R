context("as.yaml")

test_that("named list is converted",
{
  expect_equal("foo: bar\n", as.yaml(list(foo="bar")))

  x <- list(foo=1:10, bar=c("junk", "test"))
  y <- yaml.load(as.yaml(x))
  expect_equal(y$foo, x$foo)
  expect_equal(y$bar, x$bar)

  x <- list(foo=1:10, bar=list(foo=11:20, bar=letters[1:10]))
  y <- yaml.load(as.yaml(x))
  expect_equal(x$foo, y$foo)
  expect_equal(x$bar$foo, y$bar$foo)
  expect_equal(x$bar$bar, y$bar$bar)

  # nested lists
  x <- list(foo = list(a = 1, b = 2), bar = list(b = 4))
  y <- yaml.load(as.yaml(x))
  expect_equal(x$foo$a, y$foo$a)
  expect_equal(x$foo$b, y$foo$b)
  expect_equal(x$bar$b, y$bar$b)
})

test_that("data frame is converted",
{
  x <- data.frame(a=1:10, b=letters[1:10], c=11:20)
  y <- as.data.frame(yaml.load(as.yaml(x)))
  expect_equal(x$a, y$a)
  expect_equal(x$b, y$b)
  expect_equal(x$c, y$c)

  x <- as.yaml(x, column.major = FALSE)
  y <- yaml.load(x)
  expect_equal(5L,  y[[5]]$a)
  expect_equal("e", y[[5]]$b)
  expect_equal(15L, y[[5]]$c)
})

test_that("empty nested list is converted",
{
  x <- list(foo=list())
  expect_equal("foo: []\n", as.yaml(x))
})

test_that("empty nested data frame is converted",
{
  x <- list(foo=data.frame())
  expect_equal("foo: {}\n", as.yaml(x))
})

test_that("empty nested vector is converted",
{
  x <- list(foo=character())
  expect_equal("foo: []\n", as.yaml(x))
})

test_that("list is converted as omap",
{
  x <- list(a=1:2, b=3:4)
  expected <- "!!omap\n- a:\n  - 1\n  - 2\n- b:\n  - 3\n  - 4\n"
  expect_equal(expected, as.yaml(x, omap=TRUE))
})

test_that("nested list is converted as omap",
{
  x <- list(a=list(c=list(e=1L, f=2L)), b=list(d=list(g=3L, h=4L)))
  expected <- "!!omap\n- a: !!omap\n  - c: !!omap\n    - e: 1\n    - f: 2\n- b: !!omap\n  - d: !!omap\n    - g: 3\n    - h: 4\n"
  expect_equal(expected, as.yaml(x, omap=TRUE))
})

test_that("omap is loaded",
{
  x <- yaml.load(as.yaml(list(a=1:2, b=3:4, c=5:6, d=7:8), omap=TRUE))
  expect_equal(c("a", "b", "c", "d"), names(x))
})

test_that("numeric is converted correctly",
{
  result <- as.yaml(c(1, 5, 10, 15))
  expect_equal(result, "- 1.0\n- 5.0\n- 10.0\n- 15.0\n", label = result)
})

test_that("multiline string is converted",
{
  expect_equal("|-\n  foo\n  bar\n", as.yaml("foo\nbar"))
  expect_equal("- foo\n- |-\n  bar\n  baz\n", as.yaml(c("foo", "bar\nbaz")))
  expect_equal("foo: |-\n  foo\n  bar\n", as.yaml(list(foo = "foo\nbar")))
  expect_equal("a:\n- foo\n- bar\n- |-\n  baz\n  quux\n", as.yaml(data.frame(a = c('foo', 'bar', 'baz\nquux'))))
})

test_that("function is converted",
{
  x <- function() { runif(100) }
  expected <- "!expr |\n  function ()\n  {\n      runif(100)\n  }\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("list with unnamed items is converted",
{
  x <- list(foo=list(list(a = 1L, b = 2L), list(a = 3L, b = 4L)))
  expected <- "foo:\n- a: 1\n  b: 2\n- a: 3\n  b: 4\n"
  result <- as.yaml(x)
  expect_equal(result, expected)
})

test_that("pound signs are escaped in strings",
{
  result <- as.yaml("foo # bar")
  expect_equal("'foo # bar'\n", result)
})

test_that("null is converted",
{
  expect_equal("~\n", as.yaml(NULL))
})

test_that("different line seps are used",
{
  result <- as.yaml(c('foo', 'bar'), line.sep = "\n")
  expect_equal("- foo\n- bar\n", result)

  result <- as.yaml(c('foo', 'bar'), line.sep = "\r\n")
  expect_equal("- foo\r\n- bar\r\n", result)

  result <- as.yaml(c('foo', 'bar'), line.sep = "\r")
  expect_equal("- foo\r- bar\r", result)
})

test_that("custom indent is used",
{
  result <- as.yaml(list(foo=list(bar=list('foo', 'bar'))), indent = 3)
  expect_equal("foo:\n   bar:\n   - foo\n   - bar\n", result)
})

test_that("block sequences in mapping context are indented when option is true",
{
  result <- as.yaml(list(foo=list(bar=list('foo', 'bar'))), indent.mapping.sequence = TRUE)
  expect_equal("foo:\n  bar:\n    - foo\n    - bar\n", result)
})

test_that("indent value is validated",
{
  expect_exception(as.yaml(list(foo=list(bar=list('foo', 'bar'))), indent = 0))
})

test_that("strings are escaped properly",
{
  result <- as.yaml("12345")
  expect_equal("'12345'\n", result)
})

test_that("unicode strings are not escaped",
{
  # list('??????' = '????????????????????', '??????????????' = '????????????????')
  a <- "\u0438\u043C\u0435" # name 1
  b <- "\u0410\u043B\u0435\u043A\u0441\u0430\u043D\u0434\u0430\u0440" # value 1
  c <- "\u043F\u0440\u0435\u0437\u0438\u043C\u0435" # name 2
  d <- "\u0411\u043B\u0430\u0433\u043E\u0442\u0438\u045B" # value 2
  x <- list(b, d)
  names(x) <- c(a, c)
  expected <- paste(a, ": ", b, "\n", c, ": ", d, "\n", sep="")
  result <- as.yaml(x, unicode = TRUE)
  expect_equal(expected, result, label = result)
})

test_that("unicode strings are escaped",
{
  # '??'
  x <- "\u00e9"
  result <- as.yaml(x, unicode = FALSE)
  expect_equal("\"\\xE9\"\n", result)
})

test_that("unicode strings are not escaped by default",
{
  # list('??')
  x <- list("\u00e9")
  result <- as.yaml(x)
  expect_equal("- \u00e9\n", result)
})

test_that("named list with unicode character is correct converted",
{
  x <- list(special.char = "\u00e9")
  result <- as.yaml(x)
  expect_equal("special.char: \u00e9\n", result)
})

test_that("unknown objects cause error",
{
  expect_error(as.yaml(expression(foo <- bar)))
})

test_that("inf is emitted properly",
{
  result <- as.yaml(Inf)
  expect_equal(".inf\n", result)
})

test_that("negative inf is emitted properly",
{
  result <- as.yaml(-Inf)
  expect_equal("-.inf\n", result)
})

test_that("nan is emitted properly",
{
  result <- as.yaml(NaN)
  expect_equal(".nan\n", result)
})

test_that("logical na is emitted properly",
{
  result <- as.yaml(NA)
  expect_equal(".na\n", result)
})

test_that("numeric na is emitted properly",
{
  result <- as.yaml(NA_real_)
  expect_equal(".na.real\n", result)
})

test_that("integer na is emitted properly",
{
  result <- as.yaml(NA_integer_)
  expect_equal(".na.integer\n", result)
})

test_that("character na is emitted properly",
{
  result <- as.yaml(NA_character_)
  expect_equal(".na.character\n", result)
})

test_that("true is emitted properly",
{
  result <- as.yaml(TRUE)
  expect_equal("yes\n", result)
})

test_that("false is emitted properly",
{
  result <- as.yaml(FALSE)
  expect_equal("no\n", result)
})

test_that("named list keys are escaped properly",
{
  result <- as.yaml(list(n = 123))
  expect_equal("'n': 123.0\n", result)
})

test_that("data frame keys are escaped properly when row major",
{
  result <- as.yaml(data.frame(n=1:3), column.major = FALSE)
  expect_equal("- 'n': 1\n- 'n': 2\n- 'n': 3\n", result)
})

test_that("scientific notation is valid yaml",
{
  result <- as.yaml(10000000)
  expect_equal("1.0e+07\n", result)
})

test_that("precision must be in the range 1..22",
{
  expect_error(as.yaml(12345, precision = -1))
  expect_error(as.yaml(12345, precision = 0))
  expect_error(as.yaml(12345, precision = 23))
})

test_that("factor with missing values is emitted properly",
{
  x <- factor('foo', levels=c('bar', 'baz'))
  result <- as.yaml(x)
  expect_equal(".na.character\n", result)
})

test_that("very small negative float is emitted properly",
{
  result <- as.yaml(-7.62e-24)
  expect_equal("-7.62e-24\n", result)
})

test_that("very small positive float is emitted properly",
{
  result <- as.yaml(7.62e-24)
  expect_equal("7.62e-24\n", result)
})

test_that("numeric zero is emitted properly",
{
  result <- as.yaml(0.0)
  expect_equal("0.0\n", result)
})

test_that("numeric negative zero is emitted properly",
{
  result <- as.yaml(-0.0)
  expect_equal("-0.0\n", result)
})

test_that("custom handler is run for first class",
{
  x <- "foo"
  class(x) <- "bar"
  result <- as.yaml(x, handlers = list(bar = function(x) paste0("x", x, "x")))
  expect_equal("xfoox\n", result)
})

test_that("custom handler is run for second class",
{
  x <- "foo"
  class(x) <- c("bar", "baz")
  result <- as.yaml(x, handlers = list(baz = function(x) paste0("x", x, "x")))
  expect_equal("xfoox\n", result)
})

test_that("custom handler with verbatim result",
{
  result <- as.yaml(TRUE, handlers = list(
    logical = function(x) {
      result <- ifelse(x, "true", "false")
      class(result) <- "verbatim"
      return(result)
    }
  ))
  expect_equal("true\n", result)
})

test_that("custom handler with sequence result",
{
  result <- as.yaml(c(1, 2, 3), handlers = list(
    numeric = function(x) {
      x + 1
    }
  ))
  expect_equal("- 2.0\n- 3.0\n- 4.0\n", result)
})

test_that("custom handler with mapping result",
{
  result <- as.yaml(1, handlers = list(
    numeric = function(x) {
      list(foo = 1:2, bar = 3:4)
    }
  ))
  expect_equal("foo:\n- 1\n- 2\nbar:\n- 3\n- 4\n", result)
})

test_that("custom handler with function result",
{
  result <- as.yaml(1, handlers = list(
    numeric = function(x) {
      function(y) y + 1
    }
  ))
  expected <- "!expr |\n  function (y)\n  y + 1\n"
  expect_equal(expected, result)
})

test_that("custom tag for function",
{
  f <- function(x) x + 1
  attr(f, "tag") <- "!foo"
  expected <- "!foo |\n  function (x)\n  x + 1\n"
  result <- as.yaml(f)
  expect_equal(expected, result)
})

test_that("custom tag for numeric sequence",
{
  x <- c(1, 2, 3)
  attr(x, "tag") <- "!foo"
  expected <- "!foo\n- 1.0\n- 2.0\n- 3.0\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("custom tag for numeric scalar",
{
  x <- 1
  attr(x, "tag") <- "!foo"
  expected <- "!foo 1.0\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("custom tag for integer sequence",
{
  x <- 1L:3L
  attr(x, "tag") <- "!foo"
  expected <- "!foo\n- 1\n- 2\n- 3\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("custom tag for integer scalar",
{
  x <- 1L
  attr(x, "tag") <- "!foo"
  expected <- "!foo 1\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("custom tag for logical sequence",
{
  x <- c(TRUE, FALSE)
  attr(x, "tag") <- "!foo"
  expected <- "!foo\n- yes\n- no\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("custom tag for logical scalar",
{
  x <- TRUE
  attr(x, "tag") <- "!foo"
  expected <- "!foo yes\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("custom tag for factor sequence",
{
  x <- factor(c("foo", "bar"))
  attr(x, "tag") <- "!foo"
  expected <- "!foo\n- foo\n- bar\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("custom tag for factor scalar",
{
  x <- factor("foo")
  attr(x, "tag") <- "!foo"
  expected <- "!foo foo\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("custom tag for character sequence",
{
  x <- c("foo", "bar")
  attr(x, "tag") <- "!foo"
  expected <- "!foo\n- foo\n- bar\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("custom tag for character scalar",
{
  x <- "foo"
  attr(x, "tag") <- "!foo"
  expected <- "!foo foo\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("custom tag for data frame",
{
  x <- data.frame(a = 1:3, b = 4:6)
  attr(x, "tag") <- "!foo"
  expected <- "!foo\na:\n- 1\n- 2\n- 3\nb:\n- 4\n- 5\n- 6\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("custom tag for data frame column",
{
  x <- data.frame(a = 1:3, b = 4:6)
  attr(x$a, "tag") <- "!foo"
  expected <- "a: !foo\n- 1\n- 2\n- 3\nb:\n- 4\n- 5\n- 6\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("custom tag for omap",
{
  x <- list(a=1:2, b=3:4)
  attr(x, "tag") <- "!foo"
  expected <- "!foo\n- a:\n  - 1\n  - 2\n- b:\n  - 3\n  - 4\n"
  result <- as.yaml(x, omap = TRUE)
  expect_equal(expected, result)
})

test_that("custom tag for named list",
{
  x <- list(a=1:2, b=3:4)
  attr(x, "tag") <- "!foo"
  expected <- "!foo\na:\n- 1\n- 2\nb:\n- 3\n- 4\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("custom tag for unnamed list",
{
  x <- list(1, 2, 3)
  attr(x, "tag") <- "!foo"
  expected <- "!foo\n- 1.0\n- 2.0\n- 3.0\n"
  result <- as.yaml(x)
  expect_equal(expected, result)
})

test_that("quotes for string",
{
  port_def <- "80:80"
  attr(port_def, "quoted") <- TRUE
  x <- list(ports = list(port_def))
  result <- as.yaml(x)
  expected <- "ports:\n- \"80:80\"\n"
  expect_equal(expected, result)
})

test_that("no dots at end",
{
  result <- yaml12::as.yaml(list(eol = "\n", a = 1), line.sep = "\n")
  expect_equal("eol: |2+\n\na: 1.0\n", result)
})
