test_that("named list is not returned",
{
  x <- yaml.load("hey: man\n123: 456\n", FALSE)
  expect_equal(2L, length(x))
  expect_equal(2L, length(attr(x, "keys")))

  x <- yaml.load("- dude\n- sup\n- 1.2345", FALSE)
  expect_equal(3L, length(x))
  expect_equal(0L, length(attr(x, "keys")))
  expect_equal("sup", x[[2]])

  x <- yaml.load("dude:\n  - 123\n  - sup", FALSE)
  expect_equal(1L, length(x))
  expect_equal(1L, length(attr(x, "keys")))
  expect_equal(2L, length(x[[1]]))
})

test_that("key conflicts throw errors",
{
  expect_error(yaml.load("hey: buddy\nhey: guy"))
})

test_that("named list is returned",
{
  x <- yaml.load("hey: man\n123: 456\n", TRUE)
  expect_equal(2L, length(x))
  expect_equal(2L, length(names(x)))
  expect_equal(c("123", "hey"), sort(names(x)))
  expect_equal("man", x$hey)
})

test_that("uniform sequences are coerced",
{
  x <- yaml.load("- 1\n- 2\n- 3")
  expect_equal(1:3, x)

  x <- yaml.load("- yes\n- no\n- yes")
  expect_equal(c(TRUE, FALSE, TRUE), x)

  x <- yaml.load("- 3.1\n- 3.14\n- 3.141")
  expect_equal(c(3.1, 3.14, 3.141), x)

  x <- yaml.load("- hey\n- hi\n- hello")
  expect_equal(c("hey", "hi", "hello"), x)
})

test_that("tag type conflicts throws error",
{
  expect_error(yaml.load("!!str [1, 2, 3]"))
  expect_error(yaml.load("!!str {foo: bar}"))
})

test_that("sequences are not collapsed",
{
  x <- yaml.load("- [1, 2]\n- 3\n- [4, 5]")
  expect_equal(list(1:2, 3L, 4:5), x)
})

test_that("named maps are merged without warnings",
{
  x <- yaml.load("foo: bar\n<<: {baz: boo}", TRUE)
  expect_equal(2L, length(x))
  expect_equal("bar", x$foo)
  expect_equal("boo", x$baz)

  expected <- list(foo = 'bar', quux = 'quux', baz = 'blah')
  x <- expect_silent(
    yaml.load("foo: bar\n<<: [{quux: quux}, {foo: doo}, {foo: junk}, {baz: blah}, {baz: boo}]", TRUE))
  expect_equal(expected, x)

  x <- expect_silent(yaml.load("foo: bar\n<<: {foo: baz}\n<<: {foo: quux}"))
  expect_equal(1L, length(x))
  expect_equal("bar", x$foo)

  x <- expect_silent(yaml.load("<<: {foo: bar}\nfoo: baz"))
  expect_equal(list(foo = 'bar'), x)
})

test_that("named maps are merged with warnings",
{
  x <- yaml.load("foo: bar\n<<: {baz: boo}", as.named.list = TRUE, merge.warning = TRUE)
  expect_equal(2L, length(x))
  expect_equal("bar", x$foo)
  expect_equal("boo", x$baz)

  expected <- list(foo = 'bar', quux = 'quux', baz = 'blah')
  warnings <- capture_warnings({
    x <- yaml.load("foo: bar\n<<: [{quux: quux}, {foo: doo}, {foo: junk}, {baz: blah}, {baz: boo}]", as.named.list = TRUE, merge.warning = TRUE)
    expect_equal(expected, x)
  })
  
  expect_equal(c("Duplicate map key ignored during merge: 'foo'",
                 "Duplicate map key ignored during merge: 'foo'",
                 "Duplicate map key ignored during merge: 'baz'"), warnings)

  warnings <- capture_warnings({
    x <- yaml.load("foo: bar\n<<: {foo: baz}\n<<: {foo: quux}", as.named.list = TRUE, merge.warning = TRUE)
    expect_equal(1L, length(x))
  })
  expect_equal("bar", x$foo)
  expect_equal(c("Duplicate map key ignored during merge: 'foo'",
                 "Duplicate map key ignored during merge: 'foo'"), warnings)

  warnings <- capture_warnings({
    x <- yaml.load("<<: {foo: bar}\nfoo: baz", as.named.list = TRUE, merge.warning = TRUE)
    expect_equal(list(foo = 'bar'), x)   
  })

  expect_equal(c("Duplicate map key ignored after merge: 'foo'"), warnings)
})

test_that("unnamed maps are merged without warnings",
{
  x <- yaml.load("foo: bar\n<<: {baz: boo}", as.named.list = FALSE)
  expect_equal(2L, length(x))
  expect_equal(list("foo", "baz"), attr(x, 'keys'))
  expect_equal("bar", x[[1]])
  expect_equal("boo", x[[2]])

  x <- expect_silent(
    yaml.load("foo: bar\n<<: [{quux: quux}, {foo: doo}, {baz: boo}]", as.named.list = FALSE)
  )
  expect_equal(3L, length(x))
  expect_equal(list("foo", "quux", "baz"), attr(x, 'keys'))
  expect_equal("bar", x[[1]])
  expect_equal("quux", x[[2]])
  expect_equal("boo", x[[3]])

  x <- expect_silent(
    yaml.load("<<: {foo: bar}\nfoo: baz", as.named.list = FALSE)
  )
  expect_equal(1L, length(x))
  expect_equal(list("foo"), attr(x, 'keys'))
  expect_equal("bar", x[[1]])
})

test_that("unnamed maps are merged with warnings",
{
  x <- expect_silent(
    yaml.load("foo: bar\n<<: {baz: boo}", as.named.list = FALSE, merge.warning = TRUE)
  )
  expect_equal(2L, length(x))
  expect_equal(list("foo", "baz"), attr(x, 'keys'))
  expect_equal("bar", x[[1]])
  expect_equal("boo", x[[2]])

  warnings <- capture_warnings({
    x <- yaml.load("foo: bar\n<<: [{quux: quux}, {foo: doo}, {baz: boo}]", as.named.list = FALSE, merge.warning = TRUE)
    expect_equal(3L, length(x))
    expect_equal(list("foo", "quux", "baz"), attr(x, 'keys'))
    expect_equal("bar", x[[1]])
    expect_equal("quux", x[[2]])
    expect_equal("boo", x[[3]])
  })
  expect_equal("Duplicate map key ignored during merge: 'foo'", warnings)

  warnings <- capture_warnings({
    x <- yaml.load("<<: {foo: bar}\nfoo: baz", as.named.list = FALSE, merge.warning = TRUE)
    expect_equal(1L, length(x))
    expect_equal(list("foo"), attr(x, 'keys'))
    expect_equal("bar", x[[1]])
  })

  expect_equal(c("Duplicate map key ignored after merge: 'foo'"), warnings)
})

test_that("duplicate keys throws an error",
{
  expect_error(yaml.load("foo: bar\nfoo: baz\n", TRUE))
})

test_that("duplicate keys with merge first does not throw an error",
{
  result <- try(yaml.load("<<: {foo: bar}\nfoo: baz\n", TRUE))
  expect_true(!inherits(result, "try-error"))
})

test_that("invalid merges throw errors",
{
  expect_error(yaml.load("foo: bar\n<<: [{leet: hax}, blargh, 123]", TRUE))
  expect_error(yaml.load("foo: bar\n<<: [123, blargh, {leet: hax}]", TRUE))
  expect_error(yaml.load("foo: bar\n<<: junk", TRUE))
})

test_that("syntax errors throw errors",
{
  expect_error(yaml.load("[whoa, this is some messed up]: yaml?!: dang"))
})

test_that("null types are converted",
{
  x <- yaml.load("~")
  expect_equal(NULL, x)
})

#test_that("should handle binary type",
#{
#  x <- yaml.load("!!binary 0b101011")
#  expect_equal("0b101011", x)
#}

test_that("bool yes type is converted",
{
  x <- yaml.load("yes")
  expect_equal(TRUE, x)
})

test_that("bool no type is converted",
{
  x <- yaml.load("no")
  expect_equal(FALSE, x)
})

test_that("int hex type is converted",
{
  x <- yaml.load("0xF")
  expect_equal(15L, x)
})

test_that("int oct type is converted",
{
  x <- yaml.load("015")
  expect_equal(13L, x)
})

#test_that("should handle int base60 type",
#{
#  x <- yaml.load("1:20")
#  expect_equal("1:20", x)
#}

test_that("int type is converted",
{
  x <- yaml.load("31337")
  expect_equal(31337L, x)
})

test_that("explicit int type is converted",
{
  x <- yaml.load("!!int 31337")
  expect_equal(31337L, x)
})

#test_that("should handle float base60 type",
#{
#  x <- yaml.load("1:20.5")
#  expect_equal("1:20.5", x)
#}

test_that("float nan type is converted",
{
  x <- yaml.load(".NaN")
  expect_true(is.nan(x))
})

test_that("float inf type is converted",
{
  x <- yaml.load(".inf")
  expect_equal(Inf, x)
})

test_that("float neginf type is converted",
{
  x <- yaml.load("-.inf")
  expect_equal(-Inf, x)
})

test_that("float type is converted",
{
  x <- yaml.load("123.456")
  expect_equal(123.456, x)
})

#test_that("should handle timestamp iso8601 type",
#{
#  x <- yaml.load("!timestamp#iso8601 2001-12-14t21:59:43.10-05:00")
#  expect_equal("2001-12-14t21:59:43.10-05:00", x)
#}

#test_that("should handle timestamp spaced type",
#{
#  x <- yaml.load("!timestamp#spaced 2001-12-14 21:59:43.10 -5")
#  expect_equal("2001-12-14 21:59:43.10 -5", x)
#}

#test_that("should handle timestamp ymd type",
#{
#  x <- yaml.load("!timestamp#ymd 2008-03-03")
#  expect_equal("2008-03-03", x)
#}

#test_that("should handle timestamp type",
#{
#  x <- yaml.load("!timestamp 2001-12-14t21:59:43.10-05:00")
#  expect_equal("2001-12-14t21:59:43.10-05:00", x)
#}

test_that("aliases are handled",
{
  x <- yaml.load("- &foo bar\n- *foo")
  expect_equal(c("bar", "bar"), x)
})

test_that("str type is converted",
{
  x <- yaml.load("lickety split")
  expect_equal("lickety split", x)
})

test_that("bad anchors are handled",
{
  warnings <- capture_warnings({
    x <- yaml.load("*blargh")
    expected <- " yaml.bad-anchor "
    class(expected) <- " yaml.bad-anchor "
    expect_equal(expected, x)
  })
  
  expect_equal("Unknown anchor: blargh", warnings)
})

test_that("custom null handler is applied",
{
  x <- yaml.load("~", handlers=list("null"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("custom binary handler is applied",
{
  x <- yaml.load("!binary 0b101011", handlers=list("binary"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("custom bool yes handler is applied",
{
  x <- yaml.load("yes", handlers=list("bool#yes"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("custom bool no handler is applied",
{
  x <- yaml.load("no", handlers=list("bool#no"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("custom int hex handler is applied",
{
  x <- yaml.load("0xF", handlers=list("int#hex"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("custom int oct handler is applied",
{
  x <- yaml.load("015", handlers=list("int#oct"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("int base60 is not coerced by default",
{
  x <- yaml.load("1:20")
  expect_equal("1:20", x)
})

test_that("custom int base60 handler is applied",
{
  x <- yaml.load("1:20", handlers=list("int#base60"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("custom int handler is applied",
{
  x <- yaml.load("31337", handlers=list("int"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("custom float base60 handler is applied",
{
  x <- yaml.load("1:20.5", handlers=list("float#base60"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("custom float nan handler is applied",
{
  x <- yaml.load(".NaN", handlers=list("float#nan"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("custom float inf handler is applied",
{
  x <- yaml.load(".inf", handlers=list("float#inf"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("custom float neginf handler is applied",
{
  x <- yaml.load("-.inf", handlers=list("float#neginf"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("custom float handler is applied",
{
  x <- yaml.load("123.456", handlers=list("float#fix"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("custom timestamp iso8601 handler is applied",
{
  x <- yaml.load("2001-12-14t21:59:43.10-05:00", handlers=list("timestamp#iso8601"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

#test_that("should use custom timestamp spaced handler",
#{
#  x <- yaml.load('!"timestamp#spaced" 2001-12-14 21:59:43.10 -5', handlers=list("timestamp#spaced"=function(x) { "argh!" }))
#  expect_equal("argh!", x)
#}

test_that("custom timestamp ymd handler is applied",
{
  x <- yaml.load("2008-03-03", handlers=list("timestamp#ymd"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("custom merge handler is not applied",
{
  warnings <- capture_warnings({
    x <- yaml.load("foo: &foo\n  bar: 123\n  baz: 456\n\njunk:\n  <<: *foo\n  bah: 789", handlers=list("merge"=function(x) { "argh!" }))
    expect_equal(list(foo=list(bar=123, baz=456), junk=list(bar=123, baz=456, bah=789)), x)
  })
  expect_equal("Custom handling for type 'merge' is not allowed; handler ignored", warnings)
})

test_that("custom str handler is applied",
{
  x <- yaml.load("lickety split", handlers=list("str"=function(x) { "argh!" }))
  expect_equal("argh!", x)
})

test_that("handler for unknown type is applied",
{
  x <- yaml.load("!viking pillage", handlers=list(viking=function(x) { paste(x, "the village") }))
  expect_equal("pillage the village", x)
})

test_that("custom seq handler is applied",
{
  x <- yaml.load("- 1\n- 2\n- 3", handlers=list(seq=function(x) { as.integer(x) + 3L }))
  expect_equal(4:6, x)
})

test_that("custom map handler is applied",
{
  x <- yaml.load("foo: bar", handlers=list(map=function(x) { x$foo <- paste(x$foo, "yarr"); x }))
  expect_equal("bar yarr", x$foo)
})

test_that("custom typed seq handler is applied",
{
  x <- yaml.load("!foo\n- 1\n- 2", handlers=list(foo=function(x) { as.integer(x) + 1L }))
  expect_equal(2:3, x)
})

test_that("custom typed map handler is applied",
{
  x <- yaml.load("!foo\nuno: 1\ndos: 2", handlers=list(foo=function(x) { x$uno <- "uno"; x$dos <- "dos"; x }))
  expect_equal(list(uno="uno", dos="dos"), x)
})

# NOTE: this works, but R_tryEval doesn't return when called non-interactively
#test_that("should handle a bad handler",
#{
#  x <- yaml.load("foo", handlers=list(str=function(x) { blargh }))
#  str(x)
#}

test_that("empty documents are loaded",
{
  x <- yaml.load("")
  expect_equal(NULL, x)
  x <- yaml.load("# this document only has\n   # wickedly awesome comments")
  expect_equal(NULL, x)
})

test_that("omaps are loaded",
{
  x <- yaml.load("--- !omap\n- foo:\n  - 1\n  - 2\n- bar:\n  - 3\n  - 4")
  expect_equal(2L, length(x))
  expect_equal(c("foo", "bar"), names(x))
  expect_equal(1:2, x$foo)
  expect_equal(3:4, x$bar)
})

test_that("omaps are loaded when named is false",
{
  x <- yaml.load("--- !omap\n- 123:\n  - 1\n  - 2\n- bar:\n  - 3\n  - 4", FALSE)
  expect_equal(2L, length(x))
  expect_equal(list(123L, "bar"), attr(x, "keys"))
  expect_equal(1:2, x[[1]])
  expect_equal(3:4, x[[2]])
})

test_that("named opam with duplicate key causes error",
{
  expect_error(yaml.load("--- !omap\n- foo:\n  - 1\n  - 2\n- foo:\n  - 3\n  - 4"))
})

test_that("unnamed omap with duplicate key causes error",
{
  expect_error(yaml.load("--- !omap\n- foo:\n  - 1\n  - 2\n- foo:\n  - 3\n  - 4", FALSE))
})

test_that("invalid omap causes error",
{
  expect_error(yaml.load("--- !omap\nhey!"))
  expect_error(yaml.load("--- !omap\n- sup?"))
})

test_that("expressions are not implicitly converted with warning",
{
  warnings <- capture_warnings({
    x <- yaml.load("!expr |\n  function() \n  {\n    'hey!'\n  }")
    expect_equal("function() \n{\n  'hey!'\n}", x)
  })
 
#  expect_equal("function", class(x))
#  expect_equal("hey!", x())
  expect_equal("Evaluating R expressions (!expr) requires explicit `eval.expr=TRUE` option (see yaml.load help)", warnings)
})

test_that("expressions are explicitly converted without warning",
{
  x <- expect_silent({
    yaml.load("!expr |\n  function() \n  {\n    'hey!'\n  }", eval.expr = TRUE)
  })
  expect_equal("function", class(x))
  expect_equal("hey!", x())
})

test_that("expressions are explicitly not converted",
{
  x <- yaml.load("!expr 123 + 456", eval.expr = FALSE)
  expect_equal("123 + 456", x)
})

test_that("invalid expressions cause error",
{
  expect_error(yaml.load("!expr |\n  1+", eval.expr=TRUE))
})

# NOTE: this works, but R_tryEval doesn't return when called non-interactively
#test_that("should error for expressions with eval errors",
#{
#  x <- try(yaml.load("!expr |\n  1 + non.existent.variable"))
#  assert(inherits(x, "try-error"))
#}

test_that("maps are in ordered",
{
  x <- yaml.load("{a: 1, b: 2, c: 3}")
  expect_equal(c('a', 'b', 'c'), names(x))
})

test_that("illegal recursive anchor is handled",
{
  warnings <- capture_warnings({
    x <- yaml.load('&foo {foo: *foo}')
    expected <- " yaml.bad-anchor "
    class(expected) <- " yaml.bad-anchor "
    expect_equal(expected, x$foo)
  })

  expect_equal("Unknown anchor: foo", warnings)
})

test_that("dereferenced aliases have unshared names",
{
  x <- yaml.load('{foo: &foo {one: 1, two: 2}, bar: *foo}')
  x$foo$one <- 'uno'
  expect_equal(1L, x$bar$one)
})

test_that("multiple anchors are handled",
{
  x <- yaml.load('{foo: &foo {one: 1}, bar: &bar {two: 2}, baz: *foo, quux: *bar}')
  expected <- list(
    foo = list(one = 1),
    bar = list(two = 2),
    baz = list(one = 1),
    quux = list(two = 2)
  )
  expect_equal(expected, x)
})

test_that("quoted strings are preserved",
{
  x <- yaml.load("'12345'")
  expect_equal("12345", x)
})

test_that("inf is loaded properly",
{
  result <- yaml.load(".inf\n...\n")
  expect_equal(Inf, result)
})

test_that("negative inf is loaded properly",
{
  result <- yaml.load("-.inf\n...\n")
  expect_equal(-Inf, result)
})

test_that("nan is loaded properly",
{
  result <- yaml.load(".nan\n...\n")
  expect_equal(NaN, result)
})

test_that("logical na is loaded properly",
{
  result <- yaml.load(".na\n...\n")
  expect_equal(NA, result)
})

test_that("numeric na is loaded properly",
{
  result <- yaml.load(".na.real\n...\n")
  expect_equal(NA_real_, result)
})

test_that("integer na is loaded properly",
{
  result <- yaml.load(".na.integer\n...\n")
  expect_equal(NA_integer_, result)
})

test_that("character na is loaded properly",
{
  result <- yaml.load(".na.character\n...\n")
  expect_equal(NA_character_, result)
})

test_that("true is loaded properly from y",
{
  result <- yaml.load("y\n...\n")
  expect_equal(TRUE, result)
})

test_that("false is loaded properly from n",
{
  result <- yaml.load("n\n...\n")
  expect_equal(FALSE, result)
})

test_that("numeric sequence with missing values loads properly",
{
  result <- yaml.load("[1.2, 3.4, .na.real]")
  expect_equal(c(1.2, 3.4, NA), result)
})

test_that("integer sequence with missing values loads properly",
{
  result <- yaml.load("[1, 2, .na.integer]")
  expect_equal(c(1, 2, NA), result)
})

test_that("string sequence with missing values loads properly",
{
  result <- yaml.load("[foo, bar, .na.character]")
  expect_equal(c("foo", "bar", NA), result)
})

test_that("logical sequence with missing values loads properly",
{
  result <- yaml.load("[y, n, .na]")
  expect_equal(c(TRUE, FALSE, NA), result)
})

test_that("numeric sequence with nans loads properly",
{
  result <- yaml.load("[1.2, 3.4, .nan]")
  expect_equal(c(1.2, 3.4, NaN), result)
})

test_that("numeric represented in exponential form is loaded properly",
{
  expect_equal(1000000, yaml.load("1.0e+06"))
})

test_that("numeric without leading digits is loaded properly",
{
  expect_equal(0.9, yaml.load(".9"))
})

test_that("integer overflow creates a warning",
{
  expect_warning(result <- yaml.load("2147483648"))
  expect_equal(NA_integer_, result)
  expect_warning(result <- yaml.load("2147483649"))
  expect_equal(NA_integer_, result)
})

test_that("numeric overflow creates a warning",
{
  expect_warning(result <- yaml.load("1.797693e+309"))
  expect_equal(NA_real_, result)
})

test_that("list of one list is loaded properly",
{
  result <- yaml.load('a:\n -\n  - b\n  - c\n')
  expect_equal(list(a = list(c("b", "c"))), result)
})

test_that("override merge precedence",
{
  doc <- "[ &one { foo: bar }, { <<: *one, foo: baz } ]"
  expected <- list(list(foo = 'bar'), list(foo = 'baz'))
  result <- yaml.load(doc, merge.precedence = "override")
  expect_equal(expected, result)
})

test_that("explicit bool tag for true value",
{
  doc <- "!!bool 'true'"
  expected <- TRUE
  result <- yaml.load(doc)
  expect_equal(expected, result)
})

test_that("explicit bool tag for false value",
{
  doc <- "!!bool 'false'"
  expected <- FALSE
  result <- yaml.load(doc)
  expect_equal(expected, result)
})

test_that("explicit bool tag for na value",
{
  doc <- "!!bool '.na'"
  expected <- NA
  result <- yaml.load(doc)
  expect_equal(expected, result)
})

test_that("explicit bool tag for invalid value",
{
  doc <- "!!bool foo"
  warnings <- capture_warnings({
    result <- yaml.load(doc)
    expect_equal(result, NA)
  })

  expect_equal(c("NAs introduced by coercion: foo is not a recognized boolean value"), warnings)
})

test_that("builtin as handler works",
{
  x <- "{a: 1, b: 2, c: 3}"
  results <- expect_silent(
    yaml.load(x, handlers=list(int=as.double))
  )
  expect_equal(class(results$a), "numeric")
})