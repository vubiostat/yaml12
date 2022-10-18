#' Convert a YAML file into R objects
#'
#' Parse a YAML file and return R objects. Loads file and sends to \code{\link{yaml.load}}.
#' 
#' @param input filename or connection; Ff \code{input} is a filename, that file must be encoded in UTF-8. 
#' @param error.label A label to prepend to error messages (see Details). 
#' @param readLines.warn logical (default:TRUE) Suppress warnings from readLines used inside read_yaml
#' @param ...  arguments to pass to \code{\link{yaml.load}} 
#' @return If the root YAML object is a map, a named list or list with an attribute of 'keys' is
#' returned.  If the root object is a sequence, a list or vector is returned, depending
#' on the contents of the sequence.  A vector of length 1 is returned for single objects.
#' @details
#' Helper function for \code{\link{yaml.load}}. Given a file or connections it loads
#' the contents into memory and calls
#' \code{\link{yaml.load_file}} which calls \code{\link{yaml.load}} with the
#' loaded content. See \code{\link{yaml.load}} for full details.
#'
#' @examples 
#' \dontrun{
#' # reading from a file (uses readLines internally)
#' filename <- tempfile()
#' cat("foo: 123", file=filename, sep="\n")
#' yaml.load_file(filename)
#' }
#' @family yaml
#' @export
`yaml.load_file` <-
function(input, error.label, readLines.warn=TRUE, ...)
{
  if (missing(error.label))
  {
    if (inherits(input, "connection"))
    {
      # try to guess filename
      s <- try(summary(input), silent = TRUE)
      if (!inherits(s, "try-error") &&
          is.list(s)                &&
          "description" %in% names(s))
      {
        error.label <- s$description
      }
    }
    else if (is.character(input) && nzchar(input[1]))
    {
      error.label <- input[1]
    }
    else
    {
      error.label <- NULL
    }
  }
  
  if (is.character(input))
  {
    con <- file(input, encoding = 'UTF-8')
    on.exit(close(con), add = TRUE)
  } else
  {
    con <- input
  }
  yaml.load(readLines(con, warn=readLines.warn),
            error.label = error.label, ...)
}