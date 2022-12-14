#' Read a YAML file
#' 
#' Read a YAML document from a file and create an R object from it
#'
#' @param file Either a character string naming a file or a \link{connection}
#'   open for writing.
#' @param fileEncoding character string; If non-empty declares the
#'    encoding used on a file (not a connection) so the character data can
#'    be re-encoded. See \code{\link{file}}.
#' @param text character string; If \code{file} is not supplied and this is,
#'    then data are read from the value of \code{text} via a text connection.
#'    Notice that a literal string can be used to include (small) data sets
#'    within R code.
#' @param error.label a label to prepend to error messages (see Details).
#' @param readLines.warn logical (default:TRUE); Suppress warnings from readLines used inside read_yaml.
#' @param ... Arguments to pass to \code{\link{yaml.load}}.
#' @return If the root YAML object is a map, a named list or list with an attribute of 'keys' is
#' returned. If the root object is a sequence, a list or vector is returned, depending
#' on the contents of the sequence. A vector of length 1 is returned for single objects.
#'
#' @details 
#' This function is a convenient wrapper for \code{\link{yaml.load}} and is a
#' nicer alternative to \code{\link{yaml.load_file}}.
#' 
#'  You can specify a label to be prepended to error messages via the
#'  \code{error.label} argument.  If \code{error.label} is missing,
#'  \code{read_yaml} will make an educated guess for the value of
#'  \code{error.label} by either using the specified filename (when \code{file}
#'  is a character vector) or using the description of the supplied connection
#'  object (via the \code{summary} function).  If \code{text} is used, the
#'  default value of \code{error.label} will be \code{NULL}.
#'
#' @family yaml
#' @examples 
#'  \dontrun{
#'    # reading from a file connection
#'    filename <- tempfile()
#'    cat("test: data\n", file = filename)
#'    con <- file(filename, "r")
#'    read_yaml(con)
#'    close(con)
#'    
#'    # using a filename to specify input file
#'    read_yaml(filename)
#'  }
#'  
#'  # reading from a character vector
#'  read_yaml(text="- hey\n- hi\n- hello")
#' @keywords programming data manip
#' @export
`read_yaml` <-
function(file, fileEncoding = "UTF-8", text, error.label,
         readLines.warn=TRUE, ...)
{
  if (missing(file) && !missing(text))
    {
    if (missing(error.label))
    {
      error.label <- NULL
    }
    file <- textConnection(text, encoding = "UTF-8")
    on.exit(close(file))
  }
  else if (is.character(file))
  {
    if (missing(error.label))
    {
      error.label <- file
    }
    file <- if (nzchar(fileEncoding)) 
    {
      file(file, "rt", encoding = fileEncoding)
    } else
    {
      file(file, "rt")
    }
    on.exit(close(file))
  }
  else if (inherits(file, "connection"))
  {
    if (missing(error.label))
    {
      # try to guess filename
      s <- try(summary(file), silent = TRUE)
      if (!inherits(s, "try-error") &&
          is.list(s) &&
          "description" %in% names(s))
      {
        error.label <- s$description
      }
    }
    
    if (!isOpen(file, "rt"))
    {
      open(file, "rt")
      on.exit(close(file))
    }
  } else
  {
    stop("'file' must be a character string filename or connection")
  }
  
  string <- paste(readLines(file,warn=readLines.warn), collapse="\n")
  yaml.load(string, error.label = error.label, ...)
}