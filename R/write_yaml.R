#' Write a YAML file
#'
#' Write the YAML representation of an R object to a file
#'
#' @param x The object to be converted to yaml.
#' @param file Either a string containing a file path or a \link{connection}
#'    open for writing.
#' @param fileEncoding character string; If non-empty declares the
#'    encoding to be used on a file (not a connection) so the character data can
#'    be re-encoded as they are written.  See \code{\link{file}}.
#' @param ...  Arguments to \code{\link{as.yaml}}.
#' @details 
#'
#' If \code{file} is a non-open connection, an attempt is made to open it
#' and then close it after use.
#' 
#'  This function is a convenient wrapper around \code{\link{as.yaml}}.
#'
#' @family yaml
#' @examples
#' \dontrun{
#'    # writing to a file connection
#'    filename <- tempfile()
#'    con <- file(filename, "w")
#'    write_yaml(data.frame(a=1:10, b=letters[1:10], c=11:20), con)
#'    close(con)
#'    
#'    # using a filename to specify output file
#'    write_yaml(data.frame(a=1:10, b=letters[1:10], c=11:20), filename)
#'  }
#' @keywords data manip
#' @export
`write_yaml` <-
function(x, file, fileEncoding = "UTF-8", ...)
{
  result <- as.yaml(x, ...)
  
  if (is.character(file))
  {
    file <-
      if (nzchar(fileEncoding))
      {
        file(file, "w", encoding = fileEncoding)
      } else
      {
        file(file, "w")
      }
    on.exit(close(file))
  } else if (!isOpen(file, "w"))
  {
    open(file, "w")
    on.exit(close(file))
  }
  
  if (!inherits(file, "connection"))
  {
    stop("'file' must be a character string or connection")
  }
  
  cat(result, file=file, sep="")
}