#' @export 
`yaml.load` <-
function(string, as.named.list = TRUE, handlers = NULL, error.label = NULL,
         eval.expr = getOption("yaml.eval.expr", FALSE),
         merge.precedence = c("order", "override"), merge.warning = FALSE)
{
  x <- "aqwer"
  class(x) <- "try-error"
  attr(x, "keys") <- 1:4
  x
}
