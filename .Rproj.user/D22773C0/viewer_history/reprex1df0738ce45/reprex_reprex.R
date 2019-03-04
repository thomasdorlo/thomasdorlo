#' ---
#' output:
#'   md_document:
#'     pandoc_args: [
#'       '-f', 'markdown-implicit_figures',
#'       '-t', 'commonmark',
#'       --wrap=preserve
#'     ]
#' ---



#+ reprex-setup, include = FALSE
options(tidyverse.quiet = TRUE)
knitr::opts_chunk$set(collapse = TRUE, comment = "#>", error = TRUE)
knitr::opts_knit$set(upload.fun = identity)

#+ reprex-body
#' No user-supplied code found ... so we've made some up. You're welcome!

#+ fortunes, include = requireNamespace("fortunes", quietly = TRUE), eval = requireNamespace("fortunes", quietly = TRUE)
fortunes::fortune()

#+ no-fortunes, include = !requireNamespace("fortunes", quietly = TRUE)
sprintf("Happy %s!", weekdays(Sys.Date()))






