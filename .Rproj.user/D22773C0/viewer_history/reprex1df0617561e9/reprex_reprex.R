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
knitr::opts_knit$set(upload.fun = knitr::imgur_upload)

#+ reprex-body
bib2acad2(bibfile="C:/Users/thoma/Dropbox/Site/academic-kickstart-master/content/publication/bibtex/Dorlo9.bib", overwrite=T)



#' <sup>Created on `r Sys.Date()` by the [reprex package](https://reprex.tidyverse.org) (v`r utils::packageVersion("reprex")`)</sup>


