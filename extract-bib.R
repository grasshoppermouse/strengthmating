library(tidyverse)

clean_bib <- function(input_file, input_bib){
  lines <- paste(readLines(input_file), collapse = "")
  entries <- unique(str_match_all(lines, "@([a-zA-Z0-9_]+)[,;\\. \\?\\!\\]]")[[1]][, 2])
  return(entries)
  # bib <- paste(readLines(input_bib), collapse = "\n")
  # bib <- unlist(strsplit(bib, "\n@"))
  #
  # output <- sapply(entries, grep, bib, value = T)
  # output <- paste("@", output, sep = "")
  #
  # writeLines(unlist(output), output_bib)
}

# now call the function
# clean_bib(...)


keys <- clean_bib("paper.v2.qmd", "regrep.bib")

library(bib2df)
d <- bib2df("regrep.bib")

d_used <-
  d |>
  dplyr::filter(BIBTEXKEY %in% keys) |>
  dplyr::select(-ABSTRACT, -FILE)

df2bib(d_used, "paper.bib")
