library(tibble)       # Extended data frames
library(dplyr)        # Data frames manipulations
library(tidyr)        # Data "tidying"
library(magrittr)     # Pipes (%>%, %T>%, %<>%)
library(purrr)        # Functional-style methods
library(forcats)      # Dealing with factors
library(readr)        # Advanced I/O
library(ggplot2)      # Graphics
library(stringr)      # String manipulations & regex
                      #
library(RColorBrewer) # Color palette
                      #
library(rlang)        # Basic R library
library(vctrs)        # Basic type library
                      #
library(primitiveR)   # Tools to bridge {tidyverse} and {vctrs} in dev mode
library(sciplotr)     # Better axes/scales & facet for {ggplot2}
                      #
library(drake)        # Execution orchestration & reproducibility
                      #
library(tidybayes)    # Helpers for mcmc chains

reticulate::use_condaenv("r-tensorflow", required = TRUE)
library(greta)        # Tensorflow-based fitting backend


source_all <- function() {
    fs::path("R") %>%
        fs::dir_ls(recurse = TRUE, glob = "*.R") %>%
        discard(str_ends, "packages\\.R") %>%
        walk(source, encoding = "UTF-8")
}