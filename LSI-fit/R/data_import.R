#' Data import from csvs
#'
#' @param dir Directory to look for csvs
#'
#' @return A tidy \code{tibble} with all the data
import_data <- function(dir = fs::path("data")) {
    # List csvs in the directory
    fs::dir_ls(dir, glob = "*.csv") %>%
    # Parse each files' name and extract filter letter; use as names
    set_names(str_extract(., "(?<=_)[bvr](?=\\.csv)")) %>%
    # Read each file, add a column `Filter` with the letter from the file name;
    # collapse into one data frame
    imap_dfr(~read_csv(.x, col_types = cols()) %>% mutate(Filter = .y)) %>%
    # Rename/change filter letters
    transmute(
        Phase = fi,
        Qobs = q, Qerr = qErr,
        Uobs = u, Uerr = uErr,
        Filter = as_factor(toupper(Filter)) %>%
            fct_relevel("B", "V", "R")) %>%
    # Reshape data to have one observation per row
    pivot_longer(
        cols = c(Qobs, Qerr, Uobs, Uerr),
        names_pattern = "([QU])(obs|err)",
        names_to = c("Type", ".value")) %>%
    # Now `Type` indicates wether value is `Q` or `U`
    # and `Filter` indicates which filter [BVR] is used
    mutate(Type = as_factor(Type)) %>%
    rename(Obs = obs, Err = err)
}
#    Phase Filter Type     Obs    Err
#    <dbl> <fct>  <fct>  <dbl>  <dbl>
# 1 0.0455 B      Q      0.130 0.0173
# ....................................