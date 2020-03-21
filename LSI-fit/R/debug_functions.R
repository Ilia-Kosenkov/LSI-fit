view_diagram <- function(
    mdl,
    file = paste0(fs::file_temp("diagrammer_"), ".pdf")) {
    dir <- fs::path_dir()
    if (!fs::dir_exists(dir))
        fs::dir_create(dir)
    DiagrammeR::export_graph(
        mdl %>% plot %@% dgr_graph,
        file,
        stringr::str_extract(file, "(?<=\\.)\\w*$"))
    system(glue_fmt_chr("open {file}"))
}
