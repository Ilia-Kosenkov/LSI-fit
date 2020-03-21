view_diagram <- function(mdl, file) {

    dir <- fs::path_dir(file)
    if (!fs::dir_exists(dir))
        fs::dir_create(dir)
    DiagrammeR::export_graph(
        mdl %>% plot %@% dgr_graph,
        file,
        "pdf")
    file_out(file)
}
