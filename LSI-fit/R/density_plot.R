breaks <- vec_c("N", "B", "V", "R")
colors <- vec_c("black", "blue", "green", "red")
ltys <- vec_c(1, 1, 2, 3)

group_by_filter <- function(data) {
    data %>%
        mutate(.var = str_replace_all(
            .var,
            set_names(
                vec_c(":B", ":V", ":R"),
                glue_fmt_chr("\\[{1:3},1\\]$")))) %>%
        separate(".var", vec_c(".var", ".filter"), ":", fill = "right") %>%
        mutate(.filter = if_else(is.na(.filter), "N", .filter) %>% as_factor)
        
}

generate_pairs <- function(data) {
    data %>%
        pull(.var) %>%
        unique -> params

    params <- params[4:6]

    params %>%
        imap_dfr(~tibble(Row = .x, Col = params[1:.y], RowId = .y, ColId = 1:.y))
}

spread_data <- function(data) {
    data %>% pull(.var) %>% unique %->% c(left, right)

    left_table <- filter(data, .var %==% left) %>%
        transmute(.draw, .filter, !!left := .obs)
    right_table <- filter(data, .var %==% right) %>%
        transmute(.draw, .filter, !!right := .obs)

    if (vec_size(left_table) < vec_size(right_table))
        left_table <- map_dfr(vec_c("B", "V", "R"), ~ mutate(left_table, .filter = .x))
    if (vec_size(right_table) < vec_size(left_table))
        right_table <- map_dfr(vec_c("B", "V", "R"), ~ mutate(right_table, .filter = .x))



        return(inner_join(left_table, right_table, by = c(".draw", ".filter")))

}

generate_1d <- function(data, lims, plot_x_vals) {
    data %>%
    ggplot_sci(aes(
            x = .obs,
            col = .filter,
            linetype = .filter,
            group = .filter)) +
        geom_density() +
        scale_x_sci(name = NULL, limits = lims,
            labels =
                if (vec_cast(plot_x_vals, logical()) %===% TRUE)
                    waiver()
                else
                    function(x) rep("", vec_size(x)),
            sec.axis = dup_axis_sci_weak()) +
        scale_y_sci(name = NULL,
            labels = function(x) rep("", vec_size(x)),
            sec.axis = dup_axis_sci()) +
        scale_color_manual(breaks = breaks, values = colors) +
        scale_linetype_manual(breaks = breaks, values = ltys) +
        theme_sci(legend.position = "none")
}

generate_2d <- function(data, lims, plot_x_vals, plot_y_vals) {
    data %>%
        spread_data -> data

    names(data) %->% c(.draw, .filter, x_name, y_name)

    ggplot_sci(data, aes(x = !!sym(x_name), y = !!sym(y_name))) +
        geom_density2d_sci(breaks = vec_c(0.68, 0.95, 0.997)) +
        scale_x_sci(name = NULL, limits = lims[[x_name]],
            labels =
                if (vec_cast(plot_x_vals, logical()) %===% TRUE)
                    waiver()
                else
                    function(x) rep("", vec_size(x)),
            sec.axis = dup_axis_sci_weak()) +
        scale_y_sci(name = NULL, limits = lims[[y_name]],
            labels =
                if (vec_cast(plot_y_vals, logical()) %===% TRUE)
                    waiver()
                else
                    function(x) rep("", vec_size(x)),
            sec.axis = dup_axis_sci_weak()) +
        guides(fill = FALSE)
}

generate_density <- function(tbl, data, ranges) {
    sz <- tbl %>% pull(Row) %>% unique %>% vec_size
    mar_1 <- mar_(0.05 ~ cm, 0.05 ~ cm, 0.05 ~ cm, 1 ~ cm)
    mar_2 <- mar_(0.05 ~ cm, 0.05 ~ cm, 0.5 ~ cm, 1 ~ cm)
    pmap(tbl, function(Row, Col, RowId, ColId) {
        if (Row %===% Col)
            generate_1d(data %>% filter(.var %==% Row), ranges[[Row]], ColId %===% sz)
        else
            generate_2d(
                data %>% filter(.var %vin% vec_c(Row, Col)),
                ranges[vec_c(Row, Col)],
                RowId %===% sz,
                ColId %===% 1L)
        }) %>%
        map(ggplot_build %>>% ggplot_gtable) %>%
        map2(tbl$RowId, ~postprocess_axes(.x,
            axes_margin = if(.y %===% sz) mar_2 else mar_1,
            text_margin = mar_(0 ~ pt))) -> grobs

    m <- make_layout(sz)
    arrangeGrob(grobs = grobs, layout_matrix = m) -> grob

    tbl %>%
        pull(Col) %>%
        unique %>%
        map(textGrob, x = u_(0.5$npc + 0.5$cm)) -> lower_text_grobs
    tbl %>%
        pull(Row) %>%
        unique %>%
        map(textGrob, rot = 90) -> left_text_grobs
    grob %>%
        gtable_add_rows(u_(0.5 ~ cm)) %>%
        gtable_add_grob(gtable_row("bottom-row", lower_text_grobs), t = sz + 1, l = 1, r = sz) %>%
        gtable_add_cols(u_(0.5 ~ cm), pos = 0) %>%
        gtable_add_grob(gtable_col("left-col", left_text_grobs), t = 1, l = 1, b = sz)
    
}

fix_names <- function(name) {
    switch(name,
        "lambda_p" = "$\\lambda_\\mathrm{p}$",
        "phi_p" = "$\\phi_\\mathrm{p}$",
        "omega" = "$\\Omega$",
        "tau[1,1]" = "$\\tau_B$",
        "tau[2,1]" = "$\\tau_V$",
        "tau[3,1]" = "$\\tau_R$",
        glue_fmt_chr("${name}$")
           )

}

make_layout <- function(n) {
    1:n %>%
        map(~{
            tmp <- vec_init(integer(), n)
            seq <- 1:.x
            tmp[seq] <- seq + sum(0:(.x - 1L))
            tmp
        }) %>%
        as_vec %>%
        matrix(nrow = n, ncol = n, byrow = TRUE)
}

compute_ranges <- function(tbl, data) {
    tbl %>% pull(Row) %>% unique -> vars
    vars %>%
        set_names(.) %>%
        map(~filter(data, .var %==% .x) %>% pull(.obs) %>% quantile(vec_c(1e-5, 1 - 1e-5)))
}

if (!exists("data_tbl"))
    data_tbl <- read_csv(fs::path("output", "fit_3.csv"), col_types = cols()) %>%
        select(.draw, .var, .obs) %>%
        group_by_filter

#group_by_filter(data_tbl) %>%
    #filter(.var %vin% vec_c("q_0", "u_0")) %>%
    #spread_data %>% print
data_tbl %>% generate_pairs -> tbl
compute_ranges(tbl, data_tbl) -> ranges
print(tbl)

grid.newpage()
generate_density(tbl, data_tbl, ranges) %>% grid.draw




# 1. Compute 0.005 - 0.995 ?? limits and apply to all
# 2. Remove axes from inner plots
# 3. Change margins to make all plots of same size