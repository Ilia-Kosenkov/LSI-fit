breaks <- vec_c("N", "B", "V", "R")
colors <- vec_c("#000000", "#377EB8", "#4DAF4A", "#E41A1C")
ltys <- vec_c(1, 1, 2, 4)

cnt_colors_s <- scales::viridis_pal()(4)
cnt_colors_g <- vec_c(
    brewer.pal(4, "OrRd") %>% rev,
    brewer.pal(4, "BuGn") %>% rev,
    brewer.pal(4, "Blues") %>% rev,
    )

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

    #params <- params[3:5]

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
        left_table <- map_dfr(
            vec_c("B", "V", "R"),
            ~ mutate(left_table, .filter = .x))
    if (vec_size(right_table) < vec_size(left_table))
        right_table <- map_dfr(
            vec_c("B", "V", "R"),
            ~ mutate(right_table, .filter = .x))



        return(inner_join(left_table, right_table, by = c(".draw", ".filter")))

}

generate_1d <- function(data, lims, plot_x_vals, plot_y_vals = FALSE) {
    data %>%
    ggplot_sci(aes(
            x = .obs,
            col = .filter,
            linetype = .filter,
            group = .filter)) +
        geom_density(size = 1) +
        scale_x_sci(name = NULL, limits = lims,
            labels =
                if (vec_cast(plot_x_vals, logical()) %===% TRUE)
                    waiver()
                else
                    function(x) rep("", vec_size(x)),
            sec.axis = dup_axis_sci_weak(),
            breaks_n = 3L) +
        scale_y_sci(name = NULL,
            labels =
                if (vec_cast(plot_y_vals, logical()) %===% TRUE)
            waiver()
            else
                function(x) rep("", vec_size(x)),
            sec.axis = sec_axis_sci(~ .),
            breaks_n = 3L) +
        scale_color_manual(breaks = breaks, values = colors) +
        scale_linetype_manual(breaks = breaks, values = ltys) +
        theme_sci(legend.position = "none")
}

generate_2d <- function(data, lims, plot_x_vals, plot_y_vals) {
    data %>%
        spread_data -> data

    data %>% pull(.filter) %>% unique %>% vec_size %>% equals(1L) -> is_single

    names(data) %->% c(.draw, .filter, x_name, y_name)
    ggplot_sci(data, aes(
            x = !!sym(x_name), y = !!sym(y_name))) +
    #geom_density2d_sci(aes(
    #col = as_factor(.filter),
    #group = as_factor(.filter),
    #linetype = as_factor(.filter)),
    #size = 1,
    #lineend = "round",
    #breaks = 1 - 2 * (1 - pnorm(1:3))) +
        geom_density2d_filled_sci(aes(
            group = as_factor(.filter),
            linetype = as_factor(.filter),
            fill = ..group..),
            alpha = 0.75,
            col = "#000000") +
        scale_x_sci(name = NULL, limits = lims[[x_name]],
            labels =
                if (vec_cast(plot_x_vals, logical()) %===% TRUE)
                    waiver()
                else
                    function(x) rep("", vec_size(x)),
            sec.axis = dup_axis_sci_weak(),
            breaks_n = 3L) +
        scale_y_sci(name = NULL, limits = lims[[y_name]],
            labels =
                if (vec_cast(plot_y_vals, logical()) %===% TRUE)
                    waiver()
                else
                    function(x) rep("", vec_size(x)),
            sec.axis = dup_axis_sci_weak(),
            breaks_n = 3L) +
        #guides(fill = FALSE) +
        scale_fill_manual(values = if(is_single) cnt_colors_s else cnt_colors_g) +
        scale_linetype_manual(breaks = breaks, values = ltys) +
        theme_sci(legend.position = "none")
}

generate_density <- function(tbl, data, ranges) {
    sz <- tbl %>% pull(Row) %>% unique %>% vec_size
    s_mar <- u_(0.05 ~ cm)
    l_mar <- u_(0.95 ~ cm)
    pmap(tbl, function(Row, Col, RowId, ColId) {
        if (Row %===% Col)
            generate_1d(
                data %>% filter(.var %==% Row),
                ranges[[Row]],
                ColId %===% sz,
                ColId %===% 1L)
        else
            generate_2d(
                data %>% filter(.var %vin% vec_c(Row, Col)),
                ranges[vec_c(Row, Col)],
                RowId %===% sz,
                ColId %===% 1L)
        }) %>%
        map(ggplot_build %>>% ggplot_gtable) -> grobs
  
        pmap(list(grobs, tbl$RowId, tbl$ColId), ~postprocess_axes(.x,
            axes_margin = mar_(
                s_mar,
                if(..3 %===% sz) l_mar else s_mar,
                0.5 * (if(..2 %===% sz) l_mar else s_mar),
                if(..3 %===% 1L) l_mar else s_mar),
            text_margin = mar_(0 ~ pt))) -> grobs

    m <- make_layout(sz)
    arrangeGrob(grobs = grobs, layout_matrix = m) -> grob
    delta <- l_mar - s_mar

    tbl %>%
        pull(Col) %>%
        unique %>%
        map(fix_names) %>%
        imap(~ textGrob(.x, x = npc_(0.5) +
                if (.y %===% 1L)
                    delta / 2
                else if (.y %===% sz)
                    - delta / 2
                else u_(0$pt),
            gp = gpar(fontsize = 18))) -> lower_text_grobs
    tbl %>%
        pull(Row) %>%
        unique %>%
        map(fix_names) %>%
        imap(~textGrob(.x, rot = 90, y = npc_(0.5) +
                if (.y %===% sz) l_mar / 2 else u_(0$pt),
            gp = gpar(fontsize = 18))) -> left_text_grobs
    grob %>%
        gtable_add_rows(u_(0.75 ~ cm)) %>%
        gtable_add_grob(lower_text_grobs, t = sz + 1, l = 1:sz) %>%
        gtable_add_cols(u_(0.75 ~ cm), pos = 0) %>%
        gtable_add_grob(left_text_grobs[-1], t = 2:sz, l = 1) -> grob


    panel_width <- (u_(1 ~ npc) - 2 * delta - grob$widths[1]) / sz
    grob$widths[1:sz + 1] <- panel_width
    
    grob$widths[2] <- grob$widths[2] + delta
    grob$widths[1 + sz] <- grob$widths[1 + sz] + delta

    panel_height <- (u_(1 ~ npc) - 0.5 * delta - grob$heights[sz + 1]) / sz
    grob$heights[1:sz] <- panel_height
    grob$heights[sz] <- grob$heights[sz] + 0.5 * delta
    grob
}

fix_names <- function(name) {
    switch(name,
        "lambda_p" = "$\\lambda_\\mathrm{p},~^\\circ$",
        "phi_p" = "$\\phi_\\mathrm{p},~^\\circ$",
        "omega" = "$\\Omega,~^\\circ$",
        "tau" = "$f_0$",
        "i" = "$i,~^\\circ$",
        "q_0" = "$q_0$,~\\%",
        "u_0" = "$u_0$,~\\%",
        glue_fmt_chr("${name}$"))

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
        map(~filter(data, .var %==% .x) %>%
            pull(.obs) %>%
            range)
}

draw_final_plot <- function(src = drake::readd(fit_3_result_all)) {
    library(tikzDevice)
    tictoc::tic("GROB generated")
    data_tbl <- src %>%
        select(.draw, .var, .obs) %>%
        mutate(.obs = if_else(
            .var %vin% vec_c("i", "omega", "lambda_p", "phi_p"),
            rad_2_deg(.obs), .obs)) %>%
        group_by_filter

    data_tbl %>% generate_pairs -> tbl
    compute_ranges(tbl, data_tbl) -> ranges

    generate_density(tbl, data_tbl, ranges) -> grob
    tictoc::toc()
    tictoc::tic("PDF written")
    path <- fs::path("output", "posteriors.tex")
    tikz(path, width = 10 + 1, height = 10 + 0.47, standAlone = TRUE)
    tryCatch(grid.draw(grob), finally = dev.off())
    RLibs::tex_2_pdf(path)
    tictoc::toc()
}