debug_data_plot <- function() {

    import_data() %>%
        ggplot_sci(aes(
            x = Phase, y = Obs,
            ymin = Obs - Err, ymax = Obs + Err,
            col = Filter)) +
            geom_pointrange() +
            geom_line() +
            scale_x_sci() +
            scale_y_sci() +
            scale_color_manual(
                breaks = vec_c("B", "V", "R"),
                values = vec_c("blue", "green", "red")) +
            facet_sci(vars(Type), scales = "free_y")

}

debug_kepler_eq_plot <- function() {

    x <- seq(0, 2, by = 0.01) * pi
    e <- seq(0, 1 - 1e-16, by = 0.1)

    e %>%
        map_dfr(~tibble(M = x, E = solve_E(M, .x), e = .x)) %>%
        mutate(e = as_factor(e)) -> data


    data %>%
        ggplot_sci(aes(x = M, y = E, col = e, group = e)) +
        geom_line() +
        scale_x_sci() +
        scale_y_sci()
}