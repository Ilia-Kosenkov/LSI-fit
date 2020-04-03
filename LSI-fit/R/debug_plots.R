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

debug_density_plot <- function(data = drake::readd(fit_result)) {
    ggplot_sci(
            data,
            aes(x = .obs, group = .chain, col = .chain)) +
        geom_density() +
        scale_x_sci() +
        scale_y_sci() +
        facet_wrap(vars(.var), scales = "free")

}

debug_trace_plot <- function(data = drake::readd(fit_result)) {
    data %>% pull(.iteration) %>% unique %>% sample(5e2) -> ids

    ggplot_sci(
        data %>% filter(.iteration %vin% ids),
        aes(x = .iteration,
            y = .obs,
            group = .chain,
            col = .chain)) +
        geom_line(alpha = 0.5) +
        geom_smooth() +
        scale_x_sci() +
        scale_y_sci() +
        facet_wrap(~.var, scales = "free_y")
}

debug_param_source <- function() {
    tribble(~ .var, ~ .obs,
            "e", 0.1,
            "i", deg_2_rad(85.8),
            "omega", deg_2_rad(-123.1),
            "lambda_p", deg_2_rad(204.4),
            "tau", 0.03,
            "q_0",  0.04,
            "u_0",  -1.23,
            "phi_p", deg_2_rad(0)) %>%
        bind_rows(.) %>%
        mutate(.chain = as_factor(1L), .var = as_factor(.var))
}


debug_data_plot_with_model <- function() {
    drake::readd(data) %>%
        filter(Filter %==% "R") -> data

    solve_E(2 * pi * seq(0, 1, by = 0.005), params$e)

    prediction <- reconstruct_predictions(
        2 * pi * seq(0, 1, by = 0.005),
        #debug_param_source()
        drake::readd(fit_result)
        ) %>% mutate(Phase = Phase / 2 / pi)

    ggplot_sci(
            data,
            aes(x = Phase, y = Obs, ymin = Obs - Err, ymax = Obs + Err)) +
        geom_pointrange() +
        scale_x_sci() +
        scale_y_sci() +
        geom_line(aes(group = Chain, col = Chain), data = prediction) +
        facet_sci(vars(Type), scales = "free") -> plt

    #return(NULL)
    return(plt)
}

debug_data_plot_with_model_all <- function(
    data = drake::readd(data),
    params = drake::readd(fit_result_all),
    q_func = q_int, u_func = u_int
) {
    data %>%
        group_split(Filter) %>%
        vmap(select, Phase, Filter) -> phases


    params %>% pull(.var) %>% unique %>% fct_get -> vars
    common_vars <- vars[str_which(vars, "\\]$", TRUE)]
    vec_seq_along(phases) %>%
        vmap(~vec_c(
            common_vars,
            str_subset(vars, glue_fmt_chr("\\[{.x},\\ ?1\\]$")))) %>%
        vmap(~filter(params, .var %vin% .x)) %>%
        vmap(~mutate(
            .x,
            .var = as_factor(str_replace(
                .var,
                "\\[\\d,\\ ?\\d\\]$", "")))) -> split_params


    vmap2_pt(phases, split_params,
         function(phase_data, pars) {
             reconstruct_predictions(
                     2 * pi * seq(0, 1, by = 0.005),
                     pars,
                     q_func = q_func, u_func = u_func) %>%
                 mutate(Phase = Phase / 2 / pi,
                        Filter = phase_data %>% pull(Filter) %>% vec_slice(1L))
         }) -> prediction



    ggplot_sci(
            data,
            aes(x = Phase, y = Obs,
                ymin = Obs - Err, ymax = Obs + Err,
                col = Filter, fill = Filter,
                shape = Filter)) +
        geom_pointrange() +
        geom_line(
            aes(group = fct_cross(Chain, Filter), linetype = Chain),
            data = prediction) +
        scale_x_sci() +
        scale_y_sci() +
        scale_color_manual(
                breaks = vec_c("B", "V", "R"),
                values = vec_c("blue", "green", "red")) +
        scale_fill_manual(
                breaks = vec_c("B", "V", "R"),
                values = vec_c("blue", "green", "red")) +
        scale_shape_manual(
                breaks = vec_c("B", "V", "R"),
                values = vec_c(21L, 22L, 23L)) +
        facet_sci(vars(Type), scales = "free")

}