compute_stats <- function(draws) {
    draws %>%
        group_by(.var, .chain) %>%
        summarise(Mean = mean(.obs), Err = sd(.obs)) %>%
        ungroup
}

get_averages <- function(stats) {
    stats %>%
        transmute(Vars = set_names(as_list_of(Mean), fct_get(.var))) %>%
        pull(Vars)
}

reconstruct_predictions <- function(
    phi,
    draws = drake::readd(fit_result)) {

    stats <- compute_stats(draws) %>%
        group_split(.chain)

    stats %>%
        map_dfr(function(data) {
            data %>% get_averages -> vars
            chain <- data %>% dplyr::slice(1L) %>% pull(.chain)
            model <- model_polarization(
                phi = phi,
                phi_p = vars$phi_p,
                tau = vars$tau,
                e = vars$e,
                i = vars$i,
                omega = vars$omega,
                lambda_p = vars$lambda_p,
                q_0 = vars$q_0,
                u_0 = vars$u_0)

            tibble(
                Phase = vec_repeat(phi, times = 2L),
                Obs = model,
                Err = 0,
                Chain = chain,
                Type = vec_repeat(as_factor(vec_c("Q", "U")), each = vec_size(phi)))
        })
}