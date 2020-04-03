fit_with_greta <- function(
    data,
    i = vec_c(70, 110),
    e = vec_c(0, 0.25),
    omega = vec_c(-180, -90),
    lambda_p = vec_c(200, 280),
    q_0 = vec_c(-0.5, 0.5),
    u_0 = vec_c(-2, -1),
    tau = vec_c(1e-4, 1e-1),
    phi_p = vec_c(170, 270)) {
    # Trying only R filter for now

    data %<>% filter(Filter %==% "R")
    phi <- data %>% filter(Type %==% "Q") %>% pull(Phase)
    data %>% filter(Type %==% "Q") %>% select(Obs, Err) %->% c(q_obs, q_err)
    data %>% filter(Type %==% "U") %>% select(Obs, Err) %->% c(u_obs, u_err)

    i <- greta::uniform(deg_2_rad(i[1]), deg_2_rad(i[2]))
    e <- greta::uniform(e[1], e[2])
    omega <- greta::uniform(deg_2_rad(omega[1]), deg_2_rad(omega[2]))
    lambda_p <- greta::uniform(deg_2_rad(lambda_p[1]), deg_2_rad(lambda_p[2]))
    q_0 <- greta::uniform(q_0[1], q_0[2])
    u_0 <- greta::uniform(u_0[1], u_0[2])
    tau <- greta::uniform(tau[1], tau[2])
    phi_p <- greta::uniform(deg_2_rad(phi_p[1]), deg_2_rad(phi_p[2]))


    obs <- vec_c(q_obs, u_obs)
    err <- vec_c(q_err, u_err)

    model_polarization(
       phi = 2 * pi * phi,
       phi_p = phi_p,
       tau = tau,
       e = e,
       i = i,
       omega = omega,
       lambda_p = lambda_p,
       q_0 = q_0,
       u_0 = u_0) -> pred

    obs %~% greta::normal(pred, err)

    mdl <- greta::model(i, e, omega, lambda_p, q_0, u_0, tau, phi_p)

    greta::mcmc(mdl, warmup = 5e3, n_samples = 2e4) %>% tidy_draws %>% spread_draws
}

fit_with_greta_all <- function(
    data,
    i = vec_c(70, 110),
    e = vec_c(0, 0.25),
    omega = vec_c(-180, -90),
    lambda_p = vec_c(200, 280),
    q_0 = vec_c(-0.5, 0.5),
    u_0 = vec_c(-2, -1),
    tau = vec_c(1e-4, 1e-1),
    phi_p = vec_c(170, 270),
    q_func = q_int,
    u_func = u_int,
    n_warmup = 1e3,
    n_samples = 1e4) {
    # Trying only R filter for now

    data %>%
        group_split(Filter) -> split_data

    # Shared parameters
    i <- greta::uniform(deg_2_rad(i[1]), deg_2_rad(i[2]))
    e <- greta::uniform(e[1], e[2])
    omega <- greta::uniform(deg_2_rad(omega[1]), deg_2_rad(omega[2]))
    lambda_p <- greta::uniform(deg_2_rad(lambda_p[1]), deg_2_rad(lambda_p[2]))
    phi_p <- greta::uniform(deg_2_rad(phi_p[1]), deg_2_rad(phi_p[2]))

    n <- vec_size(split_data)

    # Per-filter parameters
    q_0 <- greta::uniform(q_0[1], q_0[2], n)
    u_0 <- greta::uniform(u_0[1], u_0[2], n)
    tau <- greta::uniform(tau[1], tau[2], n)

    for (j in vec_seq_along(split_data)) {
        phi <- split_data[[j]] %>% pull(Phase) %>% unique_f
        split_data[[j]] %>% filter(Type %==% "Q") %>% select(Obs, Err) %->% c(q_obs, q_err)
        split_data[[j]] %>% filter(Type %==% "U") %>% select(Obs, Err) %->% c(u_obs, u_err)

        obs <- vec_c(q_obs, u_obs)
        err <- vec_c(q_err, u_err)

        model_polarization(
           phi = 2 * pi * phi,
           phi_p = phi_p,
           tau = tau[j],  # <- per-filter
           e = e,
           i = i,
           omega = omega,
           lambda_p = lambda_p,
           q_0 = q_0[j],  # <- per-filter
           u_0 = u_0[j], # <- per-filter
           q_func = q_func,
           u_func = u_func
           ) -> pred

        obs %~% greta::normal(pred, err)
    }

    mdl <- greta::model(i, e, omega, lambda_p, q_0, u_0, tau, phi_p)

    greta::mcmc(mdl, warmup = n_warmup, n_samples = n_samples) %>%
        tidy_draws %>% spread_draws
}