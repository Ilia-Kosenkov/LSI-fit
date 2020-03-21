fit_with_greta <- function(
    data,
    i = vec_c(40, 90),
    e = vec_c(0, 0.5),
    omega = vec_c(-180, -90),
    lambda_p = vec_c(200, 250),
    q_0 = vec_c(-0.5, 0.5),
    u_0 = vec_c(-2, -1),
    tau = vec_c(1e-4, 1e-2),
    phi_p = vec_c(0, 360)) {
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

    view_diagram(mdl, fs::path("output", "diagram.pdf") %>% file_out)
}