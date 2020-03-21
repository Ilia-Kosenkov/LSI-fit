q_int <- function(
    lambda,
    lambda_p,
    tau,
    e,
    i
) {

    e2_2 <- e ^ 2 / 2
    e2_2_p_1 <- 1 + e2_2
    e2_4 <- e2_2 / 2

    -tau / (1 - e ^ 2) ^ 2 *
        ((1 + cos(i) ^ 2) * (
            e2_4 * cos(2 * lambda_p) +
            e * cos(lambda + lambda_p) +
            e2_2_p_1 * cos(2 * lambda) +
            e * cos(3 * lambda - lambda_p) +
            e2_4 * cos(2 * (2 * lambda - lambda_p))) -
        sin(i) ^ 2 * (
            e2_2_p_1 +
            2 * e * cos(lambda - lambda_p) +
            e2_2 * cos(2 * (lambda - lambda_p)))
        )
}

u_int <- function(
    lambda,
    lambda_p,
    tau,
    e,
    i) {

    e2_2 <- e ^ 2 / 2
    e2_2_p_1 <- 1 + e2_2
    e2_4 <- e2_2 / 2

    -2 * tau * cos(i) / (1 - e ^ 2) ^ 2 * (
        e2_4 * sin(2 * lambda_p) +
        e * sin(lambda + lambda_p) +
        e2_2_p_1 * sin(2 * lambda) +
        e * sin(3 * lambda - lambda_p) +
        e2_4 * sin(2 * (2 * lambda - lambda_p))
    )
}


q_rotate <- function(
    q_int,
    u_int,
    omega,
    q_0) {
    q_int * cos(omega) - u_int * sin(omega) + q_0
}

u_rotate <- function(
    q_int,
    u_int,
    omega,
    u_0) {
    q_int * sin(omega) + u_int * cos(omega) + u_0
}

rad_2_deg <- function(x) 180 * x / pi
deg_2_rad <- function(x) pi * x / 180

model_polarization <- function(
    phi,
    phi_p,
    tau,
    e,
    i,
    omega,
    lambda_p,
    q_0,
    u_0
) {
    M <- phi - phi_p
    E <- solve_E(M = M, e = e)
    l_lp <- 2 * atan(((1 + e) / (1 - e)) ^ 0.5 * tan(E))

    lambda <- l_lp + lambda_p

    q_int <- q_int(
        lambda = lambda, lambda_p = lambda_p,
        tau = tau, e = e, i = i)

    u_int <- u_int(
        lambda = lambda, lambda_p = lambda_p,
        tau = tau, e = e, i = i)

    q <- q_rotate(q_int, u_int, omega, q_0)
    u <- u_rotate(q_int, u_int, omega, u_0)

    vec_c(q, u)
}

`%~%` <- greta::`distribution<-`

spread_draws <- function(data) {
    pivot_longer(
        data,
        cols = matches("^[^\\.]"),
        names_to = ".var",
        values_to = ".obs") %>%
    mutate(across(c(.var, .chain), as_factor))
}

