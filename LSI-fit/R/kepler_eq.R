#' Kepler's equation solver
#'
#' @param M Mean anomaly (numeric vector)
#' @param e Eccentricity (scalar numeric value within [0, 1])
#'
#' @return Vector of size \code{vec_size(M)} of eccentric anomalies (E)
solve_E <- function(M, e = 0) {
    e <- vec_cast(vec_assert(e, size = 1L), double())
    if (e < 0 || e > 1)
        abort(glue_fmt_chr(
            "`e` should be within [0, 1] interval (provided {e})"),
            "lsi_fit_invalid_arg")

    M <- vec_cast(M, double())
    n <- vec_size(M)
    E <- if(e >= 0.8) vec_repeat(pi, n) else M

    threshold <- 2 * .Machine$double.eps

    err <- threshold * 10
    counter <- 0L

    while (err > threshold & counter < 30L) {
        temp <- E - (E - e * sin(E) - M) / (1 - e * cos(E))
        err <- sqrt(sum((E - temp) ^ 2) / n)
        E <- temp
        counter <- counter + 1L
    }
    return(E)
}