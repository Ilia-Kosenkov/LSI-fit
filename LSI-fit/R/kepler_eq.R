#' Kepler's equation solver
#'
#' @param M Mean anomaly (numeric vector)
#' @param e Eccentricity (scalar numeric value within [0, 1])
#' @description Simple Newtonian method applied to the inverse problem.
#' See https://en.wikipedia.org/wiki/Kepler's_equation#Numerical_approximation_of_inverse_problem
#' @return Vector of size \code{vec_size(M)} of eccentric anomalies (E)
solve_E <- function(M, e = 0) {
    # Code greatly simplified to work with {greta}
    E <- M
    counter <- 0L

    while (counter < 5L) {
        E <- E - (E - e * sin(E) - M) / (1 - e * cos(E))
        counter <- counter + 1L
    }
    return(E)
}