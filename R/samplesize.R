#' Calculates approximate sample size for two-stage Mann-Whitney-Wilcoxon statistic.
#'
#' @param omega Alternative P(Y>X)-.5
#' @param lambda11 Proportion in group 1, first evaluation
#' @param lambda12 Proportion in group 1, second evaluation
#' @param lambda21 Proportion in group 2, first evaluation
#' @param lambda22 Proportion in group 2, second evaluation
#' @param alpha2 Overall test size
#' @param beta Type II error rate.
#' @param round Logical Should sample size be rounded?
#' @return group sizes
#' @importFrom stats qnorm
#' @export
samplesize<- function (omega, lambda11 = 0.25, lambda12 = 0.25, lambda21 = 0.25, lambda22 = 0.25, alpha2 = 0.05, beta = 0.2,round=TRUE) 
{
    if (abs(lambda11 + lambda12 + lambda21 + lambda22 - 1) > 
        1e-10) {
        cat("Why don't lambdas sum to 1?\n")
        out <- NULL
    }
    else {
        N <- (qnorm(1 - alpha2) + qnorm(1 - beta))^2/(12 * (lambda11 + 
            lambda12) * (lambda21 + lambda22) * omega^2)
        out <-N * c(m1 = lambda11, m2 = lambda12, n1 = lambda21, n2 = lambda22)
        if(round) out <- round(out)
#       checkout <- checktest(out["m1"], out["n1"], out["m2"], 
#          out["n2"], qnorm(omega + 0.5) * sqrt(2))
    }
    return(out)
}
