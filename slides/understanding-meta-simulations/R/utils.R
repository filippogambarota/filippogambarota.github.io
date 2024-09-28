# Functions from the main paper -------------------------------------------

#' sim_study
#' @description Simulate a single study based on the input parameters and return a dataframe with effect size and variance or raw data.
#' @param theta number indicating the population level effect size.
#' @param nt number indicating the number of participants for the treated group.
#' @param nc number indicating the number of participants for the control group. If \code{NULL} the value assigned to \code{nt} will be used.
#' @param hedges logical. Whether the Hedges' correction should be applied. Default to \code{TRUE}.
#' @param aggregate logical. Whether the participants data need to be aggregated calculating the effect size and variance. Default to \code{TRUE}.
#'
#' @return a dataframe
#' @import metafor
#' @export
#' @examples
#' sim_study(0.3, 30, 30, TRUE, TRUE) # return aggregated data
#' sim_study(0.3, 30, 30, TRUE, TRUE) # return participants data

sim_study <- function(theta, nt, nc = NULL, hedges = TRUE, aggregate = TRUE){
    if(is.null(nc)) nc <- nt
    # generate from normal distribution
    yc <- rnorm(nc, 0, 1)
    yt <- rnorm(nt, theta, 1)
    # pooled variance
    sp <- sqrt((var(yc)*(nc - 1) + var(yt)*(nt - 1)) / (nc + nt - 2))
    # effect size
    yi <- (mean(yt) - mean(yc)) / sp
    
    # transform to hedges' g
    if(hedges){
        yi <- yi * metafor:::.cmicalc(nc + nt - 2)
    }
    
    # sampling variance
    vi <- (nc + nt)/(nc * nt) + yi^2/(2 * (nc + nt - 2))
    
    if(!aggregate){
        # return raw data
        data.frame(id = 1:(nc + nt),
                   group = rep(c("c", "t"), c(nc, nt)),
                   y = c(yc, yt))
    }else{
        # compute effect size
        data.frame(yi, vi)
    }
}

#' sim_studies
#' @description apply the \code{sim_study} function using a grid of parameters.
#' @param ... arguments passed to \code{sim_study()}
#' @param data a dataframe where the result will be attached (optional). Default to \code{NULL}
#'
#' @return a dataframe
#' @importFrom dplyr bind_rows
#' @export
#'
#' @examples
#' sim <- data.frame(k = 1:10, nt = 30, nc = 30)
#' sim_studies(sim$k, sim$nt, sim$nc, data = sim)
#' sim_stud
sim_studies <- function(..., data = NULL){
    # ... (dots) are the arguments passed to mapply, with the order required by sim_study
    res <- mapply(sim_study, ..., SIMPLIFY = FALSE)
    # everything to a dataframe
    res <- dplyr::bind_rows(res)
    
    if(!is.null(data)){
        # attach to the original dataset
        cbind(data, res)
    }else{
        res
    }
}

#' sim_study_m
#' @description Simulate a single study with multiple outcomes. The function simulate a compound symmetry structure where the correlation between sampling errors is a single value and the sampling variance is fixed to 1.
#' @param thetas a numeric vector with population level effect sizes.
#' @param nt number indicating the number of participants for the treated group.
#' @param nc number indicating the number of participants for the control group. If \code{NULL} the value assigned to \code{nt} will be used.
#' @param r number indicating the correlation between sampling errors.
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' sim_study_m(c(0.1, 0.5, 1), 30, 30, 0.7)
sim_study_m <- function(thetas, nt, nc, r){
    
    p <- length(thetas)
    
    # variance covariance matrix with sigma2 = 1
    Vcm <- r + diag(1 - r, nrow = p)
    
    yc <- MASS::mvrnorm(nt[1], rep(0, p), Vcm)
    yt <- MASS::mvrnorm(nc[1], thetas, Vcm)
    
    ytm <- apply(yt, 2, mean)
    ycm <- apply(yc, 2, mean)
    
    ytv <- apply(yt, 2, var)
    ycv <- apply(yc, 2, var)
    
    sp <- sqrt((ytv*(nt - 1) + ycv*(nc - 1)) / (nt + nc - 2))
    yi <- (ytm - ycm) / sp
    vi <- (nt + nc)/(nt * nc) + yi^2/(2 * (nt + nc - 2))
    
    data.frame(yi = yi, vi = vi)
}

#' sim_T
#' @description Simulate multivariate random-effects assuming a compound symmetry or heteroscedastic compound symmetry structure.
#' @param k number of studies
#' @param p number of outcomes
#' @param taus2 vector of tau2 values
#' @param rho correlation between population level outcomes
#'
#' @return a numeric vector
#' @importFrom MASS mvrnorm
#' @export
#'
#' @examples
#' sim_T(10, 3, c(0.1, 0.1, 0.1), 0.7) # compound symmetry
#' sim_T(10, 3, c(0.1, 0.05, 0.2), 0.7) # heteroscedastic compound symmetry
#' 
sim_T <- function(k, p, taus2, rho){
    rhoM <- rho + diag(1 - rho, nrow = p)
    tau2M <- diag(sqrt(taus2)) %*% rhoM %*% diag(sqrt(taus2))
    c(t(MASS::mvrnorm(k, rep(0, p), tau2M)))
}

#' do_sim
#' @description repeat a simulation a certain number of times. The sample size is sampled from a poisson distribution shifted using \code{nmin}.
#' @param k number of studies
#' @param theta population level effect size
#' @param tau2 heterogeneity
#' @param navg average sample size
#' @param nmin minimum sample size
#' @param nsim number of simulations
#' @param alpha the alpha level
#' @param summary logical indicating if the simulation should be summarized using the \code{summary_sim} function.
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' do_sim(10, 0.3, 0.1, 30, 10, 100)
do_sim <- function(k, theta, tau2, navg, nmin, nsim, alpha = 0.05, summary = TRUE){
    # preallocate for computation speed
    p <- vector(mode = "numeric", length = nsim)
    
    # start the simulation loop
    for(i in 1:nsim){
        theta_i <- rnorm(k, 0, sqrt(tau2))
        theta_theta_i <- theta + theta_i
        # simulate sample size
        n <- nmin + rpois(k, navg - nmin)
        # simulate the studies
        sim <- sim_studies(theta_theta_i, n, n, data = NULL)
        res <- rma(yi, vi, method = "REML", data = sim)
        p[i] <- res$pval # store the p value
    }
    if(summary){
        # return directly the power
        summary_sim(p, alpha)
    }else{
        # return the list of pvalues
        data.frame(p)
    }
}

#' summary_sim
#' @description Summarize the result of \code{do_sim()}
#' @param p the vector of p values
#' @param alpha the alpha level
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' p <- runif(1000, 0, 1) # random p-values vector
#' summary_sim(p, 0.05)
summary_sim <- function(p, alpha){
    power <- mean(p <= alpha) # compute power
    data.frame(power)
}

#' make_data
#' @description a wrapper of \code{data.frame()} to create the grid of simulation values
#' @param k the number of studies
#' @param nt number indicating the number of participants for the treated group.
#' @param nc number indicating the number of participants for the control group. If \code{NULL} the value assigned to \code{nt} will be used.
#' @param ... other named vectors that will be attached to the dataframe
#'
#' @return a dataframe
#' @export
#'
#' @examples
#' make_data(10, 30, 30, x = runif(10)) # k, nt, nc and a column x
make_data <- function(k, nt = NULL, nc = NULL, ...){
    params <- c(as.list(environment()), list(...))
    cols <- params[!sapply(params, is.null) & names(params) != "k"]
    dat <- data.frame(
        id = 1:params$k
    )
    if(length(cols) != 0){
        dat <- cbind(dat, cols)
    }
    return(dat)
}
