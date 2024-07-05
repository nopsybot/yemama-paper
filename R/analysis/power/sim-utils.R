do_sim_metaregcat <- function(
    k=40, g = 4, b0= 0.6, b1 = 0.2, tau2r=0.02, n=60, nsim=1000,
    alpha = 0.05, summary = TRUE
){
  # preallocate for computation speed
  b1_vec = rep(0,g) %>% purrr::modify_at(g,\(x) b1)
  QMp <- vector(mode = "numeric", length = nsim)
  p <- vector(mode = "numeric", length = nsim)
  progr <- progressr::progressor(along = nsim)
  # start the simulation loop
  for(i in 1:nsim){
    study_dat <- data.table(
      b0 = rep(b0,k),
      deltai = rnorm(k, 0, sqrt(tau2r)),
      pred0 = letters[1:g] %>% rep(each = k/g),
      b1 = rep(b1_vec,each=(k/g)),
      n=rep(n,k),
      vi = rep(0.001,k)
    ) %>% 
      dplyr::mutate(yi = b0 + b1 + deltai)
    
    res <- rma(yi, vi, mods = ~pred0, method = "REML", data = study_dat)
    
    QMp[i] <- res$QMp # store the p value
    p[i] <- res$pval[length(res$pval)]
    progr(message = sprintf("Run %4g of %g",i,nsim))
  }
  if(summary){
    # return directly the power
    summary_sim(p, alpha)
  }else{
    # return the list of pvalues
    data.frame(QMp,p)
  }
}

do_sim_metareg <- function(k=40, b0=0.75, b1=0.01, x_m=13, x_sd=9, tau2r=0.02, n=60, nsim=1000, alpha = 0.05, summary = TRUE){
  # preallocate for computation speed
  p <- vector(mode = "numeric", length = nsim)
  progr <- progressr::progressor(along = nsim)
  # start the simulation loop
  for(i in 1:nsim){
    
    x1 <- rnorm(k, x_m, x_sd) # random mean-age for each study
    
    study_dat <- data.table(
      b0 = b0,
      deltai = rnorm(k, 0, sqrt(tau2r)),
      pred0 = x1 - mean(x1), # centering the predictor,
      b1 = b1,
      n=n,
      vi = rnorm(k,0.004,0.0006)
    ) %>% 
      dplyr::mutate(yi = b0 + b1*pred0 + deltai)
    
    res <- rma(yi, vi, mods = ~pred0, method = "REML", data = study_dat)
    
    p[i] <- res$pval[2] # store the p value
    
    progr(message = sprintf("Run %4g of %g",i,nsim))
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
