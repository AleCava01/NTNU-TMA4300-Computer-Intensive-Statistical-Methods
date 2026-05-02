
load(url("https://www.math.ntnu.no/emner/TMA4300/2026v/hypoexponential.RData"))

em_hypo <- function(z, lambda_start, tol = 1e-8, max_iter = 1000) {
  n <- length(z)
  lambda <- lambda_start
  
  for (t in 1:max_iter) {
    l1 <- lambda[1]
    l2 <- lambda[2]
    delta <- l1 - l2
    
    if (abs(delta) < 1e-10) {
      u <- z / 2
    } else {
      u <- (1 / delta) - (z / (exp(delta * z) - 1))
    }
    
    l1_new <- n / sum(u)
    l2_new <- n / sum(z - u)
    
    lambda_new <- c(l1_new, l2_new)
    
    if (sum(abs(lambda_new - lambda)) < tol) {
      return(lambda_new)
    }
    
    lambda <- lambda_new
  }
  return(lambda)
}

parametric_bootstrap <- function(z, lambda_mle, B = 500) {
  n <- length(z)
  boot_estimates <- matrix(NA, nrow = B, ncol = 2)
  
  for (b in 1:B) {
    x_star <- rexp(n, rate = lambda_mle[1])
    y_star <- rexp(n, rate = lambda_mle[2])
    z_star <- x_star + y_star
    
    boot_estimates[b, ] <- em_hypo(z_star, lambda_start = lambda_mle)
    
    boot_estimates[b, ] <- sort(boot_estimates[b, ])
  }
  
  se <- apply(boot_estimates, 2, sd)
  return(se)
}

start_vals <- c(0.5, 0.1)
mle_results <- em_hypo(z, start_vals)
mle_results <- sort(mle_results)
se_results <- parametric_bootstrap(z, mle_results, B = 500)

cat("MLE Estimates:\n")
cat("lambda_1:", mle_results[1], "\n")
cat("lambda_2:", mle_results[2], "\n\n")

cat("Standard Errors (Parametric Bootstrap):\n")
cat("SE(lambda_1):", se_results[1], "\n")
cat("SE(lambda_2):", se_results[2], "\n")