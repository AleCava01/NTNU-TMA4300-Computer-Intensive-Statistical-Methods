set.seed(67)

# Gibbs sampler for f_{U,Z}
gibbs_sampler <- function(n_samples) {

  # storage for samples
  U <- numeric(n_samples)
  Z <- numeric(n_samples)
  
  # initial values
  U[1] <- 0.5 # must be in (0,1)
  Z[1] <- 0.0
  
  for (t in 2:(n_samples)) {
    # sample U | Z
    z_prev <- Z[t-1]
    U[t] <- runif(1, min = 0, max = exp(-z_prev^2/2))
    
    # sample Z | U
    u_curr <- U[t]
    Z[t] <- runif(1, min = -sqrt(-2*log(u_curr)), max = sqrt(-2*log(u_curr)))
  }
  
  list(U = U, Z = Z)
}

# Generate 10000 samples
samples <- gibbs_sampler(n_samples = 10000)

# Plot
plot(samples$Z, samples$U,
     pch = 16,
     cex = 0.4,
     col = rgb(0.1, 0.4, 0.8, 0.3),
     xlab = "Z",
     ylab = "U",
     main = expression("Gibbs samples from" ~ f[U*","*Z]),
     las = 1)  # axis labels horizontal
grid(col = "gray80")
box()