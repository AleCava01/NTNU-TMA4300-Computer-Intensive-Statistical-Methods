# Exam August 2024 - Problem 1b

The problem asks to find the variance of the Monte Carlo estimate based on the initial $n=1000$ realizations plus the additional $n=1000$ antithetic realizations. Therefore, considering the total estimator over the $2n$ samples:

$$
\begin{equation}
\hat \mu_{anti} = \frac 1n \sum_{i=1}^n \Big (\frac{X_i+X_i^*}{2}\Big)
\end{equation}
$$
Variance:
$$
\begin{align}
\mathrm{Var}(\hat \mu_{anti}) &= \mathrm{Var}\Bigg(\frac 1n \sum_{i=1}^n \Big (\frac{X_i+X_i^*}{2}\Big)\Bigg) \\ &= \mathrm{Var}\Bigg(\frac 1{2n} \sum_{i=1}^n \Big (X_i+X_i^*\Big)\Bigg) \\ 
&= \frac 1{4n^2}\mathrm{Var}\Bigg ( \sum_{i=1}^n \Big (X_i+X_i^*\Big)\Bigg) \\
\end{align}
$$
- Since pairs $(X_i,X_i^*)$ is independent of other pairs and identically distributed,
$\sum_{i=1}^n\mathrm{Var}(X_i+X_i^*) = n\mathrm{Var}(X_i+X_i^*)$
$$
\begin{align}
\mathrm{Var}(\hat \mu_{anti})
&= \frac 1{4n}\mathrm{Var}(X_i+X_i^*)\\
\end{align}
$$
- Expanding the variance, $\mathrm{Var}(X_i+X_i^*)=\mathrm{Var}(X_i)+\mathrm{Var}(X_i^*)+2\mathrm{Cov}(X_i,X_i^*)$ 
- Since $X_i$ and $X_i^*$ are marginally identical, $\mathrm{Var}(X_i) = \mathrm{Var}(X_i^*)=\mathrm{Var}(X)$
$$
\begin{align}
\mathrm{Var}(\hat \mu_{anti})&=\frac 1{4n}\Big [2\mathrm{Var}(X)+2\mathrm{Cov}(X_i,X_i^*)\Big] \\
&= \frac 1{2n}\Big [\mathrm{Var}(X)+\mathrm{Cov}(X_i,X_i^*)\Big] \\
&= \frac 1{2n}\Big [\mathrm{Var}(X)+\mathrm{Var}(X)\rho(X_i,X_i^*)\Big]\\
&= \boxed{\frac 1{2n}\mathrm{Var}(X)\Big (1+\rho(X_i,X_i^*)\Big)}
\end{align}
$$

That differs from the result found in the solution pdf by a factor of $\frac 12$.

---

Using:
- $n=1000$
- $\mathrm{Var}(X)=\frac 1{18}$
- $\rho(X_i,X_i^*)=-0.9314165$

The result should be:
$$
\mathrm{Var}(\hat \mu_{anti})\simeq 1.905097 \times 10^{-6}
$$

---