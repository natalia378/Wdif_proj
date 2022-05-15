source('funs.R')


# Przykład z wykładu dla modelu dwuokresowego
res <- calculate_call_cost(20, 21, 1 / 2, 3 /12, 1.1, 0.9, 0.12)
# V_0
res[[1]]
res <- calculate_put_cost(20, 21, 1 / 2, 3 /12, 1.1, 0.9, 0.12)



# Tests
K <- 48
S0 <- 50
delta_t <- 1 / 12
T <- 2
sigma <- 0.3
u <- exp(sigma * sqrt(delta_t))
d <- exp(-sigma * sqrt(delta_t))
r <- 0.02

res1 <- calculate_call_cost(S0 = S0, K = K, T = T, delta_t = delta_t,
                            u = u, d = d, r = r)
res2 <- calculate_put_cost(S0 = S0, K = K, T = T, delta_t = delta_t,
                            u = u, d = d, r = r)
res1[[1]] + K * exp(-r * T)
res2[[1]] + S0


K * exp(-r * T) - S0

