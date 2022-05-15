source('funs.R')


# Przykład z wykładu dla modelu dwuokresowego
res <- calculate_call_cost(20, 21, 1 / 2, 3 /12, 1.1, 0.9, 0.12)
# V_0
res[[1]]




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

res1[[1]]
time_vec <- seq(from = 0 , to = T, by = delta_t)

time_vec
price_tree <- generate_price_tree(S0, u, d, n-1)

 S0*u ^ 24
n <- length(time_vec)
price_tree
costs <- vector(mode = "list", length = n)
p <- (price_tree[[n-2]][2] * exp(r * delta_t) - price_tree[[n-1]][3]) /
  (price_tree[[n-1]][2] - price_tree[[n-1]][3])
p
costs[[n]] <- calc_payoff_call(K, price_tree[[n]])


i <- n-1
for(i in rev(2:n)){
  costs_matrix <- matrix(c(costs[[i]][1],
                           rep(costs[[i]][-c(1, i)], ifelse(i == 2, 0, 2)),
                           costs[[i]][length(costs[[i]])]), ncol = i - 1)
  p_matrix <- matrix(rep(c(p, 1-p), i - 1), ncol = i - 1)
  costs[[i-1]] <- exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum)
}
p_matrix
S0 - K * exp(-r * T)

