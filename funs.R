

calc_payoff_call <- function(K, St){
  pmax(St - K, 0)
}


calc_prices <- function(n, S0, u, d){
  a <- 0:n
  return(S0 * (u ^ (rev(a))) * (d ^ (a)))
}


generate_price_tree <- function(S0, u, d, periods){
  list_tree <- sapply(0:periods, calc_prices, S0 = S0, u = u, d = d)
  return(list_tree)
}


calculate_call_cost <- function(S0, K, T, delta_t, u, d, r){
  
  time_vec <- seq(from = 0 , to = T, by = delta_t)
  n <- length(time_vec)
  price_tree <- generate_price_tree(S0, u, d, n - 1)
  
  costs <- vector(mode = "list", length = n)
  p <- (price_tree[[n-1]][1] * exp(r * delta_t) - price_tree[[n]][2]) /
    (price_tree[[n]][1] - price_tree[[n]][2])
  costs[[n]] <- calc_payoff_call(K, price_tree[[n]])
  
  for(i in rev(2:n)){
    costs_matrix <- matrix(c(costs[[i]][1],
                             rep(costs[[i]][-c(1, i)], ifelse(i == 2, 0, 2)),
                             costs[[i]][i]), ncol = i - 1)
    p_matrix <- matrix(rep(c(p, 1-p), i - 1), ncol = i - 1)
    costs[[i-1]] <- exp(-r * delta_t) * apply(costs_matrix * p_matrix, 2, sum)
  }
  return(costs)
}






