sim_chap08_1 <- function(n = 1e4, seed = 555) {
  set.seed(seed)
  # Generate the potential outcome Y(.,0) and Y(.,1)
  Ydot0 <- rbinom(n, size = 1, prob = 0.05)
  Ydot1 <- rbinom(n, size = 1, prob = 0.2)
  # let A depend on Y(.,1)
  probA <- (1 - Ydot1) * 0.1 + Ydot1 * 0.8
  A <- rbinom(n, size = 1, prob = probA)
  # Generate the potential outcome S(0) and S(1)
  S0 <- rbinom(n, size = 1, prob = 0.05)
  S1 <- rbinom(n, size = 1, prob = 0.9)
  # S is a function of S0, S1 and A
  S <- (1 - A) * S0 + A * S1
  # Y is a function of Y(., 0) and Y(., 1) and S
  Y <- (1 - S) * Ydot0 + S * Ydot1
  data.frame(cbind(A, S, Y, Ydot0, Ydot1))
}
