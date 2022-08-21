test_that("frontdr_np", {

  data(fci_sim_08_01)

  out <- frontdr_np(fci_sim_08_01, formula = Y ~ A + S, exposure.name = "A",
                    surrogate.name = "S")
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    term = c("EY0", "EY1", "RD", "RR", "RR*", "OR"),
    .estimate = c(0.055847, 0.185367, 0.129520, 3.319195, 1.158992, 3.846921))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out$.estimate - target$.estimate))
  expect_lt(check, 1e-6)
})

test_that("frontdr_np: exercise 8.3", {

  sim8ex3 <- function(n = 1e4, seed = 8383) {
    set.seed(seed)
    X <- rbinom(n, size = 1, prob = 0.5)
    probA <- 0.3 + 0.4 * X
    A <- rbinom(n, size = 1, prob = probA)
    probS <- 0.4 + 0.3 * A
    S <- rbinom(n, size = 1, prob = probS)
    probY <- 0.2 + 0.5 * X * S
    Y <- rbinom(n, size = 1, prob = probY)
    data.frame(cbind(X, A, S, Y))
  }

  df <- sim8ex3()

  out <- frontdr_np(fci_sim_08_01, formula = Y ~ A + S, exposure.name = "A",
                    surrogate.name = "S")
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    term = c("EY0", "EY1", "RD", "RR", "RR*", "OR"),
    .estimate = c(0.29803247, 0.37514842, 0.07711596,
                  1.25875019, 1.12341484, 1.41409864))
  target <- target$est
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  check <- sum(abs(out$.estimate - target$.estimate))
  expect_lt(check, 1e-6)
})

