# use table 3.2 on p. 49 as an example
df_exp <- data.frame(
  "term" = c("EY0", "EY1", "RD", "RR", "RR*", "OR"),
  ".lower" = c(0.2103, 0.24095 , 0.001, 1.005, 1.001, 1.006),
  ".estimate" = c(0.23338, 0.27117, 0.038, 1.16, 1.05, 1.22),
  ".upper" = c(0.25647, 0.30139, 0.0748, 1.34, 1.11, 1.49),
  ".alpha" = 0.05,
  ".method" = "norm"
  )
df_log <- df_exp
df_log$term <- c("EY0", "EY1", "RD", "logRR", "logRR*", "logOR")
ids <- df_log$term %in% c("logRR", "logRR*", "logOR")
df_log$.lower[ids] <- log(df_log$.lower[ids])
df_log$.estimate[ids] <- log(df_log$.estimate[ids])
df_log$.upper[ids] <- log(df_log$.upper[ids])

test_that("effect_transf: log", {
  out <- effect_transf(df_log, transf = "exp")
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- df_exp

  expect_identical(out, target)
})


df_logit <- data.frame(
    "term" = c("logitP"),
    ".lower" = c(0.3914539),
    ".estimate" = c(0.6059581),
    ".upper" = c(0.7689991),
    ".alpha" = 0.05,
    ".method" = "norm"
  )

df_expit <- data.frame(
  "term" = c("P"),
  ".lower" = plogis(c(0.3914539)),
  ".estimate" = plogis(c(0.6059581)),
  ".upper" = plogis(c(0.7689991)),
  ".alpha" = 0.05,
  ".method" = "norm"
  )

test_that("effect_transf: logit", {
  out <- effect_transf(df_logit, transf = "expit")
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- df_expit

  expect_identical(out, target)
})

test_that("effect_transf: identity", {
  out <- effect_transf(df_logit)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- df_logit

  expect_identical(out, target)
})
