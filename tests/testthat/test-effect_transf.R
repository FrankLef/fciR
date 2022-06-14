test_that("effect_transf: invalid input", {
  data(fci_tbl_03_02)

  expect_error(effect_transf(fci_tbl_03_02, transf = "wrong"))
})


test_that("effect_transf: log", {
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
  sel <- df_log$term %in% c("logRR", "logRR*", "logOR")
  df_log$.lower[sel] <- log(df_log$.lower[sel])
  df_log$.estimate[sel] <- log(df_log$.estimate[sel])
  df_log$.upper[sel] <- log(df_log$.upper[sel])


  out <- effect_transf(df_log, transf = "exp")
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- df_exp

  expect_identical(out, target)
})




test_that("effect_transf: logit", {
  df_logit <- data.frame(
    "term" = c("logitP"),
    ".lower" = c(0.3914539),
    ".estimate" = c(0.6059581),
    ".upper" = c(0.7689991),
    ".alpha" = 0.05,
    ".method" = "norm"
  )

  df_expit <- df_logit
  df_expit$term[1] <- "P"
  df_expit$.lower[1] <- plogis(df_logit$.lower[1])
  df_expit$.estimate[1] <- plogis(df_logit$.estimate[1])
  df_expit$.upper[1] <- plogis(df_logit$.upper[1])


  out <- effect_transf(df_logit, transf = "expit")
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- df_expit

  expect_identical(out, target)
})

test_that("effect_transf: identity", {
  data(fci_tbl_03_02)

  out <- effect_transf(fci_tbl_03_02)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  expect_identical(out, fci_tbl_03_02)
})
