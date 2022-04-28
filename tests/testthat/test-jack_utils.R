test_that("jack_ci", {
  df <- mtcars
  conf <- 0.95

  mtcars.jack <- sapply(seq_along(df$mpg), FUN = function(i) {
    x <- df$mpg[-i]
    mean(x)
  })
  mtcars.out <- jack_ci(mtcars.jack)

  est <- mean(df$mpg)
  se <- sd(df$mpg) / sqrt(nrow(df))
  ci <- stats::qt(conf, df = nrow(df) - 1)

  target <- c("est" = mean(df$mpg),
              "se" = sd(df$mpg) / sqrt(nrow(df)),
              "conf" = conf,
              "lci" = est - ci * se,
              "uci" = est + ci * se
              )

  check <- sum(abs(mtcars.out - target))
  expect_lt(check, .Machine$double.eps^ 0.5)
})

test_that("jack_run", {
  ids <- c("E(Y-Y(0)|A=1)",
           "log(E(Y|A=1))-log(E(Y(0)|A=1))",
           "logit(E(Y|A=1))-logit(E(Y(0)|A=1))")

  is_skip <- TRUE
  if (!is_skip) {
    data(doublewhatifdat)
    out <- jack_run(doublewhatifdat, func = fciR::instr_linear, conf = 0.95,
                    outcome.name = "VL1", exposure.name = "A",
                    instrument.name = "T")
    out <- out[c(3, 4, 6), "est"]
    # cat("\n")
    # print(out)
    # cat("\n")


    data(fci_tbl_09_01a)
    target <- fci_tbl_09_01a
    target <- target[target$method == "Linear", ]
    target <- target$est[target$name %in% ids]
    # cat("\n")
    # print(target)
    # cat("\n")

    check <- sum(abs(out - target))
  }
  skip_if(is_skip, message = "Skip to save time.")
  expect_lt(check, 1e-4)
})
