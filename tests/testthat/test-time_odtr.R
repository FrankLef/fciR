test_that("time_odtr_prop", {
  data("cogdat", package = "fciR")
  out <- time_odtr_prop(cogdat, outcome.name = "Y",
                        A1 = "A1", A2 = "A2", H2 = "H2") |>
    mutate(prop = round(prop, 4)) |>
    pull(prop)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- c(0.2927, 0.1875, 0, 0.6667, 0.1071, 0.4211, 0.5, 0.2857)
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})


test_that("time_odtr_optA2", {
  data("cogdat", package = "fciR")

  df <- time_odtr_prop(cogdat, outcome.name = "Y",
                        A1 = "A1", A2 = "A2", H2 = "H2")

  out <- time_odtr_optA2(df, A1 = "A1", A2 = "A2", H2 = "H2") |>
    mutate(propA2opt = round(propA2opt, 4)) |>
    pull(propA2opt)
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- c(0.2927, 0.6667, 0.2927, 0.6667, 0.5, 0.4211, 0.5, 0.4211)
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})

test_that("time_odtr_optA1A2", {
  data("cogdat", package = "fciR")

  df <- time_odtr_prop(cogdat, outcome.name = "Y",
                       A1 = "A1", A2 = "A2", H2 = "H2")

  df <- time_odtr_optA2(df, A1 = "A1", A2 = "A2", H2 = "H2")

  out <- time_odtr_optA1A2(df, A1 = "A1", A2 = "A2", H2 = "H2") |>
    dplyr::ungroup() |>
    dplyr::mutate(propA1opt = round(propA1opt, 4)) |>
    dplyr::select(A1opt, propA1opt) |>
    as.data.frame()
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  target <- data.frame(
    A1opt = rep(1, nrow(df)),
    propA1opt = rep(0.4633, nrow(df)))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)
})


test_that("time_odtr_optimal", {
  data("cogdat", package = "fciR")

  df <- time_odtr_prop(cogdat, outcome.name = "Y",
                       A1 = "A1", A2 = "A2", H2 = "H2")

  df <- time_odtr_optA2(df, A1 = "A1", A2 = "A2", H2 = "H2")

  df <- time_odtr_optA1A2(df, A1 = "A1", A2 = "A2", H2 = "H2")

  out <- time_odtr_optimal(df, A1 = "A1", A2 = "A2", H2 = "H2") |>
    mutate(across(.cols = where(is.numeric), .fns = round, digits = 6))
  # cat("\n", "out", "\n")
  # print(out)
  # cat("\n")

  the_results <- c(
    "A1opt" = 1,
    "propA1opt" = 0.4633459,
    "A2optH20" = 1,
    "propA2optH20" = 0.5,
    "A2optH21" = 0,
    "propA2optH21" = 0.4210526)
  target <- data.frame(
    term = names(the_results),
    estimate = unname(the_results),
    std.err = NA_real_) |>
    mutate(across(.cols = where(is.numeric), .fns = round, digits = 6))
  # cat("\n", "target", "\n")
  # print(target)
  # cat("\n")

  expect_identical(out, target)


  # the genral function should give the same results
  out_all <- time_odtr(cogdat, outcome.name = "Y",
                       A1 = "A1", A2 = "A2", H2 = "H2")
  expect_identical(out, target)

})
