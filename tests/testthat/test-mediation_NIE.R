test_that("mediation_NIE", {
  data("gss", package = "fciR")

  gssmed <- gss |>
    dplyr::select(c("trump", "gthsedu", "magthsedu", "white", "female", "gt65",
                    "conservative")) |>
    tidyr::drop_na()
  stopifnot(nrow(gssmed) == 2084)

  out <- mediation_NIE(gssmed,
                       formula = trump ~ magthsedu + conservative,
                       exposure.name = "magthsedu",
                       mediator.name = "conservative") |>
    mutate(across(.cols = where(is.numeric), .fns = round, digits = 6))

  target <- data.frame(
    term = c("NIE_prod", "NIE_diff"),
    estimate = c(-0.02238265, -0.02238265),
    std.err = NA_real_) |>
    mutate(across(.cols = where(is.numeric), .fns = round, digits = 6))

  expect_identical(out, target)
})
