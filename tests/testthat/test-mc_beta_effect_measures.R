test_that("mc_beta_effect_measures", {
  # don't use skip() to avoid MonteCarlo output to screen
  is_skip <- TRUE
  if (!is_skip) {
    out <- suppressMessages(mc_beta_effect_measures(shape1_prms = 1, shape2_prms = 1,
                                                    nrep = 100, constrained = FALSE))
    expect_type(out, type = "list")

    target <- c("RD_RR" = 0,
                "RD_RRstar" = 0,
                "RD_OR" = 0,
                "RR_RRstar" = 0,
                "RR_OR" = 0,
                "RRstar_OR" = 0,
                "RD_RR_vs_RRstar_OR" = 0.0486166,
                "RD_RRstar_vs_RR_OR" = 0.0541502,
                "RD_OR_vs_RR_RRstar" = 0,
                "RD_RR_RRstar" = 0,
                "RD_RR_OR" = 0.1177866,
                "RD_RRstar_OR" = 0.1158103,
                "RR_RRstar_OR" = 0,
                "RD_RR_RRstar_OR" = 0.6636364,
                "NONE" = 0)
  }

  skip_if(is_skip, "Skip MonteCarlo to avoid output to screen.")
  expect_identical(names(out), names(target))
  check <- abs(sum(unlist(target)) - sum(unlist(out)))
  expect_lt(check, 0.0001)
})
