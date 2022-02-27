test_that("ggp_dag", {
  the_nodes <- c("U" = "Unmeasured, healthy behavior (U=1)",
                 "AD0" = "Adherence time 0",
                 "VL0" = "Viral Load time 0",
                 "T" = "Naltrexone (T=1)",
                 "A" = "Reduced drinking (A=1)",
                 "AD1" = "Adherence time 1",
                 "VL1" = "Viral Load time 1")
  coords <- data.frame(
    name = names(the_nodes),
    x = c(2, 3, 4, 1, 2, 3, 4),
    y = c(2, 2, 2, 1, 1, 1, 1)
  )
  dag <- ggdag::dagify(
    AD0 ~ U,
    VL0 ~ AD0,
    A ~ `T` + U,
    AD1 ~ A,
    VL1 ~ AD0 + AD1 + U,
    outcome = "VL1",
    exposure = "T",
    latent = "U",
    labels = the_nodes)


  text_labels <- c("A", expression(AD[0]), expression(AD[1]),
                   "T", "U", expression(VL[0]), expression(VL[1]))
  out <- ggp_dag(dag, text_labels = text_labels, text_size = 5)

  expect_s3_class(out, class = "ggplot")
})
