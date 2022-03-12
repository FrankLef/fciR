test_that("backdr_out_np", {
  data(whatifdat)
  out <- backdr_out_np(whatifdat, outcome = Y, exposure = `T`, confound = H,
                       R = 500)
  # print(out)

  target <- data.frame(
    name = c("EY0", "EY1", "RD", "RR", "RR*", "OR"),
    est = c(0.342405, 0.303788, -0.038617, 0.887218, 0.944533, 0.838007)
  )

  ids <- match(target$name, out$name, nomatch = 0L)
  check <- sum(abs(out$est[ids] - target$est))
  expect_lt(check, 0.01)
})

test_that("backdr_out_npr", {
 data(whatifdat)
 out <- backdr_out_npr(whatifdat, formula = Y ~ A + H + A:H)

 data(fci_tbl_06_01)
 target <- fci_tbl_06_01
 ids <- match(target$name, out$name, nomatch = 0L)

 check <- sum(abs(out$est[ids] - target$est))
 expect_lt(check, 0.01)
})


test_that("backdr_out_npr: With ATT", {
 data(whatifdat)
 out <- backdr_out_npr(whatifdat, formula = Y ~ A + H + A:H, att = TRUE)

 data(fci_tbl_06_04)
 target <- fci_tbl_06_04
 ids <- match(target$name, out$name, nomatch = 0L)

 check <- sum(abs(out$est[ids] - target$est))
 expect_lt(check, 0.01)
})
