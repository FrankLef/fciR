test_that("parse_frm: Validate the formula input.", {
  expect_error(parse_frm(""), class = "parse_frm_error1")
  expect_error(parse_frm(0), class = "parse_frm_error1")
  expect_error(parse_frm("y"), class = "parse_frm_error1")
  expect_error(parse_frm("y ~"), class = "parse_frm_error1")
  expect_error(parse_frm("y + x"), class = "parse_frm_error1")
  expect_error(parse_frm("y ~ x + z | cond(z= 1"), class = "parse_frm_error1")
})

test_that("parse_frm_check: Validate formula as a whole", {
  frm <- formula("y ~ x | y ~ z")
  expect_error(parse_frm_check(frm), class = "parse_frm_check_error1")
  frm <- formula("y ~ y")
  expect_error(parse_frm_check(frm), class = "parse_frm_check_error2")
})

test_that("parse_frm_rgx: very the formula pattern with regex.", {

  frm <- formula("~ x")
  expect_error(parse_frm_rgx(frm), class = "parse_frm_rgx_error1")

  frm <- formula("y ~ x + z | cond(z = 1) | bad(x =0)")
  expect_error(parse_frm_rgx(frm), class = "parse_frm_rgx_error2")
})


test_that("parse_frm: Validate output", {
  skip("manual skip")

  frm <- Y ~ T + A + A*H
  out <- parse_frm_check(frm)
  skip("manual skip")
  expect_type(out, type = "list")
  expect_identical(names(out), c("basic", "extras"))
  expect_length(out[[2]], 0)


  frm <- Y ~ T + A + A*H | cond(T = 1)
  out <- parse_frm_check(frm)
  expect_length(out[[2]], 1)

  frm <- Y ~ T + A + A*H | cond(T = 1) | do(A=1)
  out <- parse_frm_check(frm)
  expect_length(out[[2]], 2)
})
