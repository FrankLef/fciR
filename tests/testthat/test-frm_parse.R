test_that("frm_parse: Validate the formula input.", {
  expect_error(frm_parse(""), class = "frm_parse_error1")
  expect_error(frm_parse(0), class = "frm_parse_error1")
  expect_error(frm_parse("y"), class = "frm_parse_error1")
  expect_error(frm_parse("y ~"), class = "frm_parse_error1")
  expect_error(frm_parse("y + x"), class = "frm_parse_error1")
  expect_error(frm_parse("y ~ x + z | cond(z= 1"), class = "frm_parse_error1")
})

test_that("frm_parse_check: Validate formula as a whole", {
  frm <- formula("y ~ x | y ~ z")
  expect_error(frm_parse_check(frm), class = "frm_parse_check_error1")
  frm <- formula("y ~ y")
  expect_error(frm_parse_check(frm), class = "frm_parse_check_error2")
})

test_that("frm_parse_rgx: very the formula pattern with regex.", {

  frm <- formula("~ x")
  expect_error(frm_parse_rgx(frm), class = "frm_parse_rgx_error1")

  frm <- formula("y ~ x + z | cond(z = 1) | bad(x =0)")
  expect_error(frm_parse_rgx(frm), class = "frm_parse_rgx_error2")
})


test_that("frm_parse: Validate output", {

  frm <- Y ~ T + A + A*H
  out <- frm_parse(frm)
  expect_s3_class(out, class = "Formula")
  expect_identical(length(out), c(1L, 1L))

  frm <- Y ~ T + A + A*H | cond(T = 1)
  out <- frm_parse(frm)
  expect_identical(length(out), c(1L, 2L))

  frm <- Y ~ T + A + A*H | cond(T = 1) | do(A=1)
  out <- frm_parse(frm)
  expect_identical(length(out), c(1L, 3L))
})
