library(Formula)

bad <- list("f00" = "", # bad formula
            "f01" = "wrong", # bad formula
            "f02" = "x + y",  # bad formula
            "f03" = "y ~",  # bad formula
            "f04" = "y ~ x + z | cond(z= 1",  # bad formula,
            "f05" = "y ~ x + z / cond(z= 1)", 
            "f06" = "~ x",
            "f07" = "y ~ x | y ~ z",  # bad: more than 1 tildes
            "f08" = "x ~ x", # bad: same thing on both sides
            "f09" = "y ~ x", 
            "f10" = "y ~ x + z | cond(z= 1)",
            "f11" = "y ~ x + z | cond(z = 1) | do(x =0)",
            "f12" = "y ~ x + z | cond(z = 1) | bad(x =0)")
out <- sapply(bad, FUN = function(x) inherits(try(formula(x), silent = TRUE), 
                                              what = "formula"))
out

f <- sapply(bad[out], function(x) formula(x))

# bad: more than 1 tildes
s <- deparse(f[["f07"]])
lengths(regmatches(s, gregexpr("~", s)))


# bad both sides are the same thing
rlang::f_lhs(f[["f08"]]) == rlang::f_rhs(f[["f08"]])

frm <- f[["f06"]]
frm_text <- deparse(frm)
frm_text <- gsub(" ", replacement = "", x = frm_text)
frm_text

the_frm <- strsplit(frm_text, split = "[|]")
the_frm <- unlist(the_frm)  # convert list to vector
frm_n <- length(the_frm)
frm_n

frm_basic <- the_frm[1]
patrn <- "[A-Za-z]+~.+"
check <- grep(pattern = patrn, x = frm_basic)
check
length(check) == 0

frm_extras <- the_frm[2:frm_n]
patrn <- "cond[(][A-Za-z]+=[0-1][)]|do[(].+=[0-1][)]"
grep(pattern = patrn, x = frm_extras)
