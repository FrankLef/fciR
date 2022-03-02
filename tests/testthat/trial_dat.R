patrn_func <- "(cond|do)"
patrn_val <- "[(][[:alnum:]]+=[[:digit:]]+[)]"
patrn <- paste0(patrn_func, patrn_val)
frm_txt <- c("cond(x=1)", "do(h=0)", "bad(t=1)", "cond(x=)")
grep(pattern = paste0(patrn_func, patrn_val), x = frm_txt)


check <- grepl(patrn, frm_txt[4])


m <- regexpr(patrn, frm_txt[1])
t <- regmatches(frm_txt[1], m = m)
t

m <- regexpr(patrn_val, frm_txt[1])
t <- regmatches(frm_txt[1], m = m)
t
t <- gsub("[()]", replacement = "", x = t)
t
t <- strsplit(t, split = "=")
