#' RECOVERY trial of dexamethasone COVID-10 Collaborative Group
#'
#' RECOVERY trial of dexamethasone COVID-10 Collaborative Group.
#'
#' See beginning of chapter 4.
#'
#' @return Dataframe
data_recovery <- function() {
  out <- expand.grid(Y = 0:1, `T` = 0:1, M = 0:1)
  out$n <- as.integer(c(787, 2851, 368, 1412, 278, 405, 86, 238))
  out <- lapply(X = seq_len(nrow(out)), FUN = function(i) {
    data.frame(
      M = rep(out$M[i], out$n[i]),
      `T` = rep(out$`T`[i], out$n[i]),
      Y = rep(out$Y[i], out$n[i])
    )
  })
  out <- do.call(rbind, out)
  # create an id variable for use with gee() later
  out$id <- seq_len(nrow(out))
  out
}

# recovery <- data_recovery()
# usethis::use_data(recovery, overwrite = TRUE)
