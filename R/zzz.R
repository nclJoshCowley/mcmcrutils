.onLoad <- function(...) {
  s3_register("loo::waic", "mcmcarray")
  s3_register("loo::loo", "mcmcarray")
}
