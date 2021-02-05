dtrunctest <- function(x , spec, a = -Inf, b = Inf, ...)
{
  tt <- rep(0, length(x))
  g <- get(paste("d", spec, sep = ""), mode = "function")
  G <- get(paste("p", spec, sep = ""), mode = "function")
  tt[x >= a & x < b] <- g(x[x >= a & x < b], ...) / (G(b, ...) - G(a, ...))
  return(tt)
}

ptrunc <- function(x, spec, a = -Inf, b = Inf, ...) {
  tt <- x
  aa <- rep(a, length(x))
  bb <- rep(b, length(x))
  G <- get(paste("p", spec, sep = ""), mode = "function")
  tt <- G(apply(cbind(apply(cbind(x, bb), 1, min), aa), 1, max), ...)
  tt <- tt - G(aa, ...)
  tt <- tt/(G(bb, ...) - G(aa, ...))
  return(tt)
}
