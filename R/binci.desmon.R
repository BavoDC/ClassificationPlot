binci.desmon=function (r, n, conf = 0.95, digits=3)
{
  if (r < 0 | r > n)
    stop("invalid value for r")
  if (conf <= 0 | conf >= 1)
    stop("must have 0<conf<1")
  alpha <- (1 - conf)/2
  ff <- function(p, r, n, alpha) 1 - pbinom(r - 1, n, p) -
    alpha
  pl <- if (r <= 0)
    0
  else uniroot(ff, c(1e-08, 1 - 1e-08), r = r, n = n, alpha = alpha)$root
  ff <- function(p, r, n, alpha) pbinom(r, n, p) - alpha
  pu <- if (r >= n)
    1
  else uniroot(ff, c(1e-08, 1 - 1e-08), r = r, n = n, alpha = alpha)$root
  ci.out=format(pl, pu, digits=digits)

  Z=list()
  Z$out=c(pl,pu)
  Z$method="Exact"
  Z$ci=ci.out
  Z
}
