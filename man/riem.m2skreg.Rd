% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/inference_m2skreg.R
\name{riem.m2skreg}
\alias{riem.m2skreg}
\title{Manifold-to-Scalar Kernel Regression}
\usage{
riem.m2skreg(
  riemobj,
  y,
  bandwidth = 0.5,
  geometry = c("intrinsic", "extrinsic")
)
}
\arguments{
\item{riemobj}{a S3 \code{"riemdata"} class for \eqn{N} manifold-valued data corresponding to \eqn{X_1,\ldots,X_N}.}

\item{y}{a length-\eqn{N} vector of dependent variable values.}

\item{bandwidth}{a nonnegative number that controls smoothness.}

\item{geometry}{(case-insensitive) name of geometry; either geodesic (\code{"intrinsic"}) or embedded (\code{"extrinsic"}) geometry.}
}
\value{
a named list of S3 class \code{m2skreg} containing
\describe{
\item{ypred}{a length-\eqn{N} vector of smoothed responses.}
\item{bandwidth}{the bandwidth value that was originally provided, which is saved for future use.}
\item{inputs}{a list containing both \code{riemobj} and \code{y} for future use.}
}
}
\description{
Given \eqn{N} observations \eqn{X_1, X_2, \ldots, X_N \in \mathcal{M}} and 
scalars \eqn{y_1, y_2, \ldots, y_N \in \mathbf{R}}, perform the Nadaraya-Watson kernel 
regression by 
\deqn{\hat{m}_h (X) = \frac{\sum_{i=1}^n K \left( \frac{d(X,X_i)}{h}  \right) y_i}{\sum_{i=1}^n K \left( \frac{d(X,X_i)}{h}  \right)}}
where the Gaussian kernel is defined as
\deqn{K(x) := \frac{1}{\sqrt{2\pi}} \exp \left( - \frac{x^2}{2}\right)} 
with the bandwidth parameter \eqn{h > 0} that controls the degree of smoothness.
}
\examples{
\donttest{
#-------------------------------------------------------------------
#                    Example on Sphere S^2
#
#  X : equi-spaced points from (0,0,1) to (0,1,0)
#  y : sin(x) with perturbation
#-------------------------------------------------------------------
# GENERATE DATA
npts = 100
nlev = 0.25
thetas = seq(from=0, to=pi/2, length.out=npts)
Xstack = cbind(rep(0,npts), sin(thetas), cos(thetas))

Xriem  = wrap.sphere(Xstack)
ytrue  = sin(seq(from=0, to=2*pi, length.out=npts))
ynoise = ytrue + rnorm(npts, sd=nlev)

# FIT WITH DIFFERENT BANDWIDTHS
fit1 = riem.m2skreg(Xriem, ynoise, bandwidth=0.001)
fit2 = riem.m2skreg(Xriem, ynoise, bandwidth=0.01)
fit3 = riem.m2skreg(Xriem, ynoise, bandwidth=0.1)

# VISUALIZE
xgrd <- 1:npts
opar <- par(no.readonly=TRUE)
par(mfrow=c(1,3))
plot(xgrd, fit1$ypred, pch=19, cex=0.5, "b", xlab="", ylim=c(-2,2), main="h=1e-3")
lines(xgrd, ytrue, col="red", lwd=1.5)
plot(xgrd, fit2$ypred, pch=19, cex=0.5, "b", xlab="", ylim=c(-2,2), main="h=1e-2")
lines(xgrd, ytrue, col="red", lwd=1.5)
plot(xgrd, fit3$ypred, pch=19, cex=0.5, "b", xlab="", ylim=c(-2,2), main="h=1e-1")
lines(xgrd, ytrue, col="red", lwd=1.5)
par(opar)
}

}
\concept{inference}
