##
## Function to calculate variance-covariance matrix from geeglm models averaged using MuMIn
## which MuMIn can't seem to do by default because the vcov() function doesn't work on geeglm models
## instead, we need to extract their vcov matrices via model$geese$vbeta in each case
##
## See original code in MuMIn:::vcov.averaging
## the only change is on lines 19-20 below
##
## Author: Matthew Gwynfryn Thomas
##
##      {------- email --------}
##         {-- twitter --}
##      mgt@matthewgthomas.co.uk
##          {------ web -------}
##
##
## Copyright (c) 2016 Matthew Gwynfryn Thomas
## 
## This program is free software; you can redistribute it and/or modify
## it under the terms of the GNU General Public License as published by
## the Free Software Foundation; either version 2 of the License, or
## (at your option) any later version.
##
## This program is distributed in the hope that it will be useful,
## but WITHOUT ANY WARRANTY; without even the implied warranty of
## MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
## GNU General Public License for more details.
##
## You should have received a copy of the GNU General Public License along
## with this program; if not, write to the Free Software Foundation, Inc.,
## 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301 USA.
##
vcov.averaging.geeglm = function(object, full=T)
{
  object <- MuMIn:::upgrade_averaging_object(object)
  full <- MuMIn:::.checkFull(object, full)
  full <- as.logical(full)[1L]
  models <- attr(object, "modelList")
  if (is.null(models)) 
    stop("cannot calculate covariance matrix from ", "'averaging' object without component models")
  
  # get variance-covar matrices
  #vcovs <- lapply(lapply(models, vcov), as.matrix)
  vcovs <- lapply(lapply(models, function(m) m$geese$vbeta), as.matrix)
  
  names.all <- dimnames(object$coefArray)[[3L]]
  nvars <- length(names.all)
  nvarseq <- seq(nvars)
  wts <- MuMIn::Weights(object)
  wts <- wts/sum(wts)
  vcov0 <- matrix(if (full) 
    0
    else NA_real_, nrow = nvars, ncol = nvars, dimnames = list(names.all, 
                                                               names.all))
  vcovs2 <- lapply(vcovs, function(v) {
    i <- match(MuMIn:::fixCoefNames(dimnames(v)[[1L]]), names.all)
    vcov0[i, i] <- v
    return(vcov0)
  })
  b1 <- object$coefArray[, 1L, ]
  if (full) 
    b1[is.na(b1)] <- 0
  avgb <- object$coefficients[2L - full, ]
  res <- sapply(nvarseq, function(c1) sapply(nvarseq, function(c2) {
    weighted.mean(sapply(vcovs2, "[", c1, c2) + (b1[, c1] - 
                                                   avgb[c1]) * (b1[, c2] - avgb[c2]), wts, na.rm = TRUE)
  }))
  dimnames(res) <- list(names.all, names.all)
  return(res)
}