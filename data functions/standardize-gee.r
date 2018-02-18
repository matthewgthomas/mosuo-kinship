##
## Analysis code for Thomas et al. (2015): Saami reindeer herders cooperate with social group members and genetic kin
##
## This modifies the `standardize` function in the package `arm` to work with GEE models.
##
## Author: Matthew Gwynfryn Thomas
##
##      {------- email --------}
##         {-- twitter --}
##      mgt@matthewgthomas.co.uk
##          {------ web -------}
##
##
## Copyright (c) 2015 Matthew Gwynfryn Thomas
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
standardize.default <- function(call, unchanged=NULL,
                                standardize.y=FALSE, binary.inputs="center"){
  form <- call$formula
  varnames <- all.vars (form)
  n.vars <- length (varnames)
  #
  # Decide which variables will be unchanged
  #
  transform <- rep ("leave.alone", n.vars)
  if (standardize.y) {
    transform[1] <- "full"
  }
  for (i in 2:n.vars){
    v <- varnames[i]
    if (is.null(call$data)) {
      thedata <- get(v)
    }
    else {
      thedata <- get(as.character(call$data))[[v]]
    }
    if (is.na(match(v,unchanged))){
      num.categories <- length (unique(thedata[!is.na(thedata)]))
      if (num.categories==2){
        transform[i] <- binary.inputs
      }
      else if (num.categories>2 & is.numeric(thedata)){
        transform[i] <- "full"
      }
    }
  }
  #
  # New variable names:
  #   prefix with "c." if centered or "z." if centered and scaled
  #
  varnames.new <- ifelse (transform=="leave.alone", varnames,
                          ifelse (transform=="full", paste ("z", varnames, sep="."),
                                  paste ("c", varnames, sep=".")))
  transformed.variables <- (1:n.vars)[transform!="leave.alone"]
  
  
  #Define the new variables
  if (is.null(call$data)) {
    for (i in transformed.variables) {
      assign(varnames.new[i], rescale(get(varnames[i]), binary.inputs))
    }
  }
  else {
    newvars <- NULL
    for (i in transformed.variables) {
      assign(varnames.new[i], rescale(get(as.character(call$data))[[varnames[i]]], 
                                      binary.inputs))
      newvars <- cbind(newvars, get(varnames.new[i]))
    }
    assign(as.character(call$data), cbind(get(as.character(call$data)), newvars))
  }
  
  # Now call the regression with the new variables
  
  call.new <- call
  L <- sapply (as.list (varnames.new), as.name)
  names(L) <- varnames
  call.new$formula <- do.call (substitute, list (form, L))
  formula <- as.character (call.new$formula)
  if (length(formula)!=3) stop ("formula does not have three components")
  formula <- paste (formula[2],formula[1],formula[3])
  formula <- gsub ("factor(z.", "factor(", formula, fixed=TRUE)
  formula <- gsub ("factor(c.", "factor(", formula, fixed=TRUE)
  call.new$formula <- as.formula (formula) 
  return (eval (call.new))
}

standardize.gee = function(object, unchanged=NULL, 
                   standardize.y=FALSE, binary.inputs="center")
          {
            call <- object$call
            out <- standardize.default(call=call, unchanged=unchanged, 
                                       standardize.y=standardize.y, binary.inputs=binary.inputs)
            return(out)
          }
