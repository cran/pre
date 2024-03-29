% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/caret_pre_model.R
\docType{data}
\name{caret_pre_model}
\alias{caret_pre_model}
\title{Model set up for train function of package caret}
\format{
An object of class \code{list} of length 17.
}
\usage{
caret_pre_model
}
\description{
\code{caret_pre_model} is deprecated and provided for backwards compatibility
only. The object provides a model setup for function \code{train} of
package caret. It allows for tuning arguments sampfrac, maxdepth, learnrate, 
mtry, use.grad and penalty.par.val.
}
\details{
Object caret_pre_model is deprecated, and only included in package pre for backward 
compatibility. Parameters of function \code{pre()} can be tuned by using method 
\code{"pre"} in caret's function \code{train()}. See vignette on tuning for more
information and examples: \code{vignette("Tuning", package = "pre")}
}
\examples{
## Object caret_pre_model is only included in package pre for backward compatibility
## By now, function pre can be optimized in the default way by using the method "pre" 
## in caret's function train(). More information and instructions on tuning parameters
## of function pre() are provided in the vignette about tuning, which can be accessed
## from R by typing:
##
## vignette("Tuning", package = "pre")
##
}
\keyword{datasets}
