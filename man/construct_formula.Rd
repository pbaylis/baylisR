% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/construct_formula.R
\name{construct_formula}
\alias{construct_formula}
\title{Construct a formula for lfe::felm}
\usage{
construct_formula(lhs, rhs, fe = 0, clus = 0)
}
\arguments{
\item{lhs}{Character vector of length 1 with name of left-hand side variable}

\item{rhs}{Character vector with name(s) of right-hand side variables. If length(rhs) > 1, will be concatenated with " + " in between}

\item{fe}{(OPTIONAL) Character vector with name(s) of fixed effects. If length(rhs) > 1, will be concatenated with " + " in between}

\item{clus}{(OPTIONAL) Charactervector with name(s) of clustering variables. If length(rhs) > 1, will be concatenated with " + " in between}
}
\value{
Returns formula object
}
\description{
Construct a formula for lfe::felm
}
