% Generated by roxygen2 (4.1.1): do not edit by hand
% Please edit documentation in R/reshape.rasch.R
\name{reshape.rasch}
\alias{reshape.rasch}
\title{Reshape data frame in wide format into a long format}
\usage{
reshape.rasch(data, group)
}
\arguments{
\item{data}{Matrix or data frame in 'wide' format.}

\item{group}{Vector which assigns each person to a certain subgroup (external split criterion).
               Note, that this function is restricted to A = 2 subgroups.}
}
\value{
Returns a data frame with following entries:
\tabular{ll}{
  \code{group}    \tab fixed factor A (subgroup) \cr
  \code{person}   \tab random factor B (testees) \cr
  \code{item}     \tab fixed factor C (items) \cr
  \code{response} \tab dependent variable, 0 (item not solved) and 1 (item solved) \cr
}
}
\description{
This function reshapes a matrix from 'wide' into a 'long' format. This is necessary for
the three-way analysis of variance with mixed classification for testing the Rasch model.
}
\details{
In order to apply the three-way analysis of variance with mixed classification for
testing the Rasch model, data need to be in 'long' format. That is, Rasch model
data design is interpreted as a analysis of variance design (A > \strong{B}) x C,
where items are levels of a fixed factor C and the testees are levels of a random
factor B, nested within a fixed factor A of different subgroups.
}
\examples{
\dontrun{

# simulate Rasch model based data
# 100 persons, 20 items,
dat <- simul.rasch(100, items = seq(-3, 3, length.out = 20))
# reshape simulated data into 'long' format with balanced assignment
# of testees into two subgroups.
dat.long <- reshape.rasch(dat, group = rep(0:1, each = nrow(dat) / 2))
head(dat.long)

# extract variable names of items
vnames <- grep("it", names(aid_st2), value = TRUE)
# reshape aid subtest 2 data into 'long' format with split criterium sex
aid_long.sex <- reshape.rasch(aid_st2[, vnames], group = aid_st2[, "sex"])
}
}
\author{
Takuya Yanagida \email{takuya.yanagida@univie.ac.at},
Jan Steinfeld \email{jan.steinfeld@univie.ac.at}
}
\references{
Kubinger, K. D., Rasch, D., & Yanagida, T. (2009). On designing data-sampling for Rasch model
calibrating an achievement test. \emph{Psychology Science Quarterly, 51}, 370-384.

Kubinger, K. D., Rasch, D., & Yanagida, T. (2011). A new approach for testing the Rasch model.
\emph{Educational Research and Evaluation, 17}, 321-333.
}
\seealso{
\code{\link{aov.rasch}}
}

