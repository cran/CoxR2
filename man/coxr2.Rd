\name{coxr2}
\alias{coxr2}
\title{
R-Squared under the Cox model
}
\description{
Calculate the R-squared, aka explained randomness, based on the partial likelihood ratio statistic under the Cox model.
}
\usage{
##object is the result of a 'coxph'
coxr2(object)
}

\arguments{
  \item{object}{
  The result of a coxph fit
}
}

\details{
Calculate the R-squared based on the partial likelihood ratio statistic under the Cox model. Difference in log partial likelihoods between the fitted model and the null model with no regressors is divided by the number of uncensored events, while the existing summary function divides it by the number of total observations. 
}

\value{
  \item{nevent }{number of uncensored events  }
  \item{logtest }{partial likelihood ratio test statistics }
  \item{rsq }{explained randomness}
}

\references{
John O'Quigley, Ronghui Xu and Janez Stare, (2005), Explained randomness in proportional hazards models, STATISTICS IN MEDICINE, 24:479-489. 
}

\author{
Hyeri You, Rounghui Xu
}

\seealso{
coxph, summary.coxph
}

\examples{
# Create the simplest test data set
test <- list(time=c(4,3,1,1,2,2,3),
             event =c(1,1,1,0,1,1,0),
             x =c(5,2,1,1,1,5,5))

# Fit a Cox model
coxmodel <- coxph(Surv(time, event ) ~ x , test)

coxr2(coxmodel)
}


