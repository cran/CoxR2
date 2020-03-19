coxr2 <- function (object)
{
  cox <- object
  
  beta <- cox$coefficients 
  if (is.null(cox$coefficients)) {
    return(object)
  }
  nabeta <- !(is.na(beta))
  beta2 <- beta[nabeta]
  
  
  #####################################
  rval <- list()
  #####################################
  if (!is.null(cox$nevent))
    rval$nevent <- cox$nevent
  
  
  df <- length(beta2)
  ######################################################
  logtest <- -2 * (cox$loglik[1] - cox$loglik[2])
  ######################################################
  rval$logtest <- c(test = logtest, df = df, pvalue = pchisq(logtest,
                                                             df, lower.tail = FALSE))
  
  ######################################################################################
  rval$rsq <- c(rsq = 1 - exp(-logtest/cox$nevent))  ###n <- number of uncensored events
  #######################################################################################
  
  rval
}
##

