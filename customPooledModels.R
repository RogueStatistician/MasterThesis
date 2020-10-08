pool.lms <- function(models.list){
  require(dplyr)
  require(mice)
  dfcom <- df.residual(models.list[[1]])
  call <- models.list[[1]]$call
  thetas <- llply(models.list,function(x) coefficients(summary(x))[,1])
  M <- length(thetas)
  thetabar <- Reduce('+',thetas)/M
  um <- llply(models.list,function(x) vcov(x))
  ubar <- Reduce('+',um)/M
  thetaprod <- llply(thetas,function(x) (x-thetabar)%*%t((x-thetabar)))
  b <- Reduce('+',thetaprod)/(M-1)
  uhat <- ubar+(1+1/M)*Reduce('+',thetaprod)
  df <- mice:::barnard.rubin(M,diag(b),diag(uhat),dfcom)
  ret <- list(call=call,est=thetabar,variance=uhat,df=df,M=M)
  class(ret) <- c('list','pooled.model')
  ret
}

print.pooled.model <- function(pooled.model){
  cat('Call:\n')
  print(pooled.model$call)
  cat('\n')
  cat('Coefficients:\n')
  print(pooled.model$est)
  
}

summary.pooled.model <- function(pooled.model){
  M <- pooled.model$M
  std.error <- sqrt(diag(pooled.model$variance))
  statistic <- pooled.model$est/std.error
  p.value <- 2 * (1 - pt(abs(statistic), max(pooled.model$df, 0.001)))
  z <- data.frame(estimate = pooled.model$est, std.error = std.error, statistic = statistic, 
                  p.value = p.value)
  class(z) <- c("pooled.model.summary", "data.frame")
  z
}
