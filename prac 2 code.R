##### Generate Simulated Data #####
x <- seq(1:100)

set.seed(1)
error <- rnorm(100,0,0.2)

y <- sin(x/10)+ error
f <- runif(1, 0, 1)

##### customLowess Function #####

customLowess <- function(x, y, f) {
  
  y_ans <- numeric(length(x))
  
  # Calculate the span
  k <- f * length(x)
  
  for (i in 1:length(x)) {
    
    dist <- abs(x - x[i])
    
    # Determine max distances point of k points
    neighbours <- order(dist)[1:k]
    max_dist <- max(dist[neighbours])
    
    # Calculate weights
    weights <- (1 - (dist[neighbours] / max_dist)^3)^3
    ifelse(any(weights[dist[neighbours] > max_dist]), 
           weights[dist[neighbours] > max_dist] <- 0,weights <- weights)
    
    # Weighted linear regression
    X <- cbind(1, x[neighbours])  
    W <- diag(weights)  
    Y <- y[neighbours]  
    
    # Calculate Beta estimates
    beta_hat <- solve(t(X) %*% W %*% X) %*% (t(X) %*% W %*% Y)
    y_ans[i] <- beta_hat[1] + beta_hat[2] * x[i]
  }
  
  return(y_ans)
}

##### calculate and model customLowess Function #####

calcualtion <- customLowess(x, y, f)  
model <- lowess(x, y, f, iter=0)$y  

##### Plot the comparisson #####
plot(x, y, main="calculated Lowness compared to built-in Lowness")
lines(x, model, col="blue",lwd =2) 
lines(x, calcualtion, col="red", lwd =2, lty=2) 
legend("topright", legend = c("Custom Lowness", "Built-in Lowness"), 
      col = c("red", "blue"), lty = c(1, 2), lwd = 2,cex = 0.6)