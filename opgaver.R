#' PART I
#' EXERCISES
#' 
#' 
#' I) Make a function with a for loop that can calculate the product of all the entries 
#' in an input vector. Compare with the built-in function prod (don’t call your function 
#' prod, #' or you won’t be able to use the built-in function easily).
#' 
productfct=function(x){
  n=length(x)
  product=1
  for (i in 1:n){
    product=product*x[i]
  }
  return(product)
}
productfct(c(1,2,3,4))
prod(c(1,2,3,4))

#' II) Make a function that will calculate the Fibonacci number up to n (an input parameter).
#' • Does it handle n=1 or 2 correctly? (hint: an if statement may be useful here)
#' • Does it handle negative numbers correctly? (hint: the stop function can be used 
#' to give an error message)
#' • Does it handle decimal numbers correctly?


#' PART 2
#' EXERCISE 1
#' I) Consider the built-in dataset cars
#'    a) Make the design matrix X for a simple linear regression for cars (dist as 
#'    a function of speed).

X <- matrix(c(rep(1,length(cars$speed)),cars$speed),length(cars$speed),2)
Y <- cars$dist

#'    b) Estimate beta. Plot the data and the estimated line in the same figure.
#'    (hint: the function abline is useful for plotting the line)
    
A <- (t(X)%*%X)
B <- t(X)%*%Y
b=solve(A,B)
plot(cars)
abline(b)

#'    c) Estimate sigmaˆ2.
s=(t(Y-X%*%b)%*%(Y-X%*%b))/(50-(1+1))    #Sigma^2

#' II) Maybe a second order polynomial is better at capturing the relation between speed 
#' and distance? 
#' Redo exercise I with a second order polynomial. (Hint: curve may be useful)


#' PART 3
#' Brug R til at lave de samme som i Part 2 uden at skulle udfylde matricerne selv

mod1 <- lm(dist~speed, data=cars); mod1
class(mod1)
names(mod1)
