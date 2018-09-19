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
#' 
#' 
#' 
