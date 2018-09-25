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



#' Brug R til at lave de samme som i Part 2 uden at skulle udfylde matricerne selv

mod1 <- lm(dist~speed, data=cars); mod1
class(mod1)
names(mod1)

mod2=lm(dist~speed+I(speed^2),data=cars); mod2
fitted(mod2)
plot(dist~speed, data=cars)
lines(fitted(mod2) ~ speed, data = cars)

#' PART 3
#' EXERCISE 1
#' 
head(ToothGrowth)
#' a)
plot(ToothGrowth$len~ToothGrowth$dose)

#' b)
modteeth1=lm(len ~ dose, data=ToothGrowth); modteeth1
beta_hat1=coef(modteeth1); beta_hat1
plot(len ~ dose, data=ToothGrowth)
abline(beta_hat1)

#' c)
modteeth2=lm(len ~ dose+I(dose^2), data=ToothGrowth); modteeth2
beta_hat2=coef(modteeth2)
plot(len ~ dose, data=ToothGrowth)
curve(beta_hat2[1] + beta_hat2[2]*x + beta_hat2[3]*x^2,0.5,2,add=TRUE)


#' d)
modteeth2=lm(len ~ dose+I(dose^2) + I(dose^3), data=ToothGrowth); modteeth2
beta_hat2=coef(modteeth2)
plot(len ~ dose, data=ToothGrowth)
curve(beta_hat2[1] + beta_hat2[2]*x + beta_hat2[3]*x^2 + beta_hat2[4]*x^3,0.5,2,add=TRUE)
#' Den skriver NA for dose^3, da den nægter at tilpasse en tredjegradspolynomium, da vi har uendeligt mange muligheder.
#' Den skal nemlig tilpasse efter kun 3 punkter - 1 pr. dose i gennemsnitsværdierne.
#'
#'EXERCISE 2
#'a)
plot(len ~ supp, data=ToothGrowth)

#' b)
#' One-way ANOVA
mod3<-lm(len ~ supp, data=ToothGrowth); mod3
#' Dem, der har fået OJ (Oragne juice) har en gennemsnitlig tandlængde på 20.66, mens den for dem der har fået 
#' VC (Vitamine C) er -3.70 kortere. Dvs. De får længst tænder ved at få OJ.