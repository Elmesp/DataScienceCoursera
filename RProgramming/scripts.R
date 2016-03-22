TRUE & c(TRUE, FALSE, FALSE) # all vector (same for |)
TRUE && c(TRUE, FALSE, FALSE) # only first element (same for ||)
isTRUE(6 > 4)
identical('twins', 'twins')
xor(4 >= 9, 8 != 8.0)
ints <- sample(10)
ints > 5
which(ints > 7) # get indexes
any(ints < 0)
all(ints > 0)
Sys.Date()

remainder <- function(num, divisor = 2) {
  num %% divisor
}

evaluate <- function(func, dat){
  func(dat)
}
evaluate(sd, c(1.4, 3.6, 7.9, 8.8))

mad_libs <- function(...){
  # Do your argument unpacking here!
  args <- list(...)
  place <- args[["place"]]
  adjective <- args[["adjective"]]
  noun <- args[["noun"]]
  
  # Don't modify any code below this comment.
  # Notice the variables you'll need to create in order for the code below to
  # be functional!
  paste("News from", place, "today where", adjective, "students took to the streets in protest of the new", noun, "being installed on campus.")
}

# operators
"%p%" <- function(left, right){ # Remember to add arguments!
  paste(left, right)
}

# *apply
cls_list <- lapply(flags, class)
cls_vect <- sapply(flags, class)
unique_vals <- lapply(flags, unique)
lapply(unique_vals, function(elem) elem[2])
vapply(flags, class, character(1))
tapply(flags $population, flags $landmass, summary)

# debug
lm(y - x)
traceback() # where the error occurs
debug(lm(y - x))
options(error = recover) # show each value

data(iris)
tapply(iris $Sepal.Length, iris $Species, mean)
apply(iris[, 1:4], 2, mean)

# simulation and profiling
x <- rnorm(100, 2, 4)
summary(x)
str(x)

dnorm(x, mean = 0, sd = 1, log = FALSE)
pnorm(q, mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
qnorm(p, mean = 0, sd = 1, lower.tail = FALSE, log.p = FALSE)
rnorm(n, mean = 0, sd = 1)

# linear model
# y = B0 + B1x + e
# where e~N(0, 2^2). Assume x~N(0,1^2), B0 = 0.5 and B1 = 2
set.seed(20)
x <- rnorm(100)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2 * x + e
summary(y)
plot(x, y)

# x as binomial distribution
set.seed(10)
x <- rbinom(100, 1, 0.5)
e <- rnorm(100, 0, 2)
y <- 0.5 + 2 * x + e
summary(y)
plot(x, y)

# x as Poisson distribution
set.seed(1)
x <- rnorm(100)
log.mu <- 0.5 + 0.3 * x
y <- rpois(100, exp(log.mu))
summary(y)
plot(x, y)

# sampling
set.seed(1)
sample(1: 10, 4)
sample(letters, 5)
sample(1: 10) # permutation
sample(1: 10, replace = TRUE) # replacement

set.seed(1)
rpois(5, 2)


object.size(plants)
names(plants)
