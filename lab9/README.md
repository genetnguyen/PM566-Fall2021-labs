Lab 9 - HPC
================
Eugene Nguyen
11/4/2021

# Lab 9 - HPC

# Problem 1: Think

Give yourself a few minutes to think about what you just learned. List
three examples of problems that you believe may be solved using parallel
computing, and check for packages on the HPC CRAN task view that may be
related to it.

1.  Machine learning is a data science example that uses a lot of
    computational power. After browsing the HPC CRAN task view, it looks
    like the package h20 would solve that issue with explicit
    parallelism.

2.  Tensorflow is a popular data science tool to tweak and build machine
    learning algorithms. There is a tensorflow package in the HPC CRAN
    task view that utilizes GPU parallel computation.

3.  Speedglm looks to be like the generalized linear model function but
    for massive amounts of data. Can be used with data such as genetics
    which have thousands of independent variables that need to be
    processed.

# Problem 2: Before you

The following functions can be written to be more efficient without
using parallel:

1.  This function generates a n x k dataset with all its entries
    distributed poission with mean lambda.

``` r
fun1 <- function(n = 100, k = 4, lambda = 4) {
  x <- NULL
  
  for (i in 1:n)
    x <- rbind(x, rpois(k, lambda))
  
  return(x)
}

fun1alt <- function(n = 100, k = 4, lambda = 4) {
  matrix(rpois(n * k, lambda), nrow = n, ncol = k)
}

# Benchmarking
microbenchmark::microbenchmark(
  fun1(),
  fun1alt()
)
```

    ## Unit: microseconds
    ##       expr     min       lq      mean   median      uq      max neval
    ##     fun1() 239.701 312.1015 334.52697 318.7515 326.201 2570.701   100
    ##  fun1alt()  14.401  15.7010  30.89802  16.2010  16.601 1481.302   100

2.  Find the column max (hint: Checkout the function max.col()).

``` r
# Data Generating Process (10 x 10,000 matrix)
set.seed(1234)
x <- matrix(rnorm(1e4), nrow=10)

# Find each column's max value
fun2 <- function(x) {
  apply(x, 2, max)
}

fun2alt <- function(x) {
  # Find position of max value per row of x
  idx <- max.col(t(x))
  
  # Get max
  x[ cbind(idx, 1:ncol(x)) ]
}

# Check funcionts
all(fun2(x) == fun2alt(x))
```

    ## [1] TRUE

``` r
# Benchmarking
microbenchmark::microbenchmark(
  fun2(x),
  fun2alt(x)
)
```

    ## Unit: microseconds
    ##        expr     min       lq    mean   median       uq      max neval
    ##     fun2(x) 857.200 897.9505 977.803 913.8015 930.4005 3010.401   100
    ##  fun2alt(x)  84.401  87.1505 114.822  89.7505  93.2515 2320.301   100

# Problem 3: Parallelize everything

We will now turn our attention to non-parametric bootstrapping. Among
its many uses, non-parametric bootstrapping allow us to obtain
confidence intervals for parameter estimates without relying on
parametric assumptions.

The main assumption is that we can approximate many experiments by
resampling observations from our original dataset, which reflects the
population.

This function implements the non-parametric bootstrap:

``` r
my_boot <- function(dat, stat, R, ncpus = 1L) {
  
  # Getting the random indices
  n <- nrow(dat)
  idx <- matrix(sample.int(n, n*R, TRUE), nrow=n, ncol=R)
 
  # Making the cluster using `ncpus`
  # STEP 1: GOES HERE (Create cluster)
  cl <- makePSOCKcluster(ncpus)
  
  # STEP 2: GOES HERE (Prepare cluster)
  #clusterSetRNGStream(cl, 123)
  clusterExport(cl, c("stat", "dat", "idx"), envir = environment())
  
    # STEP 3: THIS FUNCTION NEEDS TO BE REPLACES WITH parLapply
  ans <- parLapply(cl = cl, seq_len(R), function(i) {
    stat(dat[idx[,i], , drop=FALSE])
  })
  
  # Coercing the list into a matrix
  ans <- do.call(rbind, ans)
  
  # STEP 4: GOES HERE
  stopCluster(cl)
  
  # Return answer
  ans
  
}
```

1.  Use the previous pseudocode, and make it work with parallel. Here is
    just an example for you to try:

``` r
# Bootstrap of an OLS
my_stat <- function(d) coef(lm(y ~ x, data=d))

# DATA SIM
set.seed(1)
n <- 500; R <- 1e4

x <- cbind(rnorm(n)); y <- x*5 + rnorm(n)

# Checking if we get something similar as lm
ans0 <- confint(lm(y~x))
ans1 <- my_boot(dat = data.frame(x, y), my_stat, R = R, ncpus = 2L)

# You should get something like this
t(apply(ans1, 2, quantile, c(.025,.975)))
```

    ##                   2.5%      97.5%
    ## (Intercept) -0.1386903 0.04856752
    ## x            4.8685162 5.04351239

``` r
##                   2.5%      97.5%
## (Intercept) -0.1372435 0.05074397
## x            4.8680977 5.04539763
ans0
```

    ##                  2.5 %     97.5 %
    ## (Intercept) -0.1379033 0.04797344
    ## x            4.8650100 5.04883353

``` r
##                  2.5 %     97.5 %
## (Intercept) -0.1379033 0.04797344
## x            4.8650100 5.04883353
```

2.  Check whether your version actually goes faster than the
    non-parallel version:

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 1L))
```

    ##    user  system elapsed 
    ##    0.08    0.02    2.56

``` r
system.time(my_boot(dat = data.frame(x, y), my_stat, R = 4000, ncpus = 2L))
```

    ##    user  system elapsed 
    ##    0.09    0.01    1.43

# Problem 4: Compile this markdown document using Rscript

Once you have saved this Rmd file, try running the following command in
your terminal:

``` r
# Rscript --vanilla -e 'rmarkdown::render("C:/Users/Eugen/Desktop/PM 566/Labs/Lab 9/lab9.Rmd")' &
```
