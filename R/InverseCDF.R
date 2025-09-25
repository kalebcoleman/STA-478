# "..." allows extra parameters like rate
Inverse.CDF <- function(iCDF, n, ...) {

  # step 1: draw uniforms U ~ Unif(0,1)
  u <- runif(n)

  # step 2: apply the inverse CDF
  # uses the given inverse CDF from the function
  # to test I use the formula from the notes: F^{-1}(u) = -log(1 - u)/rate

  # Step 3: return results
  return(iCDF(u))
}
