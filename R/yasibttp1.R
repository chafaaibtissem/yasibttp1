#'statistiques descriptives
#'@export
#' @param x numeric vector representing the values of the random variable
#' @param y numeric vector representing the probabilities
#' @param n number of iterations
rdist<-function(x,p)
{
  n=length(p)
  r=runif(1)
  b=p[1];
  if((r>=0) & (r<=b))
  {
    y=x[1]
    return(y)
  }
  else
  {
    a=p[1]
    b=b+p[2]
    for(i in 2:n)
    {
      if((r>=a) & (r <=b))
      {
        y=x[i]
        return(y)
      }
      else
      {
        a=b
        b=b+p[i+1]
        i=i+1
      }
    }
    y=x[n]
    return(y)
  }
}

rdistn <-function(x,p,n)
{
  v <- c(1:n)
  for (j in 1:n) {
    y=rdist(x,p)
    v[j] <- y

  }
  return(v)
}
SD<-function(x)
{
  par(mfrow= c(1,3))
  hist(x, col = rainbow(10))
  boxplot(x, col = 'blue' )
  par(mfrow = c(2, 2))
  data.frame( min = min(x),
              max = max(x),
              median = median(x),
              mean = mean(x))
}
