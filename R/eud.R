#' eud calculated greatest common divisor
#'
#' @param x1 first input number
#' @param x2 second input number
#'
#' @return
#' @export
#'
eud <- function(x1,x2){
  tony <- function(x){
    k=vector()
    for(i in 1:(x)){
      if(x %% i ==0){
        k=cbind(k,c(i))
      }
    }
    return(k)
  }
  a = as.vector(tony(abs(x1)))
  b = as.vector(tony(abs(x2)))

  ca = a %in% b
  caa= max(which(ca == TRUE))
  return(a[caa])
}
