## `Return network measures

#'  Return local cosine similarities between network at time 1 and time 2
#'
#' @param m1 network at time point 1
#' @param m2 network at time point 2
#'
#' @return  Vector of local csine similarities  for network 1 and 2
#' @export
#'
#' @examples ## fill in later
return.lcs <- function(m1, m2){
  m3<- m1^2
  m4 <- m2^2
  lcas <- c()
  for(j in 1:nrow(m1)){
    lcas[[j]] <-   (sum(m1[j,] * m2[j,], na.rm = TRUE))/
      (sqrt(sum(m3[j,], na.rm = TRUE))* sqrt(sum(m4[j,], na.rm= TRUE)))
  }
  for(i in 1:length(lcas)){
    if(lcas[[i]]=="NaN"){
      lcas[[i]] <- NA
    }
  }
  return(unlist(lcas))
}


