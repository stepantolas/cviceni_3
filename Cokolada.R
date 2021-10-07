Cokolada <- function(M,r,s) {
  L <- nrow(M)
  if (r = L) {
    return(M[r,s])
  } else {
    C <- M[r,s]
    Cdolu <- Cokolada(M,r+1,s)
    Csikmo <- Cokolada(M,r+1,s+1)
  }  
}  
  
  