VraceniPenez <- function(M, Mince) {
  VraceneMince <- matrix(0, ncol = length(Mince), nrow = 1)
  colnames(VraceneMince) <- Mince
  
  i <- 1
  while (M > 0) {
    while (Mince[i]>M) {
      i = i+1
    }
    M = M-Mince[i]
    VraceneMince[i] <- VraceneMince[i]+1
  }
  return(VraceneMince)
}

M <- 224
Mince <- c(50,20,10,5,2,1)
