
#function that performs alpha trimming

alpha_trimming <- function(set,alpha){
  
  len <- length(set)
  amount <- ceiling(len*alpha)
  
  set <- sort(set)
  set <- set[-((len+1-amount):len)]
  return(set[-(1:amount)])
  
}


#function fully peels the given set

convex_peeling <- function(set){
  plot(set,col="blue",yaxt = "n",
       xaxt = "n", xlab="", ylab="")
  
  while (dim(set)[1]>0) {
    convex <- chull(set)
    convex <- c(convex,convex[1])
    lines(set[convex,])
    set <- set[-convex,]
  }
  return(set)
}

#function peels a given set plotting the hulls and returns the peeled set

peeling_depth <- function(set,depth,plot=TRUE){
  if (plot) {
    plot(set,, xlab="", ylab="",col="blue",yaxt = "n",xaxt = "n")
  }
  
  
  
  
  for (i in 1:depth) {
    convex <- chull(set)
    convex <- c(convex,convex[1])
    if (plot) {
      lines(set[convex,])
      
    }
    set <- set[-convex,] 
  } 
  return(set)
  
}


#a function that takes two 2-dimensional points and calculates the squared distance between them 
square_dist_points <- function(a,b)
{
  
  return(sqrt((a[1]-b[1])^2+(a[2]-b[2])^2))
}

#a function designed to perform resistant peeling on multi-modal data
resistant_peeling <- function(set){
  
  dist <- c()
  hull <- chull(set)
  size <- dim(set)[1]
  removal_points <- hull
  
  for (i in 1:length(hull)) {
    dist <- c()
    gaps <- c()
    
    #finds the distance from the hull vertex i and all the points in set
    for (j in 1:size) {
      hull_pt <- set[hull[i],]
      dist <- append(dist,square_dist_points(hull_pt,set[j,]))
    }
    srt <- sort(dist)
    #finds the gaps between the sorted distances 
    for (n in 1:(size-1)) {
      gaps <- append(gaps,abs(srt[n+1]-srt[n]))
    }
    #finds the largest gap and finds the points to be removed from the set
    largest <- which.max(gaps)
    if (largest<size/2) {
      for (r in 1:largest) {
        index <- order(dist)[r]
        removal_points <- append(removal_points,index)
      }
    }
  }
  removal_points <- unique(removal_points)
  return(set[-removal_points,])
  
}