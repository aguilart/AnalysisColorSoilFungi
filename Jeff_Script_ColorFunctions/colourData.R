# function to obtain colour characteristics from images - written by Jeff Powell (jeffpowell2@gmail.com)
# arguments are number of points to collect data from
colour.data <- function(npoints=1){
  # establish object for output
  out <- NULL
  for(i in 1:npoints){
    # specify coordinates from which to obtain data
    message('for point ', i, ', click on two opposite corners to establish square to assess colour...')
    sq <- locator(2)
    # set up matrix to identify cells that are under colony
    mat <- matrix(F, nrow=dim(image)[1], ncol=dim(image)[2])
    # identify cells that are under colony using coordinates around colony perimeter, standardising by dimensions of plate in image and matrix size
    x <- seq(from=sq$x[1], to=sq$x[2], length.out=max(dim(image)))
    y <- seq(from=sq$y[1], to=sq$y[2], length.out=max(dim(image)))
    xx <- round((ncol(mat) * x) / max(img$x))
    yy <- round((nrow(mat) * (max(img$y) - y)) / max(img$y))  # for unknown reason, coordinates transposed on the y axis ONLY for the underlying data (not plotting)
    mat[yy, xx] <- T
    # create vectors for each colour channel
    chans <- data.frame(R = 255 * image[, , 1][mat], 
                        G = 255 * image[, , 2][mat], 
                        B = 255 * image[, , 3][mat])
    chans.sum <- summaryBy(R + G + B ~ 1, data=chans, FUN=c(mean, sd))
    points(mean(x), mean(y), cex=3, pch=21, bg=hex(sRGB(as.matrix(chans.sum[, grep('mean', names(chans.sum))])/255)))
    out <- rbind(out, cbind(chans.sum, col=hex(sRGB(as.matrix(chans.sum[, grep('mean', names(chans.sum))])/255)), stringsAsFactors=F))
    
  }
  return(out)
}