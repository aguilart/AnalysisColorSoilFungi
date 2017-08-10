# function to obtain colony diameter, growth direction, and colony colour characteristics from JPEG images - written by Jeff Powell (jeffpowell2@gmail.com)
# arguments are number of cultures in each image, plate size in mm, and number of points to use to delineate colony and plate borders
culture.data <- function(ncultures=1, npoints=8, plat.mm=90){
  # establish object for output
  out <- NULL
  for(i in 1:ncultures){
    # specify centre of inoculation point
    message('for colony ', i, ', click on centre of inoculation point...')
    cent <- locator(1)
    points(cent, pch=16)
    # specify extreme x and y coordinates for colony and plate
    message('click on colony perimeter at ', npoints, ' evenly spaced points...')
    cult <- locator(npoints)
    xx <- rep(1:npoints, 2); xx <- xx[which.min(cult$x):(which.min(cult$x) + (npoints - 1))]
    cult$x <- cult$x[xx]; cult$y <- cult$y[xx]; rm(xx)
    lines(cult); lines(cult$x[c(length(cult$x), 1)], cult$y[c(length(cult$y), 1)])
    message('click on plate perimeter at ', npoints, ' evenly spaced points...')
    plat <- locator(npoints)
    xx <- rep(1:npoints, 2); xx <- xx[which.min(plat$x):(which.min(plat$x) + (npoints - 1))]
    plat$x <- plat$x[xx]; plat$y <- plat$y[xx]; rm(xx)
    lines(plat); lines(plat$x[c(length(plat$x), 1)], plat$y[c(length(plat$y), 1)])
    
    ## estimate growth parameters
    # calculate distance of all colony points relative to centre, standardising by dimensions of plate in image and specified plate size
    grow <- NULL
    for(i in 1:npoints) {
      grow1 <- as.numeric(dist(rbind(data.frame(cent), data.frame(cult)[i, ])))
      plat1 <- as.numeric(dist(rbind(data.frame(cent), data.frame(plat)[i, ])))
      grow <- c(grow, (grow1 * (plat.mm / 2) / plat1))
      rm(grow1, plat1)
    }; rm(i)
    # estimate average colony radius
    rad.mm <- mean(grow); rm(grow)
    # estimate bias in direction of growth (-ve: more growth to left; +ve: more growth to right)
    bias.mm <- (mean(cult$x - cent$x) * (plat.mm / 2)) / diff(range(plat$x))
    
    ## estimate colour parameters
    # set up matrix to identify cells that are under colony
    cult.mat <- matrix(F, nrow=dim(image)[1], ncol=dim(image)[2])
    # identify cells that are under colony using coordinates around colony perimeter, standardising by dimensions of plate in image and matrix size
    for(i in 1:npoints){
      x <- seq(from=cent$x, to=cult$x[i], length.out=max(dim(image)))
      y <- seq(from=cent$y, to=cult$y[i], length.out=max(dim(image)))
      lines(x, y)
      x <- round((ncol(cult.mat) * x) / max(img$x))
      y <- round((nrow(cult.mat) * (max(img$y) - y)) / max(img$y))  # for unknown reason, coordinates transposed on the y axis ONLY for the underlying data (not plotting)
      for(j in 1:length(x)) cult.mat[y[j], x[j]] <- T
      rm(x, y, j)
    }; rm(i)
    # create vectors for each colour channel
    chans <- data.frame(R = 255 * image[, , 1][cult.mat], 
                        G = 255 * image[, , 2][cult.mat], 
                        B = 255 * image[, , 3][cult.mat])
    chans.sum <- summaryBy(R + G + B ~ 1, data=chans, FUN=c(mean, sd))
    points(cent, cex=3, pch=21, bg=hex(sRGB(as.matrix(chans.sum[, grep('mean', names(chans.sum))])/255)))
    out <- rbind(out, cbind(rad.mm, bias.mm, chans.sum, col=hex(sRGB(as.matrix(chans.sum[, grep('mean', names(chans.sum))])/255)), stringsAsFactors=F))
  }
  
  return(out)
}
