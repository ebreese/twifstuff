library(scales)
library(ggplot2)

halfMaxVenueLength <- 176/2
halfMaxVenueWidth <- 142/2


rotateCoordinates90 <- function(x,y,rotateLeft=TRUE)
{
  
  newX <- y
  newY <- x
  
  if(rotateLeft)
  {
    newY <- newY * -1
  }
  else
  {
    newX <- newX * -1
  }
  return(list(x = newX,y=newY))
}

generateTopBoundary<-function(venueLength,venueWidth, bounds=NA,rotate90=0)
{
  topBoundary <- data.frame(
    x = (venueLength / 2) * cos(seq(0,pi,length.out=100)),
    y= 9.6+(((venueWidth / 2) - 9.6) * sin(seq(0,pi,length.out=100)))
  )
  
  if(rotate90!= 0)
  {
    rotateLeft <- rotate90 == -1
    new<-rotateCoordinates90(topBoundary$x, topBoundary$y,rotateLeft)
    topBoundary$x <- new$x
    topBoundary$y <- new$y
  }
  prescale<<-topBoundary
  if(is.list(bounds))
  {
    if(rotate90!=0)
    {
      topBoundary$x <- rescale(topBoundary$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueWidth,halfMaxVenueWidth))
      topBoundary$y <- rescale(topBoundary$y,to=c(bounds$bottom,bounds$top),from=c(-halfMaxVenueLength,halfMaxVenueLength)) 
    }
    else
    {
      topBoundary$x <- rescale(topBoundary$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueLength,halfMaxVenueLength))
      topBoundary$y <- rescale(topBoundary$y,to=c(bounds$bottom,bounds$top),from=c(-halfMaxVenueWidth,halfMaxVenueWidth)) 
    }
    
  }
  postScale<<-topBoundary
  
  
  return(topBoundary)
}

generateBottomBoundary<-function(venueLength,venueWidth,bounds=NA,rotate90=0)
{
  bottomBoundary<-generateTopBoundary(venueLength,venueWidth)
  bottomBoundary$y <- bottomBoundary$y * -1
  
  if(rotate90!= 0)
  {
    rotateLeft <- rotate90 == -1
    new<-rotateCoordinates90(bottomBoundary$x, bottomBoundary$y,rotateLeft)
    bottomBoundary$x <- new$x
    bottomBoundary$y <- new$y
  }
  
  if(is.list(bounds))
  {
    if(rotate90!=0)
    {
      bottomBoundary$x <- rescale(bottomBoundary$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueWidth,halfMaxVenueWidth))
      bottomBoundary$y <- rescale(bottomBoundary$y,to=c(bounds$bottom,bounds$top),from=c(-halfMaxVenueLength,halfMaxVenueLength)) 
    }
    else
    {
      bottomBoundary$x <- rescale(bottomBoundary$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueLength,halfMaxVenueLength))
      bottomBoundary$y <- rescale(bottomBoundary$y,to=c(bounds$bottom,bounds$top),from=c(-halfMaxVenueWidth,halfMaxVenueWidth)) 
    }}
  
  
  
  return(bottomBoundary)
}

generateBoundary <- function(venueLength, venueWidth, bounds=NA, rotate90=0)
{
  boundary <- generateTopBoundary(venueLength,venueWidth,bounds,rotate90)
  boundary <- boundary[nrow(boundary):1,]
  boundary <- rbind(boundary,generateBottomBoundary(venueLength,venueWidth,bounds,rotate90),boundary[1,])
  return(boundary)
}

generateLeftSquare<-function(venueLength,bounds=NA,rotate90=0)
{
  halfVenueLength <- venueLength / 2
  
  leftSquare<-data.frame(x=c(-halfVenueLength,-halfVenueLength+9,-halfVenueLength+9,-halfVenueLength,-halfVenueLength,-halfVenueLength,-halfVenueLength),
                         y=c(3.2,3.2,-3.2,-3.2,3.2,9.6,-9.7))
  
  if(rotate90!= 0)
  {
    rotateLeft <- rotate90 == -1
    new<-rotateCoordinates90(leftSquare$x, leftSquare$y,rotateLeft)
    leftSquare$x <- new$x
    leftSquare$y <- new$y
  }
  
  if(is.list(bounds))
  {
    if(rotate90!=0)
    {
      leftSquare$x <- rescale(leftSquare$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueWidth,halfMaxVenueWidth))
      leftSquare$y <- rescale(leftSquare$y,to=c(bounds$bottom,bounds$top),from=c(-halfMaxVenueLength,halfMaxVenueLength)) 
    }
    else
    {
      leftSquare$x <- rescale(leftSquare$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueLength,halfMaxVenueLength))
      leftSquare$y <- rescale(leftSquare$y,to=c(bounds$bottom,bounds$top),from=c(-halfMaxVenueWidth,halfMaxVenueWidth)) 
    }}
  
  
  
  
  return(leftSquare)
  
  
}

generateRightSquare<-function(venueLength,bounds=NA,rotate90=0)
{
  rightSquare<-generateLeftSquare(venueLength)
  rightSquare$x <- rightSquare$x * -1
  
  if(rotate90!= 0)
  {
    rotateLeft <- rotate90 == -1
    new<-rotateCoordinates90(rightSquare$x, rightSquare$y,rotateLeft)
    rightSquare$x <- new$x
    rightSquare$y <- new$y
  }
  
  if(is.list(bounds))
  {
    if(rotate90!=0)
    {
      rightSquare$x <- rescale(rightSquare$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueWidth,halfMaxVenueWidth))
      rightSquare$y <- rescale(rightSquare$y,to=c(bounds$bottom,bounds$top),from=c(-halfMaxVenueLength,halfMaxVenueLength)) 
    }
    else
    {
      rightSquare$x <- rescale(rightSquare$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueLength,halfMaxVenueLength))
      rightSquare$y <- rescale(rightSquare$y,to=c(bounds$bottom,bounds$top),from=c(-halfMaxVenueWidth,halfMaxVenueWidth)) 
    }}
  
  
  
  
  return(rightSquare)
}

generateLeftArc<-function(venueLength,venueWidth,interceptAngle,bounds=NA,rotate90=0)
{
  startAngle<- pi/2 - interceptAngle
  endAngle<- -pi/2 + interceptAngle
  
  left50 <- data.frame(
    x = (-venueLength/2) + 50 * cos(seq(startAngle, endAngle, length.out = 100)),
    y = 50 * sin(seq(startAngle, endAngle, length.out = 100))
  )
  
  if(rotate90!= 0)
  {
    rotateLeft <- rotate90 == -1
    new<-rotateCoordinates90(left50$x, left50$y,rotateLeft)
    left50$x <- new$x
    left50$y <- new$y
  }
  
  if(is.list(bounds))
  {
    if(rotate90!=0)
    {
      left50$x <- rescale(left50$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueWidth,halfMaxVenueWidth))
      left50$y <- rescale(left50$y,to=c(bounds$bottom,bounds$top),from=c(-halfMaxVenueLength,halfMaxVenueLength)) 
    }
    else
    {
      left50$x <- rescale(left50$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueLength,halfMaxVenueLength))
      left50$y <- rescale(left50$y,to=c(bounds$bottom,bounds$top),from=c(-halfMaxVenueWidth,halfMaxVenueWidth)) 
    }}
  
  
  
  
  return(left50)
}

generateRightArc<-function(venueLength,venueWidth,interceptAngle,bounds=NA,rotate90=0)
{
  rightArc<-generateLeftArc(venueLength,venueWidth,interceptAngle)
  rightArc$x<-rightArc$x * -1
  
  if(rotate90!= 0)
  {
    rotateLeft <- rotate90 == -1
    new<-rotateCoordinates90(rightArc$x, rightArc$y,rotateLeft)
    rightArc$x <- new$x
    rightArc$y <- new$y
  }
  
  if(is.list(bounds))
  {
    if(rotate90!=0)
    {
      rightArc$x <- rescale(rightArc$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueWidth,halfMaxVenueWidth))
      rightArc$y <- rescale(rightArc$y,to=c(bounds$bottom,bounds$top),from=c(-halfMaxVenueLength,halfMaxVenueLength)) 
    }
    else
    {
      rightArc$x <- rescale(rightArc$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueLength,halfMaxVenueLength))
      rightArc$y <- rescale(rightArc$y,to=c(bounds$bottom,bounds$top),from=c(-halfMaxVenueWidth,halfMaxVenueWidth)) 
    }}
  
  
  
  
  return(rightArc)
}

generateCentreSquare<-function(bounds=NA,rotate90=0)
{
  centreSquare<-data.frame(x=c(-25,25,25,-25,-25),
                           y=c(25,25,-25,-25,25))
  
  if(rotate90!= 0)
  {
    rotateLeft <- rotate90 == -1
    new<-rotateCoordinates90(centreSquare$x, centreSquare$y,rotateLeft)
    centreSquare$x <- new$x
    centreSquare$y <- new$y
  }
  
  if(is.list(bounds))
  {
    if(rotate90!=0)
    {
      centreSquare$x <- rescale(centreSquare$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueWidth,halfMaxVenueWidth))
      centreSquare$y <- rescale(centreSquare$y,to=c(bounds$bottom,bounds$top),from=c(-halfMaxVenueLength,halfMaxVenueLength)) 
    }
    else
    {
      centreSquare$x <- rescale(centreSquare$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueLength,halfMaxVenueLength))
      centreSquare$y <- rescale(centreSquare$y,to=c(bounds$bottom,bounds$top),from=c(-halfMaxVenueWidth,halfMaxVenueWidth)) 
    }}
  
  
  
  
  return(centreSquare)
}

getBoundaryArcInterceptAngle<-function(venueLength=160,venueWidth=141)
{
  start <- 10- (venueLength/2)
  end <- 40 - (venueLength/2)
  df <- data.frame(x = seq(start,end,by=0.1))
  df$y1<-getY50(df$x,venueLength,venueWidth)
  df$y2<-getYBoundary(df$x,venueLength,venueWidth)
  df$ydiff <- abs(df$y1 - df$y2)
  
  index <- which(df$ydiff == min(df$ydiff))
  return(asin((df$x[index]+(venueLength/2))/df$y1[index]))
  
}

getY50<-function(x, venueLength=160,venueWidth=141)
{
  return(sqrt(2500 - (x+(venueLength/2))^2))
}

getYBoundary<-function(x,venueLength=160,venueWidth=141)
{
  A<-venueLength/2
  B<-(venueWidth/2)-9.6
  
  return(9.6+sqrt(B^2 - ((B^2)/A^2)*x^2))
}

generateFieldOutline<-function(venueLength,venueWidth,bounds=NA,rotate90=0,maxDistanceFromGoal=NA)
{
  interceptAngle<-getBoundaryArcInterceptAngle(venueLength,venueWidth)
  
  result<-list(topBoundary=generateTopBoundary(venueLength,venueWidth),
               bottomBoundary=generateBottomBoundary(venueLength,venueWidth),
               leftSquare=generateLeftSquare(venueLength),
               rightSquare=generateRightSquare(venueLength),
               leftArc=generateLeftArc(venueLength,venueWidth,interceptAngle),
               rightArc=generateRightArc(venueLength,venueWidth,interceptAngle),
               centreSquare=generateCentreSquare())
  
  
  
  for(i in 1:length(result))
  {
    if(!is.na(maxDistanceFromGoal))
    {
      maxDistance <- -1 * (maxDistanceFromGoal - halfMaxVenueLength)
      
      result <- lapply(result, function(df) {
        # Filter rows based on the condition (x >= maxDistance)
        df[df$x > maxDistance, ]
      })
    }
    
    if(rotate90!= 0)
    {
      rotateLeft <- rotate90 == -1
      new<-rotateCoordinates90(result[[i]]$x, result[[i]]$y,rotateLeft)
      result[[i]]$x <- new$x
      result[[i]]$y <- new$y
    }
    
    if(is.list(bounds))
    {
      result[[i]]$x <- rescale(result[[i]]$x,to=c(bounds$left,bounds$right),from=c(-halfMaxVenueLength,halfMaxVenueLength))
      result[[i]]$y <- rescale(result[[i]]$y,to=c(bounds$top,bounds$bottom),from=c(-halfMaxVenueWidth,halfMaxVenueWidth))
    }
  }
  
  return(result)
}

#x = field length
#y = field width
#rotate90 = 0/1, 0 = left to right field, 1 = top to bottom field
#bounds = bounds to draw if adding to a larger ggplot, list with left, right, bottom, and top as integers
#eg. fieldMarkings <- generateFieldOutline(160,141,rotate90 = 1, bounds = list(left=0,right=100,bottom=0,top=200))


drawField <- function(plot,fieldMarkings)
{
  plot <- plot + 
    geom_path(data=fieldMarkings$topBoundary,aes(x,y)) +
    geom_path(data=fieldMarkings$bottomBoundary,aes(x,y)) +
    geom_path(data=fieldMarkings$leftSquare,aes(x,y)) +
    geom_path(data=fieldMarkings$rightSquare,aes(x,y)) +
    geom_path(data=fieldMarkings$leftArc,aes(x,y)) +
    geom_path(data=fieldMarkings$rightArc,aes(x,y)) +
    geom_path(data=fieldMarkings$centreSquare,aes(x,y))
  
  return(plot)
    
}


plot <- ggplot() +
  coord_fixed(ratio = 1,
              xlim = c(0,500),
              ylim = c(0,500),
              expand=FALSE)

fieldMarkings <- generateFieldOutline(160,141,bounds=list(left=0,right=160,bottom=0,top=141))
fieldMarkings2 <- generateFieldOutline(160,141,rotate90 =0,bounds=list(left=400-320,right=400,bottom=400-282,top=400))

plot <- plot %>%
  drawField(fieldMarkings) %>%
  drawField(fieldMarkings2)

plot
  
  