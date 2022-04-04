source("Auxiliary_functions.R")

################################################################################
#Plots
{
  #In this plot we illustrate the proccess used to generate a Voronoi classifier
  
  #PLOT 1: Illustration of the method used to generate a Voronoi partition
  {
    set.seed(99)
    par(mfrow=c(1,3))
    
    points <- data.frame(x=runif(3, min = -1, max = 1), y=runif(3, min = -1, max = 1))
    
    #We plot the starting points
    plot(y ~ x, data=points, type='n', xlim=c(-1,1), ylim = c(-1,1), xlab= "", ylab = "")
    text(points[1,],label=1,col='black')
    text(points[2,],label=2,col='black')
    text(points[3,],label=3,col='black')
    
    #We generate a Voronoi classifier for K=2 with these points and calculate
    #the class of each generator.
    classifier <- GenerateVoronoiClassifier(points,2,"euclidean")
    label1 <- classifier(points[1,])
    label2 <- classifier(points[2,])
    label3 <- classifier(points[3,])
    
    #We plot the generators and their classes
    plot(y ~ x, data=points, type='n', xlim=c(-1,1), ylim = c(-1,1), xlab= "", ylab = "")
    text(points[1,],label=label1,col='black')
    text(points[2,],label=label2,col='black')
    text(points[3,],label=label3,col='black')
    
    #We generate a grid to show the classification regions
    grid <- expand.grid(xcoords = seq(from = -1, to = 1, by = 0.025),
                        ycoords = seq(from = -1, to = 1, by = 0.025))
    
    #We create two columns in the grid dataframe, one with the class of each point
    #and another with the corresponding color
    #grid$classe <- apply(X = grid, MARGIN = 1, FUN = classifier)
    grid$classe <- classifier(grid)
    
    grid$color <- apply(X=as.matrix(grid$classe), MARGIN = 1, FUN = function(m){ if (m == 1) "orange" else "blue" })
    
    #plot
    plot(x=grid$xcoords, y=grid$ycoords, col=grid$color,pch=20,cex=1, xlab= "", ylab = "")
    
    points(y ~ x, data=points, xlim=c(-1,1), ylim = c(-1,1), pch=15, cex = 2.5)
    text(points[1,],label=label1,col='white')
    text(points[2,],label=label2,col='white')
    text(points[3,],label=label3,col='white')
  }
  
  #PLOT 2: Non-connectedness of the partition, depending on n and K  (n=15 and K=3)
  {
    set.seed(99)
    par(mfrow=c(1,2))
    
    points <- data.frame(x=runif(15, min = -1, max = 1), y=runif(15, min = -1, max = 1))
    
    #We plot the starting points
    plot(y ~ x, data=points, pch = 15, xlim=c(-1,1), ylim = c(-1,1), xlab= "", ylab = "")
    
    #We generate a Voronoi classifier for K=3 with these points and calculate
    #the class of each generator.
    classifier <- GenerateVoronoiClassifier(points,3,"euclidean")
    
    #We generate a grid to show the classification regions
    grid <- expand.grid(xcoords = seq(from = -1, to = 1, by = 0.025),
                        ycoords = seq(from = -1, to = 1, by = 0.025))
    
    
    #We create two columns in the grid dataframe, one with the class of each point
    #and another with the corresponding color
    grid$classe <- classifier(grid)
    
    grid$color <- apply(X=as.matrix(grid$classe), MARGIN = 1, FUN = function(m){ if (m == 1) "orange" else if (m == 2) "blue" else "red" })
    
    #plot
    plot(x=grid$xcoords, y=grid$ycoords, col=grid$color,pch=20,cex=1, xlab= "", ylab = "")
    points(y ~ x, data=points, xlim=c(-1,1), ylim = c(-1,1), pch=15, cex = 1)
  }
  
  #PLOT 3: mixed classifier
  {
    set.seed(99)
    par(mfrow=c(1,2))
    
    points <- data.frame(x=runif(75, min = -1, max = 1), y=runif(75, min = -1, max = 1))
    
    #The admissible classifier has as a boundary the circumference of radius 1 and
    #centered in the origin
    admissivelteste <- function(ponto){
      if(ponto[2] > ponto[1] ){
        return(as.factor(1))
      }
      if(ponto[2] < ponto[1]){
        return(as.factor(2))
      }
    }
    
    #This function is a matrix version of the function above
    admissivelteste2 <- function(points){
      return(apply(X = points, MARGIN = 1, FUN = admissivelteste))
    }
    
    #We calculate the mixed classifier for 25 points
    mixedclassifier <- GeneratesMixedClassifier(points[1:25,],admissivelteste2,2,"euclidean",TRUE)
    
    #We generate a grid to show the classification regions
    grid <- expand.grid(xcoords = seq(from = -1, to = 1, by = 0.025),
                        ycoords = seq(from = -1, to = 1, by = 0.025))
    
    
    #We create two columns in the grid dataframe, one with the class of each point
    #and another with the corresponding color
    #grid$classe <- apply(X = grid, MARGIN = 1, FUN = mixedclassifier)
    grid$classe <- mixedclassifier(grid)
    
    grid$color <- apply(X=as.matrix(grid$classe), MARGIN = 1, FUN = function(m){ if (m == 1) "orange" else "blue"})
    
    #plot
    plot(x=grid$xcoords, y=grid$ycoords, col=grid$color,pch=20,cex=1, xlab= "", ylab = "")
    points(y ~ x, data=points[1:25,], xlim=c(-1,1), ylim = c(-1,1), pch=15, cex = 1)
    lines(x=c(-1,1), y=c(-1,1))
    
    #Now we update the mixed classifier for all 50 points
    classificadorMisto2 <- GeneratesMixedClassifier(points,admissivelteste2,2,"euclidean",TRUE)
    
    #We update the columns of the grid's dataframe
    grid <- subset(grid,select=-c(3,4))
    #grid$classe <- apply(X = grid, MARGIN = 1, FUN = classificadorMisto2)
    grid$classe <- classificadorMisto2(grid)
    grid$color <- apply(X=as.matrix(grid$classe), MARGIN = 1, FUN = function(m){ if (m == 1) "orange" else "blue"})
    
    #plot
    plot(x=grid$xcoords, y=grid$ycoords, col=grid$color,pch=20,cex=1, xlab= "", ylab = "")
    points(y ~ x, data=points, xlim=c(-1,1), ylim = c(-1,1), pch=15, cex = 1)
    lines(x=c(-1,1), y=c(-1,1))
  }
  
  #PLOT 4: Admissible true classifiers
  {
    set.seed(99)
    #par(mfrow=c(2,3))
    par(mar=c(1,1,1,1))
    layout(mat = matrix(c(1,1,2,2,3,3,
                          0,4,4,5,5,0), nrow = 2, byrow = TRUE))
    
    #We generate the grid
    grid <- expand.grid(V1 = seq(from = -1, to = 1, by = 0.025),
                        V2 = seq(from = -1, to = 1, by = 0.025))
    
    nomes <- c("Tree","Bag", "For", "Boo", "SVM")
    
    for (i in 1:5){
      #We generate the classifier
      admclassifier <- GeneratesAdmissibleClassifier(nomes[i],2,3)
      
      #We classify the grid's points and attribute colors
      grid$classe <- admclassifier(grid[,1:2])
      
      grid$color <- apply(X=as.matrix(grid$classe), MARGIN = 1, FUN = function(m){ if (m == 1) "orange" else if (m == 2) "blue" else "red" })
      
      #plot
      plot(x=grid[,1], y=grid[,2], col=grid$color,pch=20,cex=1, xlab= "", ylab = "")
      
    }
    
  }
  
  #PLOT 5: Mixed classifiers
  {
    set.seed(99)
    par(mfcol=c(2,5))
    par(mar=c(1,1,1,1))
    #layout(mat = matrix(c(1,1,2,2,3,3,0,4,4,5,5,0), nrow = 2, byrow = TRUE))
    
    #We generate the grid
    grid <- expand.grid(V1 = seq(from = -1, to = 1, by = 0.025),
                        V2 = seq(from = -1, to = 1, by = 0.025))
    
    #We create a generating set (Voronoi)
    generator <- GeneratePoints(50,2)
    
    nomes <- c("Arv","Bag", "Flo", "Boo", "SVM")
    
    for (i in 1:5){
      
      print(i)
      
      #We generate the classifier
      admclassifier <- GeneratesAdmissibleClassifier(nomes[i],2,3)
      
      classificadormist <- GeneratesMixedClassifier(generator,admclassifier,3,"euclidean",FALSE)
      
      #We classify the grid's points and attribute colors (admissible classifier)
      grid$classe <- admclassifier(grid[,1:2])
      grid$color <- apply(X=as.matrix(grid$classe), MARGIN = 1, FUN = function(m){ if (m == 1) "orange" else if (m == 2) "blue" else "red" })
      
      #plot
      plot(x=grid[,1], y=grid[,2], col=grid$color,pch=20,cex=1, xlab= "", ylab = "")
      
      #We classify the grid's points and attribute colors (mixed classifier)
      grid$classe <- classificadormist(grid[,1:2])
      grid$color <- apply(X=as.matrix(grid$classe), MARGIN = 1, FUN = function(m){ if (m == 1) "orange" else if (m == 2) "blue" else "red" })
      
      #plot
      plot(x=grid[,1], y=grid[,2], col=grid$color,pch=20,cex=1, xlab= "", ylab = "")
      
    }
    
  }
}