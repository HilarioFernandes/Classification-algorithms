source("Auxiliary_functions.R")

################################################################################
#Main analysis
#In this section we use the functions above to generate the data that
#will be analyzed later
{
  
  #This function generates the main dataframe of this project.
  #indmin and indmax control which values the index ind1 assume
  #(this index is associated to the true classifier)
  #with this function it is possible to split the simulations in up to 11 parts,
  #which is useful
  GeneratesDfAnalysis <- function(indmin,indmax){
    
    #These two vectors contain the labels that will be used in the main dataframe, 
    #as well as arguments of the relevant functions
    trueclass <- c("Vor","TreeMix","BagMix","ForMix","BooMix","SVMMix",
                   "Tree","Bag","For","Boo","SVM")
    
    Numbers <- c("Small", "Mid")
    
    #We create the dataframe with the results
    
    columnnames <- c("True Classifier", "Fitted Classifier", "n",
                      "k", "K", "Balancing index", "Fitting time",
                      "MeanPrecision","MicroPrecision","MacroPrecision","MacroRecall","MacroFscore")
    
    Results <- data.frame(matrix(ncol=12, nrow = 0))
    
    colnames(Results) <- columnnames
    
    #For each type of true classifier
    for (ind1 in indmin:indmax){
      
      #For each type of classifier we will fit
      for (ind2 in 1:5){
        
        #For each sample size
        for (ind3 in 1:2){
          
          if(ind3 == 1){
            n <- 100
          } else{
            n <- 1000
          }
          
          #For each number of predictors
          for(ind4 in 1:2){
            
            if(ind4 == 1){
              k <- floor(log(n)/2)
            } else{
              k <- floor(log(n))
            }
            
            #For each number of classes
            for (ind5 in 1:2){
              
              if(ind5 == 1){
                K <- max(c(2,floor(k/2)))
              } else{
                K <- 2*k
              }
              
              #counter that is used in generating new dataframe rows
              t <- nrow(Results)+1
              
              #debugging
              print("--------------------------------------------------------")
              print(c(t,ind1,ind2,ind3,ind4,ind5))
              
              #In some cases the fit of some classifier may fail.
              #That's why we use the function try(), in order to not skip any iterations.
              
              analysis <- NULL
              
              while(is.null(analysis)){
                
                trueclassifier <- NULL
                while(is.null(trueclassifier)){
                  
                  #The first step is generating the true classifier
                  message("Generating true classifier...",appendLF=TRUE)
                  try(trueclassifier <- GeneratesTrueClassifier(trueclass[ind1], k,K))
                  
                }
                
                #The second step is to generate n points in \mathbb{R}^k and attribute
                #the classes according to the generated true classifier
                message("Generating sample...",appendLF=TRUE)
                sample <- GeneratesSample(k,trueclassifier,n,K)
                #sample$classe <- as.factor(sample$classe)
                
                #The third step is to fit the relevant classifier
                message("Fitting classifier...",appendLF=TRUE)
                
                
                try(analysis <- FitsClassifier(trueclass[ind2+6], sample))
              }
              
              
              Results[t,1] <- trueclass[ind1]
              
              Results[t,2] <- trueclass[ind2+6]
              
              Results[t,3] <- Numbers[ind3]
              
              Results[t,4] <- Numbers[ind4]
              
              Results[t,5] <- Numbers[ind5]
              
              #In the sixth column we include the sample's "balancing index", which is
              #the quotient involving the cardinalities of the smallest and biggest
              #classes represented in the sample
              Results[t,6] <- min(unlist(lapply(X=c(1:K),FUN= function(i) length(which(sample$classe == i)))))/max(unlist(lapply(X=c(1:K),FUN= function(i) length(which(sample$classe == i)))))
              
              Results[t,7:12] <- analysis
              
            }
            
          }
          
        }
        
      }
      
    }
    
    return(Results)
  }
  
  #Here we generate the data in multiple steps, then we merge the dataframes.
  #Seeds are included for reproducibility.
  #Seeds were manually chosen since in many cases the generated data is pathological,
  #resulting in unsuccessful fits in the above loop.
  {
    set.seed(99)
    Results1to2 <- GeneratesDfAnalysis(1,2)
    write.csv(Results1to2, file = "Results1to2.csv", row.names = TRUE)
    
    set.seed(87)
    Results3 <- GeneratesDfAnalysis(3,3)
    write.csv(Results3, file = "Results3.csv", row.names = TRUE)
    
    set.seed(92)
    Results4 <- GeneratesDfAnalysis(4,4)
    write.csv(Results4, file = "Results4.csv", row.names = TRUE)
    
    set.seed(99)
    Results5to6 <- GeneratesDfAnalysis(5,6)
    write.csv(Results5to6, file = "Results5to6.csv", row.names = TRUE)
    
    set.seed(99)
    Results7to9 <- GeneratesDfAnalysis(7,9)
    write.csv(Results7to9, file = "Results7to9.csv", row.names = TRUE)
    
    set.seed(99)
    Results10to11 <- GeneratesDfAnalysis(10,11)
    write.csv(Results10to11, file = "Results10to11.csv", row.names = TRUE)
    
    Results <- rbind.data.frame(Results1to2, Results3, Results4, Results5to6, Results7to9, Results10to11)
    write.csv(Results, file = "Results.csv", row.names = TRUE)
  }
}
