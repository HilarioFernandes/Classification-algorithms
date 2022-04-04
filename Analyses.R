library(knitr)


#reading the file
Results <- read.csv("Results.csv", check.names=FALSE)[,-1]

#we rename the elements in columns n,k,K for plotting purposes
Results[,c(3:5)] <- sapply(Results[,c(3:5)], substring, 1, 3)

#we replace the NA and NaN in the three last columns with the value -1
Results$MacroPrecision <- ifelse(is.nan(Results$MacroPrecision),-1,Results$MacroPrecision)
Results$MacroPrecision <- ifelse(is.na(Results$MacroPrecision),-1,Results$MacroPrecision)

Results$MacroRecall <- ifelse(is.nan(Results$MacroRecall),-1,Results$MacroRecall)
Results$MacroRecall <- ifelse(is.na(Results$MacroRecall),-1,Results$MacroRecall)

Results$MacroFscore <- ifelse(is.nan(Results$MacroFscore),-1,Results$MacroFscore)
Results$MacroFscore <- ifelse(is.na(Results$MacroFscore),-1,Results$MacroFscore)

#We split the main dataframe depending on the fitted classifier
ResultsClassAjust <- split(Results, Results$`Fitted Classifier`)

#Execution time for each method
{
  #we define a dataframe with the quantiles of fitting times of each classifier
  Analysistemp <- as.data.frame(lapply(ResultsClassAjust, function(df) quantile(log(df$`Fitting time`))))
  par(mfrow = c(1,2))
  par(mar=c(3.1, 4.1, 1, 2.1))
  cores <- c("grey","white","grey","white","grey")
  boxplot(Analysistemp, range = 0, ylab = "Fitting time (log s)", col = cores)
  
  #modifying the dataframe
  Analysistemp <- as.data.frame(lapply(ResultsClassAjust, function(df) quantile(df$`Tempo de ajuste`)))
  Analysistemp <- t(Analysistemp[-c(2,4),])
  colnames(Analysistemp) <- c("Minimum","Median","Maximum")
  
  kable(Analysistemp, format = 'latex')
  
  #Plot
  par(mar=c(5.1, 4.1, 1, 2.1))
  plot(c(1:440)/440,sort(Results$`Fitting time`),pch=16,xlab = "Percentiles", ylab = "Fitting time (s)")
}

#Now we analyze the performance metrics for each classifier
#It is not needed to include the maximum values because they are 1
{
  #we create the dataframes with the quantiles of each metric
  Analysismeanprecision <- as.data.frame(lapply(ResultsClassAjust, function(df) quantile(df$MeanPrecision)))
  Analysismicroprecision <- as.data.frame(lapply(ResultsClassAjust, function(df) quantile(df$MicroPrecision)))
  Analysismacroprecision <- as.data.frame(lapply(ResultsClassAjust, function(df) quantile(df$MacroPrecision)))
  Analysismacrorecall <- as.data.frame(lapply(ResultsClassAjust, function(df) quantile(df$MacroRecall)))
  Analysismacrofscore <- as.data.frame(lapply(ResultsClassAjust, function(df) quantile(df$MacroFscore)))
  
  #boxplots
  layout(mat = matrix(c(1,1,2,2,3,3,
                        0,4,4,5,5,0), nrow = 2, byrow = TRUE))
  cores <- c("grey","white","grey","white","grey")
  par(mar=c(3.1, 4.1, 1, 2.1))
  boxplot(Analysismeanprecision, range = 0, ylab = "Mean precision", col = cores)
  boxplot(Analysismicroprecision, range = 0, ylab = "Micro-precision", col = cores)
  boxplot(Analysismacroprecision, range = 0, ylab = "Macro-precision", col = cores)
  boxplot(Analysismacrorecall, range = 0, ylab = "Macro-recall", col = cores)
  boxplot(Analysismacrofscore, range = 0, ylab = "Macro-Fscore", col = cores)
  
  
  #modifying the dataframes for a simplified visual analysis
  Analysismeanprecision <- t(Analysismeanprecision[-c(2,4,5),])
  colnames(Analysismeanprecision) <- c("Minimum (mean) Precision","Median (mean) Precision")
  
  Analysismicroprecision <- t(Analysismicroprecision[-c(2,4,5),])
  colnames(Analysismicroprecision) <- c("Minimum microprecision","Median microprecision")
  
  Analysismacroprecision <- t(Analysismacroprecision[-c(1,2,4,5),])
  colnames(Analysismacroprecision) <- c("Median macroprecision")
  
  Analysismacrorecall <- t(Analysismacrorecall[-c(1,2,4,5),])
  colnames(Analysismacrorecall) <- c("Median macrorecall")
  
  Analysismacrofscore <- t(Analysismacrofscore[-c(1,2,4,5),])
  colnames(Analysismacrofscore) <- c("Median macrofscore")
  
  #Merging the analyses of each metric
  Analise <- cbind(Analysismeanprecision, Analysismicroprecision, Analysismacroprecision, Analysismacrorecall, Analysismacrofscore)
  
  #plots
  plot(c(1:440)/440,sort(Results$MeanPrecision),pch=16,xlab = "Percentiles", ylab = "Mean precision")
  plot(c(1:440)/440,sort(Results$MicroPrecision),pch=16,xlab = "Percentiles", ylab = "Microprecision")
  plot(c(1:440)/440,sort(Results$MacroPrecision),pch=16,xlab = "Percentiles", ylab = "Macroprecision")
  plot(c(1:440)/440,sort(Results$MacroRecall),pch=16,xlab = "Percentiles", ylab = "Macrorecall")
  plot(c(1:440)/440,sort(Results$MacroFscore),pch=16,xlab = "Percentiles", ylab = "MacroFscore")
  
}

#Analysis of sample size's influence on classifier performance
{
  #splitting the main dataframe according to the two relevant columns
  ResultsClassAjustn <- split(Results, list(Results$`Fitted Classifier`, Results$n))
  
  #creating the dataframes with the quantiles of each metric
  Analysismeanprecision <- as.data.frame(lapply(ResultsClassAjustn, function(df) quantile(df$MeanPrecision)))
  Analysismicroprecision <- as.data.frame(lapply(ResultsClassAjustn, function(df) quantile(df$MicroPrecision)))
  Analysismacroprecision <- as.data.frame(lapply(ResultsClassAjustn, function(df) quantile(df$MacroPrecision)))
  Analysismacrorecall <- as.data.frame(lapply(ResultsClassAjustn, function(df) quantile(df$MacroRecall)))
  Analysismacrofscore <- as.data.frame(lapply(ResultsClassAjustn, function(df) quantile(df$MacroFscore)))
  
  #boxplots
  layout(mat = matrix(c(1,1,2,2,3,3,
                        0,4,4,5,5,0), nrow = 2, byrow = TRUE))
  cores <- c("grey","grey","white","white","grey","grey","white","white","grey","grey")
  par(mar=c(3.1, 4.1, 1, 2.1))
  boxplot(Analysismeanprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Mean precision", col= cores)
  boxplot(Analysismicroprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Micro-precision", col= cores)
  boxplot(Analysismacroprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-precision", col= cores)
  boxplot(Analysismacrorecall[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-recall", col= cores)
  boxplot(Analysismacrofscore[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-Fscore", col= cores)
  
  #modifying the dataframes for a simplified visual analysis
  Analysismeanprecision <- t(Analysismeanprecision[-c(2,4),])
  colnames(Analysismeanprecision) <- c("Minimum (mean) precision","Median (mean) precision","Maximum (mean) precision")

  Analysismicroprecision <- t(Analysismicroprecision[-c(2,4),])
  colnames(Analysismicroprecision) <- c("Minimum micro-precision","Median micro-precision","Maximum micro-precision") 
  
  Analysismacroprecision <- t(Analysismacroprecision[-c(2,4),])
  colnames(Analysismacroprecision) <- c("Minimum macro-precision","Median macro-precision","Maximum macro-precision")
  
  Analysismacrorecall <- t(Analysismacrorecall[-c(2,4),])
  colnames(Analysismacrorecall) <- c("Minimum macro-recall","Median macro-recall","Maximum macro-recall")
  
  Analysismacrofscore <- t(Analysismacrofscore[-c(2,4),])
  colnames(Analysismacrofscore) <- c("Minimum macro-Fscore","Median macro-Fscore", "Maximum macro-Fscore")
  
  #Merging the analyses of each metric
  Analise <- cbind(Analysismeanprecision, Analysismicroprecision, Analysismacroprecision, Analysismacrorecall, Analysismacrofscore)
  
  #plots
  par(mfrow=c(2,3))
  
  par(mar=c(5.1, 4.1, 1, 2.1))
  
  plot(c(1:220)/220,sort(Results[Results$n == "Sma",]$MeanPrecision),pch=16,xlab = "Percentiles", ylab = "Mean precision", col = "red", ylim = c(0, 1))
  points(c(1:220)/220,sort(Results[Results$n == "Mid",]$MeanPrecision),pch=16,xlab = "Percentiles", ylab = "Mean precision", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$n == "Sma",]$MicroPrecision),pch=16,xlab = "Percentiles", ylab = "Micro-precision", col = "red", ylim = c(0, 1))
  points(c(1:220)/220,sort(Results[Results$n == "Mid",]$MicroPrecision),pch=16,xlab = "Percentiles", ylab = "Micro-precision", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$n == "Sma",]$MacroPrecision),pch=16,xlab = "Percentiles", ylab = "Macro-precision", col = "red")
  points(c(1:220)/220,sort(Results[Results$n == "Mid",]$MacroPrecision),pch=16,xlab = "Percentiles", ylab = "Macro-precision", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$n == "Sma",]$MacroRecall),pch=16,xlab = "Percentiles", ylab = "Macro-recall", col = "red")
  points(c(1:220)/220,sort(Results[Results$n == "Mid",]$MacroRecall),pch=16,xlab = "Percentiles", ylab = "Macro-recall", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$n == "Sma",]$MacroFscore),pch=16,xlab = "Percentiles", ylab = "Macro-Fscore", col = "red")
  points(c(1:220)/220,sort(Results[Results$n == "Mid",]$MacroFscore),pch=16,xlab = "Percentiles", ylab = "Macro-Fscore", col = "blue")
  
  par(mar=c(0, 0, 0, 0))
  plot(0, type = "n", axes=FALSE, xlab="", ylab="")
  legend(x="center", legend=c("n = small", "n = mid"),
         col=c("red", "blue"), pch = 16, cex=1)
}

#Influence of dimensionality on each classifier's performance
{
  #splitting the main dataframe according to the two relevant columns
  ResultsClassAjustk <- split(Results, list(Results$`Fitted Classifier`, Results$k))
  
  #creating the dataframes with the quantiles of each metric
  Analysismeanprecision <- as.data.frame(lapply(ResultsClassAjustk, function(df) quantile(df$MeanPrecision)))
  Analysismicroprecision <- as.data.frame(lapply(ResultsClassAjustk, function(df) quantile(df$MicroPrecision)))
  Analysismacroprecision <- as.data.frame(lapply(ResultsClassAjustk, function(df) quantile(df$MacroPrecision)))
  Analysismacrorecall <- as.data.frame(lapply(ResultsClassAjustk, function(df) quantile(df$MacroRecall)))
  Analysismacrofscore <- as.data.frame(lapply(ResultsClassAjustk, function(df) quantile(df$MacroFscore)))
  
  #boxplots
  layout(mat = matrix(c(1,1,2,2,3,3,
                        0,4,4,5,5,0), nrow = 2, byrow = TRUE))
  par(mar=c(3.1, 4.1, 1, 2.1))
  boxplot(Analysismeanprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Mean precision", col = cores)
  boxplot(Analysismicroprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Micro-precision", col = cores)
  boxplot(Analysismacroprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-precision", col = cores)
  boxplot(Analysismacrorecall[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-recall", col = cores)
  boxplot(Analysismacrofscore[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-Fscore", col = cores)
  
  #modifying the dataframes for a simplified visual analysis
  Analysismeanprecision <- t(Analysismeanprecision[-c(2,4),])
  colnames(Analysismeanprecision) <- c("Minimum (mean) precision","Median (mean) precision","Maximum (mean) precision")

  Analysismicroprecision <- t(Analysismicroprecision[-c(2,4),])
  colnames(Analysismicroprecision) <- c("Minimum micro-precision","Median micro-precision","Maximum micro-precision") 

  Analysismacroprecision <- t(Analysismacroprecision[-c(2,4),])
  colnames(Analysismacroprecision) <- c("Minimum macro-precision","Median macro-precision","Maximum macro-precision")

  Analysismacrorecall <- t(Analysismacrorecall[-c(2,4),])
  colnames(Analysismacrorecall) <- c("Minimum macro-recall","Median macro-recall","Maximum macro-recall")

  Analysismacrofscore <- t(Analysismacrofscore[-c(2,4),])
  colnames(Analysismacrofscore) <- c("Minimum macro-Fscore","Median macro-Fscore", "Maximum macro-Fscore")
  
  #Merging the analyses of each metric
  Analise <- cbind(Analysismeanprecision, Analysismicroprecision, Analysismacroprecision, Analysismacrorecall, Analysismacrofscore)
  
  #plots
  par(mfrow=c(2,3))
  
  par(mar=c(5.1, 4.1, 1, 2.1))
  
  plot(c(1:220)/220,sort(Results[Results$k == "Mid",]$MeanPrecision),pch=16,xlab = "Percentiles", ylab = "Mean precision", col = "red", ylim = c(0, 1))
  points(c(1:220)/220,sort(Results[Results$k == "Sma",]$MeanPrecision),pch=16,xlab = "Percentiles", ylab = "Mean precision", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$k == "Mid",]$MicroPrecision),pch=16,xlab = "Percentiles", ylab = "Micro-precision", col = "red", ylim = c(0, 1))
  points(c(1:220)/220,sort(Results[Results$k == "Sma",]$MicroPrecision),pch=16,xlab = "Percentiles", ylab = "Micro-precision", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$k == "Mid",]$MacroPrecision),pch=16,xlab = "Percentiles", ylab = "Macro-precision", col = "red")
  points(c(1:220)/220,sort(Results[Results$k == "Sma",]$MacroPrecision),pch=16,xlab = "Percentiles", ylab = "Macro-precision", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$k == "Mid",]$MacroRecall),pch=16,xlab = "Percentiles", ylab = "Macro-recall", col = "red")
  points(c(1:220)/220,sort(Results[Results$k == "Sma",]$MacroRecall),pch=16,xlab = "Percentiles", ylab = "Macro-recall", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$k == "Mid",]$MacroFscore),pch=16,xlab = "Percentiles", ylab = "Macro-Fscore", col = "red")
  points(c(1:220)/220,sort(Results[Results$k == "Sma",]$MacroFscore),pch=16,xlab = "Percentiles", ylab = "Macro-Fscore", col = "blue")
  
  par(mar=c(0, 0, 0, 0))
  plot(0, type = "n", axes=FALSE, xlab="", ylab="")
  legend(x="center", legend=c("k = small", "k = mid"),
         col=c("red", "blue"), pch = 16, cex=1)
}

#Influence of dimensionality when K=2
{
  #Because of the choices of n,k,K, we have K=2 if and only if n is small or k is small
  ResultsK2 <- Results[which(Results$n == "Sma" | Results$k == "Sma"),]
  
  #Following the same process as before, but with the restricted dataframe
  ResultsClassAjustk <- split(ResultsK2, list(ResultsK2$`Fitted Classifier`, ResultsK2$k))
  
  #creating the dataframes with the quantiles of each metric
  Analysismeanprecision <- as.data.frame(lapply(ResultsClassAjustk, function(df) quantile(df$MeanPrecision)))
  Analysismicroprecision <- as.data.frame(lapply(ResultsClassAjustk, function(df) quantile(df$MicroPrecision)))
  Analysismacroprecision <- as.data.frame(lapply(ResultsClassAjustk, function(df) quantile(df$MacroPrecision)))
  Analysismacrorecall <- as.data.frame(lapply(ResultsClassAjustk, function(df) quantile(df$MacroRecall)))
  Analysismacrofscore <- as.data.frame(lapply(ResultsClassAjustk, function(df) quantile(df$MacroFscore)))
  
  #boxplots
  layout(mat = matrix(c(1,1,2,2,3,3,
                        0,4,4,5,5,0), nrow = 2, byrow = TRUE))
  par(mar=c(3.1, 4.1, 1, 2.1))
  boxplot(Analysismeanprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Mean precision", col = cores)
  boxplot(Analysismicroprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Micro-precision", col = cores)
  boxplot(Analysismacroprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-precision", col = cores)
  boxplot(Analysismacrorecall[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-recall", col = cores)
  boxplot(Analysismacrofscore[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-Fscore", col = cores)
  
  #modifying the dataframes for a simplified visual analysis
  Analysismeanprecision <- t(Analysismeanprecision[-c(2,4),])
  colnames(Analysismeanprecision) <- c("Minimum (mean) precision","Median (mean) precision","Maximum (mean) precision")
  
  Analysismicroprecision <- t(Analysismicroprecision[-c(2,4),])
  colnames(Analysismicroprecision) <- c("Minimum micro-precision","Median micro-precision","Maximum micro-precision") 
  
  Analysismacroprecision <- t(Analysismacroprecision[-c(2,4),])
  colnames(Analysismacroprecision) <- c("Minimum macro-precision","Median macro-precision","Maximum macro-precision")
  
  Analysismacrorecall <- t(Analysismacrorecall[-c(2,4),])
  colnames(Analysismacrorecall) <- c("Minimum macro-recall","Median macro-recall","Maximum macro-recall")
  
  Analysismacrofscore <- t(Analysismacrofscore[-c(2,4),])
  colnames(Analysismacrofscore) <- c("Minimum macro-Fscore","Median macro-Fscore", "Maximum macro-Fscore")
  
  #Merging the analyses of each metric
  Analise <- cbind(Analysismeanprecision, Analysismicroprecision, Analysismacroprecision, Analysismacrorecall, Analysismacrofscore)
  
  #plots
  par(mfrow=c(2,3))
  
  par(mar=c(5.1, 4.1, 1, 2.1))
  
  plot(c(1:220)/220,sort(Results[Results$k == "Mid",]$MeanPrecision),pch=16,xlab = "Percentiles", ylab = "Mean precision", col = "red", ylim = c(0, 1))
  points(c(1:220)/220,sort(Results[Results$k == "Sma",]$MeanPrecision),pch=16,xlab = "Percentiles", ylab = "Mean precision", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$k == "Mid",]$MicroPrecision),pch=16,xlab = "Percentiles", ylab = "Micro-precision", col = "red", ylim = c(0, 1))
  points(c(1:220)/220,sort(Results[Results$k == "Sma",]$MicroPrecision),pch=16,xlab = "Percentiles", ylab = "Micro-precision", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$k == "Mid",]$MacroPrecision),pch=16,xlab = "Percentiles", ylab = "Macro-precision", col = "red")
  points(c(1:220)/220,sort(Results[Results$k == "Sma",]$MacroPrecision),pch=16,xlab = "Percentiles", ylab = "Macro-precision", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$k == "Mid",]$MacroRecall),pch=16,xlab = "Percentiles", ylab = "Macro-recall", col = "red")
  points(c(1:220)/220,sort(Results[Results$k == "Sma",]$MacroRecall),pch=16,xlab = "Percentiles", ylab = "Macro-recall", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$k == "Mid",]$MacroFscore),pch=16,xlab = "Percentiles", ylab = "Macro-Fscore", col = "red")
  points(c(1:220)/220,sort(Results[Results$k == "Sma",]$MacroFscore),pch=16,xlab = "Percentiles", ylab = "Macro-Fscore", col = "blue")
  
  par(mar=c(0, 0, 0, 0))
  plot(0, type = "n", axes=FALSE, xlab="", ylab="")
  legend(x="center", legend=c("k = mid", "k = small"),
         col=c("red", "blue"), pch = 16, cex=1)
  
}

#Influence of the amount of classes on the performance of each classifier
{
  #splitting the main dataframe according to the two relevant columns
  ResultsClassAjustK <- split(Results, list(Results$`Fitted Classifier`, Results$K))
  
  #creating the dataframes with the quantiles of each metric
  Analysismeanprecision <- as.data.frame(lapply(ResultsClassAjustK, function(df) quantile(df$MeanPrecision)))
  Analysismicroprecision <- as.data.frame(lapply(ResultsClassAjustK, function(df) quantile(df$MicroPrecision)))
  Analysismacroprecision <- as.data.frame(lapply(ResultsClassAjustK, function(df) quantile(df$MacroPrecision)))
  Analysismacrorecall <- as.data.frame(lapply(ResultsClassAjustK, function(df) quantile(df$MacroRecall)))
  Analysismacrofscore <- as.data.frame(lapply(ResultsClassAjustK, function(df) quantile(df$MacroFscore)))
  
  #boxplots
  layout(mat = matrix(c(1,1,2,2,3,3,
                        0,4,4,5,5,0), nrow = 2, byrow = TRUE))
  par(mar=c(3.1, 4.1, 1, 2.1))
  
  boxplot(Analysismeanprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Mean precision", col = cores)
  boxplot(Analysismicroprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Micro-precision", col = cores)
  boxplot(Analysismacroprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-precision", col = cores)
  boxplot(Analysismacrorecall[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-recall", col = cores)
  boxplot(Analysismacrofscore[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-Fscore", col = cores)
  
  #modifying the dataframes for a simplified visual analysis
  Analysismeanprecision <- t(Analysismeanprecision[-c(2,4),])
  colnames(Analysismeanprecision) <- c("Minimum (mean) precision","Median (mean) precision","Maximum (mean) precision")

  Analysismicroprecision <- t(Analysismicroprecision[-c(2,4),])
  colnames(Analysismicroprecision) <- c("Minimum micro-precision","Median micro-precision","Maximum micro-precision") 

  Analysismacroprecision <- t(Analysismacroprecision[-c(2,4),])
  colnames(Analysismacroprecision) <- c("Minimum macro-precision","Median macro-precision","Maximum macro-precision")

  Analysismacrorecall <- t(Analysismacrorecall[-c(2,4),])
  colnames(Analysismacrorecall) <- c("Minimum macro-recall","Median macro-recall","Maximum macro-recall")

  Analysismacrofscore <- t(Analysismacrofscore[-c(2,4),])
  colnames(Analysismacrofscore) <- c("Minimum macro-Fscore","Median macro-Fscore", "Maximum macro-Fscore")
  
  #Merging the analyses of each metric
  Analise <- cbind(Analysismeanprecision, Analysismicroprecision, Analysismacroprecision, Analysismacrorecall, Analysismacrofscore)
  
  #plots
  par(mfrow=c(2,3))
  
  par(mar=c(5.1, 4.1, 1, 2.1))
  
  plot(c(1:220)/220,sort(Results[Results$K == "Mid",]$MeanPrecision),pch=16,xlab = "Percentiles", ylab = "Mean precision", col = "red", ylim = c(0, 1))
  points(c(1:220)/220,sort(Results[Results$K == "Sma",]$MeanPrecision),pch=16,xlab = "Percentiles", ylab = "Mean precision", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$K == "Mid",]$MicroPrecision),pch=16,xlab = "Percentiles", ylab = "Micro-precision", col = "red", ylim = c(0, 1))
  points(c(1:220)/220,sort(Results[Results$K == "Sma",]$MicroPrecision),pch=16,xlab = "Percentiles", ylab = "Micro-precision", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$K == "Mid",]$MacroPrecision),pch=16,xlab = "Percentiles", ylab = "Macro-precision", col = "red")
  points(c(1:220)/220,sort(Results[Results$K == "Sma",]$MacroPrecision),pch=16,xlab = "Percentiles", ylab = "Macro-precision", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$K == "Mid",]$MacroRecall),pch=16,xlab = "Percentiles", ylab = "Macro-recall", col = "red")
  points(c(1:220)/220,sort(Results[Results$K == "Sma",]$MacroRecall),pch=16,xlab = "Percentiles", ylab = "Macro-recall", col = "blue")
  
  plot(c(1:220)/220,sort(Results[Results$K == "Mid",]$MacroFscore),pch=16,xlab = "Percentiles", ylab = "Macro-Fscore", col = "red")
  points(c(1:220)/220,sort(Results[Results$K == "Sma",]$MacroFscore),pch=16,xlab = "Percentiles", ylab = "Macro-Fscore", col = "blue")
  
  par(mar=c(0, 0, 0, 0))
  plot(0, type = "n", axes=FALSE, xlab="", ylab="")
  legend(x="center", legend=c("K = mid", "K = small"),
         col=c("red", "blue"), pch = 16, cex=1)
}

#Influence of the true classifier on the performance of each classifier
{
  #splitting the main dataframe according to the two relevant columns
  ResultsClassVerdClassAjust <- split(Results, list(Results$`Fitted Classifier`, Results$`True Classifier`))
  
  #keeping only the cases where the true and fitted classifiers are similar
  #(eg SVM.SVM or SVM.SVMMist) or the cases where the true classifier is Voronoi (SVM.Vor)
  ResultsClassVerdClassAjust <- ResultsClassVerdClassAjust[c(1,6,51,12,17,52,23,28,53,34,39,54,45,50,55)]
  
  #creating the dataframes with the quantiles of each metric
  Analysismeanprecision <- as.data.frame(lapply(ResultsClassVerdClassAjust, function(df) quantile(df$MeanPrecision)))
  Analysismicroprecision <- as.data.frame(lapply(ResultsClassVerdClassAjust, function(df) quantile(df$MicroPrecision)))
  Analysismacroprecision <- as.data.frame(lapply(ResultsClassVerdClassAjust, function(df) quantile(df$MacroPrecision)))
  Analysismacrorecall <- as.data.frame(lapply(ResultsClassVerdClassAjust, function(df) quantile(df$MacroRecall)))
  Analysismacrofscore <- as.data.frame(lapply(ResultsClassVerdClassAjust, function(df) quantile(df$MacroFscore)))
  
  #boxplots
  layout(mat = matrix(c(1,1,2,2,3,3,
                        0,4,4,5,5,0), nrow = 2, byrow = TRUE))
  par(mar = c(6.9, 4.1, 1, 2.1))
  cores <- c("grey","grey","grey","white","white","white","grey","grey","grey","white","white","white","grey","grey","grey")
  nomes = c("Arv.Arv"," "," ","Bag.Bag","","","Boo.Boo","","","Flo.Flo","","","SVM.SVM","","")
  boxplot(Analysismeanprecision, range = 0, ylab = "Mean precision", las = 2, names= nomes, col = cores)
  boxplot(Analysismicroprecision, range = 0, ylab = "Micro-precision",  las=2, names= nomes, col = cores)
  boxplot(Analysismacroprecision, range = 0, ylab = "Macro-precision",  las=2, names= nomes, col = cores)
  boxplot(Analysismacrorecall, range = 0, ylab = "Macro-recall",  las=2, names= nomes, col = cores)
  boxplot(Analysismacrofscore, range = 0, ylab = "Macro-Fscore",  las=2, names= nomes, col = cores)
  
  #modifying the dataframes for a simplified visual analysis
  Analysismeanprecision <- t(Analysismeanprecision[-c(2,4),])
  colnames(Analysismeanprecision) <- c("Minimum (mean) precision","Median (mean) precision","Maximum (mean) precision")

  Analysismicroprecision <- t(Analysismicroprecision[-c(2,4),])
  colnames(Analysismicroprecision) <- c("Minimum micro-precision","Median micro-precision","Maximum micro-precision")

  Analysismacroprecision <- t(Analysismacroprecision[-c(2,4),])
  colnames(Analysismacroprecision) <- c("Minimum macro-precision","Median macro-precision","Maximum macro-precision")

  Analysismacrorecall <- t(Analysismacrorecall[-c(2,4),])
  colnames(Analysismacrorecall) <- c("Minimum macro-recall","Median macro-recall","Maximum macro-recall")

  Analysismacrofscore <- t(Analysismacrofscore[-c(2,4),])
  colnames(Analysismacrofscore) <- c("Minimum macro-Fscore","Median macro-Fscore","Maximum macro-Fscore")
  
  #Merging the analyses of each metric
  Analise <- cbind(Analysismeanprecision, Analysismicroprecision, Analysismacroprecision, Analysismacrorecall, Analysismacrofscore)
  
}

#comparison of classifiers when the true classifier is Voronoi
{
  ResultsClassVerdVor <- ResultsClassVerdClassAjust[c(3,6,9,12,15)]
  
  #creating the dataframes with the quantiles of each metric
  Analysismeanprecision <- as.data.frame(lapply(ResultsClassVerdVor, function(df) quantile(df$MeanPrecision)))
  Analysismicroprecision <- as.data.frame(lapply(ResultsClassVerdVor, function(df) quantile(df$MicroPrecision)))
  Analysismacroprecision <- as.data.frame(lapply(ResultsClassVerdVor, function(df) quantile(df$MacroPrecision)))
  Analysismacrorecall <- as.data.frame(lapply(ResultsClassVerdVor, function(df) quantile(df$MacroRecall)))
  Analysismacrofscore <- as.data.frame(lapply(ResultsClassVerdVor, function(df) quantile(df$MacroFscore)))
  
  #boxplots
  par(mar=c(3.1, 4.1, 1, 2.1))
  cores <- c("grey","white","grey","white","grey")
  
  boxplot(Analysismeanprecision, range = 0, ylab = "Mean precision", col = cores)
  boxplot(Analysismicroprecision, range = 0, ylab = "Micro-precision", col = cores)
  boxplot(Analysismacroprecision, range = 0, ylab = "Macro-precision", col = cores)
  boxplot(Analysismacrorecall, range = 0, ylab = "Macro-recall", col = cores)
  boxplot(Analysismacrofscore, range = 0, ylab = "Macro-Fscore", col = cores)
  
  #modifying the dataframes for a simplified visual analysis
  Analysismeanprecision <- t(Analysismeanprecision[-c(2,4),])
  colnames(Analysismeanprecision) <- c("Minimum (mean) precision","Median (mean) precision","Maximum (mean) precision")

  Analysismicroprecision <- t(Analysismicroprecision[-c(2,4),])
  colnames(Analysismicroprecision) <- c("Minimum micro-precision","Median micro-precision","Maximum micro-precision")

  Analysismacroprecision <- t(Analysismacroprecision[-c(1,2,4),])
  colnames(Analysismacroprecision) <- c("Median macro-precision","Maximum macro-precision")

  Analysismacrorecall <- t(Analysismacrorecall[-c(1,2,4),])
  colnames(Analysismacrorecall) <- c("Median macro-recall","Maximum macro-recall")

  Analysismacrofscore <- t(Analysismacrofscore[-c(1,2,4),])
  colnames(Analysismacrofscore) <- c("Median macro-Fscore","Maximum macro-Fscore")
  
  #Merging the analyses of each metric
  Analise <- cbind(Analysismeanprecision, Analysismicroprecision, Analysismacroprecision, Analysismacrorecall, Analysismacrofscore)
  
}

#Influence of balancing index on the existe of the macro-metrics
{
  par(mfrow=c(1,1))
  par(mar=c(4.6, 4.1, 1, 2.1))
  plot(c(1:440)/440,sort(Results$`Balancing index`),pch=16,xlab = "Percentiles", ylab = "Balancing index")
  
  #Reordering the dataframe rows according to the balancing indexes
  ResultsIndiceBal <- Results[order(Results$`Balancing index`),] 
  
  #Simulations where macro-precision, macro-recall and macro-Fscore don't exist
  ResultsMacroPrecisao <- Results[Results$MacroPrecision < 0,]
  ResultsMacroRecall <- Results[Results$MacroRecall < 0,]
  ResultsMacroFscore <- Results[Results$MacroFscore < 0,]
  
  #Defining a dataframe with the quantiles of the above metrics
  #The element [1,2], for example, indicates that 25% of the simulations
  #where the macro-precision didn't exist had balancing index lesser or equal to 0.05374078.
  #The last column indicates that all simulations where the metrics didn't exist
  #had balancing indexes lesser or equal to 0.6
  Analise <- rbind(quantile(ResultsMacroPrecisao$`Balancing index`),
                   quantile(ResultsMacroRecall$`Balancing index`),
                   quantile(ResultsMacroFscore$`Balancing index`))
  
  rownames(Analise) <- c("MacroPrecision","MacroRecall","MacroFscore")
  
  kable(Analise, format = 'latex')
  
  #Our objective is to calculate, given a balancing index, the percentage of simulations
  #(with balancing index greater or equal to the one given) that didn't have some of
  #the metrics above
  par(mar=c(5.1, 7.1, 1, 2.1))
  
  proporcoes <- unlist(lapply(X = c(1:440), FUN = function(i)  sum(ResultsIndiceBal[c(i:440),]$MacroPrecision < 0)/440 ) )
  plot(ResultsIndiceBal$`Balancing index`,100*proporcoes,type = "l",xlab = "Balancing index",
       ylab = "% of simulations with balancing index \n greater than given \n for which the metric is not defined")

  proporcoes <- unlist(lapply(X = c(1:440), FUN = function(i)  sum(ResultsIndiceBal[c(i:440),]$MacroRecall < 0)/440 ) )
  lines(ResultsIndiceBal$`Balancing index`,100*proporcoes, col = "red")

  proporcoes <- unlist(lapply(X = c(1:440), FUN = function(i)  sum(ResultsIndiceBal[c(i:440),]$MacroFscore < 0)/440 ) )
  lines(ResultsIndiceBal$`Balancing index`,100*proporcoes, col = "blue")
  
  legend(x=0.6,y=100*0.23, legend=c("Macro-precision", "Macro-recall", "Macro-Fscore"),
         col=c("red","black", "blue"), lty = 1, seg.len = 0.5, cex=0.75, bty = "n")
}

#Influence of balancing index on performance metrics for each classifer
{
  #Calculating the median of the balancing index
  mediana <- median(Results$`Balancing index`)
  
  #splitting the dataframes corresponding to each true classifier based on the median
  ResultsClassAjustIndBal <- split(Results, list(Results$`Fitted Classifier`, Results$`Balancing index` >= mediana))
  
  #creating the dataframes with the quantiles of each  metric
  Analysismeanprecision <- as.data.frame(lapply(ResultsClassAjustIndBal, function(df) quantile(df$MeanPrecision)))
  Analysismicroprecision <- as.data.frame(lapply(ResultsClassAjustIndBal, function(df) quantile(df$MicroPrecision)))
  Analysismacroprecision <- as.data.frame(lapply(ResultsClassAjustIndBal, function(df) quantile(df$MacroPrecision)))
  Analysismacrorecall <- as.data.frame(lapply(ResultsClassAjustIndBal, function(df) quantile(df$MacroRecall)))
  Analysismacrofscore <- as.data.frame(lapply(ResultsClassAjustIndBal, function(df) quantile(df$MacroFscore)))
  
  #boxplots
  layout(mat = matrix(c(1,1,2,2,3,3,
                        0,4,4,5,5,0), nrow = 2, byrow = TRUE))
  par(mar=c(3.1, 4.1, 1, 2.1))
  
  cores <- c("grey","grey","white","white","grey","grey","white","white","grey","grey")
  boxplot(Analysismeanprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Mean precision", col = cores)
  boxplot(Analysismicroprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Micro-precision", col = cores)
  boxplot(Analysismacroprecision[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-precision", col = cores)
  boxplot(Analysismacrorecall[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-recall", col = cores)
  boxplot(Analysismacrofscore[,c(6,1,7,2,8,3,9,4,10,5)], range = 0, ylab = "Macro-Fscore", col = cores)
  
  #modifying the dataframes for a simplified visual analysis
  Analysismeanprecision <- t(Analysismeanprecision[-c(2,4),])
  colnames(Analysismeanprecision) <- c("Minimum (mean) precision","Median (mean) precision","Maximum (mean) precision")
  
  Analysismicroprecision <- t(Analysismicroprecision[-c(2,4),])
  colnames(Analysismicroprecision) <- c("Minimum micro-precision","Median micro-precision","Maximum micro-precision") 
  
  Analysismacroprecision <- t(Analysismacroprecision[-c(2,4),])
  colnames(Analysismacroprecision) <- c("Minimum macro-precision","Median macro-precision","Maximum macro-precision")
  
  Analysismacrorecall <- t(Analysismacrorecall[-c(2,4),])
  colnames(Analysismacrorecall) <- c("Minimum macro-recall","Median macro-recall","Maximum macro-recall")
  
  Analysismacrofscore <- t(Analysismacrofscore[-c(2,4),])
  colnames(Analysismacrofscore) <- c("Minimum macro-Fscore","Median macro-Fscore", "Maximum macro-Fscore")
  
  #merging the analyses of each metric
  Analise <- cbind(Analysismeanprecision, Analysismicroprecision, Analysismacroprecision, Analysismacrorecall, Analysismacrofscore)
  
  
  #plots
  num1 <- sum(Results$`Balancing index` >= mediana)
  num2 <- 440-num1
  
  par(mfrow=c(2,3))
  
  par(mar=c(5.1, 4.1, 1, 2.1))
  
  plot(c(1:num1)/num1,sort(Results[Results$`Balancing index` >= mediana,]$MeanPrecision),pch=16,xlab = "Percentiles", ylab = "Mean precision", col = "blue")
  points(c(1:num2)/num2,sort(Results[Results$`Balancing index` < mediana,]$MeanPrecision),pch=16,xlab = "Percentiles", ylab = "Mean precision", col = "red")

  plot(c(1:num1)/num1,sort(Results[Results$`Balancing index` >= mediana,]$MicroPrecision),pch=16,xlab = "Percentiles", ylab = "Micro-precision", col = "blue")
  points(c(1:num2)/num2,sort(Results[Results$`Balancing index` < mediana,]$MicroPrecision),pch=16,xlab = "Percentiles", ylab = "Micro-precision", col = "red")

  plot(c(1:num1)/num1,sort(Results[Results$`Balancing index` >= mediana,]$MacroPrecision),pch=16,xlab = "Percentiles", ylab = "Macro-precision", col = "blue")
  points(c(1:num2)/num2,sort(Results[Results$`Balancing index` < mediana,]$MacroPrecision),pch=16,xlab = "Percentiles", ylab = "Macro-precision", col = "red")

  plot(c(1:num1)/num1,sort(Results[Results$`Balancing index` >= mediana,]$MacroRecall),pch=16,xlab = "Percentiles", ylab = "Macro-recall", col = "blue")
  points(c(1:num2)/num2,sort(Results[Results$`Balancing index` < mediana,]$MacroRecall),pch=16,xlab = "Percentiles", ylab = "Macro-recall", col = "red")

  plot(c(1:num1)/num1,sort(Results[Results$`Balancing index` >= mediana,]$MacroFscore),pch=16,xlab = "Percentiles", ylab = "Macro-Fscore", col = "blue")
  points(c(1:num2)/num2,sort(Results[Results$`Balancing index` < mediana,]$MacroFscore),pch=16,xlab = "Percentiles", ylab = "Macro-Fscore", col = "red")
  
  par(mar=c(0, 0, 0, 0))
  plot(0, type = "n", axes=FALSE, xlab="", ylab="")
  legend("center", legend=c("bal. >= median", "bal. < media"),
         col=c("red", "blue"), pch = 16, cex=1, bty = "n")
}

