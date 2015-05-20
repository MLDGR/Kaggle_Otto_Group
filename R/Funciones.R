source("librerias.R")

#' Feature Selection using H2O Random Forest
#'
#' @param x_train predictors
#' @param y_train labels
#'
#' @examples
#' ## Select Best Features
#' h2o_feaSelect(x_train, y_train)


h2o_feaSelect <- function(x_train, y_train,
                          type = "classification",
                          n_fold = 5,
                          n_repeat = 3,
                          n_tree = 100,
                          n_size = c(60:70),
                          n_seed = 1234,
                          n_threads = -1,
                          plot_graph = F,
                          balance = TRUE) {
  
  ## Combine x_train and y_train
  df_train <- data.frame(x_train, y = y_train)
  
  ## Load H2O R package (import method doesn't work for now as it is not on CRAN)
  suppressMessages(library(h2o))
  
  ## Initiate H2O Cluster
  localH2O <- h2o.init(nthreads = n_threads)
  train_hex <- as.h2o(localH2O, df_train)
  
  ## ===========================================================================
  ## Calculate Variable Importance using H2O Random Forest
  ## ===========================================================================
  
  ## Empty Shells
  df_z <- data.frame(matrix(NA, nrow = ncol(x_train), ncol = n_repeat + 1))
  colnames(df_z) <- c("var", paste0("repeat_", 1:n_repeat))
  df_z[, 1] <- colnames(x_train)
  
  ## Loop (at least 3 times)
  for (nn_repeat in 1:max(c(3, n_repeat))) {
    
    ## Train Model
    if (type == "classification") {
      model <- h2o.randomForest(x = 1:ncol(x_train),
                                y = ncol(x_train) + 1,
                                data = train_hex,
                                classification = TRUE,
                                ntree = n_tree,
                                importance = TRUE,
                                balance.classes = balance)
    } else {
      model <- h2o.randomForest(x = 1:ncol(x_train),
                                y = ncol(x_train) + 1,
                                data = train_hex,
                                classification = FALSE,
                                ntree = n_tree,
                                importance = TRUE)
    }
    
    ## Store Z-Scores
    df_z[, nn_repeat + 1] <- as.numeric(model@model$varimp[3, ])
    
  }
  
  ## Median
  df_z$median <- apply(df_z[, -1], 1, median)
  
  ## Sort
  order_z <- order(df_z$median, decreasing = TRUE)
  df_z <- df_z[order_z, ]
  
  ## Display
  cat("The 5 Most Important Variables:\n")
  print(head(df_z[, c(1, ncol(df_z))]))
  
  ## ===========================================================================
  ## Iterative Steps
  ## ===========================================================================
  
  ## Set Seed
  set.seed(n_seed)
  
  ## Empty Shell
  df_result_all <- c()
  
  ## Loop
  for (nn_repeat in 1:n_repeat) {
    
    ## Random Split
    rand_fold <- createFolds(y_train, k = n_fold)
    
    ## Empty Shells
    df_result <- data.frame(matrix(NA, nrow = n_fold, ncol = length(n_size) + 1))
    colnames(df_result) <- c("fold", n_size)
    df_result$fold <- 1:n_fold
    
    ## Iterative
    for (nn_size in 1:length(n_size)) {
      
      ## Extract Variables
      var_temp <- as.character(df_z[1:n_size[nn_size], 1])
      
      ## CV Loop
      for (nn_fold in 1:n_fold) {
        
        ## Display
        cat("\n[deepr]: CV Runs ... Repeat: ", nn_repeat, "/", n_repeat, " ... ",
            "Size: ", nn_size, "/", length(n_size), " ... ",
            "Fold: ", nn_fold, "/", n_fold, " ... ",
            "No. of Variables: ", n_size[nn_size], "\n",
            sep = "")
        
        ## Extract rows
        row_train <- as.integer(unlist(rand_fold[-nn_fold]))
        row_valid <- as.integer(unlist(rand_fold[nn_fold]))
        
        ## H2O Random Forest
        if (type == "classification") {
          model <- h2o.randomForest(x = var_temp,
                                    y = ncol(x_train) + 1,
                                    data = train_hex[row_train,],
                                    classification = TRUE,
                                    ntree = n_tree,
                                    importance = FALSE,
                                    balance.classes = balance)
        } else {
          model <- h2o.randomForest(x = var_temp,
                                    y = ncol(x_train) + 1,
                                    data = train_hex[row_train,],
                                    classification = FALSE,
                                    ntree = n_tree,
                                    importance = FALSE)
        }
        
        ## OOB Performance
        yy_valid <- as.data.frame(h2o.predict(model, train_hex[row_valid,]))
        if (type == "classification") {
          score_temp <- (confusionMatrix(yy_valid[,1], y_train[row_valid]))$overall[1]
          cat("OOB Accuracy:", round(score_temp, 4), "\n")
        } else {
          ## add regression metric later
        }
        
        ## Update df_result
        df_result[nn_fold, nn_size + 1] <- as.numeric(score_temp)
        
      }
      
    }
    
    ## Store
    df_result_all <- rbind(df_result_all, df_result)
    
  }
  
  ## ===========================================================================
  ## Summarise Results
  ## ===========================================================================
  
#   ## Reshape
#   df_ggplot <- df_result_all %>% gather(fold)
#   df_ggplot <- df_ggplot[, -1]
#   
#   ## Rename
#   if (type == "classification") {
#     colnames(df_ggplot) <- c("n_var", "accuracy")
#   } else {
#     colnames(df_ggplot) <- c("n_var", "RMSE")
#   }
#   
#   ## ggplot2
#   p <- ggplot(df_ggplot, aes(n_var, accuracy)) + geom_boxplot()
#   if (plot_graph) print(p)
  
  ## Stats
  df_summary <- data.frame(n_var = n_size,
                           avg_score = apply(df_result_all[, -1], 2, mean),
                           med_score = apply(df_result_all[, -1], 2, median),
                           sd_score = apply(df_result_all[, -1], 2, sd))
  df_summary$avg_med <- (df_summary$avg_score + df_summary$med_score) / 2
  df_summary$avg_med_sd <- df_summary$avg_med / df_summary$sd_score
  
  ## Determine best set - Rule 1: Best Median Score
  best_loc <- which(df_summary$med_score == max(df_summary$med_score))
  
  ## Rule 2: Best of Avg + Median Score
  if (length(best_loc) > 1) {
    best_loc <- which(df_summary$avg_med == max(df_summary$avg_med))
  }
  
  ## Rule 3: Best of Avg + Median Score and lowest SD
  if (length(best_loc) > 1) {
    best_loc <- which(df_summary$avg_med == max(df_summary$avg_med_sd))
  }
  
  ## Identify best set
  best_n_var <- df_summary[best_loc, 1]
  best_var <- df_z[1:best_n_var, 1]
  
  ## ===========================================================================
  ## Outputs
  ## ===========================================================================
  
  output <- list(best_var = best_var,
                 result = df_result_all,
                 summary = df_summary,
                 zscores = df_z)
  return(output)
  
}


PrimerasClases<-function(pred2){
  Orden2<-apply(pred2,1,function(pred2){
    pred2=pred2[-1]
    pred2<-order(pred2,decreasing=T)
    pred2
  })
  Orden2=t(Orden2)
  Orden2=as.data.frame(cbind(Orden2))
  Orden2[1,]
  
  clase1<-Orden2[,1]==1
  clase2<-Orden2[,1]==2
  clase3<-Orden2[,1]==3
  clase4<-Orden2[,1]==4
  clase5<-Orden2[,1]==5
  clase6<-Orden2[,1]==6
  clase7<-Orden2[,1]==7
  clase8<-Orden2[,1]==8
  clase9<-Orden2[,1]==9
  listaClases<-list(clase1,clase2,clase3,clase4,clase5,clase6,clase7,clase8,clase9)
  listaClases
}


Ordenar<-function(p){
  Orden<-apply(p,1,function(p){
    p=p[-1]
    p<-order(p,decreasing=T)
    p
  })
  Orden=t(Orden)
  Orden=as.data.frame(cbind(Orden))
  Orden[1,]
  Orden
}


SelecParamJordan<- function(hidden,epoch,learn, train,test){
  require(RSNNS)
  result<-as.data.frame(cbind(0,0,0,1,1))
   for(i in hidden ){
     cat(paste("Estamos en ",i,"de ",length(hidden)))
     for(j in epoch){
       for(l in learn){
         modelJordan <- jordan(train$inputsTrain, train$targetsTrain,shufflePatterns = T, linOut=F,
                               size=c(i), learnFuncParams=c(l), maxit=j)
         
         table<-confusionMatrix(train$targetsTrain,fitted.values(modelJordan))
         p1<-table[1,1]/sum(table[1,])
         p2<-table[2,2]/sum(table[2,])
         p3<-table[3,3]/sum(table[3,])
         p4<-table[4,4]/sum(table[4,])
         p5<-table[5,5]/sum(table[5,])
         p6<-table[6,6]/sum(table[6,])
         p7<-table[7,7]/sum(table[7,])
         p8<-table[8,8]/sum(table[8,])
         p9<-table[9,9]/sum(table[9,])
         listError<-c(p1,p2,p3,p4,p5,p6,p7,p8,p9)
         #print("JORDAN Train error")
         TrainError<-mean(listError)
         
         predictions <- predict(modelJordan,train$inputsTest)
         table<-confusionMatrix(train$targetsTest,predictions)
         p1<-table[1,1]/sum(table[1,])
         p2<-table[2,2]/sum(table[2,])
         p3<-table[3,3]/sum(table[3,])
         p4<-table[4,4]/sum(table[4,])
         p5<-table[5,5]/sum(table[5,])
         p6<-table[6,6]/sum(table[6,])
         p7<-table[7,7]/sum(table[7,])
         p8<-table[8,8]/sum(table[8,])
         p9<-table[9,9]/sum(table[9,])
         listError<-c(p1,p2,p3,p4,p5,p6,p7,p8,p9)
         #print("JORDAN Test error")
         TestError<-mean(listError)
         result<-as.data.frame(rbind(result,c(i,j,l,TrainError,TestError)))
       }
   }
 }
 
 return result  
  
}

