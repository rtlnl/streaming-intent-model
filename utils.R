library(modelr)
library(purrr)

get_pred  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  pred  <- add_predictions(data, model, type = "response") ## type response to get from 0 to 1
  return(pred)
}

get_predXGB  <- function(model, test_data){
  data  <- as.data.frame(test_data)
  data$pred <- predict(model, test_data)
  return(data)
}

align_predXGB  <- function(preds, testDT){
  preds  <- as.data.frame(preds)
  testDT$pred <- preds$pred
  return(testDT)
}

parallelXGB <- function(mat, tDT, response){
  f <- xgboost(data = mat,
               label = tDT[, ..response][[1]],
               nrounds = 20,
               objective = "binary:logistic",
               verbose = 0)
  return(f)
}

cvFit <- function(train, test, m, withIntent = F, RE = F, response = "satisfactionBin"){
  
  predModel <- glm
  
  if(withIntent){
    if(RE){
      f <- as.formula(paste(response, "~", behaviors, "+ (1|intent)"))
      predModel <- glmer
    } else {
      f <- as.formula(paste(response, "~", behaviors, "+ intent"))
    }
  } else {
    f <- as.formula(paste(response, "~", behaviors))    
  }

  if(m == "xgboost"){
    if(withIntent){
      trainIntentMatrix <- map(train,
        ~as.matrix(.[, c(.SD, .(intent=as.numeric(as.factor(intent)))),
                     .SDcols=sessionLengthByHit:nStrips])
        )

      fit <- pmap(list(trainIntentMatrix, train, response), parallelXGB)

      testIntentMatrix <- map(test,
        ~as.matrix(.[, c(.SD, .(intent=as.numeric(as.factor(intent)))),
                     .SDcols=sessionLengthByHit:nStrips])
        )
      preds <- suppressWarnings(map2(fit, testIntentMatrix, get_predXGB))
      preds <- map2_df(preds, test, align_predXGB, .id = "Run")
    } else {
      trainIntentMatrix <- map(train,
        ~as.matrix(.[, sessionLengthByHit:nStrips])
        )

      fit <- pmap(list(trainIntentMatrix, train, response), parallelXGB)

      testIntentMatrix <- map(test,
        ~as.matrix(.[, sessionLengthByHit:nStrips])
        )
      preds <- suppressWarnings(map2(fit, testIntentMatrix, get_predXGB))
      preds <- map2_df(preds, test, align_predXGB, .id = "Run")
    }
    
    
  } else {
    fit  <-  map(train, ~predModel(f, data = ., family = binomial(link='logit')))
    preds <- map2_df(fit, test, get_pred, .id = "Run")
  }

  ## h <- hist(preds$pred) #, breaks = 0:10/10)
  ## print(h$breaks)
  ## print(h$counts)
  ## print(str(preds))

  ## preds[, predBin := factor((pred > t) * 1, levels = c(1,0))]
  ## preds[, respBin := factor(get(response), levels = c(1,0))]

  ## results = preds[ ,
  ##                 .(Acc = confusionMatrix(predBin,respBin, positive = "1")$overall["Accuracy"],
  ##                   Prec = ifelse(is.na(precision(predBin, respBin, positive = "1")), 0,
  ##                                precision(predBin, respBin, positive = "1")),
  ##           Rec = ifelse(is.na(recall(predBin, respBin, positive = "1")), 0,
  ##                                recall(predBin, respBin, positive = "1")),
  ##           F1 = ifelse(is.na(F_meas(predBin, respBin, positive = "1")), 0,
  ##                                F_meas(predBin, respBin, positive = "1"))), by = Run]

  ## results = preds[ ,
  ##                 .(Acc = confusionMatrix(predBin,respBin)$overall["Accuracy"],
  ##                   Prec = precision(predBin, respBin),
  ##           Rec = recall(predBin, respBin),
  ##           F1 = F_meas(predBin, respBin)), by = Run]
  return(preds %>% setDT)
}

evaluate <- function(preds){
  CM <- preds[, .(tp = sum((pred >= t) * (true == 1)),
      fp = sum((pred >= t) * (true == 0)),
      fn = sum((pred < t) * (true == 1)),
      tn = sum((pred < t) * (true == 0))), by = .(Run, t)]

  e = 1e-8 #0
  
  results = CM[ ,
                  .(Accuracy = (tp + tn) / (tp + tn + fp + fn + e),
                    Precision = tp / (tp + fp + e),
                    Recall = tp / (tp + fn + e),
                    F1 = 2*tp / (2*tp + fp + fn + e)), by = .(Run, t)]

  # print(results)
  
   return(results)
}

summarize <- function(results, displayStyle = "mean"){
  
  if (displayStyle == "mean"){
    results %<>% .[t == optT,lapply(.SD, mean, na.rm = T),
                   .SDcols = ! names(.) %in% c("Run", "t")]
  } else if (displayStyle == "mean_sd") {
    results %<>% .[t == optT,lapply(.SD, mean_sd, na_rm = T),
                   .SDcols = ! names(.) %in% c("Run", "t")]
  } else if (displayStyle == "median_iqr") {
    results %<>% .[t == optT,lapply(.SD, median_iqr, na_rm = T),
                   .SDcols = ! names(.) %in% c("Run", "t")] 
  }

  
  return(results)
}

  
  
thresholding <- function(preds, displayStyle = "mean", t = NA, getThreshold = T, response = "satisfactionBin"){

  preds %<>% setDT

  ## browser()
  
  if(is.na(t)){
  
    ts <- 1:99 / 100
    ## ts <- 1:9 / 10

    predsAtT <- preds %>% .[,.(pred, true = get(response), Run)] %>%
      .[rep(.[, .I], length(ts))]
      predsAtT$t <- rep(ts, nrow(predsAtT) / length(ts))

  } else {
    predsAtT <- preds %>% .[,.(pred, true = get(response), Run)]
    predsAtT$t <- t
  }
  
  
  results <- evaluate(predsAtT)

  optT <- results %>%
    .[, lapply(.SD, mean), by = t,
      .SDcols = ! names(.) %in% c("Run", "t")] %>%
    .[F1 == max(F1, na.rm = T), t]

  if(length(optT) > 1){optT <- optT[1]}
  
  ## print(results)
  ## deprecated: confusion matrix over all runs
  ## CM <- confusionMatrix(factor((preds$pred > t) * 1),
  ##                       factor(preds[, ..response][[1]]))
  ## results <- c(CM$overall["Accuracy"],
  ##              CM$byClass[c("Precision", "Recall", "F1")])
  


  ## browser()
  if(getThreshold) {return(optT)} else if (!is.na(t)) {return(results[, ! c("t", "Run")])} else {return(summarize(results, displayStyle))}
}

crossT <- function(J = 10, K = 10, responded, response = "satisfactionBin", methodsParams){

 
  ## tableResults = setDT(data.frame("method" = methods))

  ## metricNames <- c("Accuracy", "Precision", "Recall", "F1")
  
  results <- list()

  set.seed(J)
  trainvalFolds <- create_folds(responded$satisfaction, k = J, type = "basic")
  
  for (trainvalFold in trainvalFolds){

    ## i = J - (j-1)
    ## ## inspired by https://stackoverflow.com/questions/61262807/stratified-k-fold-cross-validation-in-r#:~:text=it%27s%20late%20but%20i%20hope%20i%20can%20help%20someone.%20The%20sample%20code%20helps%20me%3A
    ## set.seed(j)
    ## D <- stratified(trainval, "satisfaction", 1/i, bothSets = T)
    ##  <- D$SAMP1 %>% setDT
    ## trainval <- D$SAMP2 %>% setDT

    trainval <- responded[trainvalFold]
    holdout <- responded[-trainvalFold]

    set.seed(K)
    trainFolds <- create_folds(trainval$satisfaction, k = K, type = "basic")
    
    train <- list()
    val <- list()

    for (trainFold in trainFolds) {
      train <- append(train, list(trainval[trainFold]))
      val <- append(val, list(trainval[-trainFold]))
    }
    ## for (k in 1:K){
    ##   set.seed(k)
    ##   D <- stratified(responded, "satisfaction", 1 / K, bothSets = T)
    ##   val <- append(val, list(D$SAMP1 %>% setDT))
    ##   train <- append(train, list(D$SAMP2 %>% setDT))
    ## }

    ## browser()
    
    for (method in names(methodsParams)){

      ## browser()
      
      valPreds <- do.call(cvFit, c(list(train, val, response = response), methodsParams[[method]])) 

      optT <- thresholding(valPreds, displayStyle = "mean", t = NA, getThreshold = T)

      holdoutPreds <- do.call(cvFit, c(list(list(trainval), list(holdout), response = response),
                                       methodsParams[[method]])) 

      thisResult <- thresholding(holdoutPreds, displayStyle = "mean", t = optT, getThreshold = F)
      thisResult$method <- method

      results <- append(results, list(thisResult))
      print(thisResult)
      print(optT)
    }    

  }
  
  crossTestResults <- bind_rows(results)
  crossTestResults %<>% setDT
  crossTestResults %<>% .[,lapply(.SD, mean_sd, na_rm = T), by = method]

  return(crossTestResults)
}

crossTByIntent <- function(J = 5, K = 5, response = "satisfactionBin"){

  intentCrossResults <- list()

  intentParams <- setNames(rep(list(list(m = "logistic", withIntent = F, RE = F)), length(possibleIntents)),
           possibleIntents)

  for(i in possibleIntents){

    intentCrossResults <- append(intentCrossResults,
                                 list(crossT(5, 5, responded[intent == i, ],
                                        response = response,
                                        methodsParams = intentParams[i])))
  }
  intentCrossResults <- bind_rows(intentCrossResults)
  return(intentCrossResults)
}


getCVResults <- function(d, response, t, displayStyle = "mean"){

  methods <- c("w/o intent", "w intent", possibleIntents,
               "multiLevel", "xgboost w/o intent", "xgboost w intent")

  tableResults = setDT(data.frame("method" = methods))

  ## add_predictions <- function(data, model, var = "pred", type = NULL) {
  ##   data[[var]] <- predict2(model, data, type = type)
  ##   data
  ## }

  K = 10 # fold
  set.seed(123)
  #cv  <- crossv_kfold(smoteResponded, k = 10) #responded
  cv  <- crossv_kfold(d, k = K)

  trainDT <- list()

  for(train in cv$train){
    trainDT <- c(trainDT, list(train[1]$data[train[2]$idx]))
  }

  testDT <- list()

  for(test in cv$test){
    testDT <- c(testDT, list(test[1]$data[test[2]$idx]))
  }

  metricNames <- c("Accuracy", "Precision", "Recall", "F1")
  
  tableResults[method == "w/o intent",
          (metricNames) :=
            cvFit(cv$train, cv$test, "logistic",
                  withIntent = F, t, response = response,
                  displayStyle = displayStyle)
          ]

  tableResults[method == "w intent",
          (metricNames) :=
            cvFit(cv$train, cv$test, "logistic",
                  withIntent = T, t,response = response,
                  displayStyle = displayStyle)
          ]


  ## per intent CV

  for(i in possibleIntents){

    set.seed(123)
    cvInt  <- crossv_kfold(d[intent == i], k = K)

    tableResults[method == i,
          (metricNames) :=
            cvFit(cvInt$train, cvInt$test, "logistic",
                  withIntent = F, t, response = response,
                  displayStyle = displayStyle)
          ]

  }

  tableResults[method == "multiLevel",
          (metricNames) :=
            cvFit(cv$train, cv$test, "logistic",
                  withIntent = T, t, RE = T,response =  response,
                  displayStyle = displayStyle)
          ]

  tableResults[method == "xgboost w/o intent",
          (metricNames) :=
            cvFit(trainDT, testDT, "xgboost",
                  withIntent = F, t, response = response,
                  displayStyle = displayStyle)
          ]


  tableResults[method == "xgboost w intent",
          (metricNames) :=
            cvFit(trainDT, testDT, "xgboost",
                  withIntent = T, t, response = response,
                  displayStyle = displayStyle)
          ]

  return(tableResults)
}


## https://stackoverflow.com/questions/49015578/space-after-every-five-rows-in-kable-output-with-booktabs-option-in-r-markdown
linesep<-function(x,y=character()){
  if(!length(x))
    return(y)
  linesep(x[-length(x)], c(rep('',x[length(x)]-1),'\\addlinespace',y))  
}
