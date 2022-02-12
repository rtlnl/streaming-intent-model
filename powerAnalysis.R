library(data.table)
library(caret)
library(magrittr)
library(ggplot2)
library(stringr)
library(ggcorrplot)
library(MASS)
library(hglm)
library(lme4) # alternative to hglm
library(DMwR) #remotes::install_github("cran/DMwR")
library(modelr) # CV
library(cvTools) # CV
library(smotefamily)
library(xgboost)
library(tidyverse)
library(boot)
library(brms) # bayes regressions
library(tidybayes) # bayes posterior viz
library(gdata) # mapLevels to map levels to int and back
library(knitr)
library(kableExtra)
library(reticulate)
library(qwraps2) # display mean and ci
options(knitr.table.format = "latex")
## install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))
library(cmdstanr)
install_cmdstan()

library(rsample) # more advanced crossfold
library(splitstackshape)

library(psych)

library(wesanderson)
library(RColorBrewer)

library(Hmisc)

library(corrplot)

library(splitTools)

source("utils.R")

dates <- c("20211102", "20211119", "20211124", "20211201", "20211210", "20211217",
           "20220120")
n <- c()

for(day in dates){
  d <- read.csv(paste0("data/behavior-survey/", day, ".csv"))
  n <- c(n, nrow(d))
}

ggplot(data.frame(day = as.Date(dates, "%Y%m%d"), nrow = n), aes(day, nrow)) + geom_point()

setDT(d)

names(d)

## make it a time series model for real time intent prediction
## user intent's change: we could measure the relationship between intent and content
## user groups: tiers / internal user groups

## get a business case out of it: newsletter

## how was SMOTE tuned?
## not sure what I will do with the "others" field
## how did you do train test split?
## how did you threshold? 0.2 and below and 0.8 and above

## cross session prediction of intent

## imagine you already have the  model . imagine yourself as a consumer of your own model
## think big, and focus on a niche.

## experimental design

## can I write a bout a specific point. that is a short paper, before aor after

## surveys

## maybe I don't care about users who are already happy


## Isabella notes
## user-related data on top of session-related data: age, gender, subscription time.
## is it representative of our base
## data about 7 seconds on home for premium people.

## DATA PREP

#d[, SatisfactionRatingControl:=NULL]
#d[, IP:=NULL]

d[is.na(d)] <- 0
setnames(d, "mood", "satisfaction")
d[, age := 2022 - birthDate]
d[, satisfactionBin := (satisfaction > 3) + 0]
d[, satisfied := (satisfaction == 5) + 0]
d[, dissatisfied := (satisfaction == 1) + 0]
d[, otherIntent := Ik_maak_ook_nog_om_deze_reden_ge]
d[, sessionLength := sessionLengthByHit]
## d[, Reden_gebruik := str_replace(Reden_gebruik, "Inspiration", "Explorative")]

## save other intents
## write.csv(d[otherIntent != "", .(satisfaction, otherIntent)], "data/otherIntent.csv")

## intTable <- table(d$Reden_gebruik)
## sum(intTable[grepl("/", names(intTable))])


## repeat one row per intent
intents <- d[, .(intent = unlist(strsplit(Reden_gebruik, " / "))), by = names(d)] # , type.convert = TRUE
oneHot <- dcast(intents, ... ~ intent, fun = length)
names(oneHot) %<>% str_replace("Inspiration", "Explorative") %>% str_replace("_", " - ")

responded <- oneHot[satisfaction != 0 & Reden_gebruik != "",]
responded <- responded[intents[, .(sessionId, intent)], on = "sessionId"] # ass intent back in

intentsPure <- separate(intents, intent, c("group", "intent"), "_")
intentsPure[, group := str_replace(group, "Inspiration", "Explorative")]
oneHotPure <- dcast(intentsPure, ... ~ intent, fun = length)


write.csv(responded, "data/responded.csv")

## responded[, ID := .I]
## responded[, .(intent=as.numeric(as.factor(intent)))]
## responded[, .as.factor(intent))]

## intentFac <- factor(responded$intent)
## levels(intentFac)


## (f <- factor(responded$intent))
## (mapInt <- mapLevels(f))

## ## Integer to factor
## (int <- as.integer(f))
## (mapLevels(int) <- mapInt)
## all.equal(int, f)

## (mapFac <- mapLevels(intentFac))
## (mapLevels(x = as.integer(intentFac)) <- mapFac)

# respondedIntent = responded[,c(.SD, .(satisfaction = satisfaction)), .SDcols = sessionLengthByHit : nStrips]

behaviors <- "sessionLengthByHit + numPlays + timeToFirstPlay + nStrips" # reduced behaviors that have more values above zero
behaviorNames <- names(responded[,numPlays : sessionLength])
behaviors <- names(responded[,numPlays : sessionLength]) %>%
  paste(., collapse =" + ")

possibleIntents <- names(responded[,`Decisive_catch-up`:Inspiration_watchlist])

## responded[, intent := as.factor(intent)]
## responded[, satisfactionBin := as.factor(satisfactionBin)]

## smoteResponded <- smotefamily::SMOTE(responded[, sessionLengthByHit:nStrips], responded$satisfactionBin)
smoteResponded <- smotefamily::SMOTE(
                                 responded[, c(.(ID = ID), .SD,
                                             .(intent=as.numeric(as.factor(intent)))),
                                           .SDcols=sessionLengthByHit:nStrips],
                                 responded$satisfactionBin)

smoteResponded <- smoteResponded$data
setnames(smoteResponded, "class", "satisfactionBin")

smoteResponded <- read.csv("data/smoteResponded.csv", check.names=FALSE) # otherwise catch-up becomes catch.up
smoteResponded <- setDT(smoteResponded)

smoteResponded[, satisfactionBin := (satisfaction > 3) + 0]
smoteResponded[, satisfied := (satisfaction == 5) + 0]
smoteResponded[, dissatisfied := (satisfaction == 1) + 0]

## repeat one row per intent


## VIZ

## color blind palette
## http://www.cookbook-r.com/Graphs/Colors_(ggplot2)/
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

d[, sessionLengthByHit:nStrips] %>%
##  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram()


## hist
ggplot(d, aes(x=satisfaction, y = ..count.., fill = satisfaction)) +
  geom_bar(width = 0.5) + theme_classic() +
  theme(text = element_text(size = 14)) +
  scale_fill_manual(palette=cbPalette)

ggplot(responded[intent == i, ], aes(x=satisfaction, y = ..count.., color = satisfaction)) + geom_bar(width = 0.5) + theme_classic() + scale_fill_brewer(palette="Set1")



ggplot(d[age< 10], aes(x=age)) + geom_histogram()
ggplot(d, aes(x=gender)) + geom_histogram(stat = "count")
ggplot(smoteResponded, aes(x=satisfaction)) + geom_histogram()

# ggplot(d, aes(x=System)) + geom_histogram(stat = "count")

## satisfaction VS intent
ggplot(intents, aes(x=intent)) + geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_wrap(~satisfactionBin)

## satisfaction VS behavioral
intents[satisfactionBin == 1, sessionLengthByHit:nStrips] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram()

ggplot(intents, aes(x=intent)) + geom_histogram(stat = "count") +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_wrap(~satisfactionBin)

ggplot(intents, aes(x=intent)) + geom_bar(aes(y = (..count..)/sum(..count..))) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) + facet_wrap(~satisfactionBin) + scale_y_continuous(formatter = 'percent')

satBehavLong <- melt(intents, id.vars = c("sessionId", "satisfaction"),
                     measure.vars = names(responded[,sessionLengthByHit : nStrips]),
                     variable.name = "behavioral variable")



ggplot(satBehavLong[value >= 1], aes(x= log(value), y = satisfaction, group = satisfaction, fill = as.factor(satisfaction))) +
  geom_violin() +
  ## stat_summary(geom="point", fun=mean) +
  ## geom_errorbar(aes(ymin=value-ci, ymax=value+ci)) +
  facet_wrap(~`behavioral variable`, scales = "free") +
  theme_classic() +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
  # scale_fill_manual(values=cbPalette)
##+ coord_cartesian(ylim = quantile(satBehavLong$value, c(0.1, 0.9)))

## cor plot
names(oneHot)
corrIntentBehav <- cor(oneHot[, `Decisive - catch-up`:`Explorative - watchlist`],
                       oneHot[, ..behaviorNames])

p.mat <- psych::corr.test(oneHot[, `Decisive - catch-up`:`Explorative - watchlist`],
                       oneHot[, ..behaviorNames]) #, adjust="none")

normCorr <- (corrIntentBehav > 0) * corrIntentBehav / max(corrIntentBehav) -
  (corrIntentBehav < 0) * corrIntentBehav / min(corrIntentBehav)

## pMat <- cor_pmat(oneHot[, `Decisive_catch-up`:Inspiration_watchlist],
##                        oneHot[, ..behaviorNames])
ggcorrplot(normCorr, show.legend=T, p.mat = p.mat$p, pch = 6, pch.cex = 2) + # lab = TRUE
  scale_fill_gradientn(colours = c("darkblue","white","red"),
                       values = scales::rescale(c(min(corrIntentBehav), 0, max(corrIntentBehav))),
                       labels = c(-0.10, 0.05, 0, 0.05, 0.10))

ggcorrplot(normCorr, show.legend=T, p.mat = p.mat$p, pch.col = "white", pch.cex = 2) + # lab = TRUE
  scale_fill_gradientn(colours = c("darkblue","white","red"),
                       values = scales::rescale(c(min(corrIntentBehav), 0, max(corrIntentBehav))),
                       labels = c(-0.10, 0.05, 0, 0.05, 0.10))

ggcorrplot(normCorr, show.legend=T) + # lab = TRUE
  scale_fill_gradientn(colours = c("darkblue","white","red"),
                       values = scales::rescale(c(min(corrIntentBehav), 0, max(corrIntentBehav))),
                       labels = c(-0.10, 0.05, 0, 0.05, 0.10))


phantomCorr <- corrIntentBehav
phantomCorr[phantomCorr<2] <- 0
ggcorrplot(phantomCorr, p.mat = p.mat$p)



## oneHot[, c(.SD, .(sessionId)), .SDcols=sessionLengthByHit:nStrips]

## intents <- oneHot[, c(.SD, .(sessionId = sessionId)), .SDcols=`Decisive_catch-up`:Inspiration_watchlist]
## behaviorals <- oneHot[, c(.SD, .(sessionId = sessionId)), .SDcols=sessionLengthByHit:nStrips]



## https://stackoverflow.com/questions/32022594/scatterplot-matrix-using-two-dataframes-in-r
dft <- gather(intents,var,value,-sessionId) %>% 
  left_join(gather(behaviorals,var,value,-sessionId),by="sessionId")

## https://ggplot2.tidyverse.org/reference/geom_smooth.html
binomial_smooth <- function(...) {
  geom_smooth(method = "glm", method.args = list(family = "binomial"), ...)
}

ggplot(data = dft,aes(x = value.y,y= value.x)) +
  geom_point() +
  binomial_smooth() + 
  facet_grid(var.x~var.y, scales = "free")

ggplot(data = dft,aes(x = value.y,y= value.x, group = value.x)) +
  geom_boxplot() +
  facet_grid(var.x~var.y, scales = "free") +
  scale_x_log10() 

## violin plot

ggplot(intents, aes(intent, satisfaction, fill = factor(group))) + geom_violin() +
  theme_classic() +
  theme(axis.text.x = element_text(angle = 45, hjust=1, size = 14)) +
  theme(axis.text.y = element_text(size = 14)) +
  theme(legend.position="none") +
  facet_grid(~group, 
             scales = "free_x", # Let the x axis vary across facets.
             space = "free_x",  # Let the width of facets vary and force all bars to have the same width.
             switch = "x") +
  theme(strip.placement = "outside",                      # Place facet labels outside x axis labels.
        strip.background = element_rect(fill = "grey90", color = "white"),
        strip.text = element_text(size = 14),# Make facet label background white.
        axis.title.x=element_blank(),
        axis.title.y=element_text(size = 14))  +
  scale_fill_manual(values = cbPalette[c(2,6)])



+
  (values= c(brewer.pal(n=4,"Blues"), brewer.pal(n=4,"Reds")))
  scale_fill_manual(values=c(wes_palette(n=4, name="IsleofDogs1"), wes_palette(n=4, name="GrandBudapest1")))
  ## scale_fill_brewer(palette="Set1")




## (somewhat) ORDINAL MODEL

linearRegression <- lm(satisfaction ~ sessionLengthByHit + numPlays + timeToFirstPlay + nStrips, data=responded)
summary(linearRegression)

linearRegressionAll <- lm(as.formula(paste("satisfaction ~", behaviors)), data=respondedIntent)

linearRegressionAll <- lm(satisfaction ~ ., data=respondedIntent)
summary(linearRegressionAll)

# Error in svd(X) : infinite or missing values in 'x'
ordinalLogistic <- polr(factor(satisfaction) ~ sessionLengthByHit + numPlays + timeToFirstPlay + nStrips, data=responded)
ordinalLogistic
summary(ordinalLogistic)

ordinalLogistic <- polr(factor(satisfaction) ~ ., data=respondedIntent)
ordinalLogistic
summary(ordinalLogistic)


xgb <- xgboost(data = as.matrix(responded[, sessionLengthByHit:nStrips]),
               label = responded$satisfaction,
               nrounds = 20)
hist(predict(xgb, as.matrix(responded[, sessionLengthByHit:nStrips])))

## BINARIZED MODEL

logistic <- glm(satisfactionBin ~ sessionLengthByHit + numPlays + timeToFirstPlay + nStrips, data = smoteResponded, family = "binomial")
summary(logistic)
hist(predict(logistic, type = "response"))
## everything is above 0.8, highly inbalanced data! not anymore!
hist(log(smoteResponded$nProfileClicks))

hierarchicalLogistic <- hglm(fixed = as.formula(paste("satisfactionBin ~", behaviors)),
                             random = ~ 1|factor(intent),
                             data = responded, family = binomial(link = logit))
## !! Observation 1291 is too influential! Estimates are likely unreliable !!> 
summary(hierarchicalLogistic)
print(hierarchicalLogistic, print.ranef = TRUE)

## note that it doesn't work for a sample of 1/10th of the data: overparametrized.
hierarchicalLogistic <- hglm(fixed = as.formula(paste("satisfactionBin ~", behaviors)),
                             random = ~ 1|factor(intent),
                             data = responded[sample(.N, 267)], family = binomial(link = logit))


xgbBin <- xgboost(data = as.matrix(responded[, sessionLengthByHit:nStrips]),
               label = responded$satisfactionBin,
               nrounds = 20)
hist(predict(xgbBin, as.matrix(responded[, sessionLengthByHit:nStrips])))

respIntentMatrix <- as.matrix(responded[, c(.SD, .(intent=as.numeric(as.factor(intent)))),
                                    .SDcols=sessionLengthByHit:nStrips])
xgbBinIntent <- xgboost(data = respIntentMatrix,
               label = responded$satisfactionBin,
               nrounds = 20)
hist(predict(xgbBinIntent, respIntentMatrix))


## hierarchicalLogisticBayes <- brm(
##   formula = satisfactionBin ~ sessionLengthByHit + numPlays + timeToFirstPlay + nStrips + 1|intent,
##   data = intents, family = binomial(link = logit))
## saveRDS(hierarchicalLogistic, "models/hierarchicalLogistic.rds")

hierarchicalLogisticBayes <- brm(
  formula = as.formula(paste("satisfactionBin ~", behaviors, "+ 1|intent")),
  data = responded, family = binomial(link = logit),
  file = "models/hierarchicalLogistic.rds"
)

hierarchicalLogisticBayesNoCor <- brm(
  formula = as.formula(paste("satisfactionBin ~", behaviors, "+ (1|intent)")),
  data = responded, family = bernoulli(link = logit),
  file = "models/hierarchicalLogisticNoCor.rds"
)

get_variables(hierarchicalLogisticBayesNoCor)

hierarchicalLogisticBayes %>%
  spread_draws(b_Intercept, r_intent[intent,]) %>%
  mutate(satisfaction = inv.logit(b_Intercept + r_intent) - 0.5) %>%
  ggplot(aes(y = intent, x = satisfaction)) +
  stat_halfeye() +
  xlim(0, 1)



hierarchicalLogisticBayesNoCor %>%
  spread_draws(b_Intercept, r_intent[intent,]) %>%
  mutate(satisfaction = inv.logit(b_Intercept + r_intent)) %>%
  ggplot(aes(y = intent, x = satisfaction)) +
  stat_halfeye() +
  xlim(0, 1)


hierarchicalLogisticBayes %>%
  spread_draws(b_Intercept, r_intent[condition,]) %>%
  median_qi(condition_mean = b_Intercept + r_intent, .width = c(.95, .8, .5))

## saveRDS(hierarchicalLogisticBayes, "models/hierarchicalLogistic.rds")

get_variables(hierarchicalLogisticBayes)

hierarchicalLogisticBayes %>%
  spread_draws(r_intent[c,t]) %>%
  head(10)


hierarchicalLogisticBayes <- readRDS("models/hierarchicalLogistic.rds")

### CV

source("utils.R")

methodsParams <- list("w/o intent" = list(m = "logistic", withIntent = F, RE = F),
                      "w intent"  = list(m = "logistic", withIntent = T, RE = F),
                      "multilevel"  = list(m = "logistic", withIntent = T, RE = T),
                      "xgboost w/o intent" = list(m = "xgboost", withIntent = F, RE = F),
                      "xgboost w intent" = list(m = "xgboost", withIntent = T, RE = F))

overallResults <-
  rbind(
    crossT(10, 10, responded, response = "satisfactionBin", methodsParams),
    crossTByIntent(5,5, response = "satisfactionBin")
  )

satResults <-
  rbind(
    crossT(10, 10, responded, response = "satisfied", methodsParams),
    crossTByIntent(5,5, response = "satisfied")
  )


unsatResults <-
  rbind(
    crossT(10, 10, responded, response = "dissatisfied", methodsParams),
    crossTByIntent(5,5, response = "dissatisfied")
  )
  







  

## results %<>% t(sapply(., rbind)) %>% setDT


##  %>% .[,lapply(.SD, mean, na.rm = T)]

## holdoutPreds <- cvFit(train, rep(list(holdout), K), "logistic",
##                   withIntent = F, response = "satisfactionBin")

## preds <- holdoutPreds %>% .[,.(pred, true = satisfactionBin, Run)]
## preds$t <- optT

## CM <- preds[, .(tp = sum((pred >= t) * (true == 1)),
##       fp = sum((pred >= t) * (true == 0)),
##       fn = sum((pred < t) * (true == 1)),
##       tn = sum((pred < t) * (true == 0))), by = .(Run, t)]

## e = 1e-8 #0

## results = CM[ ,
##                 .(Accuracy = (tp + tn) / (tp + tn + fp + fn + e),
##                   Precision = tp / (tp + fp + e),
##                   Recall = tp / (tp + fn + e),
##                   F1 = 2*tp / (2*tp + fp + fn + e)), by = .(Run, t)]




## cv <- vfold_cv(trainval, strata = satisfaction, v = K)
## cv[[1]][[1]] %>% as.data.frame
## cv$splits$ids
## y
## ## dSplitted <- lapply( split(d, d$satisfaction), function(dd){
## ##             indexes= sample(1:nrow(dd), size = 0.7*nrow(dd))
## ##             train= dd[indexes, ]    # Notice that you may want all columns
## ##             test= dd[-indexes, ]
## ##             # analysis goes here
## ##             })

## ## spec = c(train = .6, test = .2, validate = .2)

## g = sample(cut(
##   seq(nrow(responded)), 
##   nrow(responded)*cumsum(c(0,spec)),
##   labels = names(spec)
## ))

## res = split(responded, g)


## ## K = 10 # fold
## set.seed(123)


## set.seed(123)
## caret::createDataPartition(d$satisfaction, p = 0.6)
## createMultiFolds(d$satisfaction, k = 10, times = 5)

## str(res)

source("utils.R")

overallResults <- getCVResults(responded, "satisfactionBin", 0.8, displayStyle = "mean_sd")
satResults <- getCVResults(responded, "satisfied", 0.8, displayStyle = "mean_sd")
unsatResults <- getCVResults(responded, "dissatisfied", 0.8, displayStyle = "mean_sd")

overallResults <- getCVResults(responded, "satisfactionBin", 0.8, displayStyle = "median_iqr")
satResults <- getCVResults(responded, "satisfied", 0.8, displayStyle = "median_iqr")
unsatResults <- getCVResults(responded, "dissatisfied", 0.8, displayStyle = "median_iqr")

overallResults <- getCVResults(smoteResponded, "satisfactionBin", 0.8)
satResults <- getCVResults(smoteResponded, "satisfied", 0.8)
unsatResults <- getCVResults(smoteResponded, "dissatisfied", 0.8)

overallResults %<>% mutate_if(is.numeric, round, digits=3)
overallResultsDisp <- overallResults[, mapply(function(x)
                 {cell_spec(x, bold = ifelse(x == max(x), T, F), format="latex")},
                 .SD), .SDcols = Acc:F1]

satResults %<>% mutate_if(is.numeric, round, digits=3)
satResultsDisp <- satResults[, mapply(function(x)
                 {cell_spec(x, bold = ifelse(x == max(x), T, F), format="latex")},
                 .SD), .SDcols = Acc:F1]

unsatResults %<>% mutate_if(is.numeric, round, digits=3)
unsatResultsDisp <- unsatResults[, mapply(function(x)
                 {cell_spec(x, bold = ifelse(x == max(x), T, F), format="latex")},
                 .SD), .SDcols = Acc:F1]

results <- cbind(overallResults[,1], overallResultsDisp,
                 satResultsDisp, unsatResultsDisp)

nc <- ncol(overallResults)

results <- cbind(overallResults[,1],
                 overallResults[,Accuracy:F1],
                 satResults[,Accuracy:F1],
                 unsatResults[,Accuracy:F1])


source("utils.R")

## take the best model and remove the people who clicked on account

results$method %<>% str_replace(., "Decisive_", " ")
results$method %<>% str_replace(., "Inspiration_", " ")

## write.csv(results, "results/10-10-crossT.csv")

### generate table

## dispResults <- results

## dispResults %<>% mutate_if(is.numeric, round, digits=3)


## dispResults$Accuracy %<>% as.numeric(.)

## dispResults %<>% mutate_if(is.numeric, round, digits=3)

## use column_spec instead of cell_spec here as recommended in the doc
## dispResults$Accuracy %<>% cell_spec(., bold = ifelse(. == max(.), T, F), format="latex")
## dispResults$Precision %<>% cell_spec(., bold = ifelse(. == max(.), T, F), format="latex")
## dispResults$Recall %<>% cell_spec(., bold = ifelse(. == max(.), T, F), format="latex")
## dispResults$F1 %<>% cell_spec(., bold = ifelse(. == max(., na.rm = T), T, F), format="latex")

kbl(results, format="latex", booktabs = T, escape = F, linesep = linesep(11)) %>%
  add_header_above(
    c(" " = 1,
      "Overall ($\\\\mathds{1}_{\\\\hat{\\\\mathbf{y}} \\\\geq 4}$)" = 4,
      "Satisfied ($\\\\mathds{1}_{\\\\hat{\\\\mathbf{y}} = 5}$)" = 4,
      "Unsatisfied ($\\\\mathds{1}_{\\\\hat{\\\\mathbf{y}} = 1}$)" = 4),
    escape = F)


# lm(satisfaction ~ sessionLengthByHit, numPlays, tim, data = d)

behav <- names(d[, sessionLengthByHit : nStrips])
behav <- c(behav, "satisfaction") #, "Reden_gebruik")



lm(satisfaction ~ ., data = d[, ..behav])


### Per intent Bayes

logisticBayes <- readRDS("models/logistic_.rds")

for (i in possibleIntents){
  logisticBayes <- brm(
    formula = as.formula(paste("satisfactionBin ~", behaviors)),
    data = responded[intent == i],
    family = bernoulli(link = logit),
    file = paste0("models/logistic_", i, ".rds"),
    chains = 4, cores = 4, backend = "cmdstanr"
  )
}

logisticBayes <- do.call('rbind', lapply(list.files("models", full.names = TRUE, pattern = "logistic_"), readRDS))



j <- 1
for (i in possibleIntents){
  thisBayes <- readRDS(paste0("models/logistic_", i, ".rds")) %>%
  tidy_draws() %>%
  gather_variables() %>%
  setDT() %>%
  .[, var := str_replace(.variable, "b_", "")] %>%
  .[var %in% behaviorNames, ] %>%
    .[, satisfaction := inv.logit(.value) - 0.5] %>%
    .[, intent := i] %>%
    .[]
  if(j == 1){
    logisticBayes <- thisBayes
  } else {
    logisticBayes <- rbind(logisticBayes, thisBayes)
  }
  j <- j + 1
}




## logisticBayes  %>%
##   spread_draws(b_Intercept, b_sessionLengthByHit) %>%
##   mutate(satisfaction = inv.logit(b_sessionLengthByHit)) %>%
##   ggplot(aes(x = satisfaction)) +
##   stat_halfeye() +
##   xlim(0, 1)

## behaviors
## get_variables(logisticBayes)

logisticBayes %>%
  ggplot(aes(y = var, x = satisfaction)) +
  facet_wrap(~intent, ncol = 4) +
  stat_halfeye(scale = 5) +
  theme_classic()


logisticBayes %>%
  ggplot(aes(y = var, x = satisfaction)) +
  facet_wrap(~intent, ncol = 4) +
  stat_slab() +
  theme_classic()





logisticBayes %>%
  tidy_draws() %>%
  gather_variables() %>%
  setDT() %>%
  .[, var := str_replace(.variable, "b_", "")] %>%
  .[var %in% behaviorNames, ] %>%
  .[, satisfaction := inv.logit(.value) - 0.5] %>% .[] %>%
  ggplot(aes(y = var, x = satisfaction)) +
  facet_wrap(~intent) +
  stat_halfeye(scale = 5) +
  theme_classic()

num_cpu <- parallel::detectCores(logical = FALSE)
num_cpu_logical <- parallel::detectCores(logical = TRUE)
grainsize_default <- ceiling(N / (2 * num_cpu))
cores <- c(2^seq(0, floor(log2(num_cpu_logical))), num_cpu, num_cpu_logical)
cores <- sort(unique(cores))



# d[, sessionLengthByHit : nStrips]
#d$satisfaction %<>% factor()

## cr <- cor(mtcars)
## # This is to remove redundancy as upper correlation matrix == lower 
## cr[upper.tri(cr, diag=TRUE)] <- NA
## reshape2::melt(cr, na.rm=TRUE, value.name="cor")

## dcast(d, sessionId ~ Reden_gebruik, sep = " / ")


#d[Reden_gebruik == ""] <- "nothing"

## intents <- d[, .(intent = unlist(tstrsplit(Reden_gebruik, " / ", type.convert = TRUE))), by = "sessionId"]



## unique(intents$sessionId)
## unique(d$sessionId)
#d[,  intent := tstrsplit(Reden_gebruik, " / ")]

##                                         # Define training control
## set.seed(123)
## train.control <- trainControl(method = "cv", number = 10)
## # Train the model
## logisticCV <- train(factor(satisfactionBin) ~ sessionLengthByHit + numPlays + timeToFirstPlay + nStrips, data=responded, method = "glm", family = "binomial",
##                trControl = train.control)
## # Summarize the results
## logisticCV$results$Accuracy
## logisticCV$results$AccuracySD




library(fastglm)

n <- 10e6
p <- 20

X <- matrix(rnorm(n * p, 1/p, 1/sqrt(p)), n, ncol = p)
y <- (1/(1 + exp(-(rowSums(X) - 1))) > runif(n)) * 1

ptm <- proc.time()
fastLogistic <- fastglm(X, y, family = binomial(), method = 3)
print(proc.time() - ptm)


recall(factor(c(1,1,1), levels = c(0,1)),
       factor(c(0,0,1), levels = c(0,1)))
