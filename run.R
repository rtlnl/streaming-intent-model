library(data.table) # faster and arguably nice syntax
library(magrittr) # allows piping such as %>% or %<>%
library(stringr) # for string operations like str_replace
library(tidyr) # separate
library(ggplot2)
library(tidybayes) # to extract posteriors
library(boot) # has inv.logit
library(ggdist)
library(cowplot)
library(rstan) # for bayesian stuff
library(bayesplot) # plot bayesain model
library(scales)
library(ggcorrplot)

source("utils.R")

d <- read.csv("../intentBasedClustering/data/behavior-survey/20220120.csv") %>% setDT

## ToDo in SQL
d[is.na(d)] <- 0
setnames(d, "mood", "satisfaction")
d[, age := 2022 - birthDate]
d[, satisfactionBin := (satisfaction > 3) + 0]
d[, satisfied := (satisfaction == 5) + 0]
d[, dissatisfied := (satisfaction == 1) + 0]
d[, otherIntent := Ik_maak_ook_nog_om_deze_reden_ge]
d[, sessionLength := sessionLengthByHit]
d[, intent := `Reden_gebruik`]

## one hot encoding of intents

## repeat one row per intent
intents <- d[, .(intentHot = unlist(strsplit(intent, " / "))), by = names(d)] # , type.convert = TRUE
oneHot <- dcast(intents, ... ~ intentHot, fun = length)
possibleIntentsOld <- names(oneHot[,`Decisive_catch-up`:Inspiration_watchlist])
names(oneHot) %<>% str_replace("Inspiration", "Explorative") %>% str_replace("_", " - ")

responded <- oneHot[intent != "",] # remove people who did not answer the second question
responded <- responded[eval(intents[, .(sessionId, intent)]), on = "sessionId"] # ass intent back in

intentsPure <- separate(intents, intentHot, c("group", "intent"), "_")
intentsPure[, group := str_replace(group, "Inspiration", "Explorative")]
oneHotPure <- dcast(intentsPure, ... ~ intent, fun = length)


## write.csv(responded, "data/responded.csv")


behaviorNames <- names(responded[,numPlays : sessionLength])
behaviors <- names(responded[,numPlays : sessionLength]) %>%
  paste(., collapse =" + ")

possibleIntents <- names(responded[,`Decisive - catch-up`:`Explorative - watchlist`])

## VIZ

cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## cor plot data prep
corrIntentBehav <- cor(oneHot[, `Decisive - catch-up`:`Explorative - watchlist`],
                       oneHot[, ..behaviorNames])

p.mat <- psych::corr.test(oneHot[, `Decisive - catch-up`:`Explorative - watchlist`],
                       oneHot[, ..behaviorNames]) #, adjust="none")

scaledCorr <- corrIntentBehav * 10



## normCorr <- (corrIntentBehav > 0) * corrIntentBehav / max(corrIntentBehav) -
##   (corrIntentBehav < 0) * corrIntentBehav / min(corrIntentBehav)



ggcorrplot(scaledCorr, show.legend=T) + # lab = TRUE
  scale_fill_gradientn("",
                       colours = c("darkblue","white","red"),
                       values = scales::rescale(c(min(scaledCorr), 0, max(scaledCorr))),
                       labels = c(round(min(scaledCorr), 1), -0.60, 0,
                                  0.60, round(max(scaledCorr), 1)))

ggplot(intentsPure, aes(intent, satisfaction, fill = factor(group))) + 
  # stat_summary(fun.y=mean, geom="point", shape=23, size=2) +
  geom_violin() +
  stat_summary(fun.y=mean, geom="point") +
  # geom_jitter(shape=16, position=position_jitter(0.2), alpha = 1) +
  # geom_boxplot(width=0.1) +
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
  scale_fill_manual(values = cbPalette[c(7,8)])

## Bayes

posteriors <- list()

j <- 1
for (i in possibleIntentsOld){
  thisBayes <- readRDS(paste0("../intentBasedClustering/models/logistic_", i, ".rds"))
  thisPosterior <- thisBayes %>% as.array
  thisBayes %<>% tidy_draws()
  notConverged <- thisBayes %>% summarise_draws() %>%
    setDT() %>% .[rhat > 1.05 & grepl("b_", variable) , variable]

  posteriors[[j]] <- thisPosterior %>% .[,,!dimnames(.)$variable %in% c(notConverged, "lp__", "Intercept")]
  
  thisBayes %<>%
    gather_variables() %>%
    setDT() %>% .[!.variable %in% notConverged] %>%
    .[, var := str_replace(.variable, "b_", "")] %>%
    .[var %in% behaviorNames, ] %>%
    .[, satisfaction := .value] %>%
      .[, intent := i] %>%
    .[]

  if(j == 1){
    logisticBayes <- thisBayes
  } else {
    logisticBayes <- rbind(logisticBayes, thisBayes)
  }
  j <- j + 1
}

bayesModels <- list()


j <- 1
for (i in possibleIntentsOld){
  thisPosterior <- readRDS(paste0("../intentBasedClustering/models/logistic_", i, ".rds")) %>%
    as.array %>% setDT
  thisPosterior
  
  j <- j + 1
}


logisticBayes[, index := .I]
logisticBayes[, varIntent := paste0(var, intent)]

medians <- logisticBayes[!var %in% c("numTrailerPlays")] %>% # "timeToFirstTrailer", 
  .[, .(intent = intent, median = abs(median(satisfaction))), by = .(varIntent)]
medians <- unique(medians)
topVars <- setorder(medians, intent, -median)[, head(.SD, 3), by = intent][, varIntent]

logisticBayes$intent %<>% str_replace("Inspiration", "Explorative") %>% str_replace("_", " - ")

# Daltonian palette https://jfly.uni-koeln.de/color/
cbp1 <- c("#999999", "#E69F00", "#56B4E9", "#009E73",
          "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

logisticBayes[varIntent %in% topVars] %>%
  ggplot(aes(y = var, x = satisfaction, fill = var)) +
  facet_wrap(~intent, ncol = 4, scales = "free") +
  ## stat_gradientinterval() +
  stat_halfeye(scale = 1.5, point_size = 1.5) +
  geom_vline(xintercept = 0, linetype="dashed") +
  theme_classic() +
  theme(axis.title.y=element_blank()) +
  scale_fill_manual(values=cbp1) +
  theme(legend.position="none", strip.background = element_rect(fill = "white", color = "white")) +
  scale_x_continuous(labels = scales::number_format(accuracy = 0.1))
  #scale_x_continuous(limits = c(-0.5, 0.5))
  ## scale_fill_manual(values=c(wes_palette(name="Royal1")))

library(wesanderson)


summary(bayesModels[[3]])$summary
rhat((bayesModels[[1]]))


str(bayesModels[[1]]$fit)

get_variables(bayesModels[[1]])

posterior <- as.array(bayesModels[[1]])

notConverged <- bayesModels[[1]] %>%
  tidy_draws() %>% summarise_draws() %>%
  setDT() %>% .[rhat > 1.05 & grepl("b_", variable) , variable]

bayesModels[[1]] %>% tidy_draws() %>% gather_variables() %>% setDT() %>% .[!.variable %in% notConverged]

b_Intercept

mcmc_acf(posterior, pars = "b_numTrailerPlays", lags = 10)

P <- list()
for (i in length(possibleIntentsOld)){
  local({
    i <- i
    P[[i]] <- mcmc_parcoord(posteriors[[i]]) + theme(axis.text.x = element_text(angle = 90))
    })
}

plot_grid(plotlist = P[[1]])

mcmcChains <- function(d, i){
  mcmc_parcoord(d[[i]]) + theme(axis.text.x = element_text(angle = 90))
}

lapply(1:8, mcmcChains, d = posteriors)


logisticBayes %>%
  ggplot(aes(y = var, x = satisfaction)) +
  facet_wrap(~intent, ncol = 4) +
  stat_halfeye(scale = 5) +
  theme_classic()

  logisticBayes <- brm(
    formula = as.formula(paste("satisfactionBin ~", behaviors)),
    data = responded[intent == i],
    family = bernoulli(link = logit),
    file = paste0("models/logistic_", i, ".rds"),
    chains = 4, cores = 4, backend = "cmdstanr"
  )


possibleIntents
data.frame(possibleIntents, behaviorNames, "satisfaction")


cat(paste0(possibleIntents, sep = "' , '"))
cat(paste0(behaviorNames, sep = "' , '"))


possibleIntents <- c('Decisive - catch-up' , ' Decisive - continuewatching' , ' Decisive - livetv' , ' Decisive - specifictitle' , ' Explorative - addwatchlist' , ' Explorative - genre' , ' Explorative - new' , ' Explorative - watchlist')

behaviorNames <- c('numPlays' , ' timeToFirstPlay' , ' numTrailerPlays' , ' timeToFirstTrailer' , ' nBookmarks' , ' nProfileClicks' , ' nAccounts' , ' nStrips' , ' nSearches' , ' nSeriesDescr' , ' nMoviesDescr' , ' sessionLength')
  
N <- 3000

d <- data.frame(satisfaction = sample(1:5, N, replace = T)) %>% setDT
d[,  (possibleIntents) := sample(0:1, N, replace = T)]
d[,  (behaviorNames) := rnegbin(N, mu = 1, theta = 1)]

library(MASS)

