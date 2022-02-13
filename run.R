library(data.table) # faster and arguably nice syntax
library(magrittr) # allows piping such as %>% or %<>%
library(stringr) # for string operations like str_replace
library(tidyr) # separate

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
names(oneHot) %<>% str_replace("Inspiration", "Explorative") %>% str_replace("_", " - ")

responded <- oneHot[intent != "",] # remove people who did not answer the second question
responded <- responded[eval(intents[, .(sessionId, intent)]), on = "sessionId"] # ass intent back in

intentsPure <- separate(intents, intentHot, c("group", "intent"), "_")
intentsPure[, group := str_replace(group, "Inspiration", "Explorative")]
oneHotPure <- dcast(intentsPure, ... ~ intent, fun = length)


write.csv(responded, "data/responded.csv")


behaviorNames <- names(responded[,numPlays : sessionLength])
behaviors <- names(responded[,numPlays : sessionLength]) %>%
  paste(., collapse =" + ")

possibleIntents <- names(responded[,`Decisive - catch-up`:`Explorative - watchlist`])
