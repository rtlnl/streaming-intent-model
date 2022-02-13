Intent-based Satisfaction Modeling – From Music to Video Streaming
================
Gabriel
2/13/2022

This repository contains the supporting material for the paper
/Intent-based Satisfaction Modeling – From Music to Video Streaming/.

# libraries

``` r
# data wrangling
library(data.table) # faster and arguably nice syntax
library(magrittr) # allows piping such as %>% or %<>%
library(stringr) # for string operations like str_replace
library(tidyr) # separate
library(knitr) # especially to make tables with kable
library(dplyr)
library(purrr)

# plotting
library(ggcorrplot)
library(ggplot2)
```

## one hot encoding of intents

Intents were originally all together in one column

``` r
## repeat one row per intent
intents <- d[, .(intentHot = unlist(strsplit(intent, " / "))), by = names(d)] # , type.convert = TRUE
oneHot <- dcast(intents, ... ~ intentHot, fun = length)
names(oneHot) %<>% str_replace("Inspiration", "Explorative") %>% str_replace("_", " - ")

responded <- oneHot[intent != "",] # remove people who did not answer the second question
responded <- responded[eval(intents[, .(sessionId, intent)]), on = "sessionId"] # ass intent back in

intentsPure <- separate(intents, intentHot, c("group", "intent"), "_")
intentsPure[, group := str_replace(group, "Inspiration", "Explorative")]
oneHotPure <- dcast(intentsPure, ... ~ intent, fun = length)
```

## Available Vars

``` r
behaviorNames <- names(responded[,numPlays : sessionLength]) # get all behavior variable names
behaviors <- behaviorNames %>% paste(., collapse =" + ") # useful for modelling

possibleIntents <- names(responded[,`Decisive - catch-up`:`Explorative - watchlist`])

kable(data.frame("intents" = possibleIntents))
```

| intents                     |
| :-------------------------- |
| Decisive - catch-up         |
| Decisive - continuewatching |
| Decisive - livetv           |
| Decisive - specifictitle    |
| Explorative - addwatchlist  |
| Explorative - genre         |
| Explorative - new           |
| Explorative - watchlist     |

## Vizualizations

### Descriptive stats on Behavioral Vars

``` r
d[, ..behaviorNames] %>% gather %>% ggplot(aes(value)) + facet_wrap(~ key, scales = "free") + geom_histogram()
```

![](intentBased_files/figure-gfm/unnamed-chunk-3-1.png)<!-- -->

#### for the quite satisfied (y \>= 4)

``` r
intents[satisfactionBin == 1, sessionLengthByHit:nStrips] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram()
```

![](intentBased_files/figure-gfm/unnamed-chunk-4-1.png)<!-- -->

#### for the less satisfied (y\<4)

``` r
intents[satisfactionBin == 0, sessionLengthByHit:nStrips] %>%
  keep(is.numeric) %>% 
  gather() %>% 
  ggplot(aes(value)) +
    facet_wrap(~ key, scales = "free") +
    geom_histogram()
```

![](intentBased_files/figure-gfm/unnamed-chunk-5-1.png)<!-- -->

### The Satisfaction histogram

``` r
cbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73", "#F0E442", "#0072B2", "#D55E00", "#CC79A7")

## hist
ggplot(d, aes(x=satisfaction, y = ..count.., fill = satisfaction)) +
  geom_bar(width = 0.5) + theme_classic() +
  theme(text = element_text(size = 14)) +
  scale_fill_manual(palette=cbPalette)
```

![](intentBased_files/figure-gfm/unnamed-chunk-6-1.png)<!-- -->

### Violin Plots

#### Violins of Satisfaction and Intents

``` r
ggplot(intentsPure, aes(intent, satisfaction, fill = factor(group))) + geom_violin() +
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
```

![](intentBased_files/figure-gfm/unnamed-chunk-7-1.png)<!-- -->

#### Violins of Satisfaction and Behavioral Variables

``` r
satBehavLong <- melt(intents, id.vars = c("sessionId", "satisfaction"),
                     measure.vars = behaviorNames,
                     variable.name = "behavioral variable")

ggplot(satBehavLong[value >= 1], 
       aes(x= log(value), y = satisfaction, group = satisfaction, fill = as.factor(satisfaction))) +
  geom_violin() +
  facet_wrap(~`behavioral variable`, scales = "free") +
  theme_classic() +
  theme(legend.position="none") +
  scale_fill_brewer(palette="Set1")
```

![](intentBased_files/figure-gfm/unnamed-chunk-8-1.png)<!-- -->

### Corrplots

``` r
## cor plot data prep
corrIntentBehav <- cor(oneHot[, `Decisive - catch-up`:`Explorative - watchlist`],
                       oneHot[, ..behaviorNames])

p.mat <- psych::corr.test(oneHot[, `Decisive - catch-up`:`Explorative - watchlist`],
                       oneHot[, ..behaviorNames]) #, adjust="none")

normCorr <- (corrIntentBehav > 0) * corrIntentBehav / max(corrIntentBehav) -
  (corrIntentBehav < 0) * corrIntentBehav / min(corrIntentBehav)
```

The original corrplot

``` r
ggcorrplot(normCorr, show.legend=T) + # lab = TRUE
  scale_fill_gradientn(colours = c("darkblue","white","red"),
                       values = scales::rescale(c(min(corrIntentBehav), 0, max(corrIntentBehav))),
                       labels = c(-0.10, 0.05, 0, 0.05, 0.10))
```

![](intentBased_files/figure-gfm/unnamed-chunk-9-1.png)<!-- -->

The corrplot with significance testing (corrected for multiple testing,
significance level 0.05). A cross indicates insignificnt correlations.

``` r
ggcorrplot(normCorr, show.legend=T, p.mat = p.mat$p, pch.cex = 2) + # lab = TRUE
  scale_fill_gradientn(colours = c("darkblue","white","red"),
                       values = scales::rescale(c(min(corrIntentBehav), 0, max(corrIntentBehav))),
                       labels = c(-0.10, 0.05, 0, 0.05, 0.10))
```

![](intentBased_files/figure-gfm/unnamed-chunk-10-1.png)<!-- -->
