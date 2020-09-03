a---
title: "Diss Power"
output: html_document
---

```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE)
```

See example: https://github.com/simsem/simsem/wiki/Example-1:-Getting-Started

I have one construct with 30 items.  I am assuming standardized .7 factor loadings for each item on each construct.
```{r}
library(simsem)
loading <- matrix(0, 12, 2)
loading[1:6, 1] <- NA
loading[7:12, 2] <- NA
LY <- bind(loading, .7)
LY
```

Assuming no correlation between errors and a variance of 1.
```{r}
error.cor <- matrix(0, 12, 12)
diag(error.cor) <- 1
RTE <- binds(error.cor)
RTE
```

Assuming variance one 1 
```{r}
latent.cor <- matrix(NA, 2, 2)
diag(latent.cor) <- 1
RPS <- binds(latent.cor, 0.5); RPS
```
Put together the CFA model
```{r}
CFA.Model <- model(LY = LY, RPS = RPS, RTE = RTE, modelType="CFA")
summary(CFA.Model)
```
Cut the data function
Then run loop over function for varying n's
Then write code to grab the CFI, TLI, RMSEA, and SRMR
```{r}
n = rep(c(seq(from = 100, to = 400, by =50)), each =10)
n_samples = n
n = as.list(n)

items = list()
out = list()
out_sim = list()
chi_p_out = list()
cfi_out= list()
tli_out= list()
rmsea_out= list()
srmr_out= list()
results_out = list()
dat = list()

for(i in 1:length(n)){
dat[[i]] = generate(CFA.Model,n[[i]])
items[[i]] = apply(dat[[i]],2, function(x){CutQ(x, breaks = quantile(x, c(0, .05, .15, .30, .45, .60, .75, .85, .95, 1)), labels = c(1:9))})
items[[i]] = apply(items[[i]], 2, function(x){as.numeric(x)})
out[[i]] = analyze(CFA.Model, items[[i]])
out_sim[[i]] =  summary(out[[i]], fit.measures=TRUE,  standardized = TRUE)
out_sim[[i]]$FIT[c(5,9,10,17,21)]
chi_p_out[[i]] =  ifelse(out_sim[[i]]$FIT[c(5)] > .05, 1, 0)
cfi_out[[i]] =  ifelse(out_sim[[i]]$FIT[c(9)] >= .95, 1, 0)
tli_out[[i]] =  ifelse(out_sim[[i]]$FIT[c(10)] >= .95, 1, 0)
rmsea_out[[i]] =  ifelse(out_sim[[i]]$FIT[c(17)] < .05, 1, 0)
srmr_out[[i]] =  ifelse(out_sim[[i]]$FIT[c(21)] < .08, 1, 0)
results_out[[i]] = list(chi_p_out[[i]], cfi_out[[i]], tli_out[[i]], rmsea_out[[i]], srmr_out[[i]])
}
#chi-p, cfi, tli, rmsea, srmr five rows
results_out_dat = unlist(results_out)
results_out_dat = matrix(results_out_dat, ncol=5, byrow = TRUE)
results_out_dat = data.frame(results_out_dat)
colnames(results_out_dat) = c("chi_p", "cfi", "tli", "rmsea", "srmr")
results_out_dat$n_samples = n_samples
results_out_dat


library(dplyr)
sum_dat = results_out_dat %>% 
  group_by(n_samples) %>% 
  summarise_all(funs(sum))
sum_dat

count_dat =  results_out_dat %>% group_by(n_samples) %>% tally()

power_dat = data.frame(sum_dat, count = count_dat$n)
power_dat[,2:6] =round(power_dat[,2:6]/ power_dat$count,2)
power_dat$count = NULL
power_dat

library(reshape2)

power_long <- melt(power_dat, id.vars=c(1))
power_long
colnames(power_long) = c("n", "criteria", "power")
power_long

```
Graph the power analysis
```{r}

library(ggplot2)
power_long %>%
  ggplot( aes(x=n, y=power, group=criteria, color=criteria)) +
    geom_line()+
    geom_hline(yintercept=.8, linetype="dashed", color = "red")+
  ggtitle("Figure 1: SEASA power analysis")


```

