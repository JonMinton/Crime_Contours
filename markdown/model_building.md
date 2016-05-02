---
title: "Comparison of models"
author: "Jon Minton"
date: "30 April 2016"
output: html_document
---

## Introduction

This document will compare a series of models which predict the share of all convictions, in any given year, committed by different age/sex groups. Firsly, a series of models are developed which predict the share of convictions in any given year given age and sex only. The best of these models will be our reference 'best time invariant' model, and will be a formal representation of the age-crime (gender) curve. 

The hypothesis that the age crime curve is not invariant will be formally assessed by exploring whether meaningful improvements in the model fit can be achieved by including period-based terms which interact with age terms. 


## Load packages and data 

```{r}
rm(list=ls())


library(plyr)
library(stringr)
library(tidyr)
library(dplyr)


library(RColorBrewer)
library(ggplot2)

data <- read.csv("../data/real/scotland_all.csv") %>%
  tbl_df

names(data) <- c(
  "country",
  "year",
  "age",
  "sex",
  "convicted",
  "total"
)

data$sex <- tolower(data$sex)
data$age <- revalue(data$age, c("90 & over" = "90"))
data$age <- as.numeric(as.character(data$age))


```


We first want to see what the data look like

```{r}

print(data)
```

As we are interested primarily in whether the shape of the age-crime curve has changed over time, we calculate for each year the proportion of all convictions associated with each age/sex combination

```{r}

model_data <- data %>%
  group_by(year) %>%
  mutate(prop_convicted = convicted / sum(convicted)) %>%
  select(year, age, sex, prop_convicted) %>%
  arrange(year, sex, age)

model_data
```

We now start to fit an increasingly complicated series of linear regression models which regression the proportion convicted against other terms. For each of these, we extract the AIC, a measure of model fit penalised by model complexity (i.e. number of terms), and save these AIC scores as a new output.

```{r}
lm(prop_convicted ~ year, model_data) %>% AIC() -> a_year
lm(prop_convicted ~ sex, model_data) %>% AIC() -> a_sex
lm(prop_convicted ~ age, model_data) %>% AIC() -> a_age

c(a_year, a_sex, a_age)

```

Of these three simplest models, the age model has the lowest AIC, even though the sex model involves fewer parameters. All further models therefore contain age at the very least. 

We now look at models containing age + at least one other set of terms. To start with, we look at adding additional terms without interactions.

```{r}

lm(prop_convicted ~ age + year, model_data) %>% AIC() -> a_age_year
lm(prop_convicted ~ age + sex, model_data) %>% AIC() -> a_age_sex

c(a_age_year, a_age_sex)

```
Of these two models, age + sex beats age + year, so all further models will now contain age  and sex at a minimum. 

An important feature of the age-crime curve is its skewednes/nonlinearity. To represent this as terms in a linear regression model we will look at adding varying numbers of polynomials to the model. We compare between 1 and 10 polynomials as follows:

Firstly, we look at models which include between 1 and 10 polynomials of age, along with sex, but without interactions between them: 

```{r}

lm(prop_convicted ~ poly(age,1)  + sex, model_data) %>% AIC() -> a_age1_sex
lm(prop_convicted ~ poly(age,2)  + sex, model_data) %>% AIC() -> a_age2_sex
lm(prop_convicted ~ poly(age,3)  + sex, model_data) %>% AIC() -> a_age3_sex
lm(prop_convicted ~ poly(age,4)  + sex, model_data) %>% AIC() -> a_age4_sex
lm(prop_convicted ~ poly(age,5)  + sex, model_data) %>% AIC() -> a_age5_sex
lm(prop_convicted ~ poly(age,6)  + sex, model_data) %>% AIC() -> a_age6_sex
lm(prop_convicted ~ poly(age,7)  + sex, model_data) %>% AIC() -> a_age7_sex
lm(prop_convicted ~ poly(age,8)  + sex, model_data) %>% AIC() -> a_age8_sex
lm(prop_convicted ~ poly(age,9)  + sex, model_data) %>% AIC() -> a_age9_sex
lm(prop_convicted ~ poly(age,10)  + sex, model_data) %>% AIC() -> a_age10_sex

```

Next, we will produce a series of models, from which we extract the AIC, containing interactions between each of the age polynomials and sex. This means that the schedule of relative propensity to conviction and age is allowed to be different for males and females.

```{r}
lm(prop_convicted ~ poly(age,1)  * sex, model_data) %>% AIC() -> a_age1sex
lm(prop_convicted ~ poly(age,2)  * sex, model_data) %>% AIC() -> a_age2sex
lm(prop_convicted ~ poly(age,3)  * sex, model_data) %>% AIC() -> a_age3sex
lm(prop_convicted ~ poly(age,4)  * sex, model_data) %>% AIC() -> a_age4sex
lm(prop_convicted ~ poly(age,5)  * sex, model_data) %>% AIC() -> a_age5sex
lm(prop_convicted ~ poly(age,6)  * sex, model_data) %>% AIC() -> a_age6sex
lm(prop_convicted ~ poly(age,7)  * sex, model_data) %>% AIC() -> a_age7sex
lm(prop_convicted ~ poly(age,8)  * sex, model_data) %>% AIC() -> a_age8sex
lm(prop_convicted ~ poly(age,9)  * sex, model_data) %>% AIC() -> a_age9sex
lm(prop_convicted ~ poly(age,10)  * sex, model_data) %>% AIC() -> a_age10sex

``` 

As this has produced a lot of model fits to compare against each other, and we want to understand better the relationship between the number of polynomials and the quality of the model fit, we package the AIC values together in a dataframe to visualise the results better

```{r}

tmp <- data.frame(poly = 1:10,
                  interaction = rep(c(F, T), each = 10),
                  model = rep(c("aic", "bic"), each = 20),
                  fit =
                    c(
                      a_age1_sex, a_age2_sex, a_age3_sex, a_age4_sex, a_age5_sex,
                      a_age6_sex, a_age7_sex, a_age8_sex, a_age9_sex, a_age10_sex,
                      a_age1sex, a_age2sex, a_age3sex, a_age4sex, a_age5sex,
                      a_age6sex, a_age7sex, a_age8sex, a_age9sex, a_age10sex,
                      b_age1_sex, b_age2_sex, b_age3_sex, b_age4_sex, b_age5_sex,
                      b_age6_sex, b_age7_sex, b_age8_sex, b_age9_sex, b_age10_sex,
                      b_age1sex, b_age2sex, b_age3sex, b_age4sex, b_age5sex,
                      b_age6sex, b_age7sex, b_age8sex, b_age9sex, b_age10sex


                    )
)


```

We can now produce a plot showing this relationship as follows

```{r}
qplot(x = as.factor(poly), y = fit, group = interaction, colour = interaction, shape = model, data = tmp)

```

From this we can conclude firstly that the models with interactions between age and sex tend to greatly outperform the non-interaction models, and so interactions should be included. 
We can conclude secondly that penalised model fit continues to improve with each additional polynomial added. However, most of the improvement in fit comes only from the first three polynomials: age, age squared, and age cubed. Although lower AIC is better, slightly simpler models tend to be a bit easier to interpret, and so as a compromise we select the model with interactions and a third order polynomial as our best model to represent the hypothesis that the age-crime curve is time-invariant.

```{r}
# best_invariant_model <- lm(prop_convicted ~ poly(age,3)  * sex, model_data)
# 
# 
# model_with_year <- update(best_invariant_model, . ~ . + I(year - 1989))
# 
# model_interrupted_complex <- model_data %>%
# ungroup() %>%
# mutate(year2 = year - 1989) %>%
# mutate(year3 = ifelse(year - 2000 < 0, 0, year - 2000)) %>%
# lm(prop_convicted ~ poly(age,3)  * sex + year2 + year3, data = .)
# 
# 
# 
# model_interrupted_interracted <- model_data %>%
# ungroup() %>%
# mutate(year2 = year - min(year)) %>%
# mutate(post_drop = ifelse(year >= 2000, TRUE, FALSE)) %>%
# lm(prop_convicted ~ poly(age,3)  * sex * post_drop, data = .)
# 
# model_yearandinterrupted_interracted <- model_data %>%
# ungroup() %>%
# mutate(year2 = year - min(year)) %>%
# mutate(year3 = ifelse(year >= 2000, year - 2000, 0)) %>%
# lm(prop_convicted ~ (poly(age,3)  * sex )* (year2 + year3), data = .)
# 
# ```
# 
# ## Including Plots
# 
# You can also embed plots, for example:
# 
# ```{r pressure, echo=FALSE}
# plot(pressure)
# ```
# 
# Note that the `echo = FALSE` parameter was added to the code chunk to prevent printing of the R code that generated the plot.
```