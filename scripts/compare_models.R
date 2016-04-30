
rm(list=ls())


# Pre-reqs ----------------------------------------------------------------


require(plyr)
require(stringr)
require(tidyr)
require(dplyr)

# for smoothing
require(fields) 
require(spatstat)

require(RColorBrewer)
require(ggplot2)
require(lattice)
require(latticeExtra)

require(r2stl)

# Functions ---------------------------------------------------------------

smooth <- function(input, smooth_par=2){
  dta <- input %>%
  select(year, age, mf_ratio) %>%
  spread(key=age, value=mf_ratio) 
  ages <- names(dta)[-1]
  years <- dta$year
  dta$year <- NULL
  dta <- as.matrix(dta)
  rownames(dta) <- years
  colnames(dta) <- ages
  dta[is.infinite(dta) & dta < 0] <- min(dta[is.finite(dta)]) # correct for infinities
  dta[is.infinite(dta) & dta > 0] <- max(dta[is.finite(dta)])
  dta_blurred <- as.matrix(blur(as.im(dta), sigma=smooth_par))  
  rownames(dta_blurred) <- rownames(dta)
  colnames(dta_blurred) <- colnames(dta)
  output <- data.frame(
    year=years, 
    dta_blurred
  )
  output <- output %>%
  gather(key=age, value=mf_ratio, -year)
  
  output$age <- output$age %>%
  str_replace("X", "") %>%
  as.character %>%
  as.numeric
  
  return(output)
}


make_matrix <- function(x){
  ages <- x$age
  x$age <- NULL
  x <- as.matrix(x)
  x - min(x)
  rownames(x) <- ages
  return(x)
}
# Data --------------------------------------------------------------------


data <- read.csv("data/real/scotland_all.csv") %>%
tbl_df

names(data) <- c(
  "country",
  "year",
  "age",
  "sex",
  "convicted",
  "total"
)


# This converts the values in the sex vector to lowercase
data$sex <- tolower(data$sex)
data$age <- revalue(data$age, c("90 & over" = "90"))
data$age <- as.numeric(as.character(data$age))



# Modelling of differences over time  -------------------------------------


data

# Interested in proportion of convictions 'owned' 
# by any particular group in any year 

model_data <- data %>% 
group_by(year) %>% 
mutate(prop_convicted = convicted / sum(convicted)) %>%
select(year, age, sex, prop_convicted) %>%
arrange(year, sex, age)


lm(prop_convicted ~ year, model_data) %>% AIC() -> a_year
lm(prop_convicted ~ sex, model_data) %>% AIC() -> a_sex
lm(prop_convicted ~ age, model_data) %>% AIC() -> a_age


# Of these three simplest models, the age model has the lowest AIC

# Now two way linear models

lm(prop_convicted ~ age + year, model_data) %>% AIC() -> a_age_year
lm(prop_convicted ~ age + sex, model_data) %>% AIC() -> a_age_sex

lm(prop_convicted ~ age + year, model_data) %>% BIC() -> b_age_year
lm(prop_convicted ~ age + sex, model_data) %>% BIC() -> b_age_sex

# of these two, age + sex beats age + year

# But we know age does not have a linear effect - how many polynomials 
# is appropriate?

# without sex interaction
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

lm(prop_convicted ~ poly(age,1)  + sex, model_data) %>% BIC() -> b_age1_sex
lm(prop_convicted ~ poly(age,2)  + sex, model_data) %>% BIC() -> b_age2_sex
lm(prop_convicted ~ poly(age,3)  + sex, model_data) %>% BIC() -> b_age3_sex
lm(prop_convicted ~ poly(age,4)  + sex, model_data) %>% BIC() -> b_age4_sex
lm(prop_convicted ~ poly(age,5)  + sex, model_data) %>% BIC() -> b_age5_sex
lm(prop_convicted ~ poly(age,6)  + sex, model_data) %>% BIC() -> b_age6_sex
lm(prop_convicted ~ poly(age,7)  + sex, model_data) %>% BIC() -> b_age7_sex
lm(prop_convicted ~ poly(age,8)  + sex, model_data) %>% BIC() -> b_age8_sex
lm(prop_convicted ~ poly(age,9)  + sex, model_data) %>% BIC() -> b_age9_sex
lm(prop_convicted ~ poly(age,10)  + sex, model_data) %>% BIC() -> b_age10_sex


# with sex interaction
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

lm(prop_convicted ~ poly(age,1)  * sex, model_data) %>% BIC() -> b_age1sex
lm(prop_convicted ~ poly(age,2)  * sex, model_data) %>% BIC() -> b_age2sex
lm(prop_convicted ~ poly(age,3)  * sex, model_data) %>% BIC() -> b_age3sex
lm(prop_convicted ~ poly(age,4)  * sex, model_data) %>% BIC() -> b_age4sex
lm(prop_convicted ~ poly(age,5)  * sex, model_data) %>% BIC() -> b_age5sex
lm(prop_convicted ~ poly(age,6)  * sex, model_data) %>% BIC() -> b_age6sex
lm(prop_convicted ~ poly(age,7)  * sex, model_data) %>% BIC() -> b_age7sex
lm(prop_convicted ~ poly(age,8)  * sex, model_data) %>% BIC() -> b_age8sex
lm(prop_convicted ~ poly(age,9)  * sex, model_data) %>% BIC() -> b_age9sex
lm(prop_convicted ~ poly(age,10)  * sex, model_data) %>% BIC() -> b_age10sex


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

qplot(x = as.factor(poly), y = fit, group = interaction, colour = interaction, data = tmp) +
  facet_wrap(~model)


best_invariant_model <- lm(prop_convicted ~ poly(age,3)  * sex, model_data) 

?update

model_with_year <- update(best_invariant_model, . ~ . + I(year - 1989))

model_interrupted_complex <- model_data %>% 
ungroup() %>% 
mutate(year2 = year - 1989) %>%
mutate(year3 = ifelse(year - 2000 < 0, 0, year - 2000)) %>% 
lm(prop_convicted ~ poly(age,3)  * sex + year2 + year3, data = .)



model_interrupted_interracted <- model_data %>% 
ungroup() %>% 
mutate(year2 = year - min(year)) %>%
mutate(post_drop = ifelse(year >= 2000, TRUE, FALSE)) %>% 
lm(prop_convicted ~ poly(age,3)  * sex * post_drop, data = .)

model_yearandinterrupted_interracted <- model_data %>% 
ungroup() %>% 
mutate(year2 = year - min(year)) %>%
mutate(year3 = ifelse(year >= 2000, year - 2000, 0)) %>% 
lm(prop_convicted ~ (poly(age,3)  * sex )* (year2 + year3), data = .)



