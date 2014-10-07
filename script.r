rm(list=ls())

source("scripts/LoadPackages.R")


RequiredPackages(
  c(
    "plyr",
    "reshape2",
    "lattice",
    "ggplot2",
    "stringr",
    "car",
    "RColorBrewer"
  )
)


# load the data

data_raw <- read.csv("data/real/scotland_all.csv")

data <- data_raw

names(data) <- c(
  "country",
  "year",
  "age",
  "sex",
  "convicted",
  "total"
  )

data$sex <- tolower(data$sex)

# change 90 and over to 90 to allow conversion from factor to numeric

data$age <- revalue(data$age, c("90 & over" = "90"))
data$age <- as.numeric(as.character(data$age))


data <- mutate(data, convict_rate = convicted/total)

###

data_younger <- subset(
  data,
  subset=age <=60
  )

png("figures/all_scotland.png",
    2000,
    1000
    )
contourplot(
  convict_rate ~ year * age | sex, 
  data=data,
  region=T, 
  col.regions=rev(heat.colors(200)), 
  #  col.regions=rev(gray(0:199/199)),
  cuts=50
  
  )

dev.off()


# how about log scale?

png("figures/all_scotland_log.png",
    2000,
    1000
)
contourplot(
  log(convict_rate) ~ year * age | sex, 
  data=data,
  region=T, 
  col.regions=rev(heat.colors(200)), 
  #  col.regions=rev(gray(0:199/199)),
  cuts=50
  
)

dev.off()

####################################################################



png("figures/all_scot_younger.png",
    2000,
    1000
)
contourplot(
  convict_rate ~ year * age | sex, 
  data=data_younger,
  region=T, 
  col.regions=rev(heat.colors(200)), 
  #  col.regions=rev(gray(0:199/199)),
  cuts=50
  
)

dev.off()


# how about log scale?

# Using only up to age 60 as too few observations above

png("figures/all_scot_younger_log.png",
    2000,
    1000
)
contourplot(
  log(convict_rate) ~ year * age | sex, 
  data=data_younger,
  region=T, 
  col.regions=rev(heat.colors(200)), 
  #  col.regions=rev(gray(0:199/199)),
  cuts=50
  
)

dev.off()

## Difference between males and females

data_diff <- subset(
  data_younger,
  select=c("country", "year", "age", "sex", "convict_rate")
  )

data_diff <- dcast(data_diff, country + year + age ~ sex)
data_diff <- mutate(data_diff, male_excess = male - female)
data_diff$female <- NULL
data_diff$male <- NULL


png("figures/all_scot_younger_male_excess.png",
    1000,
    1000
)
contourplot(
  male_excess ~ year * age , 
  data=data_diff,
  region=T, 
  col.regions=rev(heat.colors(200)), 
  #  col.regions=rev(gray(0:199/199)),
  cuts=50
  
)

dev.off()


