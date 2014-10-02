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

data_raw <- read.csv("data/fake/Scotland_all.csv")

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

data$convicted <- revalue(data$convicted,
                          c(" " = NA, 
                            "#VALUE!" = NA
                            )
                          )

data$convicted <- as.numeric(as.character(data$convicted))


data <- mutate(data, convict_rate = convicted/total)


contourplot(
  convict_rate ~ year * age | sex, 
  data=data,
  region=T, 
  col.regions=rev(heat.colors(200)), 
  #  col.regions=rev(gray(0:199/199)),
  cuts=50
  
  )

