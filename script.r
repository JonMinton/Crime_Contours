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
    "RColorBrewer",
    "r2stl"
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


#########################################################################################
######## STL Images
#########################################################################################


fn <- function(x){
  ages <- x$age
  x$age <- NULL
  x <- as.matrix(x)
  x - min(x)
  rownames(x) <- ages
  return(x)
}

convict_matrix_male <- recast(
  subset(data_younger, subset=sex=="male", select=c("year", "age", "convict_rate")),
  age ~ year,
  id.var=c("age", "year"),
  measure="convict_rate"
  )
convict_matrix_male <- fn(convict_matrix_male)

convict_matrix_female <- recast(
  subset(data_younger, subset=sex=="female", select=c("year", "age", "convict_rate")),
  age ~ year,
  id.var=c("age", "year"),
  measure="convict_rate"
)
convict_matrix_female <- fn(convict_matrix_female)

  
  r2stl(
    x=as.numeric(rownames(convict_matrix_male)),
    y=as.numeric(colnames(convict_matrix_male)),
    z=convict_matrix_male,
    
    filename="stl/scot_younger_male.stl",
    z.expand=T,
    show.persp=F
  )

r2stl(
  x=as.numeric(rownames(convict_matrix_female)),
  y=as.numeric(colnames(convict_matrix_female)),
  z=convict_matrix_female,
  
  filename="stl/scot_younger_female.stl",
  z.expand=T,
  show.persp=F
)


# To do : both as a single stl file: combine matrices

cf <- convict_matrix_female
cm <- convict_matrix_male

colnames(cf) <- NULL
colnames(cm) <- NULL

convict_matrix_both <- cbind(cf, cm)
colnames(convict_matrix_both) <- 1:dim(convict_matrix_both)[2]


r2stl(
  x=as.numeric(rownames(convict_matrix_both)),
  y=as.numeric(colnames(convict_matrix_both)),
  z=convict_matrix_both,
  
  filename="stl/scot_younger_both_gender.stl",
  z.expand=T,
  show.persp=F
)
  

###############################################################################
# Age slides:

g1 <- ggplot(data_younger, aes(y=convict_rate, x=age))
g2 <- g1 + geom_line(aes(colour=sex, linetype=sex))
g3 <- g2 + facet_wrap( ~ year)
g4 <- g3 + labs(y="convict rate")
print(g4)

ggsave(
  "figures/age_sections.png", width=20, height=20, unit="cm"
  )


#### Cohort slides:

data_younger <- mutate(data_younger,
                       cohort=year - age
                       )

data_younger <- arrange(data_younger, sex, cohort, age)


g1 <- ggplot(data_younger,
             aes(y=convict_rate, x=age)
             )
g2 <- g1 + geom_line(aes(colour=sex, linetype=sex))
g3 <- g2 + facet_wrap( ~ cohort)
print(g3)


# Now just to look at a subset of cohort years:

g1 <- ggplot(
  subset(data_younger,
         subset=cohort >=1960 & cohort <= 1985)
         ,
             aes(y=convict_rate, x=age)
)
g2 <- g1 + geom_line(aes(colour=sex, linetype=sex))
g3 <- g2 + facet_wrap( ~ cohort)
print(g3)

ggsave(
  "figures/cohort_sub_section.png", width=25, height=20, unit="cm"
)
