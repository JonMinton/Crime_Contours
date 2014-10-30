# This first line removes and objects from the R workspace.
rm(list=ls())

# The following lines load a convenience function that someone wrote to handle
# the installation and loading of packages

source("scripts/LoadPackages.R")

# The list of packages loaded is shown below. The most important are 
# plyr : data management
# lattice: used for the contour plot
# r2stl : for producing the stl files needed for 3d visualisations
# ggplot2 : for the other visualisations
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

# The more usual way of doing the above is to install packages
# using the install.packages() function
# then load the package using the require() or library() functions


# load the data

# This reads in the data from the csv file from the csv file, into an object 
# called data
data_raw <- read.csv("data/real/scotland_all.csv")

# This copies data_raw into an object called data
data <- data_raw

# this renames the columns in data. The renamed variables are all readable but short,
# and in lower case
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

# change 90 and over to 90 to allow conversion from factor to numeric

# This turns the character string value "90 or over" into the string "90"
data$age <- revalue(data$age, c("90 & over" = "90"))
# This converts the convents of the age variable from factor to numeric
data$age <- as.numeric(as.character(data$age))
# (Because one of the values was "90 & over", it was stored as a factor
# rather than a numeric vector)

# This adds a new variable to the dataset, convict_rate,
# which is calculated as convicted/total for each row
data <- mutate(data, convict_rate = convicted/total)

###

# This creates a new dataset containing only observations in those aged 60 
# years or younger
data_younger <- subset(
  data,
  subset=age <=60
  )


###############################################################################
#
# This is the first image. It uses the function contourplot from the package 
# lattice. 

# This line creates a graphics device: a 2000 by 1000 pixel png image
png("figures/all_scotland.png",
    2000,
    1000
    )

# This is the contourplot function. As the png graphics device has been set up in the 
# previous line, the output of this function is fed to the png graphics device
contourplot(
  convict_rate ~ year * age | sex, # this argument states how the data should be arranged
  data=data, # this argument shows the data object where the function should look
  # in order to find the variables.
  
  # The arguments below are additional graphical parameters
  region=T, 
  col.regions=rev(heat.colors(200)), 
  #  col.regions=rev(gray(0:199/199)),
  cuts=50
  )

# this closes the graphics device: the output of the contourplot function
# is now available to view in a file called all_scotland.png
dev.off()


# how about log scale?

# This does the same as the above, but on a log scale
png("figures/all_scotland_log.png",
    2000,
    1000
)
contourplot(
  log(convict_rate) ~ year * age | sex,  # the only difference is that convict_rate is 
  # enclosed in the log function
  data=data,
  region=T, 
  col.regions=rev(heat.colors(200)), 
  #  col.regions=rev(gray(0:199/199)),
  cuts=50
  
)

dev.off()

####################################################################


# identity scale, younger ages 
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

# This looks at the difference between males and females in crime rates
# at different ages and in different years

# this creates a subset of the data using only the variables of interest
data_diff <- subset(
  data_younger,
  select=c("country", "year", "age", "sex", "convict_rate")
  )

# this re_arranges the data so that male and female convict_rates
# are reported in different columns, called male and female
# It uses the reshape2 package by Hadley Wickham

data_diff <- dcast(data_diff, country + year + age ~ sex)

# This creates another variable, male_excess, which is the difference 
# between male and female conviction rates for each row
data_diff <- mutate(data_diff, male_excess = male - female)

# This removes the female and male variables from the dataset
data_diff$female <- NULL
data_diff$male <- NULL


# This plots male_excess as a function of year and age in a contour plot
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

# The subsection below is used to produce the stl format images needed for the 
# 3d printers

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
  
####################################################################################
####################################################################################
# In this section the package ggplot2 is used to show how age-crime curves changed over
# time

# The basic idea with ggplot2 is to create visualisations by piecing together
# a number of separate graphical instructions

#
# Age slides:

# g1 is the first instruction: it says use the data data_younger
# to produce a visualisation in which the y axis depends on 
# convict_rate and the x axis depends on age

g1 <- ggplot(data_younger, aes(y=convict_rate, x=age))

# g2 is g1 plus one more instruction: the colour and the type of line 
# should both depend on the variable sex: this takes one of two values,
# so two different line types and colours will be used

g2 <- g1 + geom_line(aes(colour=sex, linetype=sex))
# This adds another instruction: instead of a single image, produce many 
# small multiples, showing the above separately for each year
g3 <- g2 + facet_wrap( ~ year)

# This adds a final instruction: label the y axis "convict rate"
g4 <- g3 + labs(y="convict rate")

# This command prints the image created to a graphics device. As no
# other graphics device has been stated, the device will be rstudio's internal 
# display
print(g4)

# This saves a copy of the graphic created above to a new file.  
ggsave(
  "figures/age_sections.png", width=20, height=20, unit="cm"
  )


#### Cohort slides:

# This creates a birth cohort variable and adds it to the data_younger dataset
data_younger <- mutate(data_younger,
                       cohort=year - age
                       )

#this sorts the dataset data_younger first by sex, then by cohort, then by age
data_younger <- arrange(data_younger, sex, cohort, age)


# This works in a similar way to the instructions above...
g1 <- ggplot(data_younger,
             aes(y=convict_rate, x=age)
             )
g2 <- g1 + geom_line(aes(colour=sex, linetype=sex))
g3 <- g2 + facet_wrap( ~ cohort) # .. the only difference is here. The 
# facet is now determined by the cohort variable, rather than the year variable
print(g3)


# Now just to look at a subset of cohort years:

# This does as with the above, but using a subset of cohorts.
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


#### For completeness, age facets as well 

g1 <- ggplot(subset(data_younger, subset=age <=35), aes(y=convict_rate, x=year))

# g2 is g1 plus one more instruction: the colour and the type of line 
# should both depend on the variable sex: this takes one of two values,
# so two different line types and colours will be used

g2 <- g1 + geom_line(aes(colour=sex, linetype=sex))
# This adds another instruction: instead of a single image, produce many 
# small multiples, showing the above separately for each year
g3 <- g2 + facet_wrap( ~ age)

# This adds a final instruction: label the y axis "convict rate"
g4 <- g3 + labs(y="convict rate") + theme(
  axis.text.x = element_text(angle =90, hjust=1, vjust=0.5)
  )


# This command prints the image created to a graphics device. As no
# other graphics device has been stated, the device will be rstudio's internal 
# display
print(g4)

# This saves a copy of the graphic created above to a new file.  
ggsave(
  "figures/age_facets.png", width=20, height=20, unit="cm"
)

