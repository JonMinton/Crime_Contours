# This first line removes and objects from the R workspace.
rm(list=ls())

# The following lines load a convenience function that someone wrote to handle
# the installation and loading of packages

# The list of packages loaded is shown below. The most important are 
# plyr : data management
# lattice: used for the contour plot
# r2stl : for producing the stl files needed for 3d visualisations
# ggplot2 : for the other visualisations

require(readr)

require(plyr)
require(stringr)
require(tidyr)
require(dplyr)

require(r2stl)
require(lattice)
require(ggplot2)
require(RColorBrewer)


# The more usual way of doing the above is to install packages
# using the install.packages() function
# then load the package using the require() or library() functions


# load the data

# This reads in the data from the csv file from the csv file, into an object 
# called data
data_raw <- read_csv("data/real/scotland_all.csv")

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
data_younger <- data  %>% 
  tbl_df  %>% 
  filter(age <= 60)


###############################################################################
#
# This is the first image. It uses the function contourplot from the package 
# lattice. 

png(filename="figures/all_scotland.png", 
    width=40, height=20, res=300, units="cm"
)
# Let's look at the mort rates only
contourplot(
  convict_rate ~ year * age | sex, 
  data=data, 
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab=list(label="Age in years", cex=1.4),
  xlab=list(label="Year", cex=1.4),
  cex=1.4,
  cuts=20,
  col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200),
  main=NULL,
  labels=list(cex=1.2),
  col="blue",
  scales=list(
    x=list(cex=1.4), 
    y=list(cex=1.4),
    alternating=3
  )
)
dev.off()


####################################################################


png(filename="figures/all_scot_younger.png", 
    width=40, height=20, res=300, units="cm"
)
# Let's look at the mort rates only
contourplot(
  convict_rate ~ year * age | sex, 
  data=data_younger, 
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab=list(label="Age in years", cex=1.4),
  xlab=list(label="Year", cex=1.4),
  cex=1.4,
  cuts=20,
  col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200),
  main=NULL,
  labels=list(cex=1.2),
  col="blue",
  scales=list(
    x=list(cex=1.4), 
    y=list(cex=1.4),
    alternating=3
  )
)
dev.off()


# males and females separately


# identity scale, younger ages 
png(filename="figures/male_scot_younger.png", 
    width=23, height=20, res=300, units="cm"
)
# Let's look at the mort rates only
contourplot(
  convict_rate ~ year * age | sex, 
  data=subset(data_younger, subset=sex=="male"), 
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab=list(label="Age in years", cex=1.4),
  xlab=list(label="Year", cex=1.4),
  cex=1.4,
  cuts=20,
  col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200),
  main=NULL,
  labels=list(cex=1.2),
  col="blue",
  scales=list(
    x=list(cex=1.4), 
    y=list(cex=1.4),
    alternating=3
  )
)
dev.off()


png(filename="figures/female_scot_younger.png", 
    width=23, height=20, res=300, units="cm"
)
# Let's look at the mort rates only
contourplot(
  convict_rate ~ year * age | sex, 
  data=subset(data_younger, subset=sex=="female"), 
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab=list(label="Age in years", cex=1.4),
  xlab=list(label="Year", cex=1.4),
  cex=1.4,
  cuts=20,
  col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200),
  main=NULL,
  labels=list(cex=1.2),
  col="blue",
  scales=list(
    x=list(cex=1.4), 
    y=list(cex=1.4),
    alternating=3
  )
)
dev.off()


## Difference between males and females


# This looks at the difference between males and females in crime rates
# at different ages and in different years

# this creates a subset of the data using only the variables of interest

data_mfratio <- data_younger  %>% 
  tbl_df   %>% 
  select(-convicted, -total) %>% 
  spread(key=sex, value=convict_rate)  %>% 
  mutate(mf_ratio = male/female)

# this re_arranges the data so that male and female convict_rates
# are reported in different columns, called male and female
# It uses the reshape2 package by Hadley Wickham


# This plots male_excess as a function of year and age in a contour plot
png("figures/mf_ratio.png",
    width=23, height=20, res=300, units="cm"
)
contourplot(
  mf_ratio ~ year * age, 
  data=subset(data_mfratio, age <=50), 
  region=T, 
  par.strip.text=list(cex=1.4, fontface="bold"),
  ylab=list(label="Age in years", cex=1.4),
  xlab=list(label="Year", cex=1.4),
  cex=1.4,
  at=1:10,
  col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200),
  main=NULL,
  labels=list(cex=1.2),
  col="blue",
  scales=list(
    x=list(cex=1.4), 
    y=list(cex=1.4),
    alternating=3
  )
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
data_younger %>%
  ggplot(data=., aes(y=convict_rate, x=age)) +
  geom_line(aes(colour=sex, linetype=sex)) +
  facet_wrap(~ year) +
  labs(x="Age in years", y="Conviction rate") +
  guides(colour=guide_legend("Sex"), linetype=guide_legend("Sex"))

# This saves a copy of the graphic created above to a new file.  
ggsave(
  "figures/age_sections.png", 
  width=20, height=20, dpi=300, unit="cm"
)

#### Cohort slides:

# This creates a birth cohort variable and adds it to the data_younger dataset

data_younger <- data_younger %>%
  mutate(cohort = year - age) %>%
  arrange(sex, cohort, age)


data_younger %>%
  ggplot(data=., aes(y=convict_rate, x=age)) +
  geom_line(aes(colour=sex, linetype=sex)) +
  facet_wrap(~ cohort)

data_younger %>%
  filter(cohort >=1960 & cohort <=1985) %>%
  ggplot(data=., aes(y=convict_rate, x=age)) +
  geom_line(aes(colour=sex, linetype=sex)) +
  facet_wrap(~ cohort) +
  labs(x="Age in years", y="Conviction rate") +
  guides(colour=guide_legend("Sex"), linetype=guide_legend("Sex"))

ggsave(
  "figures/cohort_sub_section.png", 
  width=25, height=20, dpi=300, unit="cm"
)


#### For completeness, age facets as well 
data_younger %>%
  filter(age <=35) %>%
  ggplot(data=., aes(y=convict_rate, x=year)) +
  geom_line(aes(colour=sex, linetype=sex)) +
  facet_wrap( ~ age) +
  labs(x="Age in years", y="Conviction rate") +
  guides(colour=guide_legend("Sex"), linetype=guide_legend("Sex")) +
  theme(
    axis.text.x = element_text(angle =90, hjust=1, vjust=0.5)
  )


# This saves a copy of the graphic created above to a new file.  
ggsave(
  "figures/age_facets.png", 
  width=20, height=20, unit="cm", dpi=300
)


#

# Relative age crime-curves by year -------------------------------------

data_younger  %>% 
  filter(year %in% c(1990, 1995, 2000, 2005, 2010))  %>% 
  group_by(year, sex)  %>% 
  mutate(r_rate = convict_rate/ max(convict_rate))  %>% 
  ggplot(data=.) + 
  geom_line(
    aes(
      x=age, 
      y=r_rate, 
      group=factor(year), 
      colour=factor(year)
    )
  ) + 
  facet_grid(~sex)


data_younger  %>% 
  group_by(year, sex)  %>% 
  summarise(mval=max(convict_rate))  %>% 
  ggplot(data=.) + 
  geom_line(aes(x=year, y=mval, group=sex, colour=sex, linetype=sex))


data_younger  %>% 
  group_by(year, sex)  %>% 
  summarise(mval=max(convict_rate))  %>% 
  ggplot(data=.) + 
  geom_line(aes(x=year, y=mval)) + 
  facet_wrap(~sex, scales = "free")



# Total crimes ------------------------------------------------------------


# Plot of total number of convictions each year

data %>% group_by(year) %>% 
  summarise(total_convicted = sum(convicted)) %>% 
  arrange(year) %>% 
  ggplot(.) +
  geom_line(aes(x= year, y = total_convicted)) + 
  ylim(x = 30000, y = 65000) + 
  labs(x = "Year", y = "Total number of people convicted")




# Proportion of convictions by single age ---------------------------------
png(filename="figures/prop_convictions_both.png", 
    width=40, height=20, res=300, units="cm"
)


data %>% 
  arrange(sex, year, age) %>% 
  group_by(year) %>% 
  mutate(prop_of_convictions = convicted / sum(convicted)) %>% 
  contourplot(
    prop_of_convictions ~ year * age | sex, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=20,
    col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200),
    main=NULL,
    labels=list(cex=1.2),
    col="blue",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
dev.off()



# Proportion of convictions by single age ---------------------------------
png(filename="figures/prop_convictions_combined.png", 
    width=20, height=20, res=300, units="cm"
)


data %>% 
  arrange(sex, year, age) %>%
  group_by(year, age) %>% 
  summarise(convicted = sum(convicted)) %>% 
  group_by(year) %>% 
  mutate(prop_of_convictions = convicted / sum(convicted)) %>% 
  contourplot(
    prop_of_convictions ~ year * age, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=20,
    col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200),
    main=NULL,
    labels=list(cex=1.2),
    col="blue",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
dev.off()


# Proportion of convictions by single age---------------------------------
png(filename="figures/prop_convictions_combined.png", 
    width=20, height=20, res=300, units="cm"
)


data %>% 
  arrange(sex, year, age) %>%
  group_by(year, age) %>% 
  summarise(convicted = sum(convicted)) %>% 
  group_by(year) %>% 
  mutate(prop_of_convictions = convicted / sum(convicted)) %>% 
  contourplot(
    prop_of_convictions ~ year * age, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=20,
    col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200),
    main=NULL,
    labels=list(cex=1.2),
    col="blue",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
dev.off()



# proportion of convictions in year by age, by sex but with differ --------




png(filename="figures/prop_convictions_males.png", 
    width=20, height=20, res=300, units="cm"
)


data %>% 
  arrange(sex, year, age) %>% 
  group_by(year) %>% 
  mutate(prop_of_convictions = convicted / sum(convicted)) %>%
  filter(sex == "male") %>% 
  contourplot(
    prop_of_convictions ~ year * age, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=20,
    col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200),
    main=NULL,
    labels=list(cex=1.2),
    col="blue",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
dev.off()


png(filename="figures/prop_convictions_females.png", 
    width=20, height=20, res=300, units="cm"
)


data %>% 
  arrange(sex, year, age) %>% 
  group_by(year) %>% 
  mutate(prop_of_convictions = convicted / sum(convicted)) %>%
  filter(sex == "female") %>% 
  contourplot(
    prop_of_convictions ~ year * age, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=20,
    col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200),
    main=NULL,
    labels=list(cex=1.2),
    col="blue",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
dev.off()





# Cumulative proportion of convictions ------------------------------------



# Proportion of convictions by single age ---------------------------------
png(filename="figures/prop_convictions_combined.png", 
    width=20, height=20, res=300, units="cm"
)


data %>% 
  arrange(sex, year, age) %>%
  group_by(year, age) %>% 
  summarise(convicted = sum(convicted)) %>% 
  group_by(year) %>% 
  mutate(prop_of_convictions = convicted / sum(convicted)) %>% 
  contourplot(
    prop_of_convictions ~ year * age, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=20,
    col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200),
    main=NULL,
    labels=list(cex=1.2),
    col="blue",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
dev.off()


# Proportion of convictions by single age---------------------------------
png(filename="figures/prop__cumulative_convictions_combined.png", 
    width=20, height=20, res=300, units="cm"
)


data %>% 
  arrange(sex, year, age) %>%
  group_by(year, age) %>% 
  summarise(convicted = sum(convicted)) %>%
  mutate(
    cumulative_convicted = cumsum(convicted),
    prop_cumulative_convicted = cumulative_convicted / sum(convicted)
    ) %>% 
  contourplot(
    prop_cumulative_convicted ~ year * age, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=20,
    col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200),
    main=NULL,
    labels=list(cex=1.2),
    col="blue",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
dev.off()



# Proportion of convictions by single age, by sex---------------------------------



data %>% 
  arrange(sex, year, age) %>%
  group_by(sex, year, age) %>% 
  summarise(convicted = sum(convicted)) %>%
  mutate(
    cumulative_convicted = cumsum(convicted),
    prop_cumulative_convicted = cumulative_convicted / sum(convicted)
  ) %>% 
  contourplot(
    prop_cumulative_convicted ~ year * age | sex, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=20,
    col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200),
    main=NULL,
    labels=list(cex=1.2),
    col="blue",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
dev.off()


tmp1 <- data %>% 
  arrange(year, age) %>% 
  group_by(year, age) %>% 
  summarise(convicted_yrage = sum(convicted)) %>% 
  group_by(year) %>% 
  mutate(cumulative_convicted_in_year = cumsum(convicted_yrage))

tmp2 <- data %>% group_by(year) %>% 
  summarise(total_convicted_in_year = sum(convicted))



png(filename="figures/prop__cumulative_convictions_by_sex.png", 
    width=40, height=20, res=300, units="cm"
)
data %>%
  group_by(sex, year) %>% 
  arrange(age) %>% 
  mutate(cumulative_convicted_in_year_sex = cumsum(convicted)) %>% 
  left_join(tmp2) %>% 
  mutate(prop_cumulative_convicted_in_year_sex = cumulative_convicted_in_year_sex / total_convicted_in_year) %>% 
  select(sex, year, age, convicted, proportion = prop_cumulative_convicted_in_year_sex) %>% 
  contourplot(
    proportion ~ year * age | sex, 
    data=., 
    region=T, 
    par.strip.text=list(cex=1.4, fontface="bold"),
    ylab=list(label="Age in years", cex=1.4),
    xlab=list(label="Year", cex=1.4),
    cex=1.4,
    cuts=20,
    col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200),
    main=NULL,
    labels=list(cex=1.2),
    col="blue",
    scales=list(
      x=list(cex=1.4), 
      y=list(cex=1.4),
      alternating=3
    )
  )
dev.off()
