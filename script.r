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

data <- data  %>% 
  mutate(convict_rate = convicted/total) %>%
  mutate(cohort = year - age) %>%
  arrange(sex, cohort, age)

data_younger <- data  %>% 
  filter(age <= 60) 
  
data_mfratio <- data_younger  %>% 
  tbl_df   %>% 
  select(-convicted, -total) %>% 
  spread(key=sex, value=convict_rate)  %>% 
  mutate(mf_ratio = male/female)

data_mfratio_smoothed <- smooth(data_mfratio, 1.5)

# Contour plot - all ages, both sexes  ------------------------------------------------

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


# Contour plot: younger ages, both sexes ----------------------------------

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



# contour plot: younger ages, males ---------------------------------------


# identity scale, younger ages 
png(filename="figures/male_scot_younger.png", 
    width=23, height=20, res=300, units="cm"
)
# Let's look at the mort rates only
contourplot(
  convict_rate ~ year * age, 
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


# contour plot: younger ages, females -------------------------------------

png(filename="figures/female_scot_younger.png", 
    width=23, height=20, res=300, units="cm"
)
# Let's look at the mort rates only
contourplot(
  convict_rate ~ year * age, 
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


# contour plot: male/female ratio -----------------------------------------


png("figures/mf_ratio.png",
    width=23, height=20, res=300, units="cm"
)
p1 <- contourplot(
      mf_ratio ~ year * age, 
      data=subset(data_mfratio_smoothed, age <=50),
      cex=1.4,
      ylab="",
      xlab="",
      at=1:10,
      main=NULL,
      col="blue",
      labels=list(cex=1.2),
      scales=list(NULL)
)

p2 <- levelplot(
  mf_ratio ~ year * age, 
  data = subset(data_mfratio, age <=50),
  region=T,
  ylab=list(label="Age in years", cex=1.4),
  xlab=list(label="Year", cex=1.4),
  scales=list(
    x=list(cex=1.4), 
    y=list(cex=1.4),
    alternating=3
  ),
  col.regions=colorRampPalette(brewer.pal(6, "Greys"))(200)
)

print(p2 + p1)
dev.off()

# Illustration of Lexis surface -------------------------------------------
png("figures/lexis_plot_illustration.png",
    width=10, height=10, res=300, units="cm"
)
levelplot(
  convict_rate ~ year * age, 
  data=subset(data_younger, subset=sex=="male"), 
  region=T, 
  ylab=list(label="Age in years", cex=1),
  xlab=list(label="Year", cex=1),
  cuts=20,
  col.regions="white",
  main=NULL,
  col="blue",
  scales=list(
    x=list(cex=1), 
    y=list(cex=1),
    alternating=3
  ),
  colorkey=FALSE,
  panel=function(x,y, ...){
    panel.abline(v=seq(1990, 2010, by=5), col="red", lwd=2)
    panel.abline(h=seq(20, 60, by=5), col="green")

    panel.abline(a=-1935, b=1, col="blue", lty="dashed")
    panel.abline(a=-1940, b=1, col="blue", lty="dashed")
    panel.abline(a=-1945, b=1, col="blue", lty="dashed")
    panel.abline(a=-1950, b=1, col="blue", lty="dashed")
    panel.abline(a=-1955, b=1, col="blue", lty="dashed")
    panel.abline(a=-1960, b=1, col="blue", lty="dashed")
    panel.abline(a=-1965, b=1, col="blue", lty="dashed")
    panel.abline(a=-1970, b=1, col="blue", lty="dashed")
    panel.abline(a=-1975, b=1, col="blue", lty="dashed")
    panel.abline(a=-1980, b=1, col="blue", lty="dashed")
    panel.abline(a=-1985, b=1, col="blue", lty="dashed")  
    panel.abline(a=-1990, b=1, col="blue", lty="dashed")     
  }
)
dev.off()

# STL files ---------------------------------------------------------------




convict_matrix_male <- data_younger %>%
  filter(sex=="male") %>%
  select(year, age, convict_rate) %>%
  spread(key=year, value=convict_rate) %>%
  make_matrix


convict_matrix_female <- data_younger %>%
  filter(sex=="female") %>%
  select(year, age, convict_rate) %>%
  spread(key=year, value=convict_rate) %>%
  make_matrix

# Add 2 % to all values

r2stl(
    x=as.numeric(rownames(convict_matrix_male)),
    y=as.numeric(colnames(convict_matrix_male)),
    z=convict_matrix_male + max(convict_matrix_male) * 0.02,
    
    filename="stl/scot_younger_male.stl",
    z.expand=T,
    show.persp=F
  )

r2stl(
  x=as.numeric(rownames(convict_matrix_female)),
  y=as.numeric(colnames(convict_matrix_female)),
  z=convict_matrix_female + max(convict_matrix_male) * 0.02,
  
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
  z=convict_matrix_both + max(convict_matrix_both) * 0.02,
  
  filename="stl/scot_younger_both_gender.stl",
  z.expand=T,
  show.persp=F
)



# Small multiples ---------------------------------------------------------

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


# age- shape and scale curve: shape --------------------------------

data_younger  %>% 
  select(-country)  %>% 
  group_by(year, sex)  %>% 
  arrange(year, sex, age)  %>% 
  mutate(
    rel_convict_rate = convict_rate / max(convict_rate),
    year_group = cut(
      year, 
      breaks=c(1989, 1995, 2000, 2005, 2012),
      include.lowest=T, right=T
      )
    ) %>%
  ggplot(data=.) +
  geom_line(aes(x=age, y=rel_convict_rate, group=year, colour=year_group), alpha=0.5) + 
  facet_wrap(~ sex)  + 
  labs(x="Age in years", y="Relative conviction rate", colour="Year range") +
  guides(colour=guide_legend(nrow=2)) +
  theme(legend.position="bottom")
  
ggsave(
  "figures/relative_agecrime.png", 
  width=12, height=12, unit="cm", dpi=300
)

# with smoother
data_younger  %>% 
  select(-country)  %>% 
  group_by(year, sex)  %>% 
  arrange(year, sex, age)  %>% 
  mutate(
    rel_convict_rate = convict_rate / max(convict_rate),
    year_group = cut(
      year, 
      breaks=c(1989, 1995, 2000, 2005, 2012),
      include.lowest=T, right=T
    )
  ) %>%
  ggplot(data=.) +
  geom_point(aes(x=age, y=rel_convict_rate, group=cohort), alpha=0.2) + 
  stat_smooth(aes(x=age, y=rel_convict_rate, group=year_group, colour=year_group)) + 
  facet_wrap(~ sex)  + 
  labs(x="Age in years", y="Relative conviction rate", colour="Year range") +
  guides(colour=guide_legend(nrow=2)) +
  theme(legend.position="bottom")

ggsave(
  "figures/relative_agecrime_smoothed.png", 
  width=12, height=12, unit="cm", dpi=300
)

# age- shape and scale curve: scale --------------------------------

data_younger  %>% 
  select(-country)  %>% 
  group_by(sex, year)  %>% 
  summarise(
    max_convict_rate =  max(convict_rate)
  ) %>% 
  ggplot(data=.) +
  geom_line(aes(x=year, y=max_convict_rate)) + 
  facet_wrap(~ sex, scales="free")  + 
  labs(x="Year", y="Maximum age-specific \nconviction rate") +
  theme(
    axis.text.x = element_text(angle =90, hjust=1, vjust=0.5)
  )

ggsave(
  "figures/max_agecrime.png", 
  width=12, height=8, unit="cm", dpi=300
)


# cohort- shape and scale curve: shape --------------------------------

data_younger  %>% 
  select(-country)  %>% 
  group_by(cohort, sex)  %>% 
  arrange(cohort, sex, age)  %>%
  filter(cohort >=1970 & cohort <= 1985) %>%
  mutate(
    rel_convict_rate = convict_rate / max(convict_rate),
    cohort_group = cut(
      cohort, 
      breaks=c(1970, 1975, 1980, 1985),
      include.lowest=T, right=T
    )
  ) %>%
  ggplot(data=.) +
  geom_line(aes(x=age, y=rel_convict_rate, group=cohort, colour=cohort_group), alpha=0.5) + 
  facet_wrap(~ sex)  + 
  labs(x="Age in years", y="Relative conviction rate", colour="Cohort range") +
  guides(colour=guide_legend(nrow=2)) +
  theme(legend.position="bottom")

ggsave(
  "figures/relative_agecrime_cohort.png", 
  width=12, height=12, unit="cm", dpi=300
)

# with smoother? ----------------------------------------------------------

data_younger  %>% 
  select(-country)  %>% 
  group_by(cohort, sex)  %>% 
  arrange(cohort, sex, age)  %>%
  filter(cohort >=1970 & cohort <= 1985) %>%
  mutate(
    rel_convict_rate = convict_rate / max(convict_rate),
    cohort_group = cut(
      cohort, 
      breaks=c(1970, 1975, 1980, 1985),
      include.lowest=T, right=T
    )
  ) %>%
  ggplot(data=.) +
  geom_point(aes(x=age, y=rel_convict_rate, group=cohort), alpha=0.2) + 
  stat_smooth(aes(x=age, y=rel_convict_rate, group=cohort_group, colour=cohort_group)) + 
  facet_wrap(~ sex)  + 
  labs(x="Age in years", y="Relative conviction rate", colour="Cohort range") +
  guides(colour=guide_legend(nrow=2)) +
  theme(legend.position="bottom")

ggsave(
  "figures/relative_agecrime_smoothed_cohort.png", 
  width=12, height=12, unit="cm", dpi=300
)


# age- shape and scale curve: scale --------------------------------

data_younger  %>% 
  select(-country)  %>% 
  filter(cohort >=1970 & cohort <= 1985) %>%
  group_by(sex, cohort)  %>% 
  summarise(
    max_convict_rate =  max(convict_rate)
  ) %>% 
  ggplot(data=.) +
  geom_line(aes(x=cohort, y=max_convict_rate)) + 
  facet_wrap(~ sex, scales="free")  + 
  labs(x="Birth cohort", y="Maximum age-specific \nconviction rate") +
  theme(
    axis.text.x = element_text(angle =90, hjust=1, vjust=0.5)
  )

ggsave(
  "figures/max_agecrime_cohort.png", 
  width=12, height=8, unit="cm", dpi=300
)



# Nonlinear least squares regression on Gamma density function ------------

# Formula is 

# ((bta^-alpha) / gamma(alpha)) * (I(age - min(age)) ^ (alpha - 1)) * exp(-(1/bta) * I(age - min(age)))
# try with a single year's data
tmp <- data_younger %>%
  filter(year == 1989 & sex=="female") %>%
  nls(
    formula = convict_rate ~ ((bta^-alpha) / gamma(alpha)) * (I(age - min(age)) ^ (alpha - 1)) * exp(-(1/bta) * I(age - min(age))),
    data=.,
    algorithm="port",
    start=list(bta=1, alpha=10),
    lower=list(bta=0.01,alpha=0.01),
    trace=T
    )

preds <- function(age, alpha, beta){
  convict_rate = ((beta^-alpha) / gamma(alpha)) * 
    (age - min(age)) ^ (alpha - 1) * 
    exp(-(1/beta) * (age - min(age)))
  return(convict_rate)
}

tmp <- data_younger %>%
  filter(year == 1989 & sex =="female") %>%
  glm(convict_rate ~ I(age - min(age)), 
      data=.,
      family=Gamma(link="identity")
      )

