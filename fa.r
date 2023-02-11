# import require library
library(tidyverse)
library(psych)
library(broom)
library(modelr)
library(janitor)
library(CTT)


# load the clean dataset
dat <- read.csv("clean_data.csv")


dat_4 <- dat %>% 
  
  select(Cyberbullying.health_1:Cyberbullying.health_7) %>% 
  
  na.omit() 

# scree plot 

dat_4 %>% 
  
  scree(., factors = FALSE) 

#factor analysis 

dat_4 %>%  
  
  factanal(., factors = 2) 

dat_4 %>% cor() 

#Eigenvalues 

pcaScale4 <- dat_4 %>% 
  
  cor() %>% 
  
  eigen() 

pcaScale4$values 

# cronbach's alpha 

resultsScale4 <- dat_4 %>% 
  
  as.matrix() %>% 
  
  itemAnalysis() 

resultsScale4$alpha 

resultsScale4$itemReportl 

# dat %>%
#  view()

# check the summary of the data
# dat %>%
#     glimpse()

# dat <- dat %>% na.omit()

######
# Factor Analysis & Reliability Analysis
######

# Scale 1: Social media Usage (Columns 6 - 11)
dat_1 <- dat %>% select(SM.usage_1:SM.usage_6)

dat_1 %>% summary()
# Standard deviations of each item
dat_1 %>% map(sd)

# Correlations between items
dat_1 %>% cor()

# Factor Analysis --------------------------------------------

# calculate eigenvalues and scree plot for determining the number of factors
pca <- dat_1 %>%
    cor() %>%
    eigen()
eigenvalues <- pca$values
eigenvalues

# scree plot
dat_1 %>%
    scree(, factors = FALSE)

dat_1 %>%
    factanal(., factors = 1)

# As we can see, there is only one factor possible in this scale,
# so the answer for factor analysis of first scale is 1

#-----------------------------------------

# Reliability Analysis - Item analysis
# cronbach's alpha
results <- dat_1 %>%
    as.matrix() %>%
    itemAnalysis()
results$alpha
results$itemReport

#--------------------------------------------#
# As we can see, the alpha of this scale is 0.686, which is close to acceptable,
# but the item themselves are not so closely related to each other.
# Besides, the item 1 and item 5 has low item-rest correlation and removing those
# not affected to the Cronbach's alpha so we decide to drop them from the scale.
#--------------------------------------------#


################################
# Scale 2: Cyberbullying experience ( Columns 12 - 17)
################################


######
# Factor Analysis & Reliability Analysis
######

# Scale 1: Social media Usage (Columns 6 - 11)
dat_2 <- dat %>% select(Cyberbyllying.exp_1:Cyberbyllying.exp_6)

dat_2 %>% summary()
# Standard deviations of each item
dat_2 %>% map(sd)

# Correlations between items
dat_2 %>% cor()


# Factor Analysis
# calculate eigenvalues and scree plot for determining the number of factors
pca <- dat_2 %>%
    cor() %>%
    eigen()
eigenvalues <- pca$values
eigenvalues

# scree plot
dat_2 %>% scree(, factors = FALSE)

dat_2 %>%
    factanal(., factors = 1)

#-----------------------------------------

# Reliability Analysis - Item analysis
# cronbach's alpha
results <- dat_1 %>%
    as.matrix() %>%
    itemAnalysis()
results$alpha
results$itemReport

#---------------------------------------------------------------------------#


################################
# Scale 3: Social Isolation experience (Columns 18 - 23)
################################

# Factor Analysis & Reliability Analysis
dat_3 <- dat %>% select(COVID.19.Scale_1:COVID.19.Scale_7)

dat_3 %>% summary()
# Standard deviations of each item
dat_3 %>% map(sd)

# Correlations between items
dat_3 %>% cor()


# Factor Analysis
# calculate eigenvalues and scree plot for determining the number of factors
pca <- dat_3 %>%
    cor() %>%
    eigen()
eigenvalues <- pca$values
eigenvalues

# scree plot
dat_3 %>% scree(, factors = FALSE)

# 2 factors are possible in this scale,
# we do the factor analysis with 2 factors
dat_3 %>%
    factanal(., factors = 2)

# dimension 1: item 1, item 2
# dimension 2: item 4, item 5, item 6, item 7

#-----------------------------------------

# Reliability Analysis - Item analysis
# cronbach's alpha
results <- dat_3 %>%
    as.matrix() %>%
    itemAnalysis()
results$alpha
results$itemReport

# alpha = 0.7298, which is acceptable,
# meaning that the items are closely related to each other

################################
# Scale 4: Mental Health
################################
# Factor Analysis & Reliability Analysis
dat_4 <- dat %>% select(Cyberbullying.health_1:Cyberbullying.health_7)

dat_4 %>% summary()
# Standard deviations of each item
dat_4 %>% map(sd)

# Correlations between items
dat_4 %>% cor()


# Factor Analysis
# calculate eigenvalues and scree plot for determining the number of factors
pca <- dat_4 %>%
    cor() %>%
    eigen()
eigenvalues <- pca$values
eigenvalues

# scree plot
dat_4 %>% scree(, factors = FALSE)

# 2 factors are possible in this scale,
# we do the factor analysis with 2 factors
dat_4 %>%
    factanal(., factors = 2)

# dimension 1: item 1, item 2, item 3, item 4
# dimension 2: item 5, item 6, item 7

#-----------------------------------------

# Reliability Analysis - Item analysis
# cronbach's alpha
results <- dat_4 %>%
    as.matrix() %>%
    itemAnalysis()
results$alpha
results$itemReport

# alpha = 0.834, which is good,
# meaning that the items are closely related to each other
# however the reliability of item 1 is not good (0.37), but it still
# a good consistency, so we still keep it for analysis


