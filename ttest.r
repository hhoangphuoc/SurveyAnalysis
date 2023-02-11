
library(tidyverse)
library(janitor)
library(psych)
library(broom)
library(modelr)
library(CTT)

# load the clean dataset
data <- read.csv("clean_data.csv")

data <- na.omit(data)

# mutate the columns for specific variables
# SMUsage scale - remove item 1,5
# cyberbullying experience - not remove any item
# SocialIsolation scale - not removing any item
data <- data %>%
    mutate(
        SMUsage = (SM.usage_2 + SM.usage_3 + SM.usage_4 + SM.usage_6) / 4,
        SocialIsolation = (COVID.19.Scale_1 + COVID.19.Scale_2
            + COVID.19.Scale_3 + COVID.19.Scale_4 + COVID.19.Scale_5
            + COVID.19.Scale_6 + COVID.19.Scale_7) / 7,
        CyberbullyingExp = (Cyberbyllying.exp_1 + Cyberbyllying.exp_2
            + Cyberbyllying.exp_3 + Cyberbyllying.exp_4 + Cyberbyllying.exp_5
            + Cyberbyllying.exp_6) / 6,
        MentalHealth = (Cyberbullying.health_1 + Cyberbullying.health_2
            + Cyberbullying.health_3 + Cyberbullying.health_4
            + Cyberbullying.health_5 + Cyberbullying.health_6
            + Cyberbullying.health_7) / 7
    )

data %>% glimpse()

## TESTING RELATION BETWEEN GENDER AND CYBERBULLYING EXPERIENCE

## First t-test variable: Gender ( Male & Female)
data %>%
    tabyl(Gender)

# We see that the number of respondents answering Male(1): 40.88%; Female(2): 54.74%
# are the majority of the respondents, so we use this as our testing variable.
data_gender <- data %>%
    filter(Gender == 1 | Gender == 2)

# change gender to categorical variable
data_gender$Gender <- as.factor(data_gender$Gender)
data_gender <- data_gender %>%
    mutate(
        Gender = case_when(
            Gender == 1 ~ "Male",
            Gender == 2 ~ "Female",
        ),
        Gender = factor(Gender)
    )
data_gender %>%
    select(Gender) %>%
    summary()

data_gender %>% View()
# Do you get two values that are both >30 and roughly similar to each other?
# After this, go to the ANALYSIS section, below.

#--------------------------------------------#
# ANALYSIS
# As the first step of the analysis, verify for yourself that you have the correct data frame (use the last you worked with in the previous step),
# grouping variable (XXX) with two comparable groups, and a numerical dependent variable that you want to compare differences in the two groups on.


# First, you will want to look at the comparison you will be making visually and check the descriptive statistics.
# For this you should use a box plot with XXX on the x-axis and YYY on the y-axis.
# In the script below, replace YYY with the name of your numerical variable, XXX with the name of your grouping variable, and URDATA with the name of the data frame.
data_gender %>%
    ggplot(aes(x = Gender, y = CyberbullyingExp)) +
    geom_boxplot()
# Interpret the outcomes. Which group seems to cluster on the higher side? Let's have a look at the means and standard deviations of both groups.
# In the script below, replace YYY with the name of your numerical variable, XXX with the name of your grouping variable, and URDATA with the name of the data frame.
DescStats <- data_gender %>%
    group_by(Gender) %>%
    summarise(
        meanDependent = mean(CyberbullyingExp),
        sdDependent = sd(CyberbullyingExp),
    )
View(DescStats)

fligner.test(CyberbullyingExp ~ Gender, data = data_gender)
# All you need to do with the output of this test is look at the p-value. If this value is higher than .05, you don't need to do anything special.
# If the value is below .05, change 'var.equal' to be FALSE instead of TRUE.


# Next is the command for the t-test itself.
# In the script below, replace YYY with the name of your numerical variable, XXX with the name of your grouping variable, and URDATA with the name of the data frame.
t.test(CyberbullyingExp ~ Gender, data = data_gender, var.equal = TRUE)


#--------------------------------------------#
data_gender %>%
    ggplot(aes(x = Gender, y = MentalHealth)) +
    geom_boxplot()
DescStats <- data_gender %>%
    group_by(Gender) %>%
    summarise(
        meanDependent = mean(MentalHealth),
        sdDependent = sd(MentalHealth),
    )
View(DescStats)

fligner.test(MentalHealth ~ Gender, data = data_gender)

t.test(MentalHealth ~ Gender, data = data_gender, var.equal = TRUE)

#--------------------------------------------#

## Second t-test variable: Time Spent ( More than 3 hour & Less than 3 hour)
data %>%
    tabyl(Time.spent.SM)

data_timespent <- data %>%
    mutate(
        Time.spent.SM = case_when(
            Time.spent.SM == 1 ~ "Less than 1 hour",
            Time.spent.SM == 2 ~ "Between 1 and 3 hours",
            Time.spent.SM == 3 ~ "More than 3 hours"
        ),
        Time.spent.SM = factor(Time.spent.SM)
    )
# data_timespent <- data_timespent

levels(data_timespent$Time.spent.SM) <- list(
    "More than 3 hours" = c("More than 3 hours"),
    "Less than 3 hours" = c("Between 1 and 3 hours", "Less than 1 hour")
)


# To make sure this worked, look at how many respondents are in either group.
# Replace URDATAT with the name of your data frame, and XXX with the name of your dichotomous grouping variable.
data_timespent %>%
    select(Time.spent.SM) %>%
    summary()

data_timespent %>% View()

#--------------------------------------------#
# ANALYSIS
# As the first step of the analysis, verify for yourself that you have the correct data frame (use the last you worked with in the previous step),
# grouping variable (XXX) with two comparable groups, and a numerical dependent variable that you want to compare differences in the two groups on.
data_timespent %>%
    ggplot(aes(x = Time.spent.SM, y = SocialIsolation)) +
    geom_boxplot()

DescStats <- data_timespent %>%
    group_by(Time.spent.SM) %>%
    summarise(
        meanDependent = mean(SocialIsolation),
        sdDependent = sd(SocialIsolation),
    )
View(DescStats)

fligner.test(SocialIsolation ~ Time.spent.SM, data = data_timespent)

# Next is the command for the t-test itself.
# In the script below, replace YYY with the name of your numerical variable, XXX with the name of your grouping variable, and URDATA with the name of the data frame.
t.test(SocialIsolation ~ Time.spent.SM, data = data_timespent, var.equal = TRUE)
