# This script gives you an example of how to prepare a csv file with survey answers
# downloaded from qualtrics for data analysis

# start by opening a project
# save the csv file to the project folder, as well as this script and open the script

library(tidyverse)


# we will use the numeric data, since it is easier to analyze with numeric answer instead of text.
survey_data <- read.csv("survey_data_numeric.csv", sep = ",")

# The first row is just the question that corresponds with the variable.
# Since we don't want that as a datarow, but do want to keep the data around, we can assign this row as a label
library(Hmisc)
label(survey_data) <- survey_data[1, ]
# Now that we safely stored this extra info as labels, we can remove the first 2 rows, so we only keep the actual data.
survey_data <- survey_data[-c(1, 2), ]

# Remove all rows where the distribution channel is "Preview" because this was just us testing out the survey
survey_data <- survey_data %>%
  filter(DistributionChannel != "preview")

# change missing values (where the value is "") into NA
survey_data <- survey_data %>%
  na_if("")

# can we now start analyzing the data?
survey_data %>% glimpse()
# No, because R sees all numbers in the dataset as text

# We can convert a column to numeric with the as.numeric function
# If a value can't be converted to numeric because it is text, it will change it to NA
# Therefore, we can apply the as.numeric function to all columns where the amount of NA's doesn't increase if we apply the function
survey_data <- survey_data %>%
  mutate_if(
    function(x) {
      x %>%
        as.numeric() %>%
        is.na() %>%
        sum() <= x %>%
        is.na() %>%
        sum()
    }, as.numeric
  )
# As you can see from the complicated code above, the as.numeric function is tricky

# Now we remove some rows and columns which we don't need for the analysis
# remove unnecessary columns
survey_data <- survey_data %>%
  select(-c(1:17))

# remove the row which respondent not consent to participate in the survey
# and they are using social media
survey_data <- survey_data %>%
  filter(Consent == 1 & SM.use == 1)


#----------------------------------------------------------
# We can also using these code to remove all columns by name, which are not
# related to the analysis.
survey_data <- survey_data %>% select(-c(
  "Gender_4_TEXT",
  "Most.used.SM_9_TEXT",
  "Education.level",
  "Education.level_4_TEXT",
  "Closure"
))

# omit all NA's data
survey_data <- survey_data %>% na.omit()
#----------------------------------------------------------

# View the final clean data
survey_data %>% View()


#----------------------------------------------------------
# Write the clean data to other file
# alternatively, you can write the file to your project folder
write_csv(survey_data, "clean_data.csv")
# We can now start analyzing
