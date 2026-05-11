# Using the Qualtrics API

# Load Libraries
# install.packages("qualtRics")
library(qualtRics)

# Add Qualtrics credentials ####
## Instructions for finding crendentials: https://api.qualtrics.com/1aea264443797-base-url-and-datacenter-i-ds
## Note: you'll only need to do this step once if you include "install = true".
qualtrics_api_credentials(
  api_key = "<YOUR-QUALTRICS_API_KEY>", 
  base_url = "<YOUR-QUALTRICS_BASE_URL>",
  install = TRUE)

# Generate table surveys that are available to you ####
surveys <- all_surveys()

# Access data from a specific survey ####
example_survey_tbl <- fetch_survey(
  surveyID = "surveyID",
  verbose = TRUE
)

# Create data dictionary ####
data_dictionary <- extract_colmap(example_survey_tbl)
