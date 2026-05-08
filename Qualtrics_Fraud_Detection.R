# Identifying Potential Spam Responses Using Qualtrics Fraud Detection Fields

library(janitor)
library(tidyverse)
library(stringr)

survey_tbl <- raw_survey_tbl %>%
  # Clean the variable names from Qualtrics
  clean_names() %>%

  # Create spam indicator variables
  mutate(
    ## Create bot detection and duplicate detection indicator variables 
    recap_exclude = ifelse(q_recaptcha_score<0.5, 1, 0), # Recaptcha/Bot Detection
    dup_exclude = ifelse(q_duplicate_respondent == TRUE, 1, 0), # Duplicate Detection
    
    ## Create additive exclusion score
    exclude = recap_exclude + dup_exclude, # Exclusion is an additive score
    
    ## Identify potential spam, including an example of tweaking criteria to maintain responses that seem legitimate
    potential_spam = case_when(
      q2 == "Yes" & str_length(q3) == 4 ~ 0, # Exclude postcard responses with a 4-digit postcard code from fraud detection
      exclude > 0 ~ 1,
      TRUE ~ 0)
    ) 

# Investigate the breakdown of spam vs. non-spam responses ####
survey_tbl %>% tabyl(potential_spam)

# Investigate spam responses ####
potential_spam <- survey_tbl %>%
  filter(potential_spam == 1)

# Remove spam from analysis file ####
analysis_tbl <- survey_tbl %>%
  filter(potential_spam == 0)
