
# 0.0 Install Packages and Load Libraries 
# install.packages("tidyverse")
# install.packages("tidycensus")
# install.packages("viridisLite")
# install.packages("viridis")
# install.packages("xlsx")
# install.packages("sf")
# install.packages("sampling")
# install.packages("survey")
# install.packages("plotly")

library(tidyverse) # data wrangling
library(tidycensus) 
library(viridisLite)
library(viridis)
library(sf)
library(sampling)
library(plotly)
library(readxl) # reading in excel files
library(janitor) # clean up
library(magrittr)
library(stringr)
library(scales) # to use the percent() for the summary file
library(gtsummary) # statistical tests and table
library(gt) # Editing GT tables
library(survey)
options(tigris_use_cache = TRUE)

# Example Dataset: This dataset is a synthetic dataset modeled after the data collection strategy used in Mecklenburg County's CHA. It includes a mix of probability and non-probability responses, with a variable indicating the municipality of residence for each respondent. This dataset is similar to a csv export from Qualtrics. 

# Read data
analysis_dataset <- read_csv("synthetic_qualtrics_dataset.csv") %>%
  mutate(
    municipality = str_to_title(municipality),
    response_type = factor(response_type)
  )

##  Basic structure 
# Dataset dimensions
dim(analysis_dataset)

# Variable names and types
glimpse(analysis_dataset)


## Response counts 
# Number of responses by mode (Web vs. Postcard)
table(analysis_dataset$response_type)


##  Municipality distribution 
# Number of responses by municipality
table(analysis_dataset$municipality)

# Bar chart: responses by municipality
analysis_dataset %>%
  ggplot(aes(x = municipality)) +
  geom_bar(fill = "#619CFF") +
  labs(
    title = "Survey Responses by Municipality",
    x = "Municipality",
    y = "Number of Responses"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1)
  )

# 1.0 Investigate ACS Variables ----
## 1.1  Base Tables/Detailed Tables (B) ----
acs_vars_b <- load_variables(2023, "acs5", cache = FALSE) # Update year

acs_vars_b_fil <- acs_vars_b %>% filter(geography == "tract")

## 1.2 Data Profiles (DP) ----
acs_vars_dp <- load_variables(2023, "acs5/profile", cache = FALSE) # Update year

## 1.3 Subject tables (S) ----
acs_vars_s <- load_variables(2023, "acs5/subject", cache = FALSE) # Update year


# 2.0 Pull ACS Data from B/DP/S Tables -----------------------------------------
# ACS considers these municipalities (towns) as "Place"
acs_tbl <- get_acs(
  geography = "place",
  year = 2023, # Default is most recent year
  state = "NC",
  survey = "acs5", # Options are acs1, acs3 (for available yrs), acs5
  geometry = F, # If TRUE, it gives feature geometry (for R)

  # Plug in the variables you want to pull, example variables included below
  variables = c(
    total_pop = "S0101_C01_026",
    #Gender
    male_pop = "S0101_C03_026",
    female_pop = "S0101_C05_026",
    age_18_24 = "S0101_C01_023",
    age_25_29 = "S0101_C01_007",
    age_30_34 = "S0101_C01_008",
    age_35_39 = "S0101_C01_009",
    age_40_44 = "S0101_C01_010",
    age_45_49 = "S0101_C01_011",
    age_50_54 = "S0101_C01_012",
    age_55_59 = "S0101_C01_013",
    age_60_64 = "S0101_C01_014",
    age_65_69 = "S0101_C01_015",
    age_70_74 = "S0101_C01_016",
    age_75_79 = "S0101_C01_017",
    age_80_84 = "S0101_C01_018",
    age_85_up = "S0101_C01_019",
    # Race
    wht_total = "B01001A_001",
    wht_under_5_male = "B01001A_003",
    wht_5_9_male = "B01001A_004",
    wht_10_14_male = "B01001A_005",
    wht_15_17_male = "B01001A_006",
    wht_under_5_female = "B01001A_018",
    wht_5_9_female = "B01001A_019",
    wht_10_14_female = "B01001A_020",
    wht_15_17_female = "B01001A_021",
    blk_total = "B01001B_001",
    blk_under_5_male = "B01001B_003",
    blk_5_9_male = "B01001B_004",
    blk_10_14_male = "B01001B_005",
    blk_15_17_male = "B01001B_006",
    blk_under_5_female = "B01001B_018",
    blk_5_9_female = "B01001B_019",
    blk_10_14_female = "B01001B_020",
    blk_15_17_female = "B01001B_021",
    aian_total = "B01001C_001",
    aian_under_5_male = "B01001C_003",
    aian_5_9_male = "B01001C_004",
    aian_10_14_male = "B01001C_005",
    aian_15_17_male = "B01001C_006",
    aian_under_5_female = "B01001C_018",
    aian_5_9_female = "B01001C_019",
    aian_10_14_female = "B01001C_020",
    aian_15_17_female = "B01001C_021",
    asian_total = "B01001D_001",
    asian_under_5_male = "B01001D_003",
    asian_5_9_male = "B01001D_004",
    asian_10_14_male = "B01001D_005",
    asian_15_17_male = "B01001D_006",
    asian_under_5_female = "B01001D_018",
    asian_5_9_female = "B01001D_019",
    asian_10_14_female = "B01001D_020",
    asian_15_17_female = "B01001D_021",
    nhopi_total = "B01001E_001",
    nhopi_under_5_male = "B01001E_003",
    nhopi_5_9_male = "B01001E_004",
    nhopi_10_14_male = "B01001E_005",
    nhopi_15_17_male = "B01001E_006",
    nhopi_under_5_female = "B01001E_018",
    nhopi_5_9_female = "B01001E_019",
    nhopi_10_14_female = "B01001E_020",
    nhopi_15_17_female = "B01001E_021",
    other_total = "B01001F_001",
    other_under_5_male = "B01001F_003",
    other_5_9_male = "B01001F_004",
    other_10_14_male = "B01001F_005",
    other_15_17_male = "B01001F_006",
    other_under_5_female = "B01001F_018",
    other_5_9_female = "B01001F_019",
    other_10_14_female = "B01001F_020",
    other_15_17_female = "B01001F_021",
    mult_total = "B01001G_001",
    mult_under_5_male = "B01001G_003",
    mult_5_9_male = "B01001G_004",
    mult_10_14_male = "B01001G_005",
    mult_15_17_male = "B01001G_006",
    mult_under_5_female = "B01001G_018",
    mult_5_9_female = "B01001G_019",
    mult_10_14_female = "B01001G_020",
    mult_15_17_female = "B01001G_021",
    hisp_total = "B01001I_001",
    hisp_under_5_male = "B01001I_003",
    hisp_5_9_male = "B01001I_004",
    hisp_10_14_male = "B01001I_005",
    hisp_15_17_male = "B01001I_006",
    hisp_under_5_female = "B01001I_018",
    hisp_5_9_female = "B01001I_019",
    hisp_10_14_female = "B01001I_020",
    hisp_15_17_female = "B01001I_021"
  ),
  output = "wide" 
)


# 3.0 Filter for Mecklenburg County Municipalities and Select Variables ----
# Identify the GEOIDs for the municipalities in Mecklenburg County. 
meck_geoids <- c(3712000, 3714700, 3716400, 3733120, 3741960, 3743480, 3752220)

municipality <- c("CHARLOTTE", "CORNELIUS", "DAVIDSON", "HUNTERSVILLE", "MATTHEWS", "MINT HILL", "PINEVILLE" )

# Filter 
acs_total <- acs_tbl %>%
  filter(GEOID %in% meck_geoids) %>%
  select(
    GEOID,
    total_popE) %>%
  cbind(municipality, .)

## 3.1 Fixing age groups ----
acs_age <- acs_tbl %>%
  filter(GEOID %in% meck_geoids) %>%
  select(
    GEOID,
    #total_popE,
    #age_under_18E,
    age_18_24E,
    age_25_29E,
    age_30_34E,
    age_35_39E,
    age_40_44E,
    age_45_49E,
    age_50_54E,
    age_55_59E,
    age_60_64E,
    age_65_69E,
    age_70_74E,
    age_75_79E,
    age_80_84E,
    age_85_upE
  ) %>%
  mutate(
    age_25_44E = age_25_29E + age_30_34E + age_35_39E + age_40_44E,
    age_45_64E = age_45_49E + age_50_54E + age_55_59E + age_60_64E,
    age_65_84E = age_65_69E + age_70_74E + age_75_79E + age_80_84E
  ) %>%
  select(
    GEOID,
    #total_popE,
    #age_under_18E,
    age_18_24E,
    age_25_44E,
    age_45_64E,
    age_65_84E,
    age_85_upE
  ) %>%
  cbind(municipality, .) %>%
  pivot_longer(
    cols = starts_with("age_"),
    names_to = "age_grp",
    values_to = "acs_age_pop"
  ) %>%
  mutate(
    age_grp = recode(age_grp,
      "age_18_24E" = "18-24 years",
      "age_25_44E" = "25-44 years",
      "age_45_64E" = "45-64 years",
      "age_65_84E" = "65-84 years",
      "age_85_upE" = "85+ years"
    ),
    municipality = str_to_title(municipality)
  )

## 3.2 Race ----
acs_race <- acs_tbl %>%
  filter(GEOID %in% meck_geoids) %>%
  mutate(
    wht_under_18 = wht_under_5_maleE + wht_5_9_maleE + wht_10_14_maleE + 
    wht_15_17_maleE + wht_under_5_femaleE + wht_5_9_femaleE + wht_10_14_femaleE + 
    wht_15_17_femaleE,
    race_white_pop = wht_totalE - wht_under_18,
    blk_under_18 = blk_under_5_maleE + blk_5_9_maleE + blk_10_14_maleE + 
    blk_15_17_maleE + blk_under_5_femaleE + blk_5_9_femaleE + blk_10_14_femaleE + 
    blk_15_17_femaleE,
    race_black_pop = blk_totalE - blk_under_18,
    aian_under_18 = aian_under_5_maleE + aian_5_9_maleE + aian_10_14_maleE + 
    aian_15_17_maleE + aian_under_5_femaleE + aian_5_9_femaleE + aian_10_14_femaleE + 
    aian_15_17_femaleE,
    race_aian_pop = aian_totalE - aian_under_18,
    asian_under_18 = asian_under_5_maleE + asian_5_9_maleE + asian_10_14_maleE + 
    asian_15_17_maleE + asian_under_5_femaleE + asian_5_9_femaleE + asian_10_14_femaleE + 
    asian_15_17_femaleE,
    race_asian_pop = asian_totalE - asian_under_18,
    nhopi_under_18 = nhopi_under_5_maleE + nhopi_5_9_maleE + nhopi_10_14_maleE + 
    nhopi_15_17_maleE + nhopi_under_5_femaleE + nhopi_5_9_femaleE + nhopi_10_14_femaleE + 
    nhopi_15_17_femaleE,
    race_nhopi_pop = nhopi_totalE - nhopi_under_18,
    other_under_18 = other_under_5_maleE + other_5_9_maleE + other_10_14_maleE + 
    other_15_17_maleE + other_under_5_femaleE + other_5_9_femaleE + other_10_14_femaleE + 
    other_15_17_femaleE,
    race_other_pop = other_totalE - other_under_18,
    mult_under_18 = mult_under_5_maleE + mult_5_9_maleE + mult_10_14_maleE + 
    mult_15_17_maleE + mult_under_5_femaleE + mult_5_9_femaleE + mult_10_14_femaleE + 
    mult_15_17_femaleE,
    race_multi_pop = mult_totalE - mult_under_18,
  ) %>%
  select(
    GEOID,
    race_white_pop,
    race_black_pop,
    race_asian_pop,
    race_aian_pop,
    race_nhopi_pop,
    race_other_pop,
    race_multi_pop
  ) %>%
  cbind(municipality, .) %>%
  pivot_longer(
    cols = starts_with("race_"),
    names_to = "race",
    values_to = "acs_race_pop"
  ) %>%
  mutate(
    race = recode(race,
      "race_white_pop" = "White",
      "race_black_pop" = "Black or African American",
      "race_aian_pop" = "American Indian or Alaskan Native",
      "race_asian_pop" = "Asian",
      "race_nhopi_pop" = "Native Hawaiian or Other Pacific Islander",
      "race_other_pop" = "Other race not listed here",
      "race_multi_pop" = "Multiracial"
    )
  )

## 3.3 Race/Eth ----
acs_eth <- acs_tbl %>%
  filter(GEOID %in% meck_geoids) %>%
  mutate(
    hisp_under_18 = hisp_under_5_maleE + hisp_5_9_maleE + hisp_10_14_maleE + 
    hisp_15_17_maleE + hisp_under_5_femaleE + hisp_5_9_femaleE + hisp_10_14_femaleE + 
    hisp_15_17_femaleE,
    hisp_pop = hisp_totalE - hisp_under_18,
    non_hisp_pop = total_popE - hisp_pop,
    ) %>%
    select(
      GEOID,
      hisp_pop,
      non_hisp_pop
    ) %>%
  cbind(municipality, .) %>%
  pivot_longer(
    cols = c("hisp_pop", "non_hisp_pop"),
    names_to = "hisp",
    values_to = "acs_eth_pop"
  ) %>%
  mutate(
    hisp = recode(hisp,
      "hisp_pop" = "Yes",
      "non_hisp_pop" = "No"
    ))

# Bar Chart Population totals from ACS

# Converting population totals into proportions
acs_prop <- acs_total %>%
  mutate(
    municipality = str_to_title(municipality),
    prop = total_popE / sum(total_popE),
    source = "ACS Population (Target)"
  ) %>%
  select(municipality, prop, source)

# Converting survey response totals into proportions
sample_props <- analysis_dataset %>%
  mutate(
    municipality = str_to_title(municipality)
  ) %>%
  count(municipality) %>%
  mutate(
    prop = n / sum(n),
    source = "Survey Sample"
  ) %>%
  select(municipality, prop, source)

muni_levels <- acs_prop %>%
  pull(municipality) %>%
  unique()

compare_props <- bind_rows(acs_prop, sample_props) %>%
  mutate(
    municipality = factor(municipality, levels = muni_levels)
  )

# Bar Chart: Comparing population targets from ACS with survey sample proportions by municipality
compare_props %>%
  ggplot(aes(x = municipality, y = prop, fill = source)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~ source, nrow = 1) +
  scale_y_continuous(labels = scales::percent_format()) +
  labs(
    title = "Population Targets vs. Survey Sample by Municipality",
    x = "Municipality",
    y = "Proportion of Total"
  ) +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1),
    strip.text = element_text(face = "bold")
  )


## 4.0 Aligning Probability Sample counts with ACS data ---------------------------------------------
# Craete a df that is a sum of the addressed by municplaity 
# Population Totals by Municipality 
acs_total_tbl <- acs_total %>%
  rename(acs_known_pop = total_popE ) %>%
  mutate(municipality = str_to_title(municipality)) %>%
  select(municipality,acs_known_pop) 

# Probability Sample Counts 
# For simplcity, we are creating a table with the original sample size by municipality. In practice, you would have a dataset with all the respondents you reached out to into in your probability sample, and you would need to sum the number of respondents by municipality to get the counts for weighting.
prob_sample_counts <- tibble(
  municipality = c(
    "Charlotte",
    "Cornelius",
    "Huntersville",
    "Pineville",
    "Davidson",
    "Mint Hill",
    "Matthews"
  ),
  # Made up sample size counts for each municipality, these should be the number of respondents you reached out to in your probability sample by municipality (not the number of respondents). 
  sample_size = c(
    500,  # Charlotte
    18,  # Cornelius
    34,  # Huntersville
    6,  # Pineville
    9,  # Davidson
    16,  # Mint Hill
    17   # Matthews
  )
)

# Merge into one table for easier processing
sample_acs <- left_join(prob_sample_counts, acs_total_tbl, by = "municipality")
sample_acs <- as.data.frame(sample_acs)

## 5.0 Counts of Postcard & Web responses by Municipality -----------------------

weight_data <- analysis_dataset %>%
  count(response_type, municipality) %>%
  pivot_wider(
    names_from = response_type,
    values_from = n,
    values_fill = list(n = 0)
  ) %>%
  filter(!municipality == "I'm not sure") %>%
  rename(c(ps_comp = Postcard, nps_comp = Web)) %>%
  mutate(municipality = str_to_title(municipality)) %>%
  left_join(sample_acs, by = "municipality") %>%
  relocate(c(acs_known_pop, sample_size), .after = municipality) %>%
  arrange(municipality)

# 5.0 Compute Weights: Probability Sample --------------------------------------
## 5.1 Probability Sample Weights (base & coverage weights) ----
weight_data <- weight_data %>%
  mutate(
    total_comp = ps_comp + nps_comp,
    ps = sample_size / acs_known_pop, # probability of selection for sample
    ps_bw = 1 / ps, # Base weights
    ps_rr = ps_comp / sample_size, # Response rate
    ps_naf = 1 / ps_rr, # coverage weight/nonresponse adjustment factor
    ps_wgt = ps_bw * ps_naf
  ) # Multiply the base weight and coverage weight

# 5.2 Compute Weights: Non-Probability Sample ----------------------------------
weight_data <- weight_data %>%
  mutate(
    total_known_pop = sum(weight_data$acs_known_pop),
    total_sample_size = sum(weight_data$nps_comp),
    pop_proportion = acs_known_pop / total_known_pop,
    samp_proportion = nps_comp / total_sample_size,
    post_strat_adj = pop_proportion / samp_proportion,
    nps_bw = 1, # base weights
    nps_wgt = nps_bw * post_strat_adj
  )

## 5.3 Checking Weights ----
sum_ps_wgt <- sum(weight_data$ps_comp * weight_data$ps_wgt) # Should equal the total population
sum_nps_wgt <- sum(weight_data$nps_comp * weight_data$nps_wgt) # Should equal the total number of NPS completions

sum_ps_wgt

sum_nps_wgt

## 5.4 Combine weights & Scaling----
weight_data <- weight_data %>%
  mutate(
    ps_sf = sum(acs_known_pop) / sum(ps_comp * ps_wgt), # Scaling factor for PS (total population / sum of weights)
    nps_sf = sum(acs_known_pop) / sum(nps_comp * nps_wgt), # Scaling factor for NPS
    ps_wgt_final = (ps_wgt * ps_sf) / 2, # Rescaled weight, final to use for PS respondents
    nps_wgt_final = (nps_wgt * nps_sf) / 2
  ) # Rescaled weight, final to use for NPS respondents


## 5.5 Apply weights to dataset ----
weighted_analysis_file <- analysis_dataset %>%
  left_join(weight_data, by = "municipality") %>%
  mutate(
    wgt_combined = case_when(
      response_type == "Web" ~ nps_wgt_final,
      response_type == "Postcard" ~ ps_wgt_final
    ),
    id_num = row_number()
  ) %>%
  select(
    -acs_known_pop,
    -sample_size,
    -ps_comp,
    -nps_comp,
    -total_comp,
    -ps,
    -ps_bw,
    -ps_rr,
    -ps_naf,
    -ps_wgt,
    -total_known_pop,
    -total_sample_size,
    -pop_proportion,
    -samp_proportion,
    -post_strat_adj,
    -nps_bw,
    -nps_wgt,
    -ps_sf,
    -nps_sf,
    -ps_wgt_final,
    -nps_wgt_final
  )

## 5.6 Check weights ----
wgt_check1 <- sum(weighted_analysis_file$wgt_combined)
wgt_check2 <- weighted_analysis_file %>%
  group_by(response_type) %>%
  summarize(sum = sum(wgt_combined))

wgt_check1
wgt_check2


# 6.0 Create Survey Design Object ----
survey_design <- svydesign(
  id = ~id_num,
  strata = ~municipality,   
  weights = ~wgt_combined,
  data = weighted_analysis_file,
  nest = TRUE
)

# 6.1 Example: Weighted Proportions ----
# Calculate weighted proportions for a variable of interest
accessCare_wgt_prop <- svymean(~accessCare, design = survey_design)

accessCare_wgt_prop

# Comprare to unweighted proportions
accessCare_unwgt_prop <- prop.table(table(weighted_analysis_file$accessCare))
accessCare_unwgt_prop

# Merge our ACS data with our weighted analysis file to get the population totals by municipality for the table.
weighted_analysis_final <- weighted_analysis_file %>%
  left_join(acs_total_tbl, by = "municipality") %>%  # merge municipality population totals 
  left_join(acs_age, by = c("municipality", "age_grp"))  # merge age group population totals 
 

# 6.2 Example: Weighted Demographic table using Municipality ----
# Create a weighted demographic table by municipality using the gtsummary package. 

# Weighted Table that shows the distribution of respondents by municipality, weighted to reflect the population distribution from the ACS.

# Get unweighted counts by municipality 
unweighted_n <- weighted_analysis_final %>%
  count(municipality, name = "n_unweighted")

# Get weighted proportions by municipality using the survey design object
weighted_p <- svymean(
  ~ municipality,
  survey_design,
  vartype = NULL
) %>%
  as.data.frame() %>%
  rownames_to_column("municipality") %>%
  rename(p_weighted = 1) %>%
  mutate(
    municipality = municipality %>%
      gsub("^municipality", "", .) %>%
      str_to_title()
  )

# Get population proportions from ACS by municipality
acs_p <- weighted_analysis_final %>%
  distinct(municipality, acs_known_pop) %>%
  mutate(
    acs_percent = acs_known_pop / sum(acs_known_pop)
  )

# Merge all the data together and create formatted columns for the table
municipality_table <- unweighted_n %>%
  left_join(weighted_p, by = "municipality") %>%
  left_join(acs_p, by = "municipality") %>%
  mutate(
    # ensure numeric
    p_weighted  = as.numeric(mean),
    acs_percent = as.numeric(acs_percent),
    # formatted columns
    survey_col = paste0(
      n_unweighted,
      " (",
      percent(p_weighted, accuracy = 0.1),
      ")"
    ),
    acs_col = percent(acs_percent, accuracy = 0.1)
  ) %>%
  select(
    Municipality = municipality,
    `Survey Responses: N (Weighted %)` = survey_col,
    `ACS Population (%)` = acs_col
  )

municipality_table %>%
  gt() %>%
  tab_header(
    title = "Municipality Distribution of Survey Respondents Compared to ACS Population"
  ) %>%
  tab_source_note(
    source_note =
      "Survey column shows unweighted counts with survey-weighted percentages.
       ACS column shows population percentages from the American Community Survey."
  )