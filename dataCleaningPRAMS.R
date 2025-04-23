library(tidyverse)

# Load the data
data <- read_csv("~/Downloads/CDC_PRAMStat_Data_for_2011.csv")

# ----- GENERAL CLEANING ----
# Rename columns to consistent camelCase
cleanedData <- data %>%
  rename(
    year                = Year,
    stateAbbr           = LocationAbbr,
    location            = LocationDesc,
    class               = Class,
    topic               = Topic,
    question            = Question,
    response            = Response,
    value               = Data_Value,
    footnote            = Data_Value_Footnote,
    standardError       = Data_Value_Std_Err,
    lowConfidenceLimit  = Low_Confidence_Limit,
    highConfidenceLimit = High_Confidence_Limit,
    sampleSize          = Sample_Size,
    breakOut            = Break_Out,
    breakOutCategory    = Break_Out_Category,
    geolocation         = Geolocation,
    questionID          = QuestionId,
    locationID          = LocationId,
    responseID          = ResponseId
  ) %>%
  
# Drop totally unnecessary columns
  select(
    -DataSource, -Data_Value_Unit, -Data_Value_Type,
    -Data_Value_Footnote_Symbol, -ClassId, -TopicId,
    -BreakOutId, -BreakOutCategoryid
  ) %>%
  
# Turn empty strings into real NAs, and factors to characters
  mutate(
    across(where(is.factor), as.character),
    across(where(is.character), ~ na_if(str_trim(.x), ""))
  ) %>%
  
# Drop any rows where cleanedData$value = NA or cleanedData$response = NA
  filter(!is.na(value)) %>%
  filter(!is.na(response)) %>%
  
# Normalize YES/NO text into TRUE/FALSE strings
  mutate(
    across(
      where(is.character),
      ~ case_when(
        str_to_upper(.x) %in% c("YES", "YES (CHECKED)")    ~ "TRUE",
        str_to_upper(.x) %in% c("NO",  "NO (UNCHECKED)")  ~ "FALSE",
        TRUE                                              ~ .x
      )
    )
  ) %>%

  # Convert any column that now only has "TRUE"/"FALSE" into logical
  mutate(
    across(
      everything(),
      ~ {
        chr <- as.character(.x)
        if (all(na.omit(chr) %in% c("TRUE", "FALSE")))
          as.logical(chr)
        else
          .x
      }
    )
  )

# --- PROJECT SPECIFIC CLEANING -----

# Filter to our specific questions and pick only the columns we need
filteredData <- cleanedData %>%
  filter(questionID %in% c("QUO91", "QUO65")) %>%
  select(
    question, location, response, value, lowConfidenceLimit, highConfidenceLimit,
    sampleSize, breakOut, breakOutCategory) %>%
  arrange(question, breakOutCategory, breakOut)

# Rename questions for data legibility and to account for typos  
filteredData <- filteredData %>%
  mutate(
    # remap your question text
    question = case_when(
      question %in% c(
        "Before you got pregnant  did a doctor  nurse  or other health care worker talk to you about how to prepare for a healthy pregnancy and baby?",
        "Before you got pregnant, did a doctor, nurse, or other health care worker talk to you about how to prepare for a healthy pregnancy and baby?"
      ) ~ "Spoke to health care?",
      question %in% c(
        "(*PCH) During the month before you got pregnant with your new baby  did you take a daily multivitamin?",
        "(*PCH) During the month before you got pregnant with your new baby, did you take a daily multivitamin?"
      ) ~ "Multivitamin?",
      TRUE ~ question
    ),
    
# Convert binary breakOut values to TRUE/FALSE for future analysis and rename the categories accordingly
    breakOutCategory = case_when(
      breakOut == "Birth Weight"            ~ "Normal Birth Weight?",
      breakOut == "Marital Status"          ~ "Married?",
      breakOut == "On WIC during Pregnancy" ~ "On WIC?",
      breakOut == "Medicaid Recipient"      ~ "On Medicaid?",
      breakOut == "Pregnancy Intendedness"  ~ "Intended Pregnancy?",
      TRUE                                  ~ breakOutCategory
    ),
    breakOut = case_when(
      breakOut == "ADEQUATE PNC"     ~ "Adequate PNC",
      breakOut == "INADEQUATE PNC"   ~ "Inadequate PNC",
      breakOut == "INTERMEDIATE PNC" ~ "Intermediate PNC",
      breakOut == "MARRIED"          ~ "TRUE",
      breakOut == "OTHER"            ~ "FALSE",
      breakOut == "UNKNOWN PNC"      ~ NA,
      breakOut == "LBW (<=2500g)"    ~ "FALSE",
      breakOut == "NBW (>2500g)"     ~ "TRUE",
      breakOut == "WIC"              ~ "TRUE",
      breakOut == "Non-WIC"          ~ "FALSE",
      breakOut == "Smoker"           ~ "TRUE",
      breakOut == "Non-Smoker"       ~ "FALSE",
      breakOut == "Medicaid"         ~ "TRUE",
      breakOut == "Non-Medicaid"     ~ "FALSE",
      breakOut == "Hispanic"         ~ "TRUE",
      breakOut == "Non-Hispanic"     ~ "FALSE",
      breakOut == "Intended"         ~ "TRUE",
      breakOut == "Unintended"       ~ "FALSE",
      TRUE                           ~ breakOut
    )
  )

# ----- SPLIT DATA INTO TWO DIFF CSVs -----

# Split into two smaller data frames
multivitaminResponses <- filteredData %>%
  filter(question == "Multivitamin?")

healthCareInteractionResponses <- filteredData %>%
  filter(question == "Spoke to health care?")

# Export each to its own CSV on your desktop
write_csv(multivitaminResponses,            "~/Desktop/multivitaminResponses.csv")
write_csv(healthCareInteractionResponses,   "~/Desktop/healthCareInteractionResponses.csv")
