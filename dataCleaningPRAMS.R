library(tidyverse)

# Load the data
data <- read_csv("~/Downloads/CDC_PRAMStat_Data_for_2008.csv")

# Clean and reshape in one pipeline
cleanedData <- data %>%
  # 1) Rename columns to consistent camelCase
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
  
  # 2) Drop totally unnecessary columns
  select(
    -DataSource, -Data_Value_Unit, -Data_Value_Type,
    -Data_Value_Footnote_Symbol, -ClassId, -TopicId,
    -BreakOutId, -BreakOutCategoryid
  ) %>%
  
  # 3) Turn empty strings into real NAs, and factors to characters
  mutate(
    across(where(is.factor), as.character),
    across(where(is.character), ~ na_if(str_trim(.x), ""))
  ) %>%
  
  # 4) Drop any rows where cleanedData$value = NA or cleanedData$response = NA
  filter(!is.na(value)) %>%
  filter(!is.na(response)) %>%
  
  # 5) Normalize YES/NO text into TRUE/FALSE strings
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
  
  # 6) Convert any column that now only has "TRUE"/"FALSE" into logical
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

# Filter to our specific questions and pick only the columns we need
filteredData <- cleanedData %>%
  filter(questionID %in% c("QUO91", "QUO7", "QUO65")) %>%
  select(
    geolocation, question, location, response, value, lowConfidenceLimit, highConfidenceLimit,
    sampleSize, breakOut, breakOutCategory, questionID, locationID
  ) %>%
  arrange(questionID, geolocation, breakOutCategory)

# Rename questions for ease and to account for typos and fix capitilzation in breakOut values and 
filteredData <- filteredData %>%
  mutate(
    question = case_when(
      question %in% c(
        "Before you got pregnant  did a doctor  nurse  or other health care worker talk to you about how to prepare for a healthy pregnancy and baby?",
        "Before you got pregnant, did a doctor, nurse, or other health care worker talk to you about how to prepare for a healthy pregnancy and baby?"
      ) ~ "Spoke to health care?",
      question %in% c(
        "(*PCH) During the month before you got pregnant with your new baby  did you take a daily multivitamin?",
        "(*PCH) During the month before you got pregnant with your new baby, did you take a daily multivitamin?"
      ) ~ "Multivitamin?",
      question ==
        "The baby's weight  classified as low birth weight (LBW) if the weight was less than or equal to 2500 grams or normal birth weight (NBW) if the weight was greater than 2500 grams"
      ~ "Baby's Weight",
      TRUE ~ question
    ),
    breakOut = case_when(
      breakOut == "ADEQUATE PNC"     ~ "Adequate PNC",
      breakOut == "INADEQUATE PNC"   ~ "Inadequate PNC",
      breakOut == "INTERMEDIATE PNC" ~ "Intermediate PNC",
      breakOut == "MARRIED"          ~ "Married",
      breakOut == "OTHER"            ~ "Other",
      breakOut == "UNKNOWN PNC"      ~ "Unknown PNC",
      TRUE                           ~ breakOut
    )
  )
