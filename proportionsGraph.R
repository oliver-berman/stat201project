#  -----------------------------  CLEAN THE DATA -----------------------------
# Load libraries
library(tidyverse)
library(janitor)

# Import the data
originalData <- read.csv(
  "~/Downloads/CDC_PRAMStat_Data_for_2011_20250505.csv", 
  stringsAsFactors = TRUE
)

# Rough clean up of the column names
originalData <- originalData %>%
  clean_names(case = "lower_camel")

# Drop unnecessary columns
relevantData <- originalData %>% 
  select(
    -year, -locationAbbr, -class, -dataSource, 
    -dataValueUnit, -dataValueType, -dataValueFootnoteSymbol, 
    -dataValueFootnote, -dataValueStdErr, -geolocation, -classId,
    -topicId, -questionId, -locationId, -breakOutId, 
    -breakOutCategoryid, -responseId
  )

# Filter to PRAMS totals & the multivitamin question, then drop those columns
relevantData <- relevantData %>%
  filter(
    locationDesc == "PRAMS Total",
    question == "(*PCH) During the month before you got pregnant with your new baby  did you take a daily multivitamin?"
  ) %>%
  select(
    -topic,
    -question,
    -locationDesc
  )

# Drop rows where response is empty or "NO"
relevantData <- relevantData %>%
  filter(
    response != "",
    response != "NO"
  ) %>%
  select(
    -response
  )

# Rename the columns again for the purposes of our project
relevantData <- relevantData %>%
  rename(
    proportion          = dataValue,
    sampleSize          = sampleSize,
    demographic         = breakOut,
    category            = breakOutCategory
  )

# Reorder the columns for clarity
relevantData <- relevantData %>%
  select(category, demographic, proportion, sampleSize, lowConfidenceLimit, 
         highConfidenceLimit)

# Rename 'none' to population for clarity
relevantData <- relevantData %>%
  mutate(
    category = recode(category, "None" = "Population"),
    demographic = recode(demographic, "None" = "Population")
  )

#  -----------------------------  VISUALIZE THE PROPORTIONS -----------------------------

#  Define a function (plotByCategory) to create a bar chart for any category

plotByCategory <- function(cat) {
  plotData <- relevantData %>%
    filter(category == cat) %>%
    transmute(    # Creates a new data frame with YES/NO proportions
      demographic, 
      Yes = proportion,
      No  = 100 - proportion # Calculates the NO proportion from the YES proportion value
    ) %>%
    pivot_longer(c(Yes, No), names_to = "Response", values_to = "Value") %>% # pivots the data
    mutate(Response = factor(Response, levels = c("Yes", "No"))) # Ensures there are only two values of data
  
  ggplot(plotData, aes(x = demographic, y = Value / 100, fill = Response)) + #Creates a stacked bar chart
    geom_col(
      width    = 0.4,
      position = position_stack(reverse = TRUE) # Ensures that the YES value is on the bottom
    ) +
    geom_text( 
      aes(label = paste0(Value, "%")), # Adding/formatting  the percentage labels
      position = position_stack(vjust = 0.5, reverse = TRUE),
      family   = "NYTFranklin",
      fontface = "bold",
      size     = 4
    ) +
    scale_fill_manual( # Adds the keys and the colors
      name   = NULL,
      values = c("Yes" = "#80bbe2", "No" = "#a3cbe7"),
      labels = c("Yes Daily Multivitamin", "No Daily Multivitamin"),
      breaks = c("Yes", "No")
    ) +
    scale_y_continuous( #Scales the graph and adds the 100% label
      breaks = 1,
      labels = "100%",
      expand = c(0, 0)
    ) +
    labs( # Adds a different title for each value of 'cat' (i.e., each category)
      title = paste0("Multivitamin Use by ", cat),
      x     = NULL,
      y     = NULL
    ) +
    coord_fixed(ratio = 2.75) + # Keeps the aspect ratio of the bars 1 x 7.5
    theme_minimal(base_family = "NYTFranklin") + # Font
    theme(
      text             = element_text(face = "bold"),
      plot.title       = element_text(size = 14, hjust = 0.5, face = "bold", margin = margin(b = 10)),
      axis.line        = element_line(color = "black"),
      axis.ticks       = element_line(color = "black"),
      axis.text.x      = element_text(angle = 0, vjust = 0.5),
      panel.grid.major = element_blank(),
      panel.grid.minor = element_blank(),
      legend.position  = "right"
    )
}

# Make a list of plots for every category in relevantData
allCats   <- unique(relevantData$category)

# Applies the function to each element in allCats
plotsList <- map(allCats, plotByCategory) %>% 
  set_names(allCats)

# Make sure that it saves to a folder on my desktop
dir.create(path = "~/Desktop/plots", showWarnings = FALSE)

# 4. Save each plot as a PNG in ~/Desktop/plots
walk2( #Repeats the function for every itme in the list 
  plotsList,
  names(plotsList),
  ~ ggsave(
    filename = paste0("~/Desktop/plots/", .y, ".png"),
    plot     = .x,
    width    = 8,
    height   = 6,
    dpi      = 300
  )
)
