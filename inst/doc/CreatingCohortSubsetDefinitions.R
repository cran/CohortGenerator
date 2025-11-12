## ----setup, include=FALSE-----------------------------------------------------
old <- options(width = 80)
knitr::opts_chunk$set(
  cache = FALSE,
  comment = "#>",
  error = FALSE
)
someFolder <- tempdir()
packageRoot <- tempdir()
library(CohortGenerator)

## ----echo = TRUE, warning = FALSE, message = FALSE, error = FALSE-------------
jsonFilePath <- system.file("testdata", "CohortsToSubset.JSON", package = "CohortGenerator")
cohortDefinitionSet <- jsonlite::fromJSON(jsonFilePath)
cohortDefinitionSet <- cohortDefinitionSet |>
  dplyr::filter(cohortId %in% c(1, 2))

cohortDefinitionSet |>
  dplyr::select("cohortId", "cohortName") |>
  knitr::kable()

## -----------------------------------------------------------------------------
ageCriteria <- CohortGenerator::createDemographicSubsetOperator(
  ageMin = 20,
  ageMax = 50
)

## -----------------------------------------------------------------------------
limitToLastEver <- CohortGenerator::createLimitSubsetOperator(
  name = "Last event during 1 January 2000 and 31 December 2008",
  priorTime = 0,
  followUpTime = 0,
  limitTo = "lastEver",
  calendarStartDate = as.Date("2000-01-01"),
  calendarEndDate = as.Date("2008-12-31")
)

## -----------------------------------------------------------------------------
ibuprofenSubset <- CohortGenerator::createCohortSubsetOperator(
  name = "ibuprofen exposure",
  cohortIds = 2, # Ibuprofen cohort
  cohortCombinationOperator = "any", # Look for any Ibuprofen exposure
  negate = FALSE, # We want to include (not exclude) participants exposed to Ibuprofen,
  windows = list(
    CohortGenerator::createSubsetCohortWindow(
      startDay = 0,
      endDay = 9999,
      targetAnchor = "cohortStart",
      subsetAnchor = "cohortStart"
    ),
    CohortGenerator::createSubsetCohortWindow(
      startDay = -9999,
      endDay = 0,
      targetAnchor = "cohortEnd",
      subsetAnchor = "cohortStart"
    )
  )
)

## -----------------------------------------------------------------------------
ageRequirementSubset <- CohortGenerator::createCohortSubsetDefinition(
  name = "Patients 20 to 50 years old ",
  definitionId = 10,
  subsetOperators = list(
    ageCriteria
  ),
  subsetCohortNameTemplate = "@baseCohortName - @subsetDefinitionName"
)

## -----------------------------------------------------------------------------
ibuprofenWCelcoxib <- CohortGenerator::createCohortSubsetDefinition(
  name = "Aged 20-50 yrs, last ibuprofen exposure from 2000-2008",
  definitionId = 11, # Unique ID for this subset
  subsetOperators = list(
    ageCriteria,
    ibuprofenSubset,
    limitToLastEver
  ),
  subsetCohortNameTemplate = "@baseCohortName - @subsetDefinitionName"
)

## -----------------------------------------------------------------------------
cohortDefinitionSet <- cohortDefinitionSet |>
  CohortGenerator::addCohortSubsetDefinition(ageRequirementSubset, targetCohortIds = c(1)) |>
  CohortGenerator::addCohortSubsetDefinition(ibuprofenWCelcoxib, targetCohortIds = c(1))
cohortDefinitionSet |>
  dplyr::select("cohortId", "cohortName") |>
  knitr::kable()

## ----echo = TRUE, warning = FALSE, message = FALSE, error = FALSE-------------
databaseFile <- tempfile(fileext = ".duckdb")
duckdbConnectionDetails <- DatabaseConnector::createConnectionDetails(
  dbms = "duckdb",
  server = databaseFile
)
resultsSchema <- "main"
connection <- DatabaseConnector::connect(duckdbConnectionDetails)

DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = resultsSchema,
  tableName = "person",
  data = omopCdmPerson
)
# Insert updated data into the 'drug_exposure' table
DatabaseConnector::insertTable(
  connection = connection,
  databaseSchema = resultsSchema,
  tableName = "drug_exposure",
  data = omopCdmDrugExposure
)

## ----echo = TRUE, warning = FALSE, message = FALSE, error = FALSE,results= 'hide'----
cohortTableNames <- CohortGenerator::getCohortTableNames()
CohortGenerator::createCohortTables(
  connection = connection,
  cohortDatabaseSchema = "main",
  cohortTableNames = cohortTableNames
)


### As subsets are a big side effect we need to be clear what was generated and have good naming conventions
CohortGenerator::generateCohortSet(
  connection = connection,
  cdmDatabaseSchema = "main",
  cohortDatabaseSchema = "main",
  cohortTableNames = CohortGenerator::getCohortTableNames(),
  cohortDefinitionSet = cohortDefinitionSet
)

## ----echo = FALSE, warning = FALSE, message = FALSE, error = FALSE, results = 'hide', fig.width = 10, fig.height = 6----
library(ggplot2)
cohorts <- DatabaseConnector::querySql(
  connection = connection,
  sql = "
    SELECT c.*, p.gender_concept_id, YEAR(c.cohort_start_date) - p.year_of_birth AS age
    FROM main.cohort c
    INNER JOIN main.person p ON c.subject_id = p.person_id
    ORDER BY c.COHORT_DEFINITION_ID, c.SUBJECT_ID, c.COHORT_START_DATE;"
)
names(cohorts) <- tolower(names(cohorts))
cohort_data <- cohorts |>
  dplyr::inner_join(cohortDefinitionSet[, c("cohortId", "cohortName")], by = c("cohort_definition_id" = "cohortId")) |>
  dplyr::mutate(cohort_legend = paste(cohort_definition_id, "-", cohortName)) |>
  dplyr::mutate(
    gender = ifelse(gender_concept_id == 8532, "Female", "Male"),
    facet_label = paste("Subject:", subject_id, "- Gender:", gender)
  ) |>
  dplyr::mutate(
    cohort_definition_id_factor = factor(
      cohort_definition_id,
      levels = sort(unique(cohort_definition_id), decreasing = TRUE)
    )
  )

# Order factor levels for facets by subject_id
cohort_data$facet_label <- factor(cohort_data$facet_label,
  levels = unique(cohort_data$facet_label[order(cohort_data$subject_id)])
)

# Order factor levels for legend by cohort_definition_id
cohort_data$cohort_legend <- factor(cohort_data$cohort_legend,
  levels = unique(cohort_data$cohort_legend[order(cohort_data$cohort_definition_id)])
)

# Create the plot with annotations for age when cohort_definition_id == 1
ggplot(cohort_data, aes(
  x = cohort_start_date, xend = cohort_end_date,
  y = cohort_definition_id_factor,
  group = interaction(cohort_definition_id, subject_id),
  color = cohort_legend
)) +
  geom_segment(aes(xend = cohort_end_date, yend = cohort_definition_id_factor), size = 2) +
  geom_point(aes(x = cohort_start_date, y = cohort_definition_id_factor), size = 4, shape = 21, fill = "black") +
  geom_text(
    data = cohort_data[cohort_data$cohort_definition_id == 1, ],
    aes(
      label = age,
      x = as.Date((as.numeric(cohort_start_date) + as.numeric(cohort_end_date)) / 2, origin = "1970-01-01"),
      y = cohort_definition_id_factor
    ),
    nudge_y = -0.5, size = 4, color = "black"
  ) +
  scale_y_discrete(limits = levels(cohort_data$cohort_definition_id_factor)) +
  labs(
    title = "Cohort subset membership by subject",
    x = "Date",
    y = "Cohort Definition ID",
    color = "Cohort Definition"
  ) +
  scale_x_date(limits = as.Date(c("2002-01-01", "2004-06-31")), date_breaks = "1 year", date_labels = "%Y") +
  scale_color_brewer(palette = "Paired") +
  guides(color = guide_legend(nrow = 2, byrow = TRUE)) + # Wrap legend into 2 rows
  theme_minimal() +
  theme(
    legend.position = "bottom", # Position the legend at the bottom
    legend.box = "vertical", # Stack the legend vertically if it overflows
    legend.text = element_text(size = 10), # Adjust font size for readability
    legend.spacing.x = unit(0.5, "cm"), # Add spacing between legend items
    plot.margin = margin(10, 10, 50, 10) # Increase bottom margin to accommodate the legend
  ) +
  facet_wrap(~facet_label) # Correct facet wrapping

## ----eval=FALSE---------------------------------------------------------------
# saveCohortDefinitionSet(cohortDefinitionSet,
#   subsetJsonFolder = "<path_to_my_subset_definition>"
# )

## ----eval=FALSE---------------------------------------------------------------
# cohortDefinitionSet <- getCohortDefinitionSet(
#   subsetJsonFolder = "<path_to_my_subset_definition>"
# )

## ----results='hide', eval=FALSE-----------------------------------------------
# jsonDefinition <- subsetDef$toJSON()

## ----results='hide', eval=FALSE-----------------------------------------------
# # Save to a file
# ParallelLogger::saveSettingsToJson(subsetDef$toList(), "subsetDefinition1.json")

## ----echo=FALSE, results='hide'-----------------------------------------------
options(old)

