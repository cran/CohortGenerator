## -----------------------------------------------------------------------------
library(CohortGenerator)

cohortDefinitionSet <- createEmptyCohortDefinitionSet()

sql <- "INSERT INTO @cohort_database_schema.@cohort_table
              (cohort_definition_id, subject_id, cohort_start_date, cohort_end_date)
         SELECT 1 as cohort_definition_id,
                person_id as subject_id,
                drug_era_start_date as cohort_start_date,
                drug_era_end_date as cohort_end_date
         FROM @cdm_database_schema.drug_era de
         INNER JOIN @cdm_database_schema.concept c on de.drug_concept_id = c.concept_id
         -- Find any matches of drugs named 'asprin' in the drug concept table
         WHERE lower(c.concept_name) like '%asprin%'; "

cohortDefinitionSet <- cohortDefinitionSet |>
  addSqlCohortDefinition(
    sql = sql,
    cohortId = 1,
    cohortName = "my asprin cohort"
  )

connection <- DatabaseConnector::connect(Eunomia::getEunomiaConnectionDetails())
createCohortTables(connection = connection, cohortDatabaseSchema = "main")
status <- generateCohortSet(
  connection = connection,
  cdmDatabaseSchema = "main",
  cohortTableNames = getCohortTableNames(),
  cohortDefinitionSet = cohortDefinitionSet,
  incremental = TRUE
)

## -----------------------------------------------------------------------------
getCohortValidationCounts(
  connection = connection,
  cdmDatabaseSchema = "main",
  cohortDatabaseSchema = "main"
)

## -----------------------------------------------------------------------------
# Library imports
library(CohortGenerator)
library(DatabaseConnector)

# Create RxNorm ingredient cohort template
rxNormDefinition <- createRxNormCohortTemplateDefinition(
  connection = connection, # Replace with your DatabaseConnector connection
  cdmDatabaseSchema = "main",
  priorObservationPeriod = 365
)

cohortDefinitionSet <- cohortDefinitionSet |>
  addCohortTemplateDefintion(cohortTemplateDefintion = rxNormDefinition)


# View details of generated template references
rxNormReferences <- rxNormDefinition$getTemplateReferences()
head(rxNormReferences)

## ----eval=FALSE---------------------------------------------------------------
# # Create ATC-based cohort template
# atcDefinition <- createAtcCohortTemplateDefinition(
#   connection = connection, # Replace with your DatabaseConnector connection
#   cdmDatabaseSchema = "main",
#   mergeIngredientEras = TRUE,
#   priorObservationPeriod = 365
# )
# 
# cohortDefinitionSet <- cohortDefinitionSet |>
#   addCohortTemplateDefintion(cohortTemplateDefintion = atcDefinition)
# 
# # View ATC template references
# atcReferences <- atcDefinition$getTemplateReferences()
# head(atcReferences)

## -----------------------------------------------------------------------------
# Create SNOMED cohort template
snomedDefinition <- createSnomedCohortTemplateDefinition(
  connection = connection, # Replace with your DatabaseConnector connection
  cdmDatabaseSchema = "main",
  priorObservationPeriod = 180 # Require 180 days prior observation
)

cohortDefinitionSet <- cohortDefinitionSet |>
  addCohortTemplateDefintion(cohortTemplateDefintion = snomedDefinition)

# View the condition template references
snomedReferences <- snomedDefinition$getTemplateReferences()
head(snomedReferences)

## -----------------------------------------------------------------------------
status <- generateCohortSet(
  connection = connection,
  cdmDatabaseSchema = "main",
  cohortTableNames = getCohortTableNames(),
  cohortDefinitionSet = cohortDefinitionSet,
  incremental = TRUE
)

