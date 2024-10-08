% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/CohortDefinitionSet.R
\name{getCohortDefinitionSet}
\alias{getCohortDefinitionSet}
\title{Get a cohort definition set}
\usage{
getCohortDefinitionSet(
  settingsFileName = "Cohorts.csv",
  jsonFolder = "cohorts",
  sqlFolder = "sql/sql_server",
  cohortFileNameFormat = "\%s",
  cohortFileNameValue = c("cohortId"),
  subsetJsonFolder = "inst/cohort_subset_definitions/",
  packageName = NULL,
  warnOnMissingJson = TRUE,
  verbose = FALSE
)
}
\arguments{
\item{settingsFileName}{The name of the CSV file that will hold the cohort information
including the cohortId and cohortName}

\item{jsonFolder}{The name of the folder that will hold the JSON representation
of the cohort if it is available in the cohortDefinitionSet}

\item{sqlFolder}{The name of the folder that will hold the SQL representation
of the cohort.}

\item{cohortFileNameFormat}{Defines the format string  for naming the cohort
JSON and SQL files. The format string follows the
standard defined in the base sprintf function.}

\item{cohortFileNameValue}{Defines the columns in the cohortDefinitionSet to use
in conjunction with the cohortFileNameFormat parameter.}

\item{subsetJsonFolder}{Defines the folder to store the subset JSON}

\item{packageName}{The name of the package containing the cohort definitions.}

\item{warnOnMissingJson}{Provide a warning if a .JSON file is not found for a
cohort in the settings file}

\item{verbose}{When TRUE, extra logging messages are emitted}
}
\value{
Returns a cohort set data.frame
}
\description{
This function supports the legacy way of retrieving a cohort definition set
from the file system or in a package. This function supports the legacy way of
storing a cohort definition set in a package with a CSV file, JSON files,
and SQL files in the `inst` folder.
}
