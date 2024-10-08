% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/ResultsDataModel.R
\name{getDataMigrator}
\alias{getDataMigrator}
\title{Get database migrations instance}
\usage{
getDataMigrator(connectionDetails, databaseSchema, tablePrefix = "")
}
\arguments{
\item{connectionDetails}{DatabaseConnector connection details object}

\item{databaseSchema}{String schema where database schema lives}

\item{tablePrefix}{(Optional) Use if a table prefix is used before table names (e.g. "cg_")}
}
\value{
Instance of ResultModelManager::DataMigrationManager that has interface for converting existing data models
}
\description{
Returns ResultModelManager DataMigrationsManager instance.
}
