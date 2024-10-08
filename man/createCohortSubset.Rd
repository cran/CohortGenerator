% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/Subsets.R
\name{createCohortSubset}
\alias{createCohortSubset}
\title{A definition of subset functions to be applied to a set of cohorts}
\usage{
createCohortSubset(
  name = NULL,
  cohortIds,
  cohortCombinationOperator,
  negate,
  startWindow,
  endWindow
)
}
\arguments{
\item{name}{optional name of operator}

\item{cohortIds}{integer - set of cohort ids to subset to}

\item{cohortCombinationOperator}{"any" or "all" if using more than one cohort id allow a subject to be in any cohort
or require that they are in all cohorts in specified windows}

\item{negate}{The opposite of this definition - include patients who do NOT meet the specified criteria}

\item{startWindow}{A SubsetCohortWindow that patients must fall inside (see createSubsetCohortWindow)}

\item{endWindow}{A SubsetCohortWindow that patients must fall inside (see createSubsetCohortWindow)}
}
\value{
a CohortSubsetOperator instance
}
\description{
A definition of subset functions to be applied to a set of cohorts
}
