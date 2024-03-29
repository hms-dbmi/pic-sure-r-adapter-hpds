% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/bdc.R
\name{bdc.setResource}
\alias{bdc.setResource}
\title{Set the resource to use for queries.}
\usage{
bdc.setResource(session, resourceName)
}
\arguments{
\item{session}{Current session}

\item{resourceName}{Name of resource to select. Valid values are OPEN or AUTH}
}
\value{
A PIC-SURE session
}
\description{
Set the resource to use for queries.
}
\examples{

# session <- picsure::bdc.setResource(session, 'AUTH')

}
