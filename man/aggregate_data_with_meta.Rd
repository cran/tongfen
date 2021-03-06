% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/tongfen.R
\name{aggregate_data_with_meta}
\alias{aggregate_data_with_meta}
\title{Aggregate variables in grouped data}
\usage{
aggregate_data_with_meta(data, meta, geo = FALSE, na.rm = TRUE, quiet = FALSE)
}
\arguments{
\item{data}{census data as obtained from get_census call, grouped by TongfenID}

\item{meta}{list with variables and aggregation information as obtained from meta_for_vectors}

\item{geo}{logical, should also aggregate geographic data}

\item{na.rm}{logical, should NA values be ignored or carried through.}

\item{quiet}{logical, don't emit messages if set to `TRUE`}
}
\value{
data frame with variables aggregated to new common geography
}
\description{
\lifecycle{maturing}

Aggregate census data up, assumes data is grouped for aggregation
Uses data from meta to determine how to aggregate up
}
\examples{
# Aggregate population from DA level to grouped by CT_UID
\dontrun{
geo <- cancensus::get_census("CA06",regions=list(CSD="5915022"),level='DA')
meta <- meta_for_additive_variables("CA06","Population")
result <- aggregate_data_with_meta(geo \%>\% group_by(CT_UID),meta)
}
}
