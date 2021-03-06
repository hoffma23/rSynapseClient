\name{synapseQuery}
\alias{synapseQuery}
%- Also NEED an '\alias' for EACH other topic documented here.
\title{
Query The Synapse API
}
\description{
Provides an interface to the Synapse API
}
\usage{
synapseQuery(queryStatement)
}
%- maybe also 'usage' for other objects documented here.
\arguments{
  \item{queryStatement}{
The query statement
}
}
\details{
See the Query service API wiki for details on how to form a query: http://sagebionetworks.jira.com/wiki/display/PLFM/Repository+Service+API#RepositoryServiceAPI-QueryAPI
}
\value{
list that is generated by calling toJSON on the server response
}

\author{
Nicole Deflaux
}
