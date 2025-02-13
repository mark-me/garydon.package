% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/company_hierarchy.R
\name{list_company_hierarchy_graphs}
\alias{list_company_hierarchy_graphs}
\title{Decomposes a graph with all company hierarchies to a list where the items contain
a single company hierarchy}
\usage{
list_company_hierarchy_graphs(graph_all_companies)
}
\arguments{
\item{graph_company_hierarchies}{The graph containing all the hierarchical company trees}
}
\value{
A list of company hierarchy graphs
}
\description{
Decomposes a graph with all company hierarchies to a list where the items contain
a single company hierarchy
}
\examples{
list_graphs <- list_company_hierarchy_graphs(graph_company_hierarchies)
}
\keyword{company}
\keyword{graph}
\keyword{hierarchy}
