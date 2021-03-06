% Generated by roxygen2: do not edit by hand
% Please edit documentation in R/get_listings_urls.R
\name{get_listings_urls}
\alias{get_listings_urls}
\title{Puts together a vector of property listing URLs}
\usage{
get_listings_urls(urls, pause = 3)
}
\arguments{
\item{urls}{A vector of results page URLs.}

\item{pause}{The duration (in seconds) to pause between scraping each page to
avoid rate limiting. Defaults to 3 seconds.}
}
\value{
A vector of listings URLs.
}
\description{
Scrapes the search results page of domain.com.au given search parameters and
returns a vector of the URLs for all the available listings.
}
