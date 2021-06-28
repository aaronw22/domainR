#' Puts together a vector of property listing URLs
#'
#' Scrapes the search results page of domain.com.au given search parameters and
#' returns a vector of the URLs for all the available listings.
#'
#' @param urls A vector of results page URLs.
#' @param pause The duration (in seconds) to pause between scraping each page to
#'   avoid rate limiting. Defaults to 3 seconds.
#' @return A vector of listings URLs.
#' @export

get_listings_urls <- function(urls, pause = 3) {
  # Define a function for purrr::map to read a results page on domain.com.au and
  # get all the listings URLs
  scrape_listing_urls <-
    function(url,
             ua = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36",
             pause_2 = pause) {
      results_urls <-
        rvest::session(url = url, ua) %>%
        rvest::read_html() %>%
        rvest::html_nodes("a.address") %>%
        rvest::html_attr("href")

      Sys.sleep(pause_2) # Pause script to avoid rate limiting
      return(results_urls)
    }

  # Gets the URLs of all the individual listings
  list_listings <-
    urls %>%
    purrrgress::pro_map(purrr::possibly(scrape_listing_urls, NA)) %>%
    purrr::flatten_chr()

  # Remove NA results
  list_listings <- list_listings[!is.na(list_listings)]

  return(list_listings)

}
