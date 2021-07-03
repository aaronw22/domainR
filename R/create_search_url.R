#' Creates URLs for a Domain search
#'
#' Creates URLs given parameters that is used when searching properties on
#' domain.com.au. If the number of results exceeds a page, creates a vector of
#' URLs that points to every results page.
#'
#' @param sale_type 'rent' to search for rentals or 'sold-listings' to search
#'   for recent sales.
#' @param suburbs A vector of suburbs with the format
#'   'suburb-name-state-postcode'.
#' @param beds A (optional) 2-element vector of minimum and maximum bedrooms.
#'   c(NA, NA) means any number of bedrooms, c(1, 2) means 1-2 bedrooms and c(2,
#'   NA) means 2 or more bedrooms.
#' @param baths A (optional) 2-element vector of minimum and maximum bathrooms
#'   with the same format as the number of bedrooms parameter.
#' @param price A (optional) 2-element vector of minimum and maximum rents or
#'   prices with the same format as the number of bedrooms parameter. For rental
#'   searches, provide the rent per week, and for sold listings provide the sale
#'   price in dollars.
#' @param exclude_houses If TRUE (default), excludes detached houses from the
#'   search results.
#' @param exclude If TRUE (default), excludes properties with a deposit already
#'   taken from rental searches and excludes properites with price withheld from
#'   sales searches. If FALSE, includes all results.
#' @param ua A valid user agent for rvest (optional).
#' @return A vector of URLs.
#' @import data.table
#' @export

create_search_url <- function(sale_type = c("rent", "sold-listings"),
                              suburbs,
                              beds = NULL,
                              baths = NULL,
                              price = NULL,
                              exclude_houses = T,
                              exclude = T,
                              ua = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36") {
  ## Checks

  # Check if vector of suburbs are correctly formatted
  # Format: suburb-name-state-postcode
  if (grepl("[\\w\\-]*-\\w{3}-\\d{4}", suburbs[[1]]) == FALSE) {
    stop("List of suburbs is not formatted correctly. Correct format is suburb-name-state-postcode. Only works for NSW.")
  }

  # Check if prices are sensible depending if rent or sold-listings
  if (sale_type == "rent" & !is.null(price)) {
    if(!is.na(price[[2]])) {
      if(price[[2]] > 100000) {
        warning("Are you sure that you have specified your min/max rental price correctly?")
      }
    }
  } else if (sale_type == "sold-listings" & !is.null(price)) {
    if(!is.na(price[[2]])) {
      if (price[[2]] < 100000) {
        warning("Are you sure that you have specified your min/max sale price correctly?")
      }
    }
  }

  ## Construct the first page of the results page to get the total number of
  ## results so we can figure out how many pages to scrape

  # Concatenate suburb list
  suburbs <- paste0(suburbs, collapse = ",")

  # Beds, baths and price range maker
  range_maker <- function(min, max) {
    if(is.null(min) & is.null(max)) {
      out <- "0-any"
    } else if (!is.na(min) & !is.na(max)) {
      out <- paste0(min, "-", max)
    } else if (is.na(max)) {
      out <- paste0(min, "-any")
    } else if (is.na(min)) {
      out <- paste0("0-", max)
    }

    return(out)
  }

  # Bedrooms
  beds <- range_maker(beds[[1]], beds[[2]])

  # Bathrooms
  baths <- range_maker(baths[[1]], baths[[2]])

  # Price
  price <- range_maker(price[[1]], price[[2]])

  # Exclude deposit/withheld
  if (sale_type == "rent") {
    if (exclude == T) {
      exclude <- "&excludedeposittaken=1"
    } else {
      exclude <- "&excludedeposittaken=0"
    }
  } else if (sale_type == "sold-listings") {
    if (exclude == T) {
      exclude <- "&excludepricewithheld=1"
    } else {
      exclude <- "&excludepricewithheld=0"
    }
  } else {
    exclude <- ""
  }

  # Exclude houses
  if (exclude_houses == T) {
    exclude_houses <- "&ptype=apartment-unit-flat,block-of-units,new-apartments,pent-house,studio,town-house"
  } else {
    exclude_houses <- ""
  }

  # Domain URL components
  url <-
    sprintf("https://www.domain.com.au/%s/?suburb=%s%s&bedrooms=%s&bathrooms=%s%s&establishedtype=established&ssubs=0", sale_type, suburbs, exclude_houses, beds, baths, exclude)

  # Load page 1 of Domain results
  page_one_html <-
    rvest::session(url, ua) %>%
    rvest::read_html()

  # Figure out the number of results pages that need to be scraped
  page_num <-
    page_one_html %>%
    rvest::html_nodes("#skip-link-content > div.css-1ned5tb > div.css-9ny10o > h1 > strong") %>%
    rvest::html_text() %>%
    stringr::str_remove(" Properties") %>%
    as.integer() %>%
    magrittr::divide_by(20) %>% # There are 20 listings per page
    ceiling()

  # Create table with correct URLs of results pages
  url_table <-
    data.table::data.table(
      url_front = rep_len(url, page_num),
      page_num = as.character(seq.int(1, page_num, 1))
    )

  url_table[, url := paste0(url_front, "&page=", page_num)]

  # Return just a vector of URLs
  url_table <- url_table$url

  return(url_table)
}
