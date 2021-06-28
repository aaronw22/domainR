#' Scrapes details of properties
#'
#' Scrapes details of properties given a vector of URLS of their listings and
#' simplifies the results into a table of property characteristics.
#'
#' @param urls A vector of property listing URLs.
#' @param get_sale If FALSE, treats listings as rental listings. If TRUE, treats
#'   listings as sale listings and gets sale date and method in addition to
#'   existing parameters.
#' @return A table of properties with details on address, asking price,
#'   bedrooms, bathrooms, parking spaces, property features and description.
#' @export

get_property_details <- function(urls, get_sale = F) {
  # Function to scrape property attributes from an individual listing
  scrape_prop_details <-
    function(
      url,
      ua = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36",
      pause = runif(n = 1, min = 0, max = 3)
    ) {
      # Scrape HTML of the listing
      html_data <- rvest::session(url, ua) %>% rvest::read_html()

      # Pull individual attributes out of the listing
      address <-
        html_data %>%
        rvest::html_node(xpath = '//div[@data-testid="listing-details__button-copy-wrapper"]/h1') %>%
        rvest::html_text()

      # Get rent/sale price
      ask_price <-
        html_data %>%
        rvest::html_node(xpath = '//div[@data-testid="listing-details__summary-title"]') %>%
        rvest::html_text()

      # Get bedrooms
      beds <-
        html_data %>%
        rvest::html_node(xpath = '//div[@data-testid="property-features-wrapper"]/span[1]/span/text()') %>%
        rvest::html_text()

      # Get bathrooms
      baths <-
        html_data %>%
        rvest::html_node(xpath = '//div[@data-testid="property-features-wrapper"]/span[2]/span/text()') %>%
        rvest::html_text()

      # Get parking spaces
      parks <-
        html_data %>%
        rvest::html_node(xpath = '//div[@data-testid="property-features-wrapper"]/span[3]/span/text()') %>%
        rvest::html_text()

      # Get list of features
      features <-
        html_data %>%
        rvest::html_node("div#property-features > div > div > div") %>%
        rvest::html_nodes(xpath = '//li[@data-testid="listing-details__additional-features-listing"]') %>%
        rvest::html_text() %>%
        stringr::str_flatten(collapse = ", ")

      # Get property description
      descript <-
        html_data %>%
        rvest::html_nodes(xpath = '//div[@name="listing-details__description"]/div/div/div/div/p') %>%
        rvest::html_text() %>%
        stringr::str_flatten(collapse = " ")

      # Get more details if it is a sale
      if (get_sale == T) {
        sale_info <-
          html_data %>%
          rvest::html_node(xpath = '//span[@data-testid="listing-details__listing-tag"]') %>%
          rvest::html_text()

        sale_date <-
          sale_info %>%
          stringr::str_remove("(Sold at auction |Sold prior to auction |Sold by private treaty )") %>%
          as.Date(format = "%d %b %Y")

        sale_method <-
          sale_info %>%
          stringr::str_extract("(Sold at auction|Sold prior to auction|Sold by private treaty)")
      }

      # Combine into a list of attributes
      if (get_sale == T) {
        attr_list <-
          list(
            address = address,
            ask_price = ask_price,
            sale_date = sale_date,
            sale_method = sale_method,
            bedrooms = beds,
            bathrooms = baths,
            parking = parks,
            features_list = features,
            description = descript,
            url = url
          )

      } else {
        attr_list <-
          list(
            address = address,
            ask_price = ask_price,
            bedrooms = beds,
            bathrooms = baths,
            parking = parks,
            features_list = features,
            description = descript,
            url = url
          )

      }

      Sys.sleep(pause) # Wait to avoid getting blocked
      return(attr_list)
    }

  # Get details for all properties
  # With a progress bar and returns NA for bad URLs instead of breaking
  prop_details <-
    urls %>%
    purrrgress::pro_map(purrr::possibly(scrape_prop_details, NA)) %>%
    data.table::rbindlist()

  # Filter out bad results using address column
  prop_details <- prop_details[!is.na(address), ]

  # Convert bed/baths/parks to integer
  cols <- c("bedrooms", "bathrooms", "parking")
  suppressWarnings(prop_details[, (cols) := lapply(.SD, as.integer), .SDcols = cols])

  # Tidy the ask_price column
  prop_details[, ask_price := stringr::str_extract(ask_price, "\\$[\\d,]*")]
  prop_details[, ask_price := stringr::str_remove_all(ask_price, stringr::fixed("$"))]
  prop_details[, ask_price := stringr::str_remove_all(ask_price, stringr::fixed(","))]
  prop_details[, ask_price := as.numeric(ask_price)]

  return(prop_details)
}
