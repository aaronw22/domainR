test_that("get_listings_urls returns a vector of listings URLs", {
  url <- "https://www.domain.com.au/sold-listings/?suburb=marrickville-nsw-2204,dulwich-hill-nsw-2203,stanmore-nsw-2048&ptype=apartment-unit-flat,block-of-units,new-apartments,pent-house,studio,town-house&bedrooms=0-any&bathrooms=0-any&excludepricewithheld=1&establishedtype=established&ssubs=0&page=1"

  url_results <- get_listings_urls(rep(url, 3))

  # There should be at least 1 result
  expect_gte(length(url_results), 1)

  # The result should be a valid webpage
  expect_equal(
    rvest::session(url_results[[1]], ua = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36") %>% rvest::read_html() %>% class(),
    c("xml_document", "xml_node")
  )
})

test_that("get_listings_urls returns valid results if one of the URLs are invalid", {
  url_error <- c(
    "https://www.domain.com.au/sold-listings/?suburb=marrickville-nsw-2204,dulwich-hill-nsw-2203,stanmore-nsw-2048&ptype=apartment-unit-flat,block-of-units,new-apartments,pent-house,studio,town-house&bedrooms=0-any&bathrooms=0-any&excludepricewithheld=1&establishedtype=established&ssubs=0&page=1",
    "https://www.domain.com.au/sold-listings/?suburb=marrbrokentudio,town-house&bedrooms=0-any&bathrooms=0-any&excludepricewithheld=1&establishedtype=established&ssubs=0&page=1",
    "https://www.domain.com.au/sold-listings/?suburb=marrickville-nsw-2204,dulwich-hill-nsw-2203,stanmore-nsw-2048&ptype=apartment-unit-flat,block-of-units,new-apartments,pent-house,studio,town-house&bedrooms=0-any&bathrooms=0-any&excludepricewithheld=1&establishedtype=established&ssubs=0&page=1"
  )

  url_no_error <- c(
    "https://www.domain.com.au/sold-listings/?suburb=marrickville-nsw-2204,dulwich-hill-nsw-2203,stanmore-nsw-2048&ptype=apartment-unit-flat,block-of-units,new-apartments,pent-house,studio,town-house&bedrooms=0-any&bathrooms=0-any&excludepricewithheld=1&establishedtype=established&ssubs=0&page=1",
    "https://www.domain.com.au/sold-listings/?suburb=marrickville-nsw-2204,dulwich-hill-nsw-2203,stanmore-nsw-2048&ptype=apartment-unit-flat,block-of-units,new-apartments,pent-house,studio,town-house&bedrooms=0-any&bathrooms=0-any&excludepricewithheld=1&establishedtype=established&ssubs=0&page=1"
  )

  url_results_error <- suppressWarnings(get_listings_urls(url_error))
  url_results_no_error <- get_listings_urls(url_no_error)

  # Result with 1 NA in the middle should be the same as no NAs because the NAs
  # should be removed
  expect_equal(url_results_error, url_results_no_error)
})


