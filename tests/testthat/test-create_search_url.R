test_that("create_search_url creates valid rental listings results", {
  url_list <- create_search_url(
    sale_type = "rent",
    suburbs = c("marrickville-nsw-2204", "dulwich-hill-nsw-2203", "stanmore-nsw-2048"),
    exclude = T
  )

  # Check if it returns at least 1 URL
  expect_gt(length(url_list), 1)

  # Check if the URL is valid
  expect_equal(
    rvest::session(url_list[[1]], ua = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36") %>%
      rvest::read_html() %>%
      class(),
    c("xml_document", "xml_node")
  )

})

test_that("create_search_url creates valid sales listings results", {
  url_list <- create_search_url(
    sale_type = "sold-listings",
    suburbs = c("marrickville-nsw-2204", "dulwich-hill-nsw-2203", "stanmore-nsw-2048"),
    exclude = T
  )

  # Check if it returns at least 1 URL
  expect_gt(length(url_list), 1)

  # Check if the URL is valid
  expect_equal(
    rvest::session(url_list[[1]], ua = "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36") %>%
      rvest::read_html() %>%
      class(),
    c("xml_document", "xml_node")
  )

})

test_that("create_search_url work with filters on bedrooms, bathrooms and price", {
  url_list_1 <- create_search_url(
    sale_type = "sold-listings",
    suburbs = c("marrickville-nsw-2204", "dulwich-hill-nsw-2203", "stanmore-nsw-2048"),
    beds = c(2, NA),
    baths = c(1, NA),
    price = c(NA, 900000),
    exclude = T
  )

  url_list_2 <- create_search_url(
    sale_type = "sold-listings",
    suburbs = c("marrickville-nsw-2204", "dulwich-hill-nsw-2203", "stanmore-nsw-2048"),
    beds = c(2, 3),
    baths = c(1, 2),
    price = c(500000, 900000),
    exclude = T
  )

  url_list_3 <- create_search_url(
    sale_type = "rent",
    suburbs = c("marrickville-nsw-2204", "dulwich-hill-nsw-2203", "stanmore-nsw-2048"),
    beds = c(2, NA),
    baths = c(1, NA),
    price = c(100, NA),
    exclude = T
  )

  url_list_4 <- create_search_url(
    sale_type = "rent",
    suburbs = c("marrickville-nsw-2204", "dulwich-hill-nsw-2203", "stanmore-nsw-2048"),
    beds = c(2, 3),
    baths = c(1, 2),
    price = c(100, 700),
    exclude = T
  )


  # Check if it returns at least 1 URL
  expect_gt(length(url_list_1), 1)
  expect_gt(length(url_list_2), 1)
  expect_gt(length(url_list_3), 1)
  expect_gt(length(url_list_4), 1)

  # Check if the URL is valid
  ua <- "Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/58.0.3029.110 Safari/537.36"

  expect_equal(
    rvest::session(url_list_1[[1]], ua) %>% rvest::read_html() %>% class(),
    c("xml_document", "xml_node")
  )

  expect_equal(
    rvest::session(url_list_2[[1]], ua) %>% rvest::read_html() %>% class(),
    c("xml_document", "xml_node")
  )

  expect_equal(
    rvest::session(url_list_3[[1]], ua) %>% rvest::read_html() %>% class(),
    c("xml_document", "xml_node")
  )

  expect_equal(
    rvest::session(url_list_4[[1]], ua) %>% rvest::read_html() %>% class(),
    c("xml_document", "xml_node")
  )
})

test_that("create_search_url warns if unusual prices are provided", {
  expect_warning(
    create_search_url(
      sale_type = "rent",
      suburbs = c("marrickville-nsw-2204"),
      price = c(1e6, 2e6),
      exclude = T
    ),
    "Are you sure that you have specified your min/max rental price correctly?"
  )

  expect_warning(
    create_search_url(
      sale_type = "sold-listings",
      suburbs = c("marrickville-nsw-2204"),
      price = c(200, 600),
      exclude = T
    ),
    "Are you sure that you have specified your min/max sale price correctly?"
  )
})
