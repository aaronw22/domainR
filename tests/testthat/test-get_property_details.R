test_that("get_property_details scrapes valid sales data", {
  # Listings are dynamic so we need to get a new listing rather than rely on a
  # static page that will disappear in the future

  results <- create_search_url(sale_type = "sold-listings", suburbs = c("dulwich-hill-nsw-2203", "zetland-nsw-2017"), exclude = T)
  results <- get_listings_urls(results[[2]])
  results <- get_property_details(results[1:5], get_sale = T)

  # Check column types
  expect_named(results, c("address", "ask_price", "sale_date", "sale_method", "bedrooms", "bathrooms", "parking", "features_list", "description", "url"))

  # Check table dimensions
  expect_equal(dim(results), c(5, 10))

  # Check numbers are sensible
  expect_gt(mean(results$ask_price, na.rm = T), 100000)
  expect_lt(mean(results$bedrooms, na.rm = T), 5)
  expect_lt(mean(results$bathrooms, na.rm = T), 5)
})


test_that("get_property_details scrapes valid rental data", {
  # Listings are dynamic so we need to get a new listing rather than rely on a
  # static page that will disappear in the future

  results <- create_search_url(sale_type = "rent", suburbs = c("dulwich-hill-nsw-2203", "zetland-nsw-2017"), exclude = T)
  results <- get_listings_urls(results[[2]])
  results <- get_property_details(results[1:5], get_sale = F)

  # Check column types
  expect_named(results, c("address", "ask_price", "bedrooms", "bathrooms", "parking", "features_list", "description", "url"))

  # Check table dimensions
  expect_equal(dim(results), c(5, 8))

  # Check numbers are sensible
  expect_lt(mean(results$ask_price, na.rm = T), 2000)
  expect_lt(mean(results$bedrooms, na.rm = T), 5)
  expect_lt(mean(results$bathrooms, na.rm = T), 5)

})
