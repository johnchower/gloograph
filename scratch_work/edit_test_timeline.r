test_timeline <- gloograph::test_timeline %>%
  dplyr::mutate(owner_type = "User")

  devtools::use_data(test_timeline, test_timeline, overwrite = T)
