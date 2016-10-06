test_timeline <- gloograph::test_timeline %>%
  dplyr::mutate(time = as.Date(time, origin = '2016-01-01')) 
  devtools::use_data(test_timeline, test_timeline, overwrite = T)
