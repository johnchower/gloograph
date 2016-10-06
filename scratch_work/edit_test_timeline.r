test_timeline <- gloograph::test_timeline %>%
  dplyr::mutate(object_type = ifelse(object_type == 'space'
                                     , "Space"
                                     , ifelse(object_type == 'feed'
                                              , "Timeline"
                                              , object_type))
  )

  devtools::use_data(test_timeline, test_timeline, overwrite = T)
