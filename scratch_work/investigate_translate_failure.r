timeline <- create_random_test_timeline(1
                                        , max_users = 10
                                        , min_users = 5
                                        , max_spaces = 5
                                        , min_spaces = 3
                                        , max_timeline_posts_per_user = 9
                                        , max_space_post_per_user_space = 9)
.env$view(timeline)
test_translated <- create_translated_test_data(timeline)
test_organized <- create_organized_test_data(timeline)
result_translated <- translate_data(test_organized)

test_translated
result_translated

comparison_timeline <- cbind(result_translated
      , dplyr::select(test_translated
               , test_comments = comments
               , test_shares = shares) )

comparison_timeline %>%
  dplyr::filter(test_comments != comments) %>% .view
comparison_timeline %>%
  dplyr::filter(test_shares != shares) %>% .view
