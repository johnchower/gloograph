# Test correctness of the engagement score pipeline

test_that("calculate_post_engagement returns correct result.", {
  timeline <- gloograph::test_timeline
  test_translated <- gloograph::create_translated_test_data(timeline)
  test_calculated <- gloograph::create_calculated_test_data(timeline)
  
  expect_equal(gloograph::calculate_post_engagement(data_in = test_translated)
               , test_calculated)
})

test_that("translate_data returns correct result", {
  TRUE
#   test_posts <- data.frame()            
#   test_comments <- data.frame()
#   test_shares <- data.frame()
#   test_space_membership <- data.frame()
#   test_connections <- data.frame()
#   test_follows <- data.frame()
# 
#   test_input <- list(
#     posts = test_posts
#     , comments = test_comments
#     , shares = test_shares
#     , space_membership = test_space_membership
#     , connections = test_connections
#     , follows = test_follows
#   ) 
# 
#   expected_result <- data.frame(
#     post_id = 1
#     , reach = 1
#     , comments = 1
#     , shares = 1
#   )                                
# 
#   expect_equal(do.call(what = gloograph::translate_data
#                        , args = test_input)
#                , expected_result)
})
