library(data.table)

test_that("translate_data returns correct result", {
  test_posts <- data.frame()            
  test_comments <- data.frame()
  test_shares <- data.frame()
  test_space_membership <- data.frame()
  test_connections <- data.frame()
  test_follows <- data.frame()

  test_input <- list(
    posts = test_posts
    , comments = test_comments
    , shares = test_shares
    , space_membership = test_space_membership
    , connections = test_connections
    , follows = test_follows
  ) 

  expected_result <- data.frame(
    post_id = 1
    , reach = 1
    , comments = 1
    , shares = 1
  )                                

})
