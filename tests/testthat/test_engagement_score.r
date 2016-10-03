library(data.table)
loadNamespace("data.table")

test_that("calculate_post_engagement returns correct result.", {
  test_in <- data.table(
    post_id = 1:4
    , reach = rep(10, times = 4)
    , comments = c(0,0,2,2)
    , shares = c(0,1,0,1)
  )
  expected_result <- data.table(
    post_id = 1:4
    , engagement_score = c(0, 10^(.2), 10^(.2), 2*10^(.2))
  )
  expect_equal(gloograph::calculate_post_engagement(data_in = test_in)
               , expected_result)
})
