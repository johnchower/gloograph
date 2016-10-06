# Test correctness of the engagement score pipeline

test_that("calculate_post_engagement returns correct result.", {
  timeline <- gloograph::test_timeline
  test_translated <- gloograph::create_translated_test_data(timeline)
  test_calculated <- gloograph::create_calculated_test_data(timeline)
  
  expect_equal(gloograph::calculate_post_engagement(data_in = test_translated)
               , test_calculated)
})

test_that("translate_data returns correct result", {
  timeline <- gloograph::test_timeline
  test_translated <- gloograph::create_translated_test_data(timeline)
  test_organized <- gloograph::create_organized_test_data(timeline)

  expect_equal(gloograph::translate_data(test_organized)
               , test_translated)
})
