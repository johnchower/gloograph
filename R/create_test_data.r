# Functions to create test data sets

#' Create 'calculated' test data set.
#'
#' @param timeline A data frame that describes a sequence of post-related
#' actions.
#' @param weight_comment, weight_share, reach_modifier Parameters for the
#' actual calculation step.
#' @return A data frame consisting of two columns: post_id engagement_score.
#' @importFrom magrittr %>%

create_calculated_test_data <- function(timeline
                                        , weight_comment = 5
                                        , weight_share = 10
                                        , reach_modifier = .8){
  post_ids <- timeline %>%
    dplyr::filter(action == "posts") %>%
    {.$post_id} %>%
    unique

  out <- data.table()

  for (post in post_ids){
   post_time <- timeline %>%
     dplyr::filter(action == "posts", post_id == post) %>%
     {.$time} %>%
     min

   post_location <- timeline %>%
     dplyr::filter(action == "posts", post_id == post) %>%
     dplyr::arrange(time) %>%
     dplyr::slice(1) %>%
     {.$object_id}

   post_location_type <- timeline %>%
     dplyr::filter(action == "posts", post_id == post) %>%
     dplyr::arrange(time) %>%
     dplyr::slice(1) %>%
     {.$object_type}

   post_owner <- timeline %>%
     dplyr::filter(action == "posts", post_id == post) %>%
     dplyr::arrange(time) %>%
     dplyr::slice(1) %>%
     {.$user_id}

  # Determine reach by counting all users who joined or connected before post
  # time.
  if (post_location_type == "feed"){
    reach <- timeline %>%
      dplyr::filter(
        # time <= post_time
        # , 
        action %in% c("connects", "follows")
        , object_id == post_location
        , user_id != post_owner
    ) %>%
    {.$user_id} %>%
    unique %>%
    length
  } else {
    reach <- timeline %>%
      dplyr::filter(
        # time <= post_time
        # , 
        action == "joins"
        , object_id == post_location
        , user_id != post_owner
      ) %>%
      {.$user_id} %>%
      unique %>%
      length
  }

  # Find number of comments
  comments  <- timeline %>%
    dplyr::filter(
      time >= post_time
      , action == "comments"
      , object_id == post
    ) %>%
    {.$user_id} %>%
    unique %>%
    length
    
  # Find number of shares
  shares <- timeline %>%  
    dplyr::filter(
      time >= post_time
      , action == "shares"
      , object_id == post
    ) %>%
    {.$user_id} %>%
    unique %>%
    length

  score <- 
    (weight_comment*comments 
     + weight_share*shares
    )/reach^reach_modifier
  
  out <- rbind(out, data.table(post_id = post
                               , engagement_score = score))

  }
  return(out)
}

#' Create 'translated' test data set.
#'
#' @param timeline A data frame that describes a sequence of post-related
#' actions.


create_translated_test_data <- function(timeline){
    post_ids <- timeline %>%
    dplyr::filter(action == "posts") %>%
    {.$post_id} %>%
    unique

  out <- data.table()

  for (post in post_ids){
   post_time <- timeline %>%
     dplyr::filter(action == "posts", post_id == post) %>%
     {.$time} %>%
     min

   post_location <- timeline %>%
     dplyr::filter(action == "posts", post_id == post) %>%
     dplyr::arrange(time) %>%
     dplyr::slice(1) %>%
     {.$object_id}

   post_location_type <- timeline %>%
     dplyr::filter(action == "posts", post_id == post) %>%
     dplyr::arrange(time) %>%
     dplyr::slice(1) %>%
     {.$object_type}

   post_owner <- timeline %>%
     dplyr::filter(action == "posts", post_id == post) %>%
     dplyr::arrange(time) %>%
     dplyr::slice(1) %>%
     {.$user_id}

  # Determine reach by counting all users who joined or connected before post
  # time.
  if (post_location_type == "feed"){
    reach <- timeline %>%
      dplyr::filter(
        # time <= post_time
        # , 
        action %in% c("connects", "follows")
        , object_id == post_location
        , user_id != post_owner
    ) %>%
    {.$user_id} %>%
    unique %>%
    length
  } else {
    reach <- timeline %>%
      dplyr::filter(
        # time <= post_time
        # , 
        action == "joins"
        , object_id == post_location
        , user_id != post_owner
      ) %>%
      {.$user_id} %>%
      unique %>%
      length
  }

  # Find number of comments
  comments  <- timeline %>%
    dplyr::filter(
      time >= post_time
      , action == "comments"
      , object_id == post
    ) %>%
    {.$user_id} %>%
    unique %>%
    length
    
  # Find number of shares
  shares <- timeline %>%  
    dplyr::filter(
      time >= post_time
      , action == "shares"
      , object_id == post
    ) %>%
    {.$user_id} %>%
    unique %>%
    length

  out <- rbind(out, data.table(post_id = post
                               , reach = reach
                               , comments = comments
                               , shares = shares))

  }
  return(out)
}

#' Create 'organized' test data.
#'
#' @param timeline A data frame that describes a sequence of post-related
#' actions.


create_organized_test_data <- function(timeline){
  "hello"
}

#' Create 'pulled' test data.
#'
#' @param timeline A data frame that describes a sequence of post-related
#' actions.


create_pulled_test_data <- function(timeline){
  "hello"
}
