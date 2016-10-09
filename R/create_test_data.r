# Functions to create test data sets

#' Create random test timeline.
#'
#' @param seed Optional random seed to reproduce results.
#' @return A test timeline data frame of the same form as 
#' gloograph::test_timeline
#' @importFrom magrittr %>%

create_random_test_timeline <- function(seed = NULL){
  
  num_users <- sample(1:40, 1)
  num_spaces <- sample(5:15, 1)


}

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
     {.$owner_id}

  post_owner_type  <- timeline %>%
     dplyr::filter(action == "posts", post_id == post) %>%
     dplyr::arrange(time) %>%
     dplyr::slice(1) %>%
     {.$owner_type}

  # Determine reach by counting all users who have ever joined the space or
  # connected to the poster, depending on post type
  if (post_location_type == "Timeline"){
    reach <- timeline %>%
      dplyr::filter(
        # time <= post_time
        # , 
        action %in% c("connects", "follows")
        , object_id == post_location
        , owner_id != post_owner | owner_type != post_owner_type
    ) %>%
    {data.frame(id = .$owner_id, type = .$owner_type)} %>%
    unique %>%
    nrow
  } else {
    reach <- timeline %>%
      dplyr::filter(
        # time <= post_time
        # , 
        action == "joins"
        , object_id == post_location
        , owner_id != post_owner | owner_type != post_owner_type
      ) %>%
      {data.frame(id = .$owner_id, type = .$owner_type)} %>%
      unique %>%
      nrow
  }

  # Find number of comments
  comments  <- timeline %>%
    dplyr::filter(
      time >= post_time
      , action == "comments"
      , object_id == post
    ) %>%
    {data.frame(id = .$owner_id, type = .$owner_type)} %>%
    unique %>%
    nrow
    
  # Find number of shares
  shares <- timeline %>%  
    dplyr::filter(
      time >= post_time
      , action == "shares"
      , object_id == post
    ) %>%
    {data.frame(id = .$owner_id, type = .$owner_type)} %>%
    unique %>%
    nrow

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
     {.$owner_id}

  post_owner_type  <- timeline %>%
     dplyr::filter(action == "posts", post_id == post) %>%
     dplyr::arrange(time) %>%
     dplyr::slice(1) %>%
     {.$owner_type}

  # Determine reach by counting all users who have ever joined the space or
  # connected to the poster, depending on post type
  if (post_location_type == "Timeline"){
    reach <- timeline %>%
      dplyr::filter(
        # time <= post_time
        # , 
        action %in% c("connects", "follows")
        , object_id == post_location
#         , owner_id != post_owner | owner_type != post_owner_type
    ) %>%
    {data.frame(id = .$owner_id, type = .$owner_type)} %>%
    unique %>%
    nrow
  } else {
    reach <- timeline %>%
      dplyr::filter(
        # time <= post_time
        # , 
        action == "joins"
        , object_id == post_location
        , owner_id != post_owner | owner_type != post_owner_type
      ) %>%
      {data.frame(id = .$owner_id, type = .$owner_type)} %>%
      unique %>%
      nrow
  }

  # Find number of comments
  comments  <- timeline %>%
    dplyr::filter(
      time >= post_time
      , action == "comments"
      , object_id == post
    ) %>%
    {data.frame(id = .$owner_id, type = .$owner_type)} %>%
    unique %>%
    nrow
    
  # Find number of shares
  shares <- timeline %>%  
    dplyr::filter(
      time >= post_time
      , action == "shares"
      , object_id == post
    ) %>%
    {data.frame(id = .$owner_id, type = .$owner_type)} %>%
    unique %>%
    nrow

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

  Posts <- timeline %>%
    dplyr::filter(action == "posts") %>%
    dplyr::select(
      post_id
      , owner_id
      , owner_type
      , postable_id = object_id
      , postable_type = object_type
      , created_at = time
    )

  Comments <- timeline %>%
    dplyr::filter(action == "comments") %>%
    dplyr::select(
      post_id = object_id
      , user_id = owner_id
      , created_at = time
    )                  

  Shares <- timeline %>%
    dplyr::filter(action == "shares") %>%
    dplyr::select(
      post_id = object_id
      , sharer_id = owner_id
      , shared_at = time
    )
 
  Spaces_Users <- timeline %>%
    dplyr::filter(action %in% c('joins', 'leaves')
                  , object_type == "Space") %>%
    dplyr::select(
      space_id = object_id
      , user_id = owner_id
      , event_type = action
      , event_date = time
    )

  Connections <- timeline %>%
    dplyr::filter(action == 'connects') %>%
    dplyr::select(
      connectable1_id = owner_id
      , connectable2_id = object_id
      , connectable2_type = object_type
      , created_at = time
    ) 

  Follows <- timeline %>%
    dplyr::filter(action == 'follows') %>%
    dplyr::select(
      follower_id = owner_id
      , followable_id = object_id
      , followable_type = object_type
      , created_at = time
    )

  list(
    posts = Posts
    , comments = Comments
    , shares = Shares
    , spaces_users = Spaces_Users
    , connections = Connections
    , follows = Follows
  ) %>%
  lapply(data.table::as.data.table)
}

#' Create 'pulled' test data.
#'
#' @param timeline A data frame that describes a sequence of post-related
#' actions.


create_pulled_test_data <- function(timeline){
  "hello"
}
