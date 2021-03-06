# Functions to create test data sets

#' Create random test timeline.
#'
#' @param seed Optional random seed to reproduce results.
#' @return A randomly-generated test timeline data frame of the same form as 
#' gloograph::test_timeline
#' @importFrom magrittr %>%

create_random_test_timeline <- function(
  seed = NULL
  , max_users = 40
  , min_users = 10
  , max_spaces = 15
  , min_spaces = 5
  , max_timeline_posts_per_user = 20
  , max_space_post_per_user_space = 5){

  if(!is.null(seed)){
    set.seed(seed)
  }

  date_range <- seq.Date(
    from = as.Date('2016-01-01')
    , to = as.Date('2016-06-30')
    , by = 1
  )  

  num_users <- sample(min_users:max_users, 1)
  num_spaces <- sample(min_spaces:max_spaces, 1)
  
  users <- 1:num_users
  spaces <- 1:num_spaces

  space_join_actions <- 
    users %>%
    as.list %>%
    plyr::ldply(.fun = function(user){
      number_of_spaces_to_join <- sample(1:num_spaces, 1)
      spaces_to_join  <- sample(x = spaces
                                , size = number_of_spaces_to_join
                                , replace = F)
      times_to_join <- sample(x = date_range
                              , size = number_of_spaces_to_join
                              , replace = T)
      data.frame(user_id = rep(user, times = number_of_spaces_to_join)
                 , space_id = spaces_to_join
                 , created_at = times_to_join
                 , stringsAsFactors = F) 
    })

  follow_actions <-
    users %>%
    as.list %>%
    plyr::ldply(.fun = function(user){
      number_of_users_to_connect_to <- sample(1:(num_users-1), 1)
      users_to_connect_to <- sample(setdiff(users, user)
                                    , size = number_of_users_to_connect_to
                                    , replace = F)
      times_to_connect <- sample(x = date_range
                                 , size = number_of_users_to_connect_to
                                 , replace = T)
      data.frame(follower_id = rep(user, times = number_of_users_to_connect_to)
                 , followable_id = users_to_connect_to
                 , created_at = times_to_connect
                 , stringsAsFactors = F)
    })

  connection_actions <- 
    users %>%
    as.list %>%
    plyr::ldply(.fun = function(user){
      number_of_users_to_connect_to <- sample(1:(num_users-1), 1)
      users_to_connect_to <- sample(setdiff(users, user)
                                    , size = number_of_users_to_connect_to
                                    , replace = F)
      times_to_connect <- sample(x = date_range
                                 , size = number_of_users_to_connect_to
                                 , replace = T)
      
      connectable1_id <- c(rep(user, times = number_of_users_to_connect_to)
                           , users_to_connect_to)
      connectable2_id <- c(users_to_connect_to
                           , rep(user, times = number_of_users_to_connect_to))
      created_at <- rep(times_to_connect, times = 2)
      out <- data.frame(connectable1_id
                 , connectable2_id
                 , created_at
                 , stringsAsFactors = F)
    }) %>%
    dplyr::group_by(connectable1_id, connectable2_id) %>%
    dplyr::filter(created_at == min(created_at)) %>%
    dplyr::ungroup()

  timeline_post_actions <- 
    users %>%
    as.list %>%
    plyr::ldply(.fun = function(user){
      number_of_posts_to_make <- sample(1:max_timeline_posts_per_user, 1)
      times_to_post <- sample(x = date_range
                              , size = number_of_posts_to_make
                              , replace = F)
      data.frame(user_id = user
                 , postable_id = user
                 , postable_type = "Timeline"
                 , created_at = times_to_post
                 , stringsAsFactors = F)
    })

  space_post_actions <- 
    users %>%
    as.list %>%
    plyr::ldply(.fun = function(user){
      space_joins <- space_join_actions %>%
        dplyr::filter(user_id == user) %>%
        dplyr::group_by(user_id, space_id) 
      space_joins %>%
        dplyr::do({
          min_date <- .$created_at[1]
          number_of_posts_to_make <-
            sample(1:max_space_post_per_user_space, 1)
          times_to_post <- sample(
            seq.Date(min_date
                     , as.Date('2016-06-30')
                     , by = 1)
            , number_of_posts_to_make
            , replace = T
          )
          data.frame(user_id = rep(.$user_id[1]
                                   , times = number_of_posts_to_make)
                     , postable_id = rep(.$space_id[1]
                                   , times = number_of_posts_to_make)
                     , postable_type = "Space"
                     , created_at = times_to_post
                     , stringsAsFactors = F)
        })
    }) %>%
    dplyr::select(-space_id)

  post_actions <- rbind(space_post_actions
                        , timeline_post_actions) %>%
    dplyr::arrange(created_at) %>%
    dplyr::mutate(post_id = 1:nrow(.))

  comments_shares <- post_actions %>% 
    dplyr::rename(poster_id = user_id) %>%
    dplyr::group_by(postable_id
                    , postable_type
                    , created_at
                    , post_id
                    , poster_id) %>%
    dplyr::do({
      if(.$postable_type[1] == "Space"){
        current_space_id <- .$postable_id[1]
        current_post_created_at <- .$created_at[1]
        current_user_id <- .$poster_id[1]
        users_belonging_to_current_space <- 
          space_join_actions %>%
          dplyr::filter(space_id == current_space_id
                        , user_id != current_user_id) %>%
          dplyr::select(user_id) %>%
          unique
        number_of_users_in_audience <- 
          nrow(users_belonging_to_current_space)
        number_of_users_to_respond <- 
          sample(1:number_of_users_in_audience
                 , 1)
        users_who_responded_to_post <- 
          users_belonging_to_current_space %>%
          dplyr::slice(sample(1:nrow(.)
                              , number_of_users_to_respond
          )) %>%
          dplyr::mutate(
            response_type = sample(
              c('comments', 'shares')                                   
              , nrow(.)
              , replace = T
            )
          )
      } else {
        current_user_id <- .$postable_id[1]
        current_post_created_at <- .$created_at[1]
        users_connected_to_current_user <- 
          connection_actions %>%
          dplyr::filter(connectable1_id == current_user_id) %>%
          dplyr::select(user_id = connectable2_id) %>%
          unique
        users_following_current_user <- 
          follow_actions %>%
          dplyr::filter(followable_id == current_user_id) %>%
          dplyr::select(user_id = follower_id) %>%
          unique
        users_in_audience <- 
          rbind(users_connected_to_current_user
                , users_following_current_user) 
        number_of_users_in_audience <- 
          users_in_audience %>%
          unique %>%
          nrow
        number_of_users_to_respond <- 
          sample(1:number_of_users_in_audience
                 , 1)
        users_who_responded_to_post <- 
          users_in_audience %>%
          dplyr::slice(sample(1:nrow(.)
                              , number_of_users_to_respond
          )) %>%
          dplyr::mutate(
            response_type = sample(
              c('comments', 'shares')                                   
              , nrow(.)
              , replace = T
            )
          )
      }
      return(users_who_responded_to_post)
    }) %>%
    dplyr::ungroup() %>%
    dplyr::group_by(created_at) %>%
    dplyr::mutate(
      response_time = 
        sample(seq.Date(min(created_at) # Try changing this line
                        , as.Date('2016-06-30')
                        , by = 1)
               , n()
               , replace = T)) %>%
    dplyr::ungroup()
               
  timeline_comments_shares <- comments_shares %>%
    dplyr::select(time = response_time
                  , action = response_type
                  , object_id = post_id
                  , owner_id = user_id) %>%
    dplyr::mutate(post_id = NA
                  , owner_type = "User"
                  , object_type = 'post')
           
  timeline_connection_actions <- connection_actions %>%
    dplyr::select(time = created_at
                  , object_id = connectable2_id
                  , owner_id = connectable1_id) %>%
    dplyr::mutate(action = 'connects'
                  , object_type = 'user'
                  , post_id = NA
                  , owner_type = 'User')

  timeline_follow_actions <- follow_actions %>%
    dplyr::select(time = created_at
                  , object_id = followable_id
                  , owner_id = follower_id) %>%
    dplyr::mutate(action = 'follows'
                  , object_type = 'user'
                  , post_id = NA
                  , owner_type = 'User') %>%
    dplyr::select(time
                  , action
                  , object_type
                  , object_id
                  , owner_id
                  , post_id
                  , owner_type)

  timeline_post_actions <- post_actions %>%
    dplyr::select(time = created_at
                  , object_type = postable_type
                  , object_id = postable_id
                  , owner_id = user_id
                  , post_id) %>%
    dplyr::mutate(action = 'posts'
                  , owner_type = 'User') %>%
    dplyr::select(time
                  , action
                  , object_type
                  , object_id
                  , owner_id
                  , post_id
                  , owner_type)
                  
  timeline_space_join_actions <- space_join_actions %>%
    dplyr::select(time = created_at
                  , object_id = space_id
                  , owner_id = user_id) %>%
    dplyr::mutate(action = 'joins'
                  , object_type = 'Space'
                  , post_id = NA
                  , owner_type = 'User') %>%
    dplyr::select(time
                  , action
                  , object_type
                  , object_id
                  , owner_id
                  , post_id
                  , owner_type)
  timeline_out <- rbind(timeline_comments_shares
                        , timeline_connection_actions
                        , timeline_follow_actions
                        , timeline_post_actions
                        , timeline_space_join_actions) %>%
    dplyr::arrange(time)

  return(timeline_out)
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
    x <- find_reach_comments_shares(post, timeline)
    comments <- x$Comments
    shares <- x$Shares
    reach <- x$Reach
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
    x <- find_reach_comments_shares(post, timeline)
    comments <- x$Comments
    shares <- x$Shares
    reach <- x$Reach
    out <- rbind(out, data.table(post_id = post
                                 , reach = reach
                                 , comments = comments
                                 , shares = shares))

  }
  return(out)
}

#' Find reach, comments, shares.
#' 
#' @param post A post_id
#' @param timeline A timeline of post-related actions, in the same form as
#' gloograph::test_timeline
#' @importFrom magrittr %>%

find_reach_comments_shares <- function(post
                                       , timeline){

  post_info <- timeline %>%
    dplyr::filter(action == "posts", post_id == post) %>%
    dplyr::arrange(time) %>%
    dplyr::slice(1) 

  post_time <- post_info$time
  post_location <- post_info$object_id
  post_location_type <- post_info$object_type
  post_owner <- post_info$owner_id
  post_owner_type  <- post_info$owner_type

  # Determine reach by counting all users who have ever joined the space or
  # connected to the poster, depending on post type
  reach <- timeline %>% {
    if (post_location_type == "Timeline"){
      dplyr::filter(.
        , action %in% c("connects", "follows")
        , object_id == post_location
        , owner_id != post_owner | owner_type != post_owner_type
      )
    } else {
      dplyr::filter(.
        , action == "joins"
        , object_id == post_location
        , owner_id != post_owner | owner_type != post_owner_type
      )
    }
  } %>%
  {data.frame(id = .$owner_id, type = .$owner_type)} %>%
  unique %>%
  nrow

  # Find number of comments
  comments  <- timeline %>%
    dplyr::filter(
      time >= post_time
      , action == "comments"
      , object_id == post
    ) %>%
    nrow
    
  # Find number of shares
  shares <- timeline %>%  
    dplyr::filter(
      time >= post_time
      , action == "shares"
      , object_id == post
    ) %>%
    nrow

  list(Reach = reach, Comments = comments, Shares = shares)
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
  "hi"
}

#' Pull a small set of data from production.
#'
#' @param con Connection object for production database.
#' 
#' @return A list of data frames:
#'  spaces_users_small
#'  connections_small
#'  follows_small
#'  posts_small
#'  comments_small
#'  shares_small

pull_small_production_data <- function(){
   
}

