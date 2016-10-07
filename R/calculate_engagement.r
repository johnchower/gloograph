# This file contains functions for calculating engagement scores for each post.

#' Calculate the engagement score of a post, from translated data.
#' 
#' @param data_in A data frame (or data table) consisting of 4 columns : post_id reach comments
#' shares
#' @param weight_comment, weight_share Weights for comment and share variables.
#' @param reach_modifier Exponent to apply to the audience size (reach)
#' @return A data frame consisting of 2 columns: post_id engagement_score
#' @import data.table

calculate_post_engagement <- function(data_in
                                      , weight_comment = 5
                                      , weight_share = 10
                                      , reach_modifier = .8){
     
  data2 <- data.table(data_in)
  out <- 
    data2[
      , .(engagement_score = 
          (weight_comment*comments 
           + weight_share*shares
          )/reach^reach_modifier
        )
      , by = post_id
    ]
  return(out)
  
}

#' Translate data to prep it for the engagement score calculation.
#' 
#' @param organized_data_in A list of data frames (the result of orgainizing
#' the data pulled from production.)
#' @return A data frame (or data table) consisting of 4 columns : 
#' post_id reach comments
#' shares
#' @import data.table

translate_data <- function(organized_data_in){
  posts <- organized_data_in$posts
  data.table::setkey(posts, postable_type)

  # Calculate reach of space posts
  space_posts_bare <- posts[.("Space"), .(space_id = postable_id, post_id)]
  data.table::setkey(space_posts_bare, space_id)

  spaces_users_bare <- organized_data_in$spaces_users[,.(space_id, user_id)]
  data.table::setkey(spaces_users_bare, space_id)

  space_posts_reach <- space_posts_bare[
    ,
    .(
      reach = 
        .SD[
          spaces_users_bare,
          length(unique(user_id)) - 1,
          nomatch = 0L
        ]
    ),
    by=post_id
  ]

  # Calculate reach of feed posts
  feed_posts_bare <- posts[.("Timeline"),
                           .(poster_id = postable_id,
                           post_id)]
  data.table::setkey(feed_posts_bare, poster_id)

  follows_bare <- organized_data_in$follows[
                                            , .(follower_id, followable_id)
                                            ,]
  
  connections_bare_1 <- 
    organized_data_in$connections[
      , .(follower_id = connectable1_id,
          followable_id = connectable2_id)
    ,]
  connections_bare_2 <- 
    organized_data_in$connections[
      , .(follower_id = connectable2_id,
          followable_id = connectable1_id)
    ,]
  
  connections_bare <- rbind(connections_bare_1
                            , connections_bare_2
                            , follows_bare)  
  
  data.table::setkey(connections_bare, followable_id)

  feed_posts_reach <- feed_posts_bare[
    ,
    .(
      reach = 
        .SD[
          connections_bare,
          length(unique(follower_id)),
          nomatch = 0L
        ]
    ),
    by = post_id
  ]

  # Total reach of each post
  post_reach <- rbind(feed_posts_reach, space_posts_reach)
  data.table::setkey(post_reach, post_id)

  # Calculate number of comments
  comments <- organized_data_in$comments[
    ,
    .(comments = .N),
    by = post_id
  ]
  data.table::setkey(comments, post_id)

  # Calculate the number of shares
  shares <- organized_data_in$shares[
    ,
    .(shares = .N),
    by = post_id
  ]
  data.table::setkey(shares, post_id)

  post_list <- posts[,.(post_id)]
  data.table::setkey(post_list, post_id)

  out <- post_reach[comments[shares[post_list]]]
  out[is.na(out)] <- 0
  data.table::setkey(out, NULL)
  out
}
