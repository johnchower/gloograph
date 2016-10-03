# This file contains functions for calculating engagement scores from
# translated data.

#' Calculate the engagement score of a post.
#' 
#' @param .data A data frame (or data table) consisting of 4 columns : post_id reach comments
#' shares
#' @param weight_comment, weight_share Weights for comment and share variables.
#' @param reach_modifier Exponent to apply to the audience size (reach)
#' @return A data frame consisting of 2 columns: post_id engagement_score

calculate_post_engagement <- function(.data
                                      , weight_comment = 5
                                      , weight_share = 10
                                      , reach_modifier = .8){
  data.table(
    post_id = 1:4
    , engagement_score = 
        (weight_comment*.data$comments 
         + weight_share*.data$shares
        )/.data$reach^reach_modifier
  )
}
