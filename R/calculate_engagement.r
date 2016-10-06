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
#' @return A data frame (or data table) consisting of 4 columns : post_id reach comments
#' shares
#' @import data.table

translate_data <- function(organized_data_in){
  "Hello"
}
