rm(list = ls())
organized_data_in <- .organized_data_in

feed_posts_bare[,.SD[connections_bare,length(unique(follower_id)), nomatch = 0L],by = post_id]

feed_posts_bare[post_id == 4][connections_bare, nomatch = 0L]
