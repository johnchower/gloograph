"/* Connections */ select connectable1_id , connectable2_id , created_at from connections where connectable1_type = 'User' and connectable2_type = 'User' ;"

"/* Follows */ select followable_id , follower_id , created_at from follows where follower_type = 'User' and followable_type = 'User' ;"

"/* Posts */ select owner_id , id post_id , postable_type , postable_id , created_at from posts where owner_type = 'User' ;"

"/* Comments */ select user_id , post_id , created_at from comments where owner_type = 'User' ;"

"/* Shares */ select posts.owner_id user_id, artifacts.id artifact_id from posts join artifacts on posts.artifact_id = artifacts.id where posts.owner_type = 'User' and posts.post_type = 0 and (posts.postable_type = 'Timeline' or posts.postable_type = 'Space') ;"


