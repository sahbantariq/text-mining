library(Rfacebook)
libray(dplyr)
library(xlsx)

# Fb Authorization
fb_oauth <- Rfacebook::fbOAuth(app_id="1380496555352781", 
                               app_secret="eb3abc42d1e00536e6f4e37e58fc0b5d", 
                               extended_permissions = TRUE)

# Saving variable fb_oauth in a file and loading it
save(fb_oauth, file="fb_oauth")
load("fb_oauth")

# Extract posts from turkish airlines page
turkishairlines <- getPage(page = "turkishairlines", token = fb_oauth, n = 2000)

# Save the posts in R data file
saveRDS(turkishairlines, "turkishairlines_2000")

# Read R data file and store in new variable
data_turkishairlines <- readRDS("turkishairlines_2000")

# Storing the post ids in a new variable
data_turkishairlines_id <- data_turkishairlines$id

# Extracting comments and reactions from the extracted posts
# The comments are extracted using post ids. Therefore, id variable
# is used. 
all_posts <- lapply(data_turkishairlines_id[1:300], getPost, n = 50000, 
                    token=fb_oauth, comments = TRUE, reactions = TRUE)

# Save first 300 posts comments in a R data file.
saveRDS(all_posts, "300_posts")

# Reading the saved RDS file instead of applying getPost function repeatedly.
# The resultant variable is a list which includes a post specific list.
# Within each post specific list there are post comments and reactions.
threehundred_posts <- readRDS("300_posts") 


# Saving comments for first post in new variable
full_comments <- setNames(as.data.frame(threehundred_posts[[1]]$comments$message), 
                          "comments") %>% 
  dplyr::mutate(id = data_turkishairlines_id[1])

# Exanding full_comments by adding comments of remaining posts. 
for (i in 1:299) {
  comment <- setNames(as.data.frame(threehundred_posts[[i+1]]$comments$message), 
                      "comments") %>% 
    dplyr::mutate(id = data_turkishairlines_id[i+1])
  full_comments = rbind(full_comments, comment)
}

# Assigning the post ids to its comments. The posts ids are repeated when there 
# are more than one comments.  
full_comment_post <- dplyr::left_join(full_comments, 
                                      data_turkishairlines, by = "id")

# Saving the comments of posts in R data file. 
saveRDS(full_comment_post, "300_posts_comments")

