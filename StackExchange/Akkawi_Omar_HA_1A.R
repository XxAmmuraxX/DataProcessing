#loading data
data_names <- c("Posts", "Users", "Comments", "Badges","Votes","PostLinks","Tags")
for(d in data_names) {
  assign(d,read.csv(paste(d, ".csv", sep="")))
}

library(dplyr)
library(compare)
library(microbenchmark)

#________________Task1 Base R_____________________

duplicated_posts <- subset(PostLinks, LinkTypeId == 3)
duplicated_counts <- aggregate(Id ~ OwnerUserId, 
                               data = Posts[Posts$Id %in% duplicated_posts$PostId, ],
                               FUN = length)
colnames(duplicated_counts) <- c("UserId", "DuplicatedPostCount")

question_count <- as.data.frame(table(Posts$OwnerUserId[Posts$PostTypeId == 1]))
answer_count <- as.data.frame(table(Posts$OwnerUserId[Posts$PostTypeId == 2]))
comment_count <- as.data.frame(table(Comments$UserId))

colnames(question_count) <- c("UserId", "TotalQuestions")
colnames(answer_count) <- c("UserId", "TotalAnswers")
colnames(comment_count) <- c("UserId", "TotalComments")

score <- aggregate(Posts$Score, by = Posts['OwnerUserId'], sum)
colnames(score) <- c("UserId", "OverallScore")

result <- merge(duplicated_counts, question_count, by = "UserId", all.x = TRUE)
result <- merge(result, answer_count, by = "UserId", all.x = TRUE)
result <- merge(result, comment_count, by = "UserId", all.x = TRUE)
result <- merge(result, score, by = "UserId", all.x = TRUE)
result <- merge(result, Users[c('Id','DisplayName')], by.x = "UserId", by.y = "Id", all.x = TRUE)

result <- head(result[order(result$DuplicatedPostCount,decreasing = TRUE),], 10)

#______________________Task1 using dplyr_____________________
duplicated_counts <- PostLinks %>%
  filter(LinkTypeId == 3) %>%
  inner_join(Posts, by = c("PostId" = "Id")) %>%
  distinct(PostId,.keep_all = TRUE) %>%
  group_by(OwnerUserId) %>%
  summarise(DuplicatedPostCount = n(), .groups = "drop")

question_count <- Posts %>%
  filter(PostTypeId == 1) %>%
  count(OwnerUserId, name = "TotalQuestions")

answer_count <- Posts %>%
  filter(PostTypeId == 2) %>%
  count(OwnerUserId, name = "TotalAnswers")

comment_count <- Comments %>%
  count(UserId, name = "TotalComments")

score <- Posts %>%
  group_by(OwnerUserId) %>%
  summarise(OverallScore = sum(Score, na.rm = TRUE), .groups = "drop")

result_dplyr <- duplicated_counts %>%
  left_join(question_count, by = "OwnerUserId") %>%
  left_join(answer_count, by = "OwnerUserId") %>%
  left_join(comment_count, by = c("OwnerUserId" = "UserId")) %>%
  left_join(score, by = "OwnerUserId") %>%
  left_join(Users %>% select(Id, DisplayName), by = c("OwnerUserId" = "Id")) %>%
  arrange(desc(DuplicatedPostCount)) %>%
  slice_head(n = 10)

#______________________Task 1 Comparing the results_____________________
compare::compare(result, as.data.frame(result_dplyr),allowAll=TRUE)

#output:
#TRUE
#  renamed
#  renamed rows
#  dropped names
#  dropped row names


#______________________Task 1 Benchmarking_____________________

microbenchmark(
  BaseR = {
    
    duplicated_posts <- subset(PostLinks, LinkTypeId == 3)
    duplicated_counts <- aggregate(Id ~ OwnerUserId, 
                                   data = Posts[Posts$Id %in% duplicated_posts$PostId, ],
                                   FUN = length)
    colnames(duplicated_counts) <- c("UserId", "DuplicatedPostCount")
    
    question_count <- as.data.frame(table(Posts$OwnerUserId[Posts$PostTypeId == 1]))
    answer_count <- as.data.frame(table(Posts$OwnerUserId[Posts$PostTypeId == 2]))
    comment_count <- as.data.frame(table(Comments$UserId))
    
    colnames(question_count) <- c("UserId", "TotalQuestions")
    colnames(answer_count) <- c("UserId", "TotalAnswers")
    colnames(comment_count) <- c("UserId", "TotalComments")
    
    score <- aggregate(Posts$Score, by = Posts['OwnerUserId'], sum)
    colnames(score) <- c("UserId", "OverallScore")
    
    result <- merge(duplicated_counts, question_count, by = "UserId", all.x = TRUE)
    result <- merge(result, answer_count, by = "UserId", all.x = TRUE)
    result <- merge(result, comment_count, by = "UserId", all.x = TRUE)
    result <- merge(result, score, by = "UserId", all.x = TRUE)
    result <- merge(result, Users[c('Id','DisplayName')], by.x = "UserId", by.y = "Id", all.x = TRUE)
    
    result <- head(result[order(result$DuplicatedPostCount,decreasing = TRUE),], 10)
    
  },
  Dplyr = {
    duplicated_counts <- PostLinks %>%
      filter(LinkTypeId == 3) %>%
      inner_join(Posts, by = c("PostId" = "Id")) %>%
      distinct(PostId,.keep_all = TRUE) %>%
      group_by(OwnerUserId) %>%
      summarise(DuplicatedPostCount = n(), .groups = "drop")
    
    question_count <- Posts %>%
      filter(PostTypeId == 1) %>%
      count(OwnerUserId, name = "TotalQuestions")
    
    answer_count <- Posts %>%
      filter(PostTypeId == 2) %>%
      count(OwnerUserId, name = "TotalAnswers")
    
    comment_count <- Comments %>%
      count(UserId, name = "TotalComments")
    
    score <- Posts %>%
      group_by(OwnerUserId) %>%
      summarise(OverallScore = sum(Score, na.rm = TRUE), .groups = "drop")
    
    result_dplyr <- duplicated_counts %>%
      left_join(question_count, by = "OwnerUserId") %>%
      left_join(answer_count, by = "OwnerUserId") %>%
      left_join(comment_count, by = c("OwnerUserId" = "UserId")) %>%
      left_join(score, by = "OwnerUserId") %>%
      left_join(Users %>% select(Id, DisplayName), by = c("OwnerUserId" = "Id")) %>%
      arrange(desc(DuplicatedPostCount)) %>%
      slice_head(n = 10)
  }, times = 5
) -> r

summary(r)

#Output:
#   expr      min       lq     mean   median       uq      max neval
#1 BaseR 543.2366 555.399 642.5190 671.5769 691.0155 751.3671     5
#2 Dplyr 514.5005 571.351 625.0144 600.5683 661.6529 776.9991     5

#_______________________Task 2 Base R_______________________

posts20to6 <- Posts[as.integer(substr(Posts$CreationDate,12,13)) >= 20 | as.integer(substr(Posts$CreationDate,12,13)) < 6,] 

user_post_counts <- aggregate(posts20to6$Id,posts20to6['OwnerUserId'], FUN=length)

colnames(user_post_counts) <- c("UserId", "PostCount")

u_pc_l <- merge(user_post_counts, Users[, c("Id", "Location")],
                                        by.x = "UserId", by.y = "Id", all.x = TRUE)

u_pc_l <- u_pc_l[order(u_pc_l$PostCount,decreasing = TRUE), ]

#_______________________Task 2 using dplyr_______________________

posts20to6 <- Posts %>%
  filter(as.integer(substr(CreationDate, 12, 13)) >= 20 | as.integer(substr(CreationDate, 12, 13)) < 6)

user_post_counts <- posts20to6 %>%
  group_by(OwnerUserId) %>%
  summarise(PostCount = n(), .groups = "drop")

u_pc_l_dplyr <- user_post_counts %>%
  left_join(Users %>% select(Id, Location), by = c("OwnerUserId" = "Id")) %>%
  filter(!is.na(OwnerUserId)) %>%
  arrange(desc(PostCount))

#_______________________Task 2 Comparing the results_______________________

compare::compare(u_pc_l, as.data.frame(u_pc_l_dplyr), allowAll=TRUE)

#Output:
#TRUE
#   renamed
#   renamed rows
#   dropped names
#   dropped row names

#_______________________Task 2 Benchmarking_______________________

microbenchmark(
  BaseR = {
    
    posts20to6 <- Posts[as.integer(substr(Posts$CreationDate,12,13)) >= 20 | as.integer(substr(Posts$CreationDate,12,13)) < 6,] 
    
    user_post_counts <- aggregate(posts20to6$Id,posts20to6['OwnerUserId'], FUN=length)
    
    colnames(user_post_counts) <- c("UserId", "PostCount")
    
    u_pc_l <- merge(user_post_counts, Users[, c("Id", "Location")],
                    by.x = "UserId", by.y = "Id", all.x = TRUE)
    
    u_pc_l <- u_pc_l[order(u_pc_l$PostCount,decreasing = TRUE), ]
    
  },
  Dplyr = {
    posts20to6 <- Posts %>%
      filter(as.integer(substr(CreationDate, 12, 13)) >= 20 | as.integer(substr(CreationDate, 12, 13)) < 6)
    
    user_post_counts <- posts20to6 %>%
      group_by(OwnerUserId) %>%
      summarise(PostCount = n(), .groups = "drop")
    
    u_pc_l_dplyr <- user_post_counts %>%
      left_join(Users %>% select(Id, Location), by = c("OwnerUserId" = "Id")) %>%
      filter(!is.na(OwnerUserId)) %>%
      arrange(desc(PostCount))
  }, times = 5
) -> r

summary(r)
 
#Output:
#   expr      min       lq     mean   median       uq      max neval
#1 BaseR 252.2370 257.0680 269.2818 265.5872 267.6853 303.8317     5
#2 Dplyr 169.9368 182.9043 215.0853 205.1753 254.3722 263.0377     5

#_______________________Task3 description_______________________

#This sql query gives us the top 10 users with the highest average number of answers to their questions.

#_______________________Task3 using dplyr_______________________
AnsCount <- Posts %>%
  filter(PostTypeId == 2) %>%                        
  group_by(ParentId) %>%                             
  summarise(AnswersCount = n())                      

PostOwner <- AnsCount %>%
  inner_join(Posts, by = c("ParentId" = "Id")) %>%
  select(AnswersCount, ParentId, OwnerUserId)              

result <- PostOwner %>%
  inner_join(Users, by = c("OwnerUserId" = "AccountId"),relationship = 'many-to-many') %>%  
  group_by(OwnerUserId, DisplayName, Location) %>%            
  summarise(AverageAnswersCount = mean(AnswersCount, na.rm = TRUE), .groups = 'drop') %>% 
  arrange(desc(AverageAnswersCount),desc(OwnerUserId)) %>%                       
  slice_head(n = 10)                                         

result <- as.data.frame(result)

#_______________________Task3 using sqldf_______________________
library(sqldf)

sql_result <- sqldf("SELECT
                    Users.AccountId,
                    Users.DisplayName,
                    Users.Location,
                    AVG(PostAuth.AnswersCount) as AverageAnswersCount
                    FROM
                    (
                      SELECT
                      AnsCount.AnswersCount,
                      Posts.Id,
                      Posts.OwnerUserId
                      FROM (
                        SELECT Posts.ParentId, COUNT(*) AS AnswersCount
                        FROM Posts
                        WHERE Posts.PostTypeId = 2
                        GROUP BY Posts.ParentId
                      ) AS AnsCount
                      JOIN Posts ON Posts.Id = AnsCount.ParentId
                    ) AS PostAuth
                    JOIN Users ON Users.AccountId=PostAuth.OwnerUserId
                    GROUP BY OwnerUserId
                    ORDER BY AverageAnswersCount DESC
                    LIMIT 10")

#_______________________Task 3 Comparing the results_______________________
compare::compare(result, sql_result,allowAll=TRUE)

#Output:
#TRUE
#  renamed
#  dropped names
