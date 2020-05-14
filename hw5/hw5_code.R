# read in data
unique_cols <- read.csv('unique_cols.csv', header = FALSE)
colnames(unique_cols) <- c('action_date', 'total_obligation', 'parent_recipient_unique_id')

unique_cols$parent_recipient_unique_id <- as.character(unique_cols$parent_recipient_unique_id)
unique_cols$total_obligation <- as.numeric(as.character(unique_cols$total_obligation))

# table the parent id
parent_id_freq <- table(unique_cols$parent_recipient_unique_id, useNA = "always")
parent_id_subset <- names(parent_id_freq[parent_id_freq > 99]) # subset to 100+
cond1 <- unique_cols$parent_recipient_unique_id %in% parent_id_subset
cond2 <- !is.na(unique_cols$total_obligation) # remove NA
cond3 <- unique_cols$total_obligation >= 1 # Remove -, 0's
data_final_sub <- unique_cols[cond1 & cond2 & cond3,]
parent_id_subset2 <- unique(data_final_sub$parent_recipient_unique_id)

# sort idea
# parent_id_sort <- sort(parent_id_subset)
# sort_100 <- data100[order(data100$parent_recipient_unique_id),]
x = parent_id_subset2[30]
# use apply on the list of parent ID's
first_digit_dist <- function(x, df_final = data_final_sub) {
  # select current ID from dataframe
  current_ID <- df_final[df_final$parent_recipient_unique_id == x,]
  # select first digit from total_obligation's rows
  total_obligation_string <- strsplit(as.character(current_ID$total_obligation), '')
  first_digits <- lapply(total_obligation_string, function(y) head(y, n = 1))
  # create factor table
  first_digit_vec <- factor(unlist(first_digits), levels = as.character(1:9))
  dist_of_digits <- table(first_digit_vec)
  return(dist_of_digits)
}

id_dist <- sapply(parent_id_subset2[10:15], function(x) first_digit_dist(x = x))
print(id_dist)

# D_KL(P,Q)
# P representing the distribution of the digits 1-9 of that particular recipient 
# Q representing the distribution of the digits 1-9 across the entire data set.

# D_KL(P,Q) = sum_{X in 1:9} P(x) log10(P(x)/Q(x))
testP1 <- id_dist[,1]
testP1_dist <- testP1 / sum(testP1)

id_row_sum <- apply(X = id_dist, MARGIN = 1, FUN = sum)  
testQ <- id_row_sum / sum(id_row_sum)
  
KLD <- function(P,Q) {
  return(sum(P*log(P/Q)))
}

KLD(testP1_dist, testQ)

# ignore cases where p(x) = 0
  
  