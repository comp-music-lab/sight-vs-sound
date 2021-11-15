### Constants ###
NUM_EXPERIMENT <- 9

### Read data ###
T <- readLines("./conditions.txt")
R <- read.csv("./responses.csv", header = FALSE)

NUM_PARTICIPANT <- length(T)

### Output data ###
T_df <- data.frame(matrix(NA, nrow = NUM_PARTICIPANT*NUM_EXPERIMENT, ncol = 11))
colnames(T_df) <- c("participant_id", "data_id", "domain", 
                   "rank_true_1st", "rank_true_2nd", "rank_true_3rd",
                   "rank_pred_1st", "rank_pred_2nd", "rank_pred_3rd",
                   "match_high_var", "match_low_var")

### Formatting (conditions)###
counter <- 1

for (i in 1:NUM_PARTICIPANT) {
  participant_i <- matrix(strsplit(T[i], "")[[1]], nrow = NUM_EXPERIMENT, byrow = TRUE)
  
  for (j in 1:NUM_EXPERIMENT) {
    T_df[counter, 1] <- i
    T_df[counter, 2] <- participant_i[j, 3]
    T_df[counter, 3] <- paste(participant_i[j, 1], participant_i[j, 2], sep = "")
    
    T_df[counter, 4:6] <- switch(participant_i[j, 4],
           "a" = c(1, 2, 3),
           "b" = c(1, 3, 2),
           "c" = c(2, 1, 3),
           "d" = c(2, 3, 1),
           "e" = c(3, 1, 2),
           "f" = c(3, 2, 1)
    )
    
    counter <- counter + 1
  }
}

### Formatting (responses)###
counter <- 1

for (i in 1:NUM_PARTICIPANT) {
  participant_i <- matrix(R[1, ], nrow = NUM_EXPERIMENT, byrow = TRUE)
  
  for (j in 1:NUM_EXPERIMENT) {
    T_df[counter, 6 + as.numeric(participant_i[j, 1])] <- 1
    T_df[counter, 6 + as.numeric(participant_i[j, 2])] <- 3
    T_df[counter, 6 + setdiff(c(1, 2, 3), c(participant_i[j, 1], participant_i[j, 2]))] <- 2
    
    ##
    ranking <- c(as.numeric(T_df[counter, 4:6]))
    
    idx_t_1 <- which(ranking == 1)
    idx_t_2 <- which(ranking == 2)
    idx_t_3 <- which(ranking == 3)
    
    tmp <- setdiff(ranking, 2)
    rel_t <- tmp[1] > tmp[2]
    rel_p <- T_df[counter, 6 + min(idx_t_1, idx_t_3)] > T_df[counter, 6 + max(idx_t_1, idx_t_3)]
    
    if (rel_t == rel_p) {
      T_df[counter, 10] = 1
    } else {
      T_df[counter, 10] = 0
    }
    
    tmp <- setdiff(ranking, 3)
    rel_t <- tmp[1] > tmp[2]
    rel_p <- T_df[counter, 6 + min(idx_t_1, idx_t_2)] > T_df[counter, 6 + max(idx_t_1, idx_t_2)]
    
    if (rel_t == rel_p) {
      T_df[counter, 11] = 1
    } else {
      T_df[counter, 11] = 0
    }
    
    counter <- counter + 1
  }
}

### Save ###
write.csv(T_df, file = "./data-relative_matching.csv", row.names = FALSE)
print("Program completed")