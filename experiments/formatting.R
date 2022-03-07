rm(list = ls())

###### import libraries ######
library(stringr)

###### configuration ######
OUTPUT_FILENAME <- "datatable_20220306.csv"
NUM_STIMULI <- 75

###### read data ######
T <- read.csv("./data/Sight vs Sound回答フォーム_1 （回答）_202203061008.csv", header = TRUE)
COLNUM_DATAID <- 53
NUM_DATA <- 50

S <- read.csv("./data/stimuli.csv", header = TRUE)

###### format stimuli master data ######
df_stimuli <- rbind(unique(data.frame(data_id = as.numeric(sapply(strsplit(substring(S[, 2], 1), "-"), function(x){return(x[[1]][1])})),
                              instrument = S[, 9],
                              varcond = S[, 11],
                              domain = "Audio-only",
                              competition = S[, 5])),
            unique(data.frame(data_id = as.numeric(sapply(strsplit(substring(S[, 3], 1), "-"), function(x){return(x[[1]][1])})),
                              instrument = S[, 9],
                              varcond = S[, 11],
                              domain = "Visual-only",
                              competition = S[, 5])),
            unique(data.frame(data_id = as.numeric(sapply(strsplit(substring(S[, 4], 1), "-"), function(x){return(x[[1]][1])})),
                              instrument = S[, 9],
                              varcond = S[, 11],
                              domain = "Audio-Visual",
                              competition = S[, 5]))
)

df_stimuli <- df_stimuli[order(df_stimuli$data_id), ]
rownames(df_stimuli) <- 1:NUM_STIMULI

if(!all(sort(unique(df_stimuli$data_id)) == (1:NUM_STIMULI))) {
  fprintf("Data id is not valid");
  return()
}

###### organize Google form data ######
df_data <- data.frame(participant_id = as.numeric(), data_id = as.numeric(), order = as.character(),
                      answer = as.numeric(), score = as.numeric(),
                      instrument = as.character(), domain = as.character(), varcond = as.character(), competitoin = as.character())

NUM_PARTICIPANT <- nrow(T)
id_i <- vector(mode = "numeric", length = NUM_DATA)
order_i <- vector(mode = "character", length = NUM_DATA)

for(i in 1:NUM_PARTICIPANT) {
  ### vectorize the stimuli presentation order ###
  counter <- 0
  s_a <- strsplit(T[i, COLNUM_DATAID], "a")[[1]]
  
  for(j in 1:length(s_a)) {
    if(grepl("b", s_a[j], fixed = TRUE)) {
      s_b <- strsplit(s_a[j], "b")[[1]]
      
      for (k in 1:length(s_b)) {
        counter <- counter + 1
        id_i[counter] <- as.numeric(s_b[k])
        order_i[counter] <- "b"
      }
      
      order_i[counter] <- "a"
    } else {
      counter <- counter + 1
      id_i[counter] <- as.numeric(s_a[j])
      order_i[counter] <- "a"
    }
  }
  
  order_i[counter] <- substring(T[i, COLNUM_DATAID], nchar(T[i, COLNUM_DATAID]))
  
  if(!(paste(paste(as.character(id_i), order_i, sep = ""), collapse = "") == T[i, COLNUM_DATAID])) {
    cat(paste("Vectorization is not valid: i = ", i, sep = ""));
    return()
  }
  
  ### decompose ###
  answer_i <- T[i, 3:(3 + NUM_DATA - 1)]
  colnames(answer_i) <- NULL
  answer_i <- sapply(answer_i, function(x) {return(str_replace(x, "一人目の演奏", "1"))})
  answer_i <- sapply(answer_i, function(x) {return(str_replace(x, "二人目の演奏", "2"))})
  
  df_i <- data.frame(participant_id = i, data_id = id_i, order = order_i, answer = as.numeric(answer_i), score = 0,
                     instrument = "", domain = "", varcond = "", competition = "")
  df_i[df_i$order == "a" & df_i$answer == 1, ]$score <- 1
  df_i[df_i$order == "b" & df_i$answer == 2, ]$score <- 1
  df_i$instrument <- df_stimuli$instrument[df_i$data_id]
  df_i$domain <- df_stimuli$domain[df_i$data_id]
  df_i$varcond <- df_stimuli$varcond[df_i$data_id]
  df_i$competition <- df_stimuli$competition[df_i$data_id]
  
  df_data <- rbind(df_data, df_i)
}

for (j in 1:NUM_STIMULI) {
  if(!all(df_stimuli$domain[df_stimuli$data_id == j] == df_data$domain[df_data$data_id == j]) ||
     !all(df_stimuli$varcond[df_stimuli$data_id == j] == df_data$varcond[df_data$data_id == j]) ||
     !all(df_stimuli$instrument[df_stimuli$data_id == j] == df_data$instrument[df_data$data_id == j]) ||
     !all(df_stimuli$competition[df_stimuli$data_id == j] == df_data$competition[df_data$data_id == j])) {
    print("df_data data frame is not valid");
    return()
  }
}

###### output ######
write.csv(df_data, file = paste("./data/", OUTPUT_FILENAME, sep = ""), row.names = FALSE)