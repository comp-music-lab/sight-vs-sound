rm(list = ls())

###### import libraries ######
library(stringr)

###### configuration ######
NUM_STIMULI <- 72
OUTPUT_FILENAME <- "datatable_pilot.csv"

###### read data ######
T <- read.csv("./data/googleforms.csv", header = TRUE)
S <- read.csv("./data/stimuli.csv", header = TRUE)

###### format stimuli master data ######
df_stimuli <- rbind(unique(data.frame(data_id = as.numeric(sapply(strsplit(substring(S[, 2], 2), "-"), function(x){return(x[[1]][1])})),
                              instrument = S[, 9],
                              varcond = S[, 11],
                              domain = "Audio-only",
                              competition = S[, 5])),
            unique(data.frame(data_id = as.numeric(sapply(strsplit(substring(S[, 3], 2), "-"), function(x){return(x[[1]][1])})),
                              instrument = S[, 9],
                              varcond = S[, 11],
                              domain = "Visual-only",
                              competition = S[, 5])),
            unique(data.frame(data_id = as.numeric(sapply(strsplit(substring(S[, 4], 2), "-"), function(x){return(x[[1]][1])})),
                              instrument = S[, 9],
                              varcond = S[, 11],
                              domain = "Audio-Visual",
                              competition = S[, 5]))
)

df_stimuli <- df_stimuli[order(df_stimuli$data_id), ]
rownames(df_stimuli) <- 1:NUM_STIMULI

if(!all(sort(unique(df_stimuli$data_id)) == (1:NUM_STIMULI))) {
  fprintf("Data id is not valid\n");
  return()
}

###### organize Google form data ######
df_data <- data.frame(participant_id = as.numeric(), data_id = as.numeric(), order = as.character(),
                      answer = as.numeric(), score = as.numeric(),
                      instrument = as.character(), domain = as.character(), varcond = as.character(), competitoin = as.character())

NUM_PARTICIPANT <- nrow(T)
id_i <- vector(mode = "numeric", length = NUM_STIMULI)
order_i <- vector(mode = "character", length = NUM_STIMULI)

for(i in 1:NUM_PARTICIPANT) {
  ### vectorize the stimuli presentation order ###
  counter <- 0
  s_a <- strsplit(T[i, 75], "a")[[1]]
  
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
  
  order_i[counter] <- substring(T[i, 75], nchar(T[i, 75]))
  
  if(!(paste(paste(as.character(id_i), order_i, sep = ""), collapse = "") == T[i, 75])) {
    print("Vectorization is not valid\n");
    return()
  }
  
  ### decompose ###
  answer_i <- T[i, 3:(3 + NUM_STIMULI - 1)]
  colnames(answer_i) <- NULL
  answer_i <- sapply(answer_i, function(x) {return(str_replace(x, "ˆêl–Ú‚Ì‰‰‘t", "1"))})
  answer_i <- sapply(answer_i, function(x) {return(str_replace(x, "“ñl–Ú‚Ì‰‰‘t", "2"))})
  
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

for (i in 1:NUM_STIMULI) {
  if(!all(df_stimuli$domain[df_stimuli$data_id == i] == df_data$domain[df_data$data_id == i]) ||
     !all(df_stimuli$varcond[df_stimuli$data_id == i] == df_data$varcond[df_data$data_id == i]) ||
     !all(df_stimuli$instrument[df_stimuli$data_id == i] == df_data$instrument[df_data$data_id == i]) ||
     !all(df_stimuli$competition[df_stimuli$data_id == i] == df_data$competition[df_data$data_id == i])) {
    print("df_data data frame is not valid\n");
    return()
  }
}

###### output ######
write.csv(df_data, file = paste("./data/", OUTPUT_FILENAME, sep = ""), row.names = FALSE)