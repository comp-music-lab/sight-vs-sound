### Load library ###
library(ggplot2)
library(gridExtra)

### Read data ###
R <- read.csv("./data-relative_matching.csv", header = TRUE)

participant_id <- unique(R[, 1])
domain <- unique(R[, 3])

NUMBER_OF_PARTICIPANT <- length(participant_id)
NUMBER_OF_DOMAIN <- length(domain)

### Data-wise predicted ranking plot ###
data_id <- sort(unique(R[, 2]))
list_g <- vector(mode = "list", length = 3)

gR_1 <- data.frame(V1 = character(), V2 = numeric())
gR_2 <- data.frame(V1 = character(), V2 = numeric())
gR_3 <- data.frame(V1 = character(), V2 = numeric())

for (i in 1:length(data_id)) {
  idx <- R[, 2] == data_id[i]
  
  for (k in 1:3) {
    R_1 <- R[idx & (R[, 4] == k), ]
    R_2 <- R[idx & (R[, 5] == k), ]
    R_3 <- R[idx & (R[, 6] == k), ]
    R_k <- as.data.frame(rbind(cbind(R_1$domain, R_1$rank_pred_1st),
      cbind(R_2$domain, R_2$rank_pred_2nd),
      cbind(R_3$domain, R_3$rank_pred_3rd)))
    R_k[, 2] <- as.numeric(R_k[, 2])
    
    if (k == 1) {
      gR_1 <- rbind(gR_1, R_k)
    } else if (k == 2) {
      gR_2 <- rbind(gR_2, R_k)
    } else if (k == 3) {
      gR_3 <- rbind(gR_3, R_k)
    }
    
    g <- ggplot()
    g <- g + geom_point(data = R_k, aes(x = V1, y = V2, color = V1),
                        position = position_dodge2(width = 0.5, preserve = "total"), alpha = 0.8)
    
    list_g[[k]] <- g + theme(legend.position = "none", axis.title.x = element_blank()) + 
    ylab(paste("Predicted rank for the rank-", k, " performer", sep = "")) + 
    scale_y_reverse(limits = c(3.5, 0.5))
  }
  
  g <- grid.arrange(grobs = list_g, ncol = 3, top = paste("Data ID = ", data_id[i], sep = ""))
  ggsave(paste("./voting_per_data_", data_id[i], ".png", sep = ""), plot = g)
}

####
list_g[[1]] <- ggplot() + geom_point(data = gR_1, aes(x = V1, y = V2, color = V1),
                    position = position_dodge2(width = 0.5, preserve = "total"), alpha = 0.8) + 
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_text(angle = -20)) + 
  ylab("1st-placed performers") + 
  scale_y_reverse(limits = c(3.5, 0.5), breaks = c(3, 2, 1), labels = c("Low", "2nd", "1st")) +
  scale_x_discrete(labels = c("Audio-only", "Audio-Visual", "Visual-only"))
list_g[[2]] <- ggplot() + geom_point(data = gR_2, aes(x = V1, y = V2, color = V1),
                                     position = position_dodge2(width = 0.5, preserve = "total"), alpha = 0.8) + 
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_text(angle = -20)) + 
  ylab("2nd-placed performers") + 
  scale_y_reverse(limits = c(3.5, 0.5), breaks = c(3, 2, 1), labels = c("Low", "2nd", "1st")) +
  scale_x_discrete(labels = c("Audio-only", "Audio-Visual", "Visual-only"))
list_g[[3]] <- ggplot() + geom_point(data = gR_3, aes(x = V1, y = V2, color = V1),
                                     position = position_dodge2(width = 0.5, preserve = "total"), alpha = 0.8) + 
  theme(legend.position = "none", axis.title.x = element_blank(), axis.text.x = element_text(angle = -20)) + 
  ylab("Low-placed performers") + 
  scale_y_reverse(limits = c(3.5, 0.5), breaks = c(3, 2, 1), labels = c("Low", "2nd", "1st")) +
  scale_x_discrete(labels = c("Audio-only", "Audio-Visual", "Visual-only"))

g <- grid.arrange(grobs = list_g, ncol = 3, top = "Predicted rank for each performer")
ggsave("./voting_per_data_ALL.png", plot = g)

gR_1$V3 <- 1
gR_2$V3 <- 2
gR_3$V3 <- 3
gR <- rbind(gR_1, gR_2, gR_3)

cat(paste("Correct choice: ",
          sum(gR[gR$V3 == 1, ]$V2 == 1), ", ",
          sum(gR[gR$V3 == 2, ]$V2 == 2), ", ",
          sum(gR[gR$V3 == 3, ]$V2 == 3), "\n",
          sep = ""))

write.csv(gR, file = "./gR.csv", row.names = FALSE)

### Output data ###
T_df <- data.frame(matrix(NA, nrow = NUMBER_OF_PARTICIPANT*NUMBER_OF_DOMAIN, ncol = 4))
colnames(T_df) <- c("participant_id", "domain", "match_high_var", "match_low_var")

### summarized by participants and domains ###
counter <- 1

for (i in 1:NUMBER_OF_PARTICIPANT) {
  for (j in 1:NUMBER_OF_DOMAIN) {
    idx <- R[, 1] == participant_id[i] & R[, 3] == domain[j]
    T_df[counter, 1] <- participant_id[i]
    T_df[counter, 2] <- domain[j]
    T_df[counter, 3] <- sum(R[idx, 10]) / sum(idx) * 100
    T_df[counter, 4] <- sum(R[idx, 11]) / sum(idx) * 100
    
    counter <- counter + 1
  }
}

### 3-sample test ###
for (i in 1:(NUMBER_OF_DOMAIN - 1)) {
  for (j in (i + 1):NUMBER_OF_DOMAIN) {
    idx_i <- T_df[, 2] == domain[i]
    idx_j <- T_df[, 2] == domain[j]
    
    x <- T_df[idx_i, 3]
    y <- T_df[idx_j, 3]
    pval <- ks.test(x, y)
    cat(paste("2-samples KS test, High-var: ", i, ", ", j, " - ", pval$p.value, "\n", sep = ""))
    
    x <- T_df[idx_i, 4]
    y <- T_df[idx_j, 4]
    pval <- ks.test(x, y)
    cat(paste("2-samples KS test, Low-var : ", i, ", ", j, " - ", pval$p.value, "\n", sep = ""))
  }
}

### plot ###
p <- ggplot(data = T_df, aes(x = domain, y = match_high_var, group = domain, color = domain))
p <- p + geom_violin(trim = TRUE, adjust = 0.5)
p <- p + geom_point(position = position_jitter(w = 0.1, h = 0))
p <- p + stat_summary(mapping = aes(x = domain, y = match_high_var),
                      colour = "black", size = 0.3,
                      fun = median, fun.min = function(z) {quantile(z, 0.25)},
                      fun.max = function(z) {quantile(z, 0.75)})

p <- p + theme(axis.title.x = element_blank(),
               plot.title = element_text(hjust = 0.5))
p <- p + ggtitle("High-variance pair") + ylab("Selecting relative winner (%)")

plot(p)
ggsave('./relative-match_high-var.png', plot = p)

p <- ggplot(data = T_df, aes(x = domain, y = match_low_var, group = domain, color = domain))
p <- p + geom_violin(trim = TRUE, adjust = 0.5)
p <- p + geom_point(position = position_jitter(w = 0.1, h = 0))
p <- p + stat_summary(mapping = aes(x = domain, y = match_high_var),
                      colour = "black", size = 0.3,
                      fun = median, fun.min = function(z) {quantile(z, 0.25)},
                      fun.max = function(z) {quantile(z, 0.75)})

p <- p + theme(axis.title.x = element_blank(),
               plot.title = element_text(hjust = 0.5))
p <- p + ggtitle("Low-variance pair") + ylab("Selecting relative winner (%)")

plot(p)
ggsave('./relative-match_low-var.png', plot = p)

###  ###
print("Correct predictions grouped by the presentation order of the 1st-placed performer")

idx <- R[, 4] == 1
print(c(sum(idx), sum(R[idx, 10]), sum(R[idx, 11])))

idx <- R[, 5] == 1
print(c(sum(idx), sum(R[idx, 10]), sum(R[idx, 11])))

idx <- R[, 6] == 1
print(c(sum(idx), sum(R[idx, 10]), sum(R[idx, 11])))