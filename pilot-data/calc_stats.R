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

for (i in 1:length(data_id)) {
  idx <- R[, 2] == data_id[i]
  
  for (k in 1:3) {
    g <- ggplot()
    g <- g + geom_point(data = R[idx & (R[, 4] == k), ], aes(x = domain, y = rank_pred_1st, color = domain),
                            position = position_jitter(w = 0.1, h = 0))
    g <- g + geom_point(data = R[idx & (R[, 5] == k), ], aes(x = domain, y = rank_pred_2nd, color = domain),
                            position = position_jitter(w = 0.1, h = 0))
    g <- g + geom_point(data = R[idx & (R[, 6] == k), ], aes(x = domain, y = rank_pred_3rd, color = domain),
                            position = position_jitter(w = 0.1, h = 0))
    list_g[[k]] <- g + theme(legend.position = "none", axis.title.x = element_blank()) + 
    ylab(paste("Predicted rank for the rank-", k, " performer", sep = "")) + 
    scale_y_reverse(limits = c(3, 1))
  }
  
  g <- grid.arrange(grobs = list_g, ncol = 3, top = paste("Data ID = ", data_id[i], sep = ""))
  ggsave(paste("./voting_per_data_", data_id[i], ".png", sep = ""), plot = g)
}

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