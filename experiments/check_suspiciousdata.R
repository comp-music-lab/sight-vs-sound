###### Import libraries ######
library(ggplot2)
library(ggpubr)

###### Configuration ######
G_WID <- 8
G_HEI <- 4.8
INPUT_FILENAME <- "datatable.csv"

###### read data ######
df_data <- read.csv(paste(OUTPUTDIR, INPUT_FILENAME, sep = ""), header = TRUE)
df_data <- df_data[df_data$domain %in% c("Audio-only", "Visual-only"), ]
df_data <- df_data[!(df_data$data_id %in% DATAID_INVALID), ]

###### data formatting by aggregation ######
df_stats <- aggregate(df_data$score, by = list(df_data$participant_id, df_data$instrument, df_data$domain, df_data$varcond), FUN = mean)
names(df_stats) <- c("participant_id", "instrument", "domain", "varcond", "score")

###### check suspicious data ######
ID_SUSPICIOUS <- c(7, 22, 58, 96, 97, 135)
df_suspicious <- df_stats[df_stats$participant_id %in% ID_SUSPICIOUS, ]
df_suspicious <- df_suspicious[order(df_suspicious$participant_id), ]
df_suspicious$participant_id <- as.character(df_suspicious$participant_id)

INSTRUMENT <- unique(df_suspicious$instrument)
g_list <- vector(mode = "list", length = 2)

for (i in 1:2) {
  g <- ggplot() + 
    scale_color_brewer(palette = "Set2")
  g <- g +
    geom_point(data = df_suspicious[df_suspicious$domain == "Audio-only" & df_suspicious$varcond == "low-variance" & df_suspicious$instrument == INSTRUMENT[i], ],
               aes(y = score, x = participant_id, shape = varcond, colour = domain), position = position_nudge(- 0.20)) + 
    geom_point(data = df_suspicious[df_suspicious$domain == "Visual-only" & df_suspicious$varcond == "low-variance" & df_suspicious$instrument == INSTRUMENT[i], ],
               aes(y = score, x = participant_id, shape = varcond, colour = domain), position = position_nudge(- 0.10)) + 
    geom_point(data = df_suspicious[df_suspicious$domain == "Audio-only" & df_suspicious$varcond == "high-variance" & df_suspicious$instrument == INSTRUMENT[i], ],
               aes(y = score, x = participant_id, shape = varcond, colour = domain), position = position_nudge(+ 0.10)) + 
    geom_point(data = df_suspicious[df_suspicious$domain == "Visual-only" & df_suspicious$varcond == "high-variance" & df_suspicious$instrument == INSTRUMENT[i], ],
               aes(y = score, x = participant_id, shape = varcond, colour = domain), position = position_nudge(+ 0.20)) + 
    guides(colour = guide_legend(title = "Domain"), shape = guide_legend(title = "Variance")) + 
    scale_y_continuous(breaks = c(0.0, 0.2, 0.4, 0.6, 0.8, 1.0), limits = c(0, 1))

  if(i == 1) {
    YLABELSTR <- "Percent correct"
  } else if(i == 2) {
    YLABELSTR <- ""
  }
  
  g <- g +
    labs(x = "Participant ID", y = YLABELSTR, title = INSTRUMENT[i]) + 
    theme(plot.title = element_text(hjust = 0.5))
  
  g_list[[i]] <- g
}

g <- ggarrange(plotlist=g_list, ncol=2, nrow=1, common.legend=TRUE, legend="bottom")

ggsave(file = paste(OUTPUTDIR, "suspiciousID.png", sep = ""), plot = g, width = G_WID, height = G_HEI)