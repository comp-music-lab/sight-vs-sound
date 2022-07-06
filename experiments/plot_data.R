###### Import libraries ######
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

###### Configuration ######
INPUT_FILENAME <- "datatable_20220701.csv"
OUTPUT_FILEID <- c("analysis_20220701_raw", "analysis_20220701_diff")

G_VIOLIN_ADJUST <- 0.6
G_VIOLIN_SCALE <- "area"
G_JITTER_WID <- 0.05
G_TITLE_SIZE <- 10
G_X_SIZE <- 8
G_Y_SIZE <- 8
G_X_ROTATE <- -0
G_WID <- 6
G_HEI <- 4.8

G_COLPALETE <- c("Set1", "Dark2")
G_YL = list(c(0, 1), c(-0.5, 0.5))
G_YBASE = c(0.5, 0)
G_XLIMITS = list(c("Audio-only.low-variance", "Visual-only.low-variance",
                   "Audio-only.high-variance", "Visual-only.high-variance"),
                 c("low-variance", "high-variance"))
G_XLABELS = list(c("Audio-only.low-variance" = "low-var ~ AO",
                   "Visual-only.low-variance" = "low-var ~ VO",
                   "Audio-only.high-variance" = "high-var ~ AO",
                   "Visual-only.high-variance" = "high-var ~ VO"),
                 c("low-variance" = "low-var\n(Visual - Audio)", "high-variance" = "high-var\n(Visual - Audio)"))

###### Read data ######
df_data <- read.csv(paste("./data/", INPUT_FILENAME, sep = ""), header = TRUE)

###### [Figure 1] Data formatting by aggregation ######
df_stats <- aggregate(df_data$score, by = list(df_data$participant_id, df_data$instrument, df_data$domain, df_data$varcond), FUN = mean)

names(df_stats) <- c("participant_id", "Instrument", "Domain", "Variance", "score")

###### [Figure 2] Data formatting by taking a diff ######
temp <- df_data
temp$score[temp$domain == "Audio-only"] <- -temp$score[temp$domain == "Audio-only"]
df_diff <- aggregate(temp$score, by = list(temp$participant_id, temp$instrument, temp$varcond), FUN = mean)

names(df_diff) <- c("participant_id", "Instrument", "Variance", "score")

###### Create ggplot objects ######
df_list = list(df_stats, df_diff)
g_list <- vector(mode = "list", length = 2)
INSTRUMENT <- sort(unique(df_stats$Instrument))

for (j in 1:2) {
  df_j <- df_list[[j]]
  
  for (i in 1:length(INSTRUMENT)) {
    idx <- df_j$Instrument == INSTRUMENT[i]
    df_i <- df_j[idx, ]
    
    # data plot
    if (j == 1) {
      g_list[[i]] <- ggplot(data = df_i, aes(x = interaction(Domain, Variance), y = score, group = interaction(Domain, Variance), colour = Domain))
    }else if (j == 2) {
      g_list[[i]] <- ggplot(data = df_i, aes(x = Variance, y = score, group = Variance, colour = Variance))
    }
    
    g_list[[i]] <- g_list[[i]] +
      geom_violin(adjust = G_VIOLIN_ADJUST, trim = TRUE, scale = G_VIOLIN_SCALE) +
      geom_jitter(width = G_JITTER_WID, height = 0)
    
    if (j == 1) {
      g_list[[i]] <- g_list[[i]] +
        geom_line(data = df_i, aes(x = interaction(Domain, Variance), y = score, group = interaction(participant_id, Variance)),
                  color = "black", alpha = 0.25, linetype = "dashed")
    }
    
    # decoration
    g_list[[i]] <- g_list[[i]] +
      scale_color_brewer(palette = G_COLPALETE[j]) + 
      geom_hline(yintercept = G_YBASE[j], linetype = "dotted", color = "grey27") +
      ylim(G_YL[[j]]) + 
      stat_summary(fun = "mean", geom = "point", shape = 23, size = 2., fill = "black") +
      labs(x = "", y = "Percent correct", title = INSTRUMENT[i]) +
      theme(legend.position = "none", plot.title = element_text(hjust = 0.5), axis.text.x = element_text(size = G_X_SIZE, angle = G_X_ROTATE),
            axis.text.y = element_text(size = G_Y_SIZE), axis.title.y = element_text(size = G_Y_SIZE)) + 
      scale_x_discrete(limits = G_XLIMITS[[j]], labels = G_XLABELS[[j]])
  }
  
  ###### Merge ggplot objects ######
  g <- grid.arrange(grobs = g_list, nrow = 2)
  
  ###### [Figure 1] Save the figure ######
  ggsave(file = paste("./output/", OUTPUT_FILEID[j], "_ggplots.png", sep = ""),
         plot = g, width = G_WID, height = G_HEI)
}