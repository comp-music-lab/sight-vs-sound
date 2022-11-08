###### Import libraries ######
library(ggplot2)
library(gridExtra)
library(RColorBrewer)

###### Configuration ######
INPUT_FILENAME <- "datatable.csv"
OUTPUT_FILEID <- c("analysis_raw", "analysis_diff", "analysis_compe", "analysis_pair")

G_VIOLIN_ADJUST <- 1.5
G_VIOLIN_SCALE <- "area"
G_TITLE_SIZE <- 10
G_X_SIZE <- 8
G_Y_SIZE <- 8
G_X_ROTATE <- -0
G_WID <- 8
G_HEI <- 4.8

G_COLPALETE <- c("Set2", "Dark2")
G_YL = list(c(0, 1), c(-0.5, 0.5))
G_YLABELSTR = c("Percent correct", "Sight-vs-sound effect")
G_YBASE = c(0.5, 0)
G_XLIMITS = list(c("Audio-only.low-variance",
                   "Visual-only.low-variance",
                   "Audio-Visual.low-variance",
                   "Audio-only.high-variance",
                   "Visual-only.high-variance",
                   "Audio-Visual.high-variance"),
                 c("low-variance", "high-variance"))
G_XLABELS = list(c("Audio-only.low-variance" = "low-var x AO",
                   "Visual-only.low-variance" = "low-var x VO",
                   "Audio-Visual.low-variance" = "low-var x AV",
                   "Audio-only.high-variance" = "high-var x AO",
                   "Visual-only.high-variance" = "high-var x VO",
                   "Audio-Visual.high-variance" = "high-var x AV"),
                 c("low-variance" = "low-var\n(Visual - Audio)",
                   "high-variance" = "high-var\n(Visual - Audio)"))

###### Read data ######
df_data <- read.csv(paste(OUTPUTDIR, INPUT_FILENAME, sep = ""), header = TRUE)
df_data <- df_data[!(df_data$data_id %in% DATAID_INVALID), ]

###### [Figure 1] Data formatting by aggregation ######
df_stats <- aggregate(df_data$score, by = list(df_data$participant_id, df_data$instrument, df_data$domain, df_data$varcond), FUN = mean)

names(df_stats) <- c("participant_id", "Instrument", "Domain", "Variance", "score")

###### [Figure 2] Data formatting by taking a diff ######
temp <- df_data[df_data$domain %in% c("Audio-only", "Visual-only"), ]
temp$score[temp$domain == "Audio-only"] <- -temp$score[temp$domain == "Audio-only"]
df_diff <- aggregate(temp$score, by = list(temp$participant_id, temp$instrument, temp$varcond), FUN = mean)

names(df_diff) <- c("participant_id", "Instrument", "Variance", "score")

###### [Figure 3] Data formatting by summarizing by competitions ######
df_competition <- aggregate(df_data$score, by = list(df_data$competition, df_data$domain, df_data$varcond), FUN = mean)

names(df_competition) <- c("Competition", "Domain", "Variance", "score")
df_competition$Domain <- factor(df_competition$Domain, levels = c("Audio-only", "Visual-only", "Audio-Visual"))

df_competition$linecolor <- "Visual-only"
COMPETITION_LIST <- unique(df_competition$Competition)
for (i in 1:length(COMPETITION_LIST)) {
  AVdiff <- df_competition$score[df_competition$Domain == "Audio-only" & df_competition$Variance == "low-variance" & df_competition$Competition == COMPETITION_LIST[i]] - df_competition$score[df_competition$Domain == "Visual-only" & df_competition$Variance == "low-variance" & df_competition$Competition == COMPETITION_LIST[i]]
  
  if (length(AVdiff) == 1 && AVdiff > 0) {
    df_competition$linecolor[df_competition$Competition == COMPETITION_LIST[i] & df_competition$Variance == "low-variance"] <- "Audio-only"
  }
  
  AVdiff <- df_competition$score[df_competition$Domain == "Audio-only" & df_competition$Variance == "high-variance" & df_competition$Competition == COMPETITION_LIST[i]] - df_competition$score[df_competition$Domain == "Visual-only" & df_competition$Variance == "high-variance" & df_competition$Competition == COMPETITION_LIST[i]]
  
  if (length(AVdiff) == 1 && AVdiff > 0) {
    df_competition$linecolor[df_competition$Competition == COMPETITION_LIST[i] & df_competition$Variance == "high-variance"] <- "Audio-only"
  }
}

###### [Figure 4] Data formatting by summarizing by performer pairs ######
tmp <- df_data
tmp$performer_pair <- paste(tmp$performer_1, ' - ', tmp$performer_2, sep = "")
df_pair <- aggregate(tmp$score, by = list(tmp$data_id, tmp$domain, tmp$varcond, tmp$instrument, tmp$performer_pair), FUN = mean)

names(df_pair) <- c("DataID", "Domain", "Variance", "Instrument", "Pair", "score")
df_pair$clipID <- (((df_pair$DataID - 1)%%25) + 1)

df_pair <- df_pair[order(df_pair$clipID), ]
df_pair <- df_pair[df_pair$clipID %in% intersect(df_pair$clipID[df_pair$Domain == "Audio-only"], df_pair$clipID[df_pair$Domain == "Visual-only"]), ]

df_CLIPORDER <- data.frame(clipID = unique(df_pair[c("clipID")])$clipID,
                           Pair = unique(df_pair[c("Pair", "clipID")])$Pair,
                           AVscorediff = df_pair$score[df_pair$Domain == "Audio-only"] - df_pair$score[df_pair$Domain == "Visual-only"],
                           Instrument = unique(df_pair[c("Instrument", "clipID")])$Instrument
                           )
df_CLIPORDER$plotname <- sapply(
        strsplit(df_CLIPORDER$Pair, " - "), function(x) return(
        paste(sapply(strsplit(x, " "), function(y) return(paste(substring(y[1], 1, 1), ".", substring(y[2], 1, 1), ".", sep = ""))), collapse = "\nvs\n")
      )
    )
df_CLIPORDER$plotname[df_CLIPORDER$Instrument == "Piano"] <- paste(df_CLIPORDER$plotname[df_CLIPORDER$Instrument == "Piano"], "\n(p)", sep="")
df_CLIPORDER$plotname[df_CLIPORDER$Instrument == "Tsugaru shamisen"] <- paste(df_CLIPORDER$plotname[df_CLIPORDER$Instrument == "Tsugaru shamisen"], "\n(t-s)", sep="")
df_CLIPORDER$plotname[df_CLIPORDER$clipID %in% c(21:25)] <- paste(df_CLIPORDER$plotname[df_CLIPORDER$clipID %in% c(21:25)], "*", sep="")

df_CLIPORDER <- df_CLIPORDER[order(df_CLIPORDER$AVscorediff, decreasing = TRUE), ]
df_CLIPORDER <- df_CLIPORDER[order(df_CLIPORDER$Instrument), ]
CLIPID_ORDER <- as.character(df_CLIPORDER$clipID)
CLIPID_PLOTNAME <- df_CLIPORDER$plotname

df_pair$linecolor <- "Visual-only"
for (i in 1:length(CLIPID_ORDER)) {
  AVdiff <- df_pair$score[df_pair$Domain == "Audio-only" & df_pair$clipID == CLIPID_ORDER[i]] - df_pair$score[df_pair$Domain == "Visual-only" & df_pair$clipID == CLIPID_ORDER[i]]
  
  if (AVdiff > 0) {
    df_pair$linecolor[df_pair$clipID == CLIPID_ORDER[i]] <- "Audio-only"
  }
}

df_pair$Domain <- factor(df_pair$Domain, levels = c("Audio-only", "Visual-only", "Audio-Visual"))
df_pair$clipID <- as.character(df_pair$clipID)

###### [Figure 1 & 2] Create ggplot objects ######
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
      geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.02)

    if (j == 1) {
      g_list[[i]] <- g_list[[i]] +
        geom_line(data = df_i[df_i$Domain %in% c("Audio-only", "Visual-only"), ], aes(x = interaction(Domain, Variance), y = score, group = interaction(participant_id, Variance)),
                  color = "black", alpha = 0.25, linetype = "dashed")
    }
    
    # decoration
    g_list[[i]] <- g_list[[i]] +
      scale_color_brewer(palette = G_COLPALETE[j]) + 
      geom_hline(yintercept = G_YBASE[j], linetype = "dotted", color = "grey27") +
      ylim(G_YL[[j]]) + 
      stat_summary(fun = "mean", geom = "point", shape = 23, size = 2., fill = "#DF536B") +
      labs(x = "", y = G_YLABELSTR[j], title = INSTRUMENT[i]) +
      theme(legend.position = "none",
            plot.title = element_text(hjust = 0.5),
            axis.text.x = element_text(size = G_X_SIZE, angle = G_X_ROTATE),
            axis.text.y = element_text(size = G_Y_SIZE),
            axis.title.y = element_text(size = G_Y_SIZE)
            ) + 
      scale_x_discrete(limits = G_XLIMITS[[j]], labels = G_XLABELS[[j]])
  }
  
  ###### Merge ggplot objects ######
  g <- grid.arrange(grobs = g_list, nrow = 2)
  
  ###### Save the figure ######
  ggsave(file = paste(OUTPUTDIR, OUTPUT_FILEID[j], "_ggplots_", FILEID, ".png", sep = ""),
         plot = g, width = G_WID, height = G_HEI)
}

###### [Figure 3 & 4] Create ggplot objects ######
for (j in 1:2) {
  if (j == 1) {
    g <- ggplot() + 
      geom_point(data = df_competition[df_competition$Variance == "high-variance", ],
                 aes(x = Competition, y = score, colour = Domain, shape = Variance),
                 position = position_nudge(- 0.15)) + 
      geom_point(data = df_competition[df_competition$Variance == "low-variance", ],
                 aes(x = Competition, y = score, colour = Domain, shape = Variance),
                 position = position_nudge(+ 0.15)) + 
      geom_line(data = df_competition[df_competition$Variance == "high-variance" & !(df_competition$Domain %in% "Audio-Visual"), ],
                aes(x = Competition, y = score, group = Competition, color = linecolor), linetype = "longdash",
                position = position_nudge(- 0.15)) + 
      geom_line(data = df_competition[df_competition$Variance == "low-variance" & !(df_competition$Domain %in% "Audio-Visual"), ],
                aes(x = Competition, y = score, group = Competition, color = linecolor), linetype = "longdash",
                position = position_nudge(+ 0.15)) 
    
    G_TITLESTR <- "Competition-wise summary"
  }else if (j == 2) {
    g <- ggplot(data = df_pair,
                          aes(x = clipID, y = score, colour = Domain, shape = Variance)) + 
      geom_point() + 
      geom_line(data = df_pair[df_pair$Domain %in% c("Audio-only", "Visual-only"), ],
                  aes(x = clipID, y = score, group = clipID, color = linecolor), linetype = "longdash")
    
    G_TITLESTR <- "Performer pair-wise summary"
  }
  
  g <- g +
    scale_color_brewer(palette = "Set2") +
    ylim(0, 1) +
    labs(x = "", y = "Percent correct", title = G_TITLESTR) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = G_X_SIZE, angle = G_X_ROTATE),
          axis.text.y = element_text(size = G_Y_SIZE),
          axis.title.y = element_text(size = G_Y_SIZE)
    )
  
  if (j == 1) {
    g <- g + 
      scale_x_discrete(
        limits = c("International Franz Liszt Piano Competition",
                   "Van Cliburn International Piano Competition",
                   "San Marino Piano Competition",
                   "Cleveland International Piano Competition",
                   "International Hirosaki Tsugaru shamisen Competition",
                   "International Michinoku Tsugaru shamisen Competition",
                   "International Biwako Tsugaru shamisen Competition"),
        labels = c("Liszt\n(p)", "Van Cliburn\n(p)", "San Marino\n(p)", "Cleveland\n(p)", "Hirosaki\n(t-s)", "Michinoku\n(t-s)", "Biwako\n(t-s)")
      )
  }else if (j == 2) {
    g <- g + 
      scale_x_discrete(limits = CLIPID_ORDER, labels = CLIPID_PLOTNAME)
  }
  
  ggsave(file = paste(OUTPUTDIR, OUTPUT_FILEID[2 + j], "_ggplots_", FILEID, ".png", sep = ""),
         plot = g, width = G_WID, height = G_HEI)
}