###### Import libraries ######
library(ggplot2)
library(gridExtra)
library(RColorBrewer)
library(ggpubr)

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
df_pair <- aggregate(tmp$score, by = list(tmp$data_id, tmp$domain, tmp$varcond, tmp$instrument, tmp$performer_pair,
                                          paste(substr(tmp$performer_1_sex, 1, 1), substr(tmp$performer_2_sex, 1, 1), sep = "")), FUN = mean)

names(df_pair) <- c("DataID", "Domain", "Variance", "Instrument", "Pair", "Gender", "score")
df_pair$clipID <- (((df_pair$DataID - 1)%%25) + 1)
df_pair$Gender <- gsub("F", "♀", gsub("M", "♂", df_pair$Gender))

df_pair <- df_pair[order(df_pair$clipID), ]
df_pair <- df_pair[df_pair$clipID %in% intersect(df_pair$clipID[df_pair$Domain == "Audio-only"], df_pair$clipID[df_pair$Domain == "Visual-only"]), ]

df_CLIPORDER <- data.frame(clipID = unique(df_pair[c("clipID")])$clipID,
                           Pair = unique(df_pair[c("Pair", "clipID")])$Pair,
                           AVscorediff = df_pair$score[df_pair$Domain == "Audio-only"] - df_pair$score[df_pair$Domain == "Visual-only"],
                           Instrument = unique(df_pair[c("Instrument", "clipID")])$Instrument,
                           Gender = unique(df_pair[c("clipID", "Gender")])$Gender
                           )
df_CLIPORDER$plotname <- sapply(
        strsplit(df_CLIPORDER$Pair, " - "), function(x) return(
        paste(sapply(strsplit(x, " "), function(y) return(paste(substring(y[1], 1, 1), ".", substring(y[2], 1, 1), ".", sep = ""))), collapse = "\nvs\n")
      )
    )
df_CLIPORDER$plotname <- paste(df_CLIPORDER$plotname, "\n", df_CLIPORDER$Gender, sep = "")
df_CLIPORDER$plotname[df_CLIPORDER$clipID %in% c(21:25)] <- paste(df_CLIPORDER$plotname[df_CLIPORDER$clipID %in% c(21:25)], "\n*", sep="")

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

###### [Figure 1] Create ggplot objects ######
INSTRUMENT <- sort(unique(df_stats$Instrument))
VARCOND <- sort(unique(df_stats$Variance), decreasing = TRUE)
g_list <- vector(mode = "list", length = length(INSTRUMENT)*length(VARCOND))

k <- 0
for (i in 1:length(INSTRUMENT)) {
  for (j in 1:length(VARCOND)) {
    k <- k + 1
    df_k <- df_stats[df_stats$Instrument == INSTRUMENT[i] & df_stats$Variance == VARCOND[j], ]
    df_k$score <- df_k$score*100
      
    g_list[[k]] <- ggplot(data = df_k,
                          aes(x = Domain, y = score, group = Domain, colour = Domain)) +
      geom_violin(adjust = G_VIOLIN_ADJUST, trim = TRUE, scale = G_VIOLIN_SCALE) +
      geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 0.5, show.legend = FALSE) +
      geom_line(data = df_k[df_k$Domain %in% c("Audio-only", "Visual-only"), ],
                aes(x = Domain, y = score, group = participant_id),
                color = "black", alpha = 0.25, linetype = "dashed", show.legend = FALSE) +
      geom_hline(yintercept = 0.5*100, linetype = "dotted", color = "grey27", show.legend = FALSE) +
      ylim(c(0, 100)) + 
      stat_summary(fun = "mean", geom = "point", shape = 23, size = 2., fill = "#DF536B", show.legend = FALSE) +
      labs(x = "", y = "") +
      theme(axis.text.x = element_blank()) +
      scale_x_discrete(limits = c("Audio-Visual", "Audio-only", "Visual-only")) + 
      scale_colour_brewer(name = "Domain",
                          palette = "Set2",
                          breaks = c("Audio-Visual", "Audio-only", "Visual-only"),
                          labels = c("Audio + visual\n(exploratory)", "Audio-only", "Visual-only")) 
    
    if(j == 1) {
      g_list[[k]] <- g_list[[k]] + labs(y = "Percent accuracy") +
        theme(axis.title.y = element_text(size = G_Y_SIZE), axis.text.y = element_text(size = G_Y_SIZE))
    } else if(j == length(VARCOND)) {
      ylabeltext <- sub(" ", "\n", INSTRUMENT[i])
      g_list[[k]] <- g_list[[k]] + annotate("text", x = 3.7, y = 0.5*100, label = ylabeltext, size = 4.5, hjust = 0) +
        coord_cartesian(xlim = c(1, 3), clip = "off")     
    }
    
    if(i == 1) {
      titlestr <- VARCOND[j]
      substr(titlestr, 1, 1) <- toupper(substr(titlestr, 1, 1))
      g_list[[k]] <- g_list[[k]] + labs(title = titlestr) + theme(plot.title = element_text(size = 16, hjust = 0.5))
    }
  }
}

g <- ggarrange(plotlist=g_list, ncol=2, nrow=2, common.legend=TRUE, legend="right")
g <- annotate_figure(g, top = text_grob("a", hjust = 0, x = 0, size =  14))

ggsave(file = paste(OUTPUTDIR, "analysis_raw", "_ggplots_", FILEID, ".png", sep = ""),
       plot = g, width = G_WID, height = G_HEI)

###### [Figure 2] Create ggplot objects ######
INSTRUMENT <- sort(unique(df_stats$Instrument))
g_list <- vector(mode = "list", length = length(INSTRUMENT))

for (i in 1:length(INSTRUMENT)) {
  idx <- df_diff$Instrument == INSTRUMENT[i]
  df_i <- df_diff[idx, ]
  df_i$score <- df_i$score*100
  
  # data plot
  g_list[[i]] <- ggplot(data = df_i, aes(x = Variance, y = score, group = Variance, colour = Variance))
  
  g_list[[i]] <- g_list[[i]] +
    geom_violin(adjust = G_VIOLIN_ADJUST, trim = TRUE, scale = G_VIOLIN_SCALE) +
    geom_dotplot(binaxis = "y", stackdir = "center", binwidth = 3.5)
  
  # decoration
  g_list[[i]] <- g_list[[i]] +
    scale_color_brewer(palette = "Dark2") + 
    geom_hline(yintercept = 0, linetype = "dotted", color = "grey27") +
    ylim(c(-0.5*100, 0.5*100)) + 
    stat_summary(fun = "mean", geom = "point", shape = 23, size = 2., fill = "#DF536B") +
    labs(x = "", y = "Sight-vs-sound effect", title = INSTRUMENT[i]) +
    theme(legend.position = "none",
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = G_X_SIZE, angle = G_X_ROTATE),
          axis.text.y = element_text(size = G_Y_SIZE),
          axis.title.y = element_text(size = G_Y_SIZE)
    ) + 
    scale_x_discrete(limits = c("low-variance", "high-variance"),
                     labels = c("Low-variance\n(Visual - Audio)", "High-variance\n(Visual - Audio)"))
}

g <- grid.arrange(grobs = g_list, nrow = 2)

ggsave(file = paste(OUTPUTDIR, OUTPUT_FILEID[2], "_ggplots_", FILEID, ".png", sep = ""),
       plot = g, width = G_WID, height = G_HEI)

###### [Figure 3 & 4] Create ggplot objects ######
df_competition$score <- df_competition$score * 100
df_pair$score <- df_pair$score * 100

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
    ylim(0, 1*100) +
    labs(x = "", y = "Percent accuracy", title = G_TITLESTR) +
    theme(plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = G_X_SIZE, angle = G_X_ROTATE),
          axis.text.y = element_text(size = G_Y_SIZE),
          axis.title.y = element_text(size = G_Y_SIZE)
    )
  
  if (j == 1) {
    textcolor <- c(rep(rgb(128/255, 64/255, 0/255), 4), rep(rgb(153/255, 0/255, 153/255), 3))
    
    g <- g + 
      scale_x_discrete(
        limits = c("International Franz Liszt Piano Competition",
                   "Van Cliburn International Piano Competition",
                   "San Marino Piano Competition",
                   "Cleveland International Piano Competition",
                   "International Hirosaki Tsugaru shamisen Competition",
                   "International Michinoku Tsugaru shamisen Competition",
                   "International Biwako Tsugaru shamisen Competition"),
        labels = c("Liszt", "Van Cliburn", "San Marino", "Cleveland\n*", "Hirosaki", "Michinoku", "Biwako")
      ) + 
      theme(axis.text.x = element_text(colour = textcolor))
  }else if (j == 2) {
    textcolor <- ifelse(df_CLIPORDER$Instrument == "Piano", rgb(128/255, 64/255, 0/255), rgb(153/255, 0/255, 153/255))
    
    g <- g + 
      scale_x_discrete(limits = CLIPID_ORDER, labels = CLIPID_PLOTNAME) + 
      theme(axis.text.x = element_text(colour = textcolor))
  }
  
  ggsave(file = paste(OUTPUTDIR, OUTPUT_FILEID[2 + j], "_ggplots_", FILEID, ".png", sep = ""),
         plot = g, width = G_WID, height = G_HEI)
}