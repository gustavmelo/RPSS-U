############# Packages ################################################

{
  if (!require("dplyr")) install.packages("dplyr", dependencies=TRUE)
  if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
  if (!require("scales")) install.packages("scales", dependencies=TRUE)
}

#### Read data ##########################################################

root_dir       <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  args <- commandArgs(trailingOnly = FALSE)
  dirname(normalizePath(sub("--file=", "", args[grep("--file=", args)][1]), winslash = "/"))
}
data_dir       <- file.path(root_dir, "00_Data", "Processed")
simulation_dir <- file.path(root_dir, "02_Simulation_files_output")
indicator_dir  <- file.path(root_dir, "03_Indicator_files_output")

all_fut_scenarios<-readRDS(file.path(simulation_dir, "Future_scenarios.RData"))
sdg_criteria<-read.csv(file.path(data_dir, "SDG_Criteria.csv"), check.names = FALSE)


########################## Indicator ##############################

number_scenarios<-1000 # Number of scenarios previously simulated
horizon<-12 # Scenario horizon previously simulated

###### SDG + wind version ######

critic_weights<-list()
critic_mabac_results<-list()
entropy_weights<-list()
entropy_mabac_results<-list()
critic_topsis_results<-list()

for (i in 1:number_scenarios) {

    wind_scenario <- data.frame(sapply(all_fut_scenarios, function(df) df[(((i - 1)*horizon)+1):(i*horizon), 4]))
    colnames(wind_scenario) <- sub("^X", "", colnames(wind_scenario))

    wind_scenario <- as.data.frame(t(wind_scenario))
    colnames(wind_scenario) <- paste0("wind_", 1:horizon)
    wind_scenario$Code <- as.numeric(rownames(wind_scenario))

    case <- merge(sdg_criteria, wind_scenario, by = "Code", all.x = TRUE)

    case <- case %>%
      mutate(across(3:21, as.numeric))

    ########################## CRITIC ##############################

    # Step 1: Select the criteria columns (columns 3 to 21)
    criteria <- case[, 3:21]

    # Step 2: Identify benefit-based and cost-based criteria
    benefit_indices <- c(3, 10:21) - 2
    cost_indices <- setdiff(1:ncol(criteria), benefit_indices)

    # Step 3: Normalize the criteria based on the type
    normalize <- function(x, type) {
      if (type == "benefit") {
        return((x - min(x, na.rm = TRUE)) / (max(x, na.rm = TRUE) - min(x, na.rm = TRUE)))
      } else { # cost-based
        return((x - max(x, na.rm = TRUE)) / (min(x, na.rm = TRUE) - max(x, na.rm = TRUE)))
      }
    }

    normalized_criteria <- as.data.frame(sapply(1:ncol(criteria), function(j) {
      if (j %in% benefit_indices) {
        normalize(criteria[[j]], "benefit")
      } else {
        normalize(criteria[[j]], "cost")
      }
    }))

    # Step 4: Compute standard deviation for each criterion
    std_dev <- apply(normalized_criteria, 2, sd, na.rm = TRUE)

    # Step 5: Compute Spearman correlation matrix
    cor_matrix <- cor(normalized_criteria, method = "spearman", use = "complete.obs")

    # Step 6: Compute information amount for each criterion
    info_amount <- std_dev * rowSums(1 - cor_matrix, na.rm = TRUE)

    # Step 7: Compute the weights
    weights <- info_amount / sum(info_amount)

    # Step 8: Create a dataframe with criteria names and their corresponding weights
    critic_weights[[i]] <- data.frame(Criteria = colnames(criteria), Weight = weights)


    ########################## MABAC ##############################

    # Step 1: Use the normalized decision matrix from the CRITIC method

    # Step 2: Compute the weighted matrix
    weighted_matrix <- as.data.frame(sapply(1:ncol(normalized_criteria), function(k) {
      (normalized_criteria[[k]] + 1) * critic_weights[[i]]$Weight[k]
    }))

    # Step 3: Compute the border approximation area for each criterion
    gj <- apply(weighted_matrix, 2, function(x) prod(x)^(1/nrow(weighted_matrix)))

    # Step 4: Compute the distance from each alternative to the border approximation area
    q_matrix <- as.data.frame(sapply(1:ncol(weighted_matrix), function(f) {
      weighted_matrix[[f]] - gj[f]
    }))
    colnames(q_matrix) <- colnames(weighted_matrix)

    # Step 5: Compute the final MABAC scores for each alternative
    final_scores <- rowSums(q_matrix)

    # Step 6: Create a final dataframe with municipalities and their MABAC scores
    critic_mabac_results[[i]] <- data.frame(Municipality = case$Municipality, Code = case$Code, Score = final_scores)

    ########################## ENTROPY ##############################

    # Step 1: Use the normalized decision matrix from the CRITIC method
    # (normalized_criteria is reused from the previous step)

    # Step 2: Compute the proportion matrix (each value divided by its column sum)
    prop_matrix <- as.data.frame(sapply(1:ncol(normalized_criteria), function(j) {
      normalized_criteria[[j]] / sum(normalized_criteria[[j]], na.rm = TRUE)
    }))

    # Step 3: Compute the entropy value for each criterion
    entropy_value <- sapply(1:ncol(prop_matrix), function(j) {
      p <- prop_matrix[[j]]
      plogp <- ifelse(p == 0, 0, p * log(p))
      -sum(plogp, na.rm = TRUE) / log(nrow(prop_matrix))
    })

    # Step 4: Compute the degree of diversification of each criterion
    diversification <- 1 - entropy_value

    # Step 5: Compute the weights
    entropy_w <- diversification / sum(diversification)

    # Step 6: Create a dataframe with criteria names and their corresponding weights
    entropy_weights[[i]] <- data.frame(Criteria = colnames(criteria), Weight = entropy_w)


    ########################## MABAC (Entropy weights) ##############################

    # Step 1: Use the normalized decision matrix from the CRITIC method
    # (normalized_criteria is reused; weights come from the Entropy method instead)

    # Step 2: Compute the weighted matrix
    weighted_matrix_entropy <- as.data.frame(sapply(1:ncol(normalized_criteria), function(k) {
      (normalized_criteria[[k]] + 1) * entropy_weights[[i]]$Weight[k]
    }))

    # Step 3: Compute the border approximation area for each criterion
    gj_entropy <- apply(weighted_matrix_entropy, 2, function(x) prod(x)^(1/nrow(weighted_matrix_entropy)))

    # Step 4: Compute the distance from each alternative to the border approximation area
    q_matrix_entropy <- as.data.frame(sapply(1:ncol(weighted_matrix_entropy), function(f) {
      weighted_matrix_entropy[[f]] - gj_entropy[f]
    }))
    colnames(q_matrix_entropy) <- colnames(weighted_matrix_entropy)

    # Step 5: Compute the final MABAC scores for each alternative
    final_scores_entropy <- rowSums(q_matrix_entropy)

    # Step 6: Create a final dataframe with municipalities and their MABAC scores
    entropy_mabac_results[[i]] <- data.frame(Municipality = case$Municipality, Code = case$Code, Score = final_scores_entropy)

    ########################## TOPSIS ##############################

    # Weighted normalized matrix (without the +1 offset used in MABAC)
    weighted_topsis <- as.data.frame(sapply(1:ncol(normalized_criteria), function(k) {
      normalized_criteria[[k]] * critic_weights[[i]]$Weight[k]
    }))

    # Positive Ideal Solution (PIS) and Negative Ideal Solution (NIS)
    pis <- apply(weighted_topsis, 2, max)
    nis <- apply(weighted_topsis, 2, min)

    # Euclidean distances to PIS and NIS
    d_plus  <- apply(weighted_topsis, 1, function(x) sqrt(sum((x - pis)^2)))
    d_minus <- apply(weighted_topsis, 1, function(x) sqrt(sum((x - nis)^2)))

    # Relative closeness score (higher = better)
    C_score <- d_minus / (d_plus + d_minus)

    critic_topsis_results[[i]] <- data.frame(Municipality = case$Municipality, Code = case$Code, Score = C_score)

}

## Saving indicator files ##

{
  saveRDS(critic_weights,          file=file.path(indicator_dir, "CRITIC_MABAC", "CRITIC_weights.RData"))
  saveRDS(critic_mabac_results,    file=file.path(indicator_dir, "CRITIC_MABAC", "CRITIC_MABAC_ranking.RData"))
  saveRDS(entropy_weights,         file=file.path(indicator_dir, "Entropy_MABAC", "Entropy_weights.RData"))
  saveRDS(entropy_mabac_results,   file=file.path(indicator_dir, "Entropy_MABAC", "Entropy_MABAC_ranking.RData"))
  saveRDS(critic_topsis_results,   file=file.path(indicator_dir, "CRITIC_TOPSIS", "CRITIC_TOPSIS_ranking.RData"))
}


########### Analysis #######################################################

### SDGs - Distribution

rename_dict <- c(
  "Percentage of population living with at most 1/2 minimum wage (%)" = "g1: Low_Income_Pop (%)",
  "Employed population (%)" = "g5: Employment (%)",
  "Natural forests (%)" = "g6: Natural_Forest (%)",
  "Households with access to electric energy (%)" = "g4: Elec_Access (%)",
  "Planted or destined to harvest area (%)" = "g2: Harvest_Area (%)",
  "Maximum capacity of reservoirs (m^3)" = "g3: Reservoir_Cap (m³)",
  "Nature conservation unit demarcated area (%)" = "g7: Protected_Area (%)"
)

sdg_criteria_2 <- sdg_criteria
colnames(sdg_criteria_2)[(ncol(sdg_criteria_2)-6):ncol(sdg_criteria_2)] <- rename_dict

criteria_cols <- tail(names(sdg_criteria_2), 7)

ordered_criteria <- c(
  "g1: Low_Income_Pop (%)",
  "g2: Harvest_Area (%)",
  "g3: Reservoir_Cap (m³)",
  "g4: Elec_Access (%)",
  "g5: Employment (%)",
  "g6: Natural_Forest (%)",
  "g7: Protected_Area (%)"
)

png(file.path(data_dir, "Combined_histograms.png"), width = 1200, height = 600)

par(mfrow = c(2, 4))

for (crit in ordered_criteria) {
  hist(sdg_criteria_2[[crit]],
       main = crit,
       xlab = "",
       ylab = "Frequency",
       col = "gray70",
       breaks = 20,
       cex.main = 2.5,
       cex.axis = 1.6,
       cex.lab = 1.8)
}

dev.off()

### Weights analysis


critic_weights<-readRDS(file.path(indicator_dir, "CRITIC_MABAC", "CRITIC_weights.RData"))
critic_mabac_results<-readRDS(file.path(indicator_dir, "CRITIC_MABAC", "CRITIC_MABAC_ranking.RData"))

wind_weight<-vector()

for (p in 1:1000) {
  wind_weight[p]<-sum(critic_weights[[p]][c(8:19),2])
}

summary(wind_weight)

### Weights distribution

rename_dict <- c(
  "Percentage of population living with at most 1/2 minimum wage (%)" = "g1: Low_Income_Pop",
  "Employed population (%)" = "g5: Employment",
  "Natural forests (%)" = "g6: Natural_Forest",
  "Households with access to electric energy (%)" = "g4: Elec_Access",
  "Planted or destined to harvest area (%)" = "g2: Harvest_Area",
  "Maximum capacity of reservoirs (m^3)" = "g3: Reservoir_Cap",
  "Nature conservation unit demarcated area (%)" = "g7: Protected_Area",
  "wind_1" = "g8: Wind_Jan",
  "wind_2" = "g9: Wind_Feb",
  "wind_3" = "g10: Wind_Mar",
  "wind_4" = "g11: Wind_Apr",
  "wind_5" = "g12: Wind_May",
  "wind_6" = "g13: Wind_Jun",
  "wind_7" = "g14: Wind_Jul",
  "wind_8" = "g15: Wind_Aug",
  "wind_9" = "g16: Wind_Sep",
  "wind_10" = "g17: Wind_Oct",
  "wind_11" = "g18: Wind_Nov",
  "wind_12" = "g19: Wind_Dec"
)

critic_weights_labeled <- lapply(critic_weights, function(df) {
  df$Criteria <- recode(df$Criteria, !!!rename_dict)
  return(df)
})

df_combined <- bind_rows(critic_weights_labeled, .id = "Scenario")

ordered_criteria <- c(
  "g3: Reservoir_Cap",
  "g7: Protected_Area",
  "g2: Harvest_Area",
  "g1: Low_Income_Pop",
  "g5: Employment",
  "g4: Elec_Access",
  "g6: Natural_Forest",
  "g8: Wind_Jan", "g9: Wind_Feb", "g10: Wind_Mar", "g11: Wind_Apr", "g12: Wind_May", "g13: Wind_Jun",
  "g14: Wind_Jul", "g15: Wind_Aug", "g16: Wind_Sep", "g17: Wind_Oct", "g18: Wind_Nov", "g19: Wind_Dec"
)

df_combined$Criteria <- factor(df_combined$Criteria, levels = ordered_criteria)

graphic<-ggplot(df_combined, aes(x = Criteria, y = Weight)) +
  geom_boxplot() +
  theme_minimal() +
  labs(x = "Criteria", y = "Weight", title = "Distribution of Criteria Weights") +
  theme(
    plot.title = element_text(size = 16, hjust = 0),
    axis.text.x = element_text(angle = 55, hjust = 1),
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 14),
    panel.grid = element_blank(),
    axis.line = element_line(color = "gray70", linewidth = 0.6),
    axis.ticks = element_line(color = "gray70", linewidth = 0.6)
  )

filename <- file.path(indicator_dir, "CRITIC_MABAC", "DistributionWeights.png")
ggsave(filename, plot = graphic, width = 7.5, height = 5, dpi = 300)

### Sort each dataframe within the list in descending order of Score

sorted_critic_mabac_results <- lapply(critic_mabac_results, function(df) {
  df[order(df$Score, decreasing = TRUE), ]
})

# Create a dataframe to store the municipalities' positions in each scenario
ranking_matrix <- do.call(cbind, lapply(sorted_critic_mabac_results, function(df) df$Municipality))
ranking_df <- as.data.frame(ranking_matrix)
municipalities <- unique(ranking_df[,1])

# Count how many times each municipality appears in the Top 10
top10_freq <- table(unlist(lapply(sorted_critic_mabac_results, function(df) df$Municipality[1:10])))
top10_freq_df <- as.data.frame(top10_freq) %>%
  rename(Municipality = Var1, Frequency = Freq) %>%
  arrange(desc(Frequency))

top10_freq_df$Frequency<-top10_freq_df$Frequency/number_scenarios

top10_freq_df<-top10_freq_df[c(1:20),]

# Visualize the most frequent municipalities in the Top 10
graphic<-ggplot(top10_freq_df, aes(x = reorder(Municipality, Frequency), y = Frequency)) +
  geom_bar(stat = "identity", fill = "gray70") +
  coord_flip() +
  labs(title = "Frequency of Municipalities in the Top 10",
       x = "Municipality", y = "Relative Frequency") +
  scale_y_continuous(breaks = seq(0, 1, by = 0.1), limits = c(0,1),labels = label_number(accuracy = 0.1)) +
  theme_minimal() +
  theme(
      plot.title = element_text(size = 16, hjust = 0),
      axis.text = element_text(size = 12, colour = "black"),
      axis.title = element_text(size = 14),
      panel.grid = element_blank(),
      axis.line = element_line(color = "gray70", size = 0.6),
      axis.ticks = element_line(color = "gray70", size = 0.6)
    )

filename <- file.path(indicator_dir, "CRITIC_MABAC", "Freq_Top10.png")
ggsave(filename, plot = graphic, width = 9, height = 5, dpi = 300)

# Create a boxplot to visualize the distribution of municipalities' positions
ranking_long <- bind_rows(lapply(seq_along(sorted_critic_mabac_results), function(i) {
  sorted_critic_mabac_results[[i]] %>%
    mutate(Position = row_number(),
           Scenario = i)
}))

# Select the 10 municipalities with the lowest median position (ties broken by the standard deviation)
top_municipalities <- ranking_long %>%
  group_by(Municipality) %>%
  summarise(
    MedianRank = median(Position),
    StdDevRank = sd(Position)
  ) %>%
  arrange(MedianRank, StdDevRank) %>%
  head(10) %>%
  pull(Municipality)

top10_sdgwind <- top_municipalities

filtered_ranking <- ranking_long %>%
  filter(Municipality %in% top_municipalities)

ordered_municipalities <- filtered_ranking %>%
  group_by(Municipality) %>%
  summarise(Median_Position = median(Position),
            SD_Position = sd(Position)) %>%
  arrange(Median_Position, SD_Position) %>%
  pull(Municipality)

filtered_ranking <- filtered_ranking %>%
  mutate(Municipality = factor(Municipality, levels = ordered_municipalities))

# Create the boxplot
graphic<-ggplot(filtered_ranking, aes(x = Municipality, y = Position)) +
  geom_boxplot(width = 0.2, outlier.size = 0.5, alpha = 0.8) +
  coord_flip() +  # Rotate the plot
  theme_minimal() +
  labs(title = "Distribution of Municipality Rankings",
       x = "Municipality",
       y = "Ranking Position") +
  scale_y_continuous(breaks = seq(1, max(filtered_ranking$Position), by = 7),
                     labels = function(x) paste0(x, "º"), limits = c(1,70))+
  scale_x_discrete(limits = rev(levels(factor(filtered_ranking$Municipality))))+
  theme(
    plot.title = element_text(size = 16, hjust = 0),
    axis.text = element_text(size = 12, colour = "black"),
    axis.title = element_text(size = 14),
    panel.grid = element_blank(),
    axis.line = element_line(color = "gray70", size = 0.6),
    axis.ticks = element_line(color = "gray70", size = 0.6)
    )

filename <- file.path(indicator_dir, "CRITIC_MABAC", "DistributionRankings.png")
ggsave(filename, plot = graphic, width = 10, height = 5, dpi = 300)

###### Wind version #####

critic_weights_2<-list()
critic_mabac_results_2<-list()

for (i in 1:number_scenarios) {

  wind_scenario <- data.frame(sapply(all_fut_scenarios, function(df) df[(((i - 1)*horizon)+1):(i*horizon), 4]))
  colnames(wind_scenario) <- sub("^X", "", colnames(wind_scenario))

  wind_scenario <- as.data.frame(t(wind_scenario))
  colnames(wind_scenario) <- paste0("wind_", 1:horizon)
  wind_scenario$Code <- as.numeric(rownames(wind_scenario))

  case <- merge(sdg_criteria, wind_scenario, by = "Code", all.x = TRUE)

  case <- case %>%
    mutate(across(3:21, as.numeric))

  ########################## CRITIC ##############################

  # Step 1: Select the criteria columns
  criteria <- case[, 10:21]

  # Step 2: Identify benefit-based and cost-based criteria
  benefit_indices <- c(1:12)

  # Step 3: Normalize the criteria based on the type

  normalized_criteria <- as.data.frame(sapply(1:ncol(criteria), function(j) {
    if (j %in% benefit_indices) {
      normalize(criteria[[j]], "benefit")
    } else {
      normalize(criteria[[j]], "cost")
    }
  }))

  # Step 4: Compute standard deviation for each criterion
  std_dev <- apply(normalized_criteria, 2, sd, na.rm = TRUE)

  # Step 5: Compute Spearman correlation matrix
  cor_matrix <- cor(normalized_criteria, method = "spearman", use = "complete.obs")

  # Step 6: Compute information amount for each criterion
  info_amount <- std_dev * rowSums(1 - cor_matrix, na.rm = TRUE)

  # Step 7: Compute the weights
  weights <- info_amount / sum(info_amount)

  # Step 8: Create a dataframe with criteria names and their corresponding weights
  critic_weights_2[[i]] <- data.frame(Criteria = colnames(criteria), Weight = weights)


  ########################## MABAC ##############################

  # Step 1: Use the normalized decision matrix from the CRITIC method

  # Step 2: Compute the weighted matrix
  weighted_matrix <- as.data.frame(sapply(1:ncol(normalized_criteria), function(k) {
    (normalized_criteria[[k]] + 1) * critic_weights_2[[i]]$Weight[k]
  }))

  # Step 3: Compute the border approximation area for each criterion
  gj <- apply(weighted_matrix, 2, function(x) prod(x)^(1/nrow(weighted_matrix)))

  # Step 4: Compute the distance from each alternative to the border approximation area
  q_matrix <- as.data.frame(sapply(1:ncol(weighted_matrix), function(f) {
    weighted_matrix[[f]] - gj[f]
  }))
  colnames(q_matrix) <- colnames(weighted_matrix)

  # Step 5: Compute the final MABAC scores for each alternative
  final_scores <- rowSums(q_matrix)

  # Step 6: Create a final dataframe with municipalities and their MABAC scores
  critic_mabac_results_2[[i]] <- data.frame(Municipality = case$Municipality, Code = case$Code, Score = final_scores)

}

## Saving indicator files ##

{
  saveRDS(critic_weights_2, file=file.path(indicator_dir, "CRITIC_MABAC", "CRITIC_weights_2.RData"))
  saveRDS(critic_mabac_results_2, file=file.path(indicator_dir, "CRITIC_MABAC", "CRITIC_MABAC_ranking_2.RData"))
}

########### Analysis #######################################################

critic_weights_2<-readRDS(file.path(indicator_dir, "CRITIC_MABAC", "CRITIC_weights_2.RData"))
critic_mabac_results_2<-readRDS(file.path(indicator_dir, "CRITIC_MABAC", "CRITIC_MABAC_ranking_2.RData"))

#Sort each dataframe within the list in descending order of Score

sorted_critic_mabac_results <- lapply(critic_mabac_results_2, function(df) {
  df[order(df$Score, decreasing = TRUE), ]
})

# Build the distribution of each municipality's position across scenarios
ranking_long <- bind_rows(lapply(seq_along(sorted_critic_mabac_results), function(i) {
  sorted_critic_mabac_results[[i]] %>%
    mutate(Position = row_number(),
           Scenario = i)
}))

# Select the 10 municipalities with the lowest median position (ties broken by the standard deviation)
top_municipalities <- ranking_long %>%
  group_by(Municipality) %>%
  summarise(
    MedianRank = median(Position),
    StdDevRank = sd(Position)
  ) %>%
  arrange(MedianRank, StdDevRank) %>%
  head(10) %>%
  pull(Municipality)

top10_windonly <- top_municipalities

# Counting municipalities present in both top-10 lists
common_municipalities <- intersect(top10_sdgwind, top10_windonly)

print(paste("Number of municipalities in common:", length(common_municipalities)))


###### Seasonal wind version (CRITIC+MABAC, granularity sensitivity) ######

critic_seasonal_weights<-list()
critic_seasonal_mabac_results<-list()

for (i in 1:number_scenarios) {

  wind_scenario <- data.frame(sapply(all_fut_scenarios, function(df) df[(((i - 1)*horizon)+1):(i*horizon), 4]))
  colnames(wind_scenario) <- sub("^X", "", colnames(wind_scenario))
  wind_scenario <- as.data.frame(t(wind_scenario))
  colnames(wind_scenario) <- paste0("wind_", 1:horizon)

  # Step 0: Aggregate the 12 monthly wind values into 4 climatic seasons
  seasonal_wind <- data.frame(
    wind_summer = rowMeans(wind_scenario[, c("wind_12", "wind_1", "wind_2")]),
    wind_autumn = rowMeans(wind_scenario[, c("wind_3", "wind_4", "wind_5")]),
    wind_winter = rowMeans(wind_scenario[, c("wind_6", "wind_7", "wind_8")]),
    wind_spring = rowMeans(wind_scenario[, c("wind_9", "wind_10", "wind_11")])
  )
  seasonal_wind$Code <- as.numeric(rownames(wind_scenario))

  case <- merge(sdg_criteria, seasonal_wind, by = "Code", all.x = TRUE)
  case <- case %>%
    mutate(across(3:13, as.numeric))

  ########################## CRITIC ##############################

  # Step 1: Select the criteria columns (columns 3 to 13)
  criteria <- case[, 3:13]

  # Step 2: Identify benefit-based and cost-based criteria
  benefit_indices <- c(3, 10:13) - 2
  cost_indices <- setdiff(1:ncol(criteria), benefit_indices)

  # Step 3: Normalize the criteria based on the type
  normalized_criteria <- as.data.frame(sapply(1:ncol(criteria), function(j) {
    if (j %in% benefit_indices) {
      normalize(criteria[[j]], "benefit")
    } else {
      normalize(criteria[[j]], "cost")
    }
  }))

  # Step 4: Compute standard deviation for each criterion
  std_dev <- apply(normalized_criteria, 2, sd, na.rm = TRUE)

  # Step 5: Compute Spearman correlation matrix
  cor_matrix <- cor(normalized_criteria, method = "spearman", use = "complete.obs")

  # Step 6: Compute information amount for each criterion
  info_amount <- std_dev * rowSums(1 - cor_matrix, na.rm = TRUE)

  # Step 7: Compute the weights
  weights <- info_amount / sum(info_amount)

  # Step 8: Create a dataframe with criteria names and their corresponding weights
  critic_seasonal_weights[[i]] <- data.frame(Criteria = colnames(criteria), Weight = weights)


  ########################## MABAC ##############################

  # Step 1: Use the normalized decision matrix from the CRITIC method

  # Step 2: Compute the weighted matrix
  weighted_matrix <- as.data.frame(sapply(1:ncol(normalized_criteria), function(k) {
    (normalized_criteria[[k]] + 1) * critic_seasonal_weights[[i]]$Weight[k]
  }))

  # Step 3: Compute the border approximation area for each criterion
  gj <- apply(weighted_matrix, 2, function(x) prod(x)^(1/nrow(weighted_matrix)))

  # Step 4: Compute the distance from each alternative to the border approximation area
  q_matrix <- as.data.frame(sapply(1:ncol(weighted_matrix), function(f) {
    weighted_matrix[[f]] - gj[f]
  }))
  colnames(q_matrix) <- colnames(weighted_matrix)

  # Step 5: Compute the final MABAC scores for each alternative
  final_scores <- rowSums(q_matrix)

  # Step 6: Create a final dataframe with municipalities and their MABAC scores
  critic_seasonal_mabac_results[[i]] <- data.frame(Municipality = case$Municipality, Code = case$Code, Score = final_scores)

}

## Saving indicator files ##

{
  saveRDS(critic_seasonal_weights,       file=file.path(indicator_dir, "CRITIC_MABAC_Seasonal", "CRITIC_weights_Seasonal.RData"))
  saveRDS(critic_seasonal_mabac_results, file=file.path(indicator_dir, "CRITIC_MABAC_Seasonal", "CRITIC_MABAC_ranking_Seasonal.RData"))
}


########### Monthly vs Seasonal sensitivity analysis (CRITIC+MABAC held constant) #######################################################

critic_mabac_results          <- readRDS(file.path(indicator_dir, "CRITIC_MABAC", "CRITIC_MABAC_ranking.RData"))
critic_seasonal_mabac_results <- readRDS(file.path(indicator_dir, "CRITIC_MABAC_Seasonal", "CRITIC_MABAC_ranking_Seasonal.RData"))

# Spearman rank correlation between Monthly and Seasonal wind aggregation rankings for each scenario
spearman_corr_seasonal <- numeric(number_scenarios)

for (i in 1:number_scenarios) {
  monthly_ranked  <- critic_mabac_results[[i]][order(critic_mabac_results[[i]]$Score,  decreasing = TRUE), ]
  seasonal_ranked <- critic_seasonal_mabac_results[[i]][order(critic_seasonal_mabac_results[[i]]$Score, decreasing = TRUE), ]

  monthly_ranked$rank_monthly   <- seq_len(nrow(monthly_ranked))
  seasonal_ranked$rank_seasonal <- seq_len(nrow(seasonal_ranked))

  merged_ranks <- merge(monthly_ranked[, c("Code","rank_monthly")],
                        seasonal_ranked[, c("Code","rank_seasonal")], by = "Code")

  spearman_corr_seasonal[i] <- cor(merged_ranks$rank_monthly, merged_ranks$rank_seasonal, method = "spearman")
}

cat("Spearman rank correlation Monthly vs Seasonal (CRITIC+MABAC):\n")
cat("  Mean   :", round(mean(spearman_corr_seasonal), 4), "\n")
cat("  Median :", round(median(spearman_corr_seasonal), 4), "\n")
cat("  Min    :", round(min(spearman_corr_seasonal), 4), "\n")
cat("  Max    :", round(max(spearman_corr_seasonal), 4), "\n")

saveRDS(spearman_corr_seasonal, file.path(indicator_dir, "Sensitivity_Analysis", "Monthly_Seasonal_spearman.RData"))

# Total wind-group weight: Monthly (12 criteria) vs Seasonal (4 criteria)
wind_weight_monthly  <- sapply(1:number_scenarios, function(p) sum(critic_weights[[p]][c(8:19), 2]))
wind_weight_seasonal <- sapply(1:number_scenarios, function(p) sum(critic_seasonal_weights[[p]][c(8:11), 2]))

cat("\nTotal wind-group weight (Monthly, 12 criteria):\n")
print(summary(wind_weight_monthly))
cat("\nTotal wind-group weight (Seasonal, 4 criteria):\n")
print(summary(wind_weight_seasonal))


########### CRITIC vs Entropy sensitivity analysis (MABAC held constant) #######################################################

critic_mabac_results  <- readRDS(file.path(indicator_dir, "CRITIC_MABAC", "CRITIC_MABAC_ranking.RData"))
entropy_mabac_results <- readRDS(file.path(indicator_dir, "Entropy_MABAC", "Entropy_MABAC_ranking.RData"))

# Spearman rank correlation between CRITIC-weighted and Entropy-weighted MABAC rankings for each scenario
spearman_corr_entropy <- numeric(number_scenarios)

for (i in 1:number_scenarios) {
  critic_ranked  <- critic_mabac_results[[i]][order(critic_mabac_results[[i]]$Score,  decreasing = TRUE), ]
  entropy_ranked <- entropy_mabac_results[[i]][order(entropy_mabac_results[[i]]$Score, decreasing = TRUE), ]

  critic_ranked$rank_critic   <- seq_len(nrow(critic_ranked))
  entropy_ranked$rank_entropy <- seq_len(nrow(entropy_ranked))

  merged_ranks <- merge(critic_ranked[, c("Code","rank_critic")],
                        entropy_ranked[, c("Code","rank_entropy")], by = "Code")

  spearman_corr_entropy[i] <- cor(merged_ranks$rank_critic, merged_ranks$rank_entropy, method = "spearman")
}

cat("Spearman rank correlation CRITIC vs Entropy (MABAC):\n")
cat("  Mean   :", round(mean(spearman_corr_entropy), 4), "\n")
cat("  Median :", round(median(spearman_corr_entropy), 4), "\n")
cat("  Min    :", round(min(spearman_corr_entropy), 4), "\n")
cat("  Max    :", round(max(spearman_corr_entropy), 4), "\n")

saveRDS(spearman_corr_entropy, file.path(indicator_dir, "Sensitivity_Analysis", "CRITIC_Entropy_spearman.RData"))

# Total wind-group weight under Entropy weighting (12 monthly criteria)
wind_weight_entropy <- sapply(1:number_scenarios, function(p) sum(entropy_weights[[p]]$Weight[8:19]))

cat("\nTotal wind-group weight (Entropy weighting):\n")
print(summary(wind_weight_entropy))


########### MABAC vs TOPSIS sensitivity analysis (CRITIC held constant) #######################################################

critic_mabac_results  <- readRDS(file.path(indicator_dir, "CRITIC_MABAC", "CRITIC_MABAC_ranking.RData"))
critic_topsis_results <- readRDS(file.path(indicator_dir, "CRITIC_TOPSIS", "CRITIC_TOPSIS_ranking.RData"))

# Spearman rank correlation between MABAC and TOPSIS rankings for each scenario
spearman_corr <- numeric(number_scenarios)

for (i in 1:number_scenarios) {
  mabac_ranked  <- critic_mabac_results[[i]][order(critic_mabac_results[[i]]$Score,  decreasing = TRUE), ]
  topsis_ranked <- critic_topsis_results[[i]][order(critic_topsis_results[[i]]$Score, decreasing = TRUE), ]

  mabac_ranked$rank_mabac   <- seq_len(nrow(mabac_ranked))
  topsis_ranked$rank_topsis <- seq_len(nrow(topsis_ranked))

  merged_ranks <- merge(mabac_ranked[, c("Code","rank_mabac")],
                        topsis_ranked[, c("Code","rank_topsis")], by = "Code")

  spearman_corr[i] <- cor(merged_ranks$rank_mabac, merged_ranks$rank_topsis, method = "spearman")
}

cat("Spearman rank correlation MABAC vs TOPSIS:\n")
cat("  Mean   :", round(mean(spearman_corr), 4), "\n")
cat("  Median :", round(median(spearman_corr), 4), "\n")
cat("  Min    :", round(min(spearman_corr), 4), "\n")
cat("  Max    :", round(max(spearman_corr), 4), "\n")

saveRDS(spearman_corr, file.path(indicator_dir, "Sensitivity_Analysis", "MABAC_TOPSIS_spearman.RData"))
