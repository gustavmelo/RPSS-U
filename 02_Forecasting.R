############# Packages ################################################

{
  if (!require("dplyr")) install.packages("dplyr", dependencies=TRUE)
  if (!require("tseries")) install.packages("tseries", dependencies=TRUE)
  if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
}

#### Read data ##########################################################

root_dir       <- if (requireNamespace("rstudioapi", quietly = TRUE) && rstudioapi::isAvailable()) {
  dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  args <- commandArgs(trailingOnly = FALSE)
  dirname(normalizePath(sub("--file=", "", args[grep("--file=", args)][1]), winslash = "/"))
}
data_dir       <- file.path(root_dir, "00_Data", "Processed")
modeling_dir   <- file.path(root_dir, "01_Modeling_files_output")
simulation_dir <- file.path(root_dir, "02_Simulation_files_output")

source(file.path(root_dir, "Functions.R")) # Call the functions

tseries<-read.csv(file.path(data_dir, "TimeSeries.csv"), header = TRUE)
colnames(tseries) <- sub("^X", "", colnames(tseries))

#### Modeling (all data) ###############################################################

## Clustering ##

municip<-list()
centroids<-list()
clusters<-list()
freq<-list()

i = 1 

start.time<-Sys.time()

for (mun in 1:(ncol(tseries)-2)) {

  municip[[mun]]<-tseries[,c(1,2,mun+2)]
  municip[[mun]]$cluster<-''
  municip[[mun]]$centroid<-''
  
  for (m in 1:12){
      clustering_result <- cluster_month(municip[[mun]], m) # Calling the function that computes the clusters for each month (m) of each municipality
      centroids[[i]]<-clustering_result[[1]]
      clusters[[i]]<-clustering_result[[2]]
      freq[[i]]<-clustering_result[[3]]
      
      ctr<-data.frame(centroids[[i]]) 
      cl<-data.frame(clusters[[i]])
      
      for (j in 1:nrow(cl)) {
        municip[[mun]]$cluster[which(municip[[mun]]$month == m)][[j]]<-cl[[1]][[j]]
        municip[[mun]]$centroid[which(municip[[mun]]$month == m)][[j]]<-ctr[[1]][[cl[[1]][[j]]]]
      }
      i = i + 1 
  }
}

end.time<-Sys.time()
time.taken<-end.time-start.time
time.taken

## Some clustering checks ##

num_clusters <- lapply(centroids, length)
num_clusters <- unlist(num_clusters)
max(num_clusters)
min(num_clusters)

size_clusters <- unlist(freq)
max(size_clusters)
min(size_clusters)

## Transition matrices ##  

MT_non_accum<-list() 
MT_accum<-list()

for (mun in 1:length(municip)) {
  for (m in 1:12){
      matr<-transition_matrix(mun,m) # Calling the function that computes the transition matrix between the months (m to m+1) for each municipality
      MT_non_accum[[(12*(mun-1)+m)]]<-list(matr[1])
      MT_accum[[(12*(mun-1)+m)]]<-list(matr[2])
  }
}

## Saving modeling files ##

{
  saveRDS(centroids, file=file.path(modeling_dir, "Centroids.RData"))
  saveRDS(clusters, file=file.path(modeling_dir, "Clusters.RData"))
  saveRDS(municip, file=file.path(modeling_dir, "Clustered_series.RData"))
  saveRDS(freq, file=file.path(modeling_dir, "Clusters_freq.RData"))
  saveRDS(MT_non_accum, file=file.path(modeling_dir, "Matrix_non_accum.RData"))
  saveRDS(MT_accum, file=file.path(modeling_dir, "Matrix_accum.RData"))
}

#### Future Scenarios Simulation #########################################################

# 1k scenarios of 12 months for each municipality

set.seed(100) # Important for result reproduction
random <- runif(12000,0,1) # Important for generating correlated scenarios. The same random vector must be used to simulate the scenarios for all municipalities. Used in the first stage.
random_2 <- runif(12000,0,1) #Idem. This vector is used in the second stage. 

fut_scenarios <- data.frame(scenario=integer(),
                            month=integer(),
                            cluster=integer(),
                            wind=double(),
                            random1=double(),
                            random2=double())

all_fut_scenarios<-list()

start.time <- Sys.time()

for (mun in 1:length(municip)) {
  i=1
  case<-as.data.frame(municip[[mun]])
  for(scenario in 1:1000) { # Approximately one hour to simulate 1k scenarios, it depends on the machine. If you want to simulate a different number of scenarios, change the value 1000 and the vectors random and random_2
    for (m in 1:12) {
          if(m==1){ # First month of each scenario
            state_orig<-case$cluster[nrow(case)]
            matrix<-as.data.frame(MT_accum[12*mun]) # Selecting the right matrix
          }else{ # Remaining months
            matrix<-as.data.frame(MT_accum[(12*(mun-1))+m-1])  # Selecting the right matrix
          }
          
          # 1st stage, calling the function that simulates the next state or cluster
          cluster_simulation<-simulat_state(state_orig, matrix, random[i]) 
          
          # 2nd stage, calling the function that samples a value belonging to the cluster 
          values_cluster<-case %>% filter(month == m & cluster == cluster_simulation)
          final_simulation<-simulat_value(values_cluster,random_2[i])
          final_simulation <- as.numeric(final_simulation)
          
          fut_scenarios[i,1]<-scenario
          fut_scenarios[i,2]<-m
          fut_scenarios[i,3]<-cluster_simulation
          fut_scenarios[i,4]<-final_simulation
          fut_scenarios[i,5]<-random[i]
          fut_scenarios[i,6]<-random_2[i]
          
          state_orig<-cluster_simulation
          i<-i+1
    }
  }
  all_fut_scenarios[[mun]]<-fut_scenarios
  names(all_fut_scenarios)[mun] <- colnames(case)[3]
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

## Saving simulation files ##

{
  saveRDS(random, file=file.path(simulation_dir, "Random_vector_stage1.RData"))
  saveRDS(random_2, file=file.path(simulation_dir, "Random_vector_stage2.RData"))
  saveRDS(all_fut_scenarios, file=file.path(simulation_dir, "Future_scenarios.RData"))
}

#### Analysis #########################################################

######## Wind speed historical data

tseries<-read.csv(file.path(data_dir, "TimeSeries.csv"), header = TRUE)
colnames(tseries) <- sub("^X", "", colnames(tseries))

### Boxplots

municip<-c("2414407","2406502","2400505") # code 
names<-c("Touros", "Lagoa Nova", "Alexandria")

for (i in 1:length(municip)) {
  
  data<-tseries[,c(2, which(colnames(tseries) == municip[i]))] # tseries was loaded at the beginning 
  colnames(data)[2]<-"wind"
  
  mean_wind <- mean(data$wind)
  
  graphic <- ggplot(data, aes(x = factor(month), y = wind)) +
    geom_boxplot() +
    geom_hline(aes(yintercept = mean_wind, color = "Total average"), 
               linetype = "dashed", size = 0.8) +  
    theme_minimal() + 
    labs(x = "Month", y = "Wind Speed (m/s)", color = "Legend") +
    scale_y_continuous(breaks = seq(3, 12, by = 1), limits = c(3, 12)) +
    scale_color_manual(values = c("Total average" = "darkgreen")) +  
    labs(title = names[i])+
    theme(
      plot.title = element_text(size = 22, hjust = 0),
      panel.grid = element_blank(),         
      axis.line = element_line(color = "gray60", size = 0.8),
      axis.text = element_text(size = 18, colour = "black"),   
      axis.title = element_text(size = 20),  
      axis.ticks = element_line(color = "gray60", size = 0.8),
      legend.title = element_blank(), 
      legend.text = element_text(size = 12),
      legend.position = "none"
    )
  
  filename <- file.path(data_dir, paste0(gsub(" ", "_", names[i]), "_box.png"))
  ggsave(filename, plot = graphic, width = 6.5, height = 4, dpi = 300)

}

### Dickey-Fuller Test

num_stationary <- 0

for (i in 3:ncol(tseries)) {  
  result <- adf.test(tseries[[i]])  

  if (result$p.value < 0.05) {
    num_stationary <- num_stationary + 1  
  }
}

num_stationary

### Municipalities mean and sd

means <- colMeans(tseries[, (ncol(tseries)-166):ncol(tseries)])
sds <- apply(tseries[, (ncol(tseries)-166):ncol(tseries)], 2, sd) 

min(means)
max(means)
min(sds)
max(sds)

######## Clusters and matrices

{
  centroids<-readRDS(file.path(modeling_dir, "Centroids.RData"))
  clusters<-readRDS(file.path(modeling_dir, "Clusters.RData"))
  municip<-readRDS(file.path(modeling_dir, "Clustered_series.RData"))
  freq<-readRDS(file.path(modeling_dir, "Clusters_freq.RData"))
  MT_non_accum<-readRDS(file.path(modeling_dir, "Matrix_non_accum.RData"))
  MT_accum<-readRDS(file.path(modeling_dir, "Matrix_accum.RData"))
}

### Example - Touros

centroids[[1902]] # June centroids 
centroids[[1903]] # July centroids
MT_non_accum[[1902]] # Transition matrix (non accumulated) from June to July 
MT_accum[[1902]] # Accumulated

######## Scenarios

### Bar chart of state frequencies

all_fut_scenarios<-readRDS(file.path(simulation_dir, "Future_scenarios.RData"))

municip<-c("2414407","2406502","2400505") # code 
names<-c("Touros", "Lagoa Nova", "Alexandria")
place <- 12*(match(municip, colnames(tseries)[-(1:2)]) - 1) + 7 # July of each municipality

for (i in 1:length(names)) {
  historical<-clusters[[place[i]]]
  scenario<-all_fut_scenarios[[municip[i]]]
  scenario<-scenario %>% filter(month == 7)
  scenario<-scenario$cluster
  
  historical_freq <- data.frame(
    cluster = factor(1:max(historical)),
    frequency = sapply(1:max(historical), function(x) sum(historical == x) / length(historical)),
    type = "Historical"
  )
  
  scenario_freq <- data.frame(
    cluster = factor(1:max(scenario)),
    frequency = sapply(1:max(scenario), function(x) sum(scenario == x) / length(scenario)),
    type = "Forecast"
  )
  
  data <- rbind(historical_freq, scenario_freq)
  
  data$type <- factor(data$type, levels = c("Historical", "Forecast"))
  
  graphic<-ggplot(data, aes(x = cluster, y = frequency, fill = type)) +
    geom_bar(stat = "identity", position = position_dodge(width = 0.84), width = 0.84) +
    scale_fill_manual(values = c("Historical" = "darkgreen", "Forecast" = "grey60")) +
    labs(title = names[i], x = "Cluster", y = "Relative Frequency") +
    scale_y_continuous(breaks = seq(0, 0.5, by = 0.1), limits = c(0, 0.5)) +
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, hjust = 0),
      legend.position = "bottom",
      legend.title = element_blank(),  
      legend.text = element_text(size = 20), 
      panel.grid = element_blank(),         
      axis.line = element_line(color = "gray60", size = 0.8),
      axis.text = element_text(size = 18, colour = "black"),   
      axis.title = element_text(size = 20),  
      axis.ticks = element_line(color = "gray60", size = 0.8)
    )
  
  filename <- file.path(simulation_dir, paste0(gsub(" ", "_", names[i]), "_Freq.png"))
  ggsave(filename, plot = graphic, width = 6, height = 4.5, dpi = 300)

}

### Kolmogorov-Smirnov test

alpha <- 0.05
equal_distributions <- 0

for (municipality in names(all_fut_scenarios)) {
  for (m in 1:12) {
    hist_data <- tseries %>% filter(month == m) %>% pull(as.character(municipality))
    sim_data <- all_fut_scenarios[[municipality]] %>% filter(month == m) %>% pull(wind)
    
    if (length(hist_data) > 0 & length(sim_data) > 0) {  
      ks_test <- ks.test(hist_data, sim_data)
      if (ks_test$p.value > alpha) {
        equal_distributions <- equal_distributions + 1
      }
    }
  }
}

equal_distributions

### Spatial correlation analysis

# Historical Spearman correlation matrix (480 months x 167 municipalities)
tseries_mat <- as.matrix(tseries[, 3:ncol(tseries)])
rho_hist <- cor(tseries_mat, method = "spearman")

# Simulated Spearman correlation matrix (12000 obs x 167 municipalities)
mun_names <- names(all_fut_scenarios)
sim_mat <- matrix(NA, nrow = 12000, ncol = length(mun_names))
colnames(sim_mat) <- mun_names
for (j in seq_along(mun_names)) {
  sim_mat[, j] <- all_fut_scenarios[[mun_names[j]]]$wind
}
rho_sim <- cor(sim_mat, method = "spearman")

# MAE between upper-triangle elements
mask <- upper.tri(rho_hist)
mae_corr <- mean(abs(rho_hist[mask] - rho_sim[mask]))
cat("MAE (historical vs simulated pairwise correlations):", round(mae_corr, 4), "\n")

saveRDS(rho_hist, file.path(simulation_dir, "Corr_hist.RData"))
saveRDS(rho_sim,  file.path(simulation_dir, "Corr_sim.RData"))

#### Modeling (without last year) ###############################################################

validation <- tail(tseries, 12)
tseries_2<- head(tseries, nrow(tseries) - 12) 


## Clustering ##

municip<-list()
centroids<-list()
clusters<-list()
freq<-list()

i = 1 

start.time<-Sys.time()

for (mun in 1:(ncol(tseries_2)-2)) {
  
  municip[[mun]]<-tseries_2[,c(1,2,mun+2)]
  municip[[mun]]$cluster<-''
  municip[[mun]]$centroid<-''
  
  for (m in 1:12){
    clustering_result <- cluster_month(municip[[mun]], m) # Calling the function that computes the clusters for each month (m) of each municipality
    centroids[[i]]<-clustering_result[[1]]
    clusters[[i]]<-clustering_result[[2]]
    freq[[i]]<-clustering_result[[3]]
    
    ctr<-data.frame(centroids[[i]]) 
    cl<-data.frame(clusters[[i]])
    
    for (j in 1:nrow(cl)) {
      municip[[mun]]$cluster[which(municip[[mun]]$month == m)][[j]]<-cl[[1]][[j]]
      municip[[mun]]$centroid[which(municip[[mun]]$month == m)][[j]]<-ctr[[1]][[cl[[1]][[j]]]]
    }
    i = i + 1 
  }
}

end.time<-Sys.time()
time.taken<-end.time-start.time
time.taken

## Some clustering checks ##

num_clusters <- lapply(centroids, length)
num_clusters <- unlist(num_clusters)
max(num_clusters)
min(num_clusters)

size_clusters <- unlist(freq)
max(size_clusters)
min(size_clusters)

## Transition matrices ##  

MT_non_accum<-list() 
MT_accum<-list()

for (mun in 1:length(municip)) {
  for (m in 1:12){
    matr<-transition_matrix(mun,m) # Calling the function that computes the transition matrix between the months (m to m+1) for each municipality
    MT_non_accum[[(12*(mun-1)+m)]]<-list(matr[1])
    MT_accum[[(12*(mun-1)+m)]]<-list(matr[2])
  }
}

## Saving modeling files ##

{
  saveRDS(centroids, file=file.path(modeling_dir, "Without_last_year", "Centroids_2.RData"))
  saveRDS(clusters, file=file.path(modeling_dir, "Without_last_year", "Clusters_2.RData"))
  saveRDS(municip, file=file.path(modeling_dir, "Without_last_year", "Clustered_series_2.RData"))
  saveRDS(freq, file=file.path(modeling_dir, "Without_last_year", "Clusters_freq_2.RData"))
  saveRDS(MT_non_accum, file=file.path(modeling_dir, "Without_last_year", "Matrix_non_accum_2.RData"))
  saveRDS(MT_accum, file=file.path(modeling_dir, "Without_last_year", "Matrix_accum_2.RData"))
}

#### Future Scenarios Simulation (model without last year) #########################################################

# 1k scenarios of 12 months for each municipality

set.seed(100) # Important for result reproduction
random <- runif(12000,0,1) # Important for generating correlated scenarios. The same random vector must be used to simulate the scenarios for all municipalities. Used in the first stage.
random_2 <- runif(12000,0,1) #Idem. This vector is used in the second stage. 

fut_scenarios <- data.frame(scenario=integer(),
                            month=integer(),
                            cluster=integer(),
                            wind=double(),
                            random1=double(),
                            random2=double())

all_fut_scenarios_2<-list()

start.time <- Sys.time()

for (mun in 1:length(municip)) {
  i=1
  case<-as.data.frame(municip[[mun]])
  for(scenario in 1:1000) { # Approximately one hour to simulate 1k scenarios, it depends on the machine. If you want to simulate a different number of scenarios, change the value 1000 and the vectors random and random_2
    for (m in 1:12) {
      if(m==1){ # First month of each scenario
        state_orig<-case$cluster[nrow(case)]
        matrix<-as.data.frame(MT_accum[12*mun]) # Selecting the right matrix
      }else{ # Remaining months
        matrix<-as.data.frame(MT_accum[(12*(mun-1))+m-1])  # Selecting the right matrix
      }
      
      # 1st stage, calling the function that simulates the next state or cluster
      cluster_simulation<-simulat_state(state_orig, matrix, random[i]) 
      
      # 2nd stage, calling the function that samples a value belonging to the cluster 
      values_cluster<-case %>% filter(month == m & cluster == cluster_simulation)
      final_simulation<-simulat_value(values_cluster,random_2[i])
      final_simulation <- as.numeric(final_simulation)
      
      fut_scenarios[i,1]<-scenario
      fut_scenarios[i,2]<-m
      fut_scenarios[i,3]<-cluster_simulation
      fut_scenarios[i,4]<-final_simulation
      fut_scenarios[i,5]<-random[i]
      fut_scenarios[i,6]<-random_2[i]
      
      state_orig<-cluster_simulation
      i<-i+1
    }
  }
  all_fut_scenarios_2[[mun]]<-fut_scenarios
  names(all_fut_scenarios_2)[mun] <- colnames(case)[3]
}

end.time <- Sys.time()
time.taken <- end.time - start.time
time.taken

## Saving simulation files ##

{
  saveRDS(random, file=file.path(simulation_dir, "Without_last_year", "Random_vector_stage1_2.RData"))
  saveRDS(random_2, file=file.path(simulation_dir, "Without_last_year", "Random_vector_stage2_2.RData"))
  saveRDS(all_fut_scenarios_2, file=file.path(simulation_dir, "Without_last_year", "Future_scenarios_2.RData"))
}

#### Analysis out-of-sample #########################################################

all_fut_scenarios_2<-readRDS(file.path(simulation_dir, "Without_last_year", "Future_scenarios_2.RData"))

### Scenarios cloud plot 

municip<-c("2414407","2406502","2400505") # code 
names<-c("Touros", "Lagoa Nova", "Alexandria")
yaxis_min<-c(3,4,3)
yaxis_max<-c(12,11,10)


for (i in 1:length(names)) {
  
  df <- all_fut_scenarios_2[[municip[i]]]  
  
  stats <- df %>%
    group_by(month) %>%
    summarise(
      min_wind = min(wind),
      max_wind = max(wind),
      mean_wind_simulated = mean(wind),
      p025 = quantile(wind, 0.025),
      p975 = quantile(wind, 0.975)
    )
  
  validat <- validation %>% select(2, all_of(municip[i]))
  colnames(validat)[2] <- "wind"
  
  
  graphic<-
    ggplot() +
    geom_ribbon(data = stats, aes(x = month, ymin = min_wind, ymax = max_wind, fill = "Scenarios"), 
                alpha = 0.5) +  
    geom_line(data = stats, aes(x = month, y = mean_wind_simulated, color = "Av. forecast"), 
              linewidth = 0.8) +  
    geom_line(data = validat, aes(x = month, y = wind, color = "Out-of-sample"), 
              linewidth = 1.2) +  
    geom_line(data = stats, aes(x = month, y = p025, color = "95% prediction interval"), 
              linetype = "dashed", linewidth = 0.9) +  
    geom_line(data = stats, aes(x = month, y = p975), color = "gray30", linetype = "dashed", linewidth = 0.9) +  
    scale_x_continuous(breaks = seq(1, 12, by = 1)) +  
    scale_y_continuous(breaks = seq(yaxis_min[i], yaxis_max[i], by = 1), limits = c(yaxis_min[i], yaxis_max[i])) +
    labs(title = names[i], x = "Month", y = "Wind Speed (m/s)") +
    scale_fill_manual(values = c("Scenarios" = "gray70")) +  
    scale_color_manual(
      values = c("Out-of-sample" = "darkgreen","95% prediction interval" = "gray30", "Av. forecast"="gray40"),
      labels = c("95% prediction interval" = "95% pred. interval"),
      guide = guide_legend(nrow = 2)) +  
    theme_minimal() +
    theme(
      plot.title = element_text(size = 22, hjust = 0),
      panel.grid = element_blank(),         
      axis.line = element_line(color = "gray60", size = 0.8), 
      axis.text = element_text(size = 18, colour = "black"),   
      axis.title = element_text(size = 20),  
      axis.ticks = element_line(color = "gray60", size = 0.8),
      legend.position = "bottom", 
      legend.title = element_blank(), 
      legend.text = element_text(size = 17),
      legend.key.width = unit(1, "cm")
    ) 
  
  filename <- file.path(simulation_dir, "Without_last_year", paste0(gsub(" ", "_", names[i]), "_Cloud.png"))
  ggsave(filename, plot = graphic, width = 7, height = 5, dpi = 300)

}

### Coverage percentage

# Share of out-of-sample observations falling within the scenario interval given by bounds_fun
coverage_percentage <- function(bounds_fun) {

  cp_results <- numeric(167)

  for (i in 3:169) {

    municipality <- colnames(validation)[i]

    real_values <- validation[, i]

    scenarios <- all_fut_scenarios_2[[municipality]]

    stats <- aggregate(wind ~ month, data = scenarios, FUN = bounds_fun)

    stats_df <- data.frame(month = stats$month,
                           lower = stats$wind[, 1],
                           upper = stats$wind[, 2])

    coverage <- sum(real_values >= stats_df$lower & real_values <= stats_df$upper)

    cp_results[i - 2] <- coverage / length(real_values)
  }

  data.frame(Municipality = colnames(validation)[3:169], CP = cp_results)
}

ci95_bounds       <- function(x) c(lower = quantile(x, probs = 0.025, na.rm = TRUE), upper = quantile(x, probs = 0.975, na.rm = TRUE))
full_range_bounds <- function(x) c(lower = min(x, na.rm = TRUE), upper = max(x, na.rm = TRUE))

cp_df <- coverage_percentage(ci95_bounds)

mean(cp_df$CP)

### Coverage percentage (full range of scenarios)

cp_df_full <- coverage_percentage(full_range_bounds)

mean(cp_df_full$CP)



