############# Packages ################################################

{
  if (!require("sf")) install.packages("sf", dependencies=TRUE)
  if (!require("ggplot2")) install.packages("ggplot2", dependencies=TRUE)
  if (!require("dplyr")) install.packages("dplyr", dependencies=TRUE)
  if (!require("cowplot")) install.packages("cowplot", dependencies=TRUE)
  if (!require("ggrepel")) install.packages("ggrepel", dependencies=TRUE)
  if (!require("gridExtra")) install.packages("gridExtra", dependencies=TRUE)
}

#### Read data ##########################################################

script_dir <- if (requireNamespace("rstudioapi", quietly=TRUE) && rstudioapi::isAvailable()) {
  dirname(rstudioapi::getSourceEditorContext()$path)
} else {
  args <- commandArgs(trailingOnly=FALSE)
  dirname(normalizePath(sub("--file=", "", args[grep("--file=", args)][1]), winslash="/"))
}
root_dir      <- dirname(script_dir)
indicator_dir <- file.path(root_dir, "03_Indicator_files_output")

#### Export RData rankings to CSV ####

critic_mabac_results   <- readRDS(file.path(indicator_dir, "CRITIC_MABAC", "CRITIC_MABAC_ranking.RData"))
critic_mabac_results_2 <- readRDS(file.path(indicator_dir, "CRITIC_MABAC", "CRITIC_MABAC_ranking_2.RData"))

bind_rows(lapply(seq_along(critic_mabac_results), function(i) {
  df <- critic_mabac_results[[i]]; df$scenario <- i; df
})) |> write.csv(file.path(script_dir, "ranking_sdgwind.csv"), row.names=FALSE)

bind_rows(lapply(seq_along(critic_mabac_results_2), function(i) {
  df <- critic_mabac_results_2[[i]]; df$scenario <- i; df
})) |> write.csv(file.path(script_dir, "ranking_windonly.csv"), row.names=FALSE)

#### Shapefiles ####
# info_geo: IBGE municipal boundaries (2021). wind_farms: ANEEL existing wind farm database.

info_geo   <- st_read(file.path(script_dir, "RN_Municipalities_2021", "RN_Municipalities_2021.shp"), quiet=TRUE)
wind_farms <- st_read(file.path(script_dir, "Existing_Wind_Farms", "Existing_Wind_Farms.shp"), quiet=TRUE)

#### Colorbar ####

n <- 200
cb_df <- data.frame(x=0.5, y=seq(0, 1, length.out=n))

colorbar_plot <- ggplot(cb_df, aes(x=x, y=y, fill=y)) +
  geom_raster(interpolate=TRUE) +
  scale_fill_gradient(low="gray85", high="#004d00", guide="none") +
  scale_y_continuous(expand=c(0, 0)) +
  scale_x_continuous(expand=c(0, 0)) +
  annotate("text",    x=1.7, y=0.78, label="Better", angle=-90, size=6, fontface="bold") +
  annotate("segment", x=1.4, xend=1.4, y=0.58, yend=0.97,
           arrow=arrow(length=unit(0.4,"cm"), type="open"), linewidth=1.0) +
  annotate("text",    x=1.7, y=0.22, label="Worse",  angle=-90, size=6, fontface="bold") +
  annotate("segment", x=1.4, xend=1.4, y=0.42, yend=0.03,
           arrow=arrow(length=unit(0.4,"cm"), type="open"), linewidth=1.0) +
  coord_cartesian(xlim=c(0.1, 2.2)) +
  theme_void() +
  theme(plot.background=element_rect(fill="white", color=NA))

#### Maps (with rank numbers, arrows, and table) ####

configs_v2 <- list(
  list(csv="ranking_sdgwind.csv",  out="wind_sdgwind_v2.png"),
  list(csv="ranking_windonly.csv", out="wind_windonly_v2.png")
)

for (cfg in configs_v2) {

  scenario_data <- read.csv(file.path(script_dir, cfg$csv))

  summary_data <- scenario_data |>
    group_by(scenario) |>
    mutate(Rank = rank(-Score, ties.method="first")) |>
    ungroup() |>
    group_by(Municipality) |>
    summarise(Score_median = median(Rank), Score_sd = sd(Rank), .groups="drop")

  top10_df <- summary_data |>
    arrange(Score_median, Score_sd) |>
    head(10) |>
    mutate(RankLabel = 1:10)

  full_data <- info_geo |>
    inner_join(summary_data, by=c("NM_MUN"="Municipality"))

  top10_geo <- info_geo |>
    inner_join(top10_df, by=c("NM_MUN"="Municipality"))

  top10_centroids <- suppressWarnings(st_centroid(top10_geo)) |>
    st_coordinates() |>
    as.data.frame() |>
    mutate(RankLabel = top10_geo$RankLabel)

  map_plot <- ggplot() +
    geom_sf(data=full_data, aes(fill=Score_median), color="gray55", linewidth=0.25) +
    scale_fill_gradient(low="#004d00", high="gray85", guide="none") +
    geom_sf(data=wind_farms, color="yellow", size=0.6, shape=16) +
    geom_sf(data=top10_geo, fill=NA, color="yellow", linewidth=1.2) +
    geom_text_repel(
      data=top10_centroids,
      aes(x=X, y=Y, label=RankLabel),
      size=6, fontface="bold", color="white",
      segment.color="white", segment.size=2.5,
      segment.curvature=0,
      arrow=arrow(length=unit(0.018, "npc"), type="open", ends="first"),
      box.padding=0.9, point.padding=0.2,
      min.segment.length=0, max.overlaps=Inf,
      seed=42
    ) +
    geom_text_repel(
      data=top10_centroids,
      aes(x=X, y=Y, label=RankLabel),
      size=6, fontface="bold", color="black",
      segment.color="black", segment.size=0.7,
      segment.curvature=0,
      arrow=arrow(length=unit(0.018, "npc"), type="open", ends="first"),
      box.padding=0.9, point.padding=0.2,
      min.segment.length=0, max.overlaps=Inf,
      bg.color="white", bg.r=0.18,
      seed=42
    ) +
    theme_void() +
    theme(plot.background=element_rect(fill="white", color=NA))

  top_row <- plot_grid(map_plot, colorbar_plot, rel_widths=c(7, 1), nrow=1)

  tt <- ttheme_minimal(
    base_size=18,
    padding=unit(c(3, 4), "mm"),
    core=list(
      fg_params=list(hjust=0, x=0.02),
      bg_params=list(fill="white", col=NA)
    ),
    colhead=list(
      fg_params=list(fontface="bold", hjust=0, x=0.02),
      bg_params=list(fill="white", col=NA)
    )
  )

  combined_df <- data.frame(
    "Rank"              = 1:5,
    "Municipality"      = top10_df$Municipality[1:5],
    " "                 = rep("", 5),
    "  "                = rep("", 5),
    "Rank​"        = 6:10,
    "Municipality​"  = top10_df$Municipality[6:10],
    check.names = FALSE
  )

  table_grob <- tableGrob(combined_df, rows=NULL, theme=tt)
  table_gg <- ggdraw() + draw_grob(table_grob, x=0, y=0, width=1, height=1)

  final <- plot_grid(top_row, table_gg, ncol=1, rel_heights=c(3.5, 1))

  ggsave(file.path(script_dir, cfg$out), plot=final, width=13, height=10, dpi=300, bg="white")
  cat("Saved:", cfg$out, "\n")
}
