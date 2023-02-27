library(tidyverse)
library(psych)
library(factoextra)
library(performance)
library(parameters)
library(cluster)

'%!in%' <- function(x,y)!('%in%'(x,y))

find_outlier <- function(x) {
  return(x < quantile(x, .25) - 1.5*IQR(x) | x > quantile(x, .75) + 1.5*IQR(x))
}

# Data loading ------------------------------------------------------------

full_dat <- read_csv("noa_data.csv") |> 
  janitor::clean_names()

long_dat <- full_dat |> 
  select(id, 
         sss,
         wpi,
         sf_36_general_health_domain,
         fiq_total,
         bsi_18_total,
         cpm_pressure, cpm_temperature,
         nt_motor_skills, nt_information_processing_speed, nt_attention, nt_executive_function, nt_memory) |> 
  pivot_longer(data = _, cols = c(sss,
                                  wpi,
                                  sf_36_general_health_domain,
                                  fiq_total,
                                  bsi_18_total,
                                  cpm_pressure, cpm_temperature,
                                  nt_motor_skills, nt_information_processing_speed, nt_attention, nt_executive_function, nt_memory),
               names_to = "var",
               values_to = "value") |> 
  filter(!is.na(value)) |> 
  group_by(var) %>%
  mutate(outlier = ifelse(find_outlier(value), id, NA)) |> 
  filter(id %!in% c("50", "112", "155"))

# Outliers ----------------------------------------------------------------

# Neurotrax measure

ggplot(long_dat |> filter(var %in% c("nt_motor_skills", 
                                     "nt_information_processing_speed", 
                                     "nt_attention", 
                                     "nt_executive_function",
                                     "nt_memory")), aes(x = var, y = value)) +
  geom_boxplot() +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -.5)

# cpm measures

ggplot(long_dat |> filter(var %in% c("cpm_pressure", 
                                     "cpm_temperature")), aes(x = var, y = value)) +
  geom_boxplot() +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -.5)

# Questionnaires 

ggplot(long_dat |> filter(var %in% c("sss",
                                     "wpi",
                                     "sf_36_general_health_domain",
                                     "fiq_total",
                                     "bsi_18_total")), aes(x = var, y = value)) +
  geom_boxplot() +
  geom_text(aes(label = outlier), na.rm = TRUE, hjust = -.5)

# Clustering data ---------------------------------------------------------

dat <- full_dat |>
  filter(id %!in% c("50", "112", "155")) |>
  select(
    id, 
    # sss,
    # wpi,
    sf_36_general_health_domain,
    fiq_total,
    bsi_18_total,
    # cpm_pressure,
    # cpm_temperature,
    # nt_motor_skills,
    # nt_information_processing_speed,
    # nt_attention,
    # nt_executive_function,
    # nt_memory
  ) |>
  na.omit()

# Clustering ---------------------------------------------------------------------

# | K-means -----------------------------------------------------------------

data_scaled <- dat |> 
  select(-id) |> 
  scale()

k <- kmeans(data_scaled, centers = 2, iter.max = 500, nstart = 100)

fviz_nbclust(data_scaled, kmeans, method = "wss")
fviz_nbclust(data_scaled, kmeans, method = "silhouette")
# fviz_nbclust(data_scaled, kmeans, method = "gap_stat", nboot = 1000)

fviz_cluster(kmeans(data_scaled, centers = 2, iter.max = 500, nstart = 100), data = data_scaled)

# Assign the cluster labels to your original data
dat$kmeans_cluster <- k$cluster

# View the results

print(k)

# Calculate the Silhouette Coefficient
kmeans_sil <- silhouette(k$cluster, dist(data_scaled))
mean(kmeans_sil[, 3])

# | Hierarchical ------------------------------------------------------------

# Determine the optimal number of clusters using the dendrogram
dist_matrix <- dist(data_scaled, method = "euclidean")
hc_complete <- hclust(dist_matrix, method = "complete")
plot(hc_complete)

# Cut the dendrogram into the desired number of clusters
cutree <- cutree(hc_complete, k = 2)

# Assign the cluster labels to your original data
dat$hierarchical_cluster <- cutree

# View the results
table(dat$hierarchical_cluster)

# Calculate the Silhouette Coefficient
hierarchical_sil <- silhouette(cutree, dist(data_scaled))
mean(hierarchical_sil[, 3])

# | GMM ---------------------------------------------------------------------

library("mclust")

# Fit the Gaussian Mixture Model
model <- Mclust(dat[,2:4])

# Extract the cluster labels from the model
clusters <- model$classification

# Assign the cluster labels to your original data
dat$gmm_cluster <- clusters

# View the results
table(dat$gmm_cluster)

# Calculate the Silhouette Coefficient
gmm_sil <- silhouette(clusters, dist(data_scaled))
mean(gmm_sil[, 3])

# Clusters - descriptives -------------------------------------------------

clustered_full_dat <- full_dat |> 
  left_join(x = _, dat |> 
              select(id, contains("cluster")),
            by = "id")

# Primary -----------------------------------------------------------------

bla <- clustered_full_dat |> 
  filter(!is.na(kmeans_cluster)) |> 
  group_by(kmeans_cluster) |> 
  summarize(n = n(),
            mean_sss = mean(sss, na.rm = TRUE),
            mean_wpi = mean(wpi, na.rm = TRUE),
            mean_sf_36_general_health_domain = mean(sf_36_general_health_domain, na.rm = TRUE),
            mean_bsi_18_total = mean(bsi_18_total, na.rm = TRUE),
            mean_fiq_total = mean(fiq_total, na.rm = TRUE),
            mean_cpm_pressure = mean(cpm_pressure, na.rm = TRUE),
            mean_cpm_temperature = mean(cpm_temperature, na.rm = TRUE),
            mean_nt_motor_skills = mean(nt_motor_skills, na.rm = TRUE),
            mean_nt_information_processing_speed = mean(nt_information_processing_speed, na.rm = TRUE),
            mean_nt_attention = mean(nt_attention, na.rm = TRUE), 
            mean_nt_executive_function = mean(nt_executive_function, na.rm = TRUE), 
            mean_nt_memory = mean(nt_memory, na.rm = TRUE),
            p_tbi = sum(p_tbi),
            p_tbi_rate = sum(p_tbi)/n()*100,
            p_concussion_whiplash = sum(p_concussion_whiplash),
            p_concussion_whiplash_rate = sum(p_concussion_whiplash)/n()*100,
            p_concussion_ptsd = sum(p_concussion_ptsd),
            p_concussion_ptsd_rate = sum(p_concussion_ptsd)/n()*100,
            p_traumatic_events_stress = sum(p_traumatic_events_stress),
            p_traumatic_events_stress_rate = sum(p_traumatic_events_stress)/n()*100,
            p_anesthesia_anoxic = sum(p_anesthesia_anoxic),
            p_anesthesia_anoxic_rate = sum(p_anesthesia_anoxic)/n()*100,
            p_viral_infection = sum(p_viral_infection),
            p_viral_infection_rate = sum(p_viral_infection)/n()*100) |> 
  View()

