library(RColorBrewer)
library(tidyverse)
library(scales)
library(viridis)
library(dunn.test)

# import data which were acquired from QGIS 
file_path <- "D:\\Users\\amand\\Documents\\qgis\\masters_qgis\\vectors\\testing_study_area_size\\road_length_statistics"
files <- list.files(file_path, pattern = "^length_.*\\.csv$", full.names = TRUE)

# assign the crossings and classes
counts <- data.frame(
  class = c(3, 2, 1, 0),
  count = c(18, 2, 39, 1175)
)

# calculate the crossing rate
counts_per_km <- function(df, file_id) {
  df <- merge(df[, c("class", "sum")], counts, by = "class")
  df$crossings_per_km <- df$count / (df$sum / 1000)
  df$file_id <- file_id
  return(df)
}

# merge the separate files into one list
result_list <- lapply(files, function(file) {
  df <- read.csv(file)
  file_name <- basename(file)
  file_id <- substr(file_name, nchar(file_name) - 5, nchar(file_name)) 
  counts_per_km(df, file_id)
})

final_result <- do.call(rbind, result_list) #join the list in one

# add single files which are not included, e.g., 0.1 crossings are different
o_point_one <- data.frame(
  class = c(3, 2, 1, 0),
  sum = c(41038.79, 4756.23, 55282.07, 257470.24),
  count = c(18, 2, 39, 1174),
  crossings_per_km = c(0.43860942293864, 0.42050111117419, 0.70547285946420, 4.55975028414935),
  file_id = "0.1"
)
ch2_data <- data.frame(
  class = c(3, 2, 1, 0),
  sum = c(43131.03, 5315.41, 61934.08, 502292.97),
  count = c(18, 2, 39, 1175),
  crossings_per_km = c(0.417332950314426, 0.376264483830974, 0.629701773240193, 2.33927223787345),
  file_id = "CH2"
)

buffer_data <- data.frame(
  class = c(3, 2, 1, 0),
  sum = c(47645.81, 29136.16, 103616.58, 553862.14),
  count = c(18, 2, 39, 1175),
  crossings_per_km = c(0.37778768, 0.068643225, 0.37638764, 2.121466544),
  file_id = "Buf"
)

final_result <- rbind(final_result, o_point_one, ch2_data, buffer_data)

# re-name the id's for more tidy output
file_id_map <- c(
  "02.csv" = "0.2",
  "03.csv" = "0.3",
  "04.csv" = "0.4",
  "05.csv" = "0.5",
  "06.csv" = "0.6",
  "07.csv" = "0.7",
  "08.csv" = "0.8",
  "09.csv" = "0.9",
  "_1.csv" = "1",
  "ch.csv" = "Conv1",
  "0.1" = "0.1",
  "CH2" = "Conv2",
  "Buf" = "Buf"
)
final_result$file_id <- unname(file_id_map[final_result$file_id])

# make variable for color, keeping in the same theme as other
coul <- brewer.pal(6, "BuPu")

# visualise in scatter plot
ggplot(final_result, aes(x = file_id, y = crossings_per_km, shape = as.factor(class))) +
  geom_point(size = 5, alpha = 0.9, aes(fill = as.factor(class)), color = "black", position = position_jitter(width = 0)) +
  scale_fill_manual(
    values = c(coul[3], coul[4], coul[5], coul[6]),
    labels = c("Local roads", "Low intensity", "Medium intensity", "High intensity")) +
  scale_shape_manual(values = c(21, 22, 23, 24),
                     labels = c("Local roads", "Low intensity", "Medium intensity", "High intensity")) +
  labs(
    x = "Study area size (concave to convex hull)",
    y = "Crossings per km",
    fill = "Road class",
    shape = "Road class"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16, angle = 45),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 16, margin = margin(r = 15)),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    legend.title = element_text(size = 16),
    legend.key.spacing.y = unit(0.5, "cm"),
    legend.text = element_text(size = 16)
  ) +
  guides(
    fill = guide_legend(
      label.theme = element_text(size = 16),
      keywidth = 1,
      keyheight = 1
    )
  )
write.csv(final_result,"studyarea_sizes.csv", row.names = FALSE)
ggsave("crossings_per_km_plot.png", width = 14, height = 8, bg = "lightgrey")



# --- road buffers ----

core_folder <- "D:\\Users\\amand\\Documents\\qgis\\masters_qgis\\vectors\\crossing_buffer"
# get all the filles and create a list to later put the df in
road_landuse <- list.files(core_folder, pattern = "\\.csv$", full.names = TRUE)
landuse_road <- list()


#fill the list with df from each csv 
for (file in road_landuse) {
  df <- read.csv(file)
  df <- df[, c("Code_18", "sum")]
  
  file_name <- tools::file_path_sans_ext(basename(file)) #name by the file name without .csv
  colnames(df)[2] <- file_name #add the name
  
  landuse_road[[file_name]] <- df #add the df to the list
}

merged_data <- reduce(landuse_road, full_join, by = "Code_18") #join them together


# renaming
landuse_names <- c(
  "313" = "313 - Mixed forest",
  "312" = "312 - Coniferous forest",
  "324" = "324 - Transitional woodland-shrub",
  "321" = "321 - Natural grasslands",
  "231" = "231 - Pastures",
  "211" = "211 - Non-irrigated arable land",
  "512" = "512 - Water bodies",
  "411" = "411 - Inland marshes",
  "412" = "412 - Peat bogs",
  "131" = "131 - Mineral extraction sites",
  "311" = "311 - Broad-leaved forest",
  "243" = "243 - Land principally occupied by agriculture with significant areas of natural vegetation",
  "242" = "242 - Complex cultivation patterns"
)

merged_data$Code_18 <- recode(merged_data$Code_18, !!!landuse_names)
merged_data[is.na(merged_data)] <- 0

write.csv(merged_data,"buffer_landuse.csv", row.names = FALSE)





# --- plotting crossing buffer landuse----

#create proportion columns for each landuse of total
proportion_data <- merged_data %>%
  mutate(across(starts_with("crossing_buffer"), 
                ~ . / sum(.), 
                .names = "prop_{col}"))

# select only proportion columns for visualisation
proportion_data_filtered <- proportion_data %>%
  dplyr::select(Code_18, starts_with("prop_crossing_buffer"))

# rename for cleaner output
proportion_data_filtered <- proportion_data_filtered %>%
  rename(
    `Local roads` = prop_crossing_buffer_0,
    `Low intensity` = prop_crossing_buffer_1,
    `Medium & high intensity` = prop_crossing_buffer_2and3
  )

# convert to long format so could use in ggplot
proportion_data_long <- proportion_data_filtered %>%
  pivot_longer(
    cols = -Code_18,
    names_to = "Buffer_Class",
    values_to = "Proportion"
  )

# plot the landuse around road crossing points
ggplot(proportion_data_long, aes(x = Buffer_Class, y = Proportion, fill = Code_18)) +
  geom_bar(stat = "identity", position = "fill") +
  scale_fill_manual(
    values = c(
      "313 - Mixed forest" = "#4DFF00",
      "312 - Coniferous forest" = "#00A600",
      "324 - Transitional woodland-shrub" = "#A6F200",
      "321 - Natural grasslands" = "#CCF24D",
      "231 - Pastures" = "#E6E64D",
      "211 - Non-irrigated arable land" = "#FFFFA8",
      "512 - Water bodies" = "#4DA6FF",
      "411 - Inland marshes" = "#A6A6FF",
      "412 - Peat bogs" = "#4D4DFF",
      "131 - Mineral extraction sites" = "#A600CC",
      "311 - Broad-leaved forest" = "#80FF00",
      "243 - Land principally occupied by agriculture with significant areas of natural vegetation" = "#E6CC4D",
      "242 - Complex cultivation patterns" = "#FFE64D"
    ),
      labels = label_wrap(50)
  ) +
  scale_y_continuous(labels = scales::percent) +
  labs(
    x = "Buffer road class",
    y = "Percentage of land use type",
    fill = "Land use"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 17, margin = margin(r = 15)),
    axis.title.x = element_text(size = 17,  margin = margin(t = 10)),
    legend.title = element_text(size = 16),
    legend.key.spacing.y = unit(0.3, "cm")
  ) +
  guides(
    fill = guide_legend(
      label.theme = element_text(size = 16),
      keywidth = 1,
      keyheight = 1
    )
  )

ggsave("road_buffers_stacked_merged_classes.png", width = 14, height = 8, bg = "lightgrey")
  


#--- statistical tests ----

kruskal_test <- kruskal.test(crossings_per_km ~ as.factor(class), data = final_result)
dunn_result <- dunn.test(final_result$crossings_per_km, as.factor(final_result$class), method = "bonferroni", list = T)
dunn_result

# #  evaluate interactions between road classes and study area sizes
# library(nparLD)
# np_test <- ld.f1(crossings_per_km ~ class * file_id, data = final_result)
# summary(np_test)


# --- statistical tests for crossings normalised  ----
file_path <- "D:\\Users\\amand\\Documents\\qgis\\masters_qgis\\testing_all_classes_100m.csv"
file <- read.csv(file_path)

# class: all classes
# Comparison          Z      P.unadj        P.adj
# 1      0 - 1  3.7847363 1.538716e-04 0.0009232298
# 2      0 - 2  0.8952324 3.706628e-01 1.0000000000
# 3      1 - 2 -0.1006730 9.198100e-01 1.0000000000
# 4      0 - 3  4.2896838 1.789277e-05 0.0001073566
# 5      1 - 3  0.7705682 4.409629e-01 1.0000000000
# 6      2 - 3  0.3848211 7.003700e-01 1.0000000000

# new_class: 2 & 3 = 3 
# Comparison         Z      P.unadj        P.adj
# 1      0 - 1 3.7847363 1.538716e-04 4.616149e-04
# 2      0 - 3 4.3470562 1.379769e-05 4.139307e-05
# 3      1 - 3 0.7050249 4.807948e-01 1.000000e+00

# class2: 1&2 = 
# 0 - 1 :  3.867351 (0.0002)*
# 0 - 3 :  4.289683 (0.0000)*
# 1 - 3 :  0.796876 (0.6383)

file$class2 <- dplyr::case_when(
  file$class == 1 | file$class == 2 ~ 1,
  file$class == 3 ~ 3
)

file$crossings[is.na(file$crossings)] <- 0
file$class2[is.na(file$class2)] <- 0
file$new_class[is.na(file$new_class)] <- 0

kruskal_test <- kruskal.test(crossings ~ as.factor(new_class), data = file)
kruskal_test
dunn_result <- dunn.test(file$crossings, as.factor(file$new_class), list = T, method = "holm", altp = T)
dunn_result


mean_crossings <- aggregate(crossings ~ new_class, data = file, mean)

#visualising the mean crossing rate 
ggplot(mean_crossings, aes(x = factor(new_class), y = crossings)) +
  geom_bar(stat = "identity", fill = "skyblue", color = "black") +
  labs(
    x = "Traffic class",
    y = "Mean crossings per 100m"
  ) +
  theme_minimal() +
  theme(
    plot.title = element_text(hjust = 0.5),
    axis.text = element_text(size = 12),
    axis.title = element_text(size = 14)
  )
