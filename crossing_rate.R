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
  df$crossings_per_km <- df$count / (df$sum / 100)
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
  crossings_per_km = c(0.043860942293864, 0.042050111117419, 0.070547285946420, 0.455975028414935),
  file_id = "0.1"
)
ch2_data <- data.frame(
  class = c(3, 2, 1, 0),
  sum = c(43131.03, 5315.41, 61934.08, 502292.97),
  count = c(18, 2, 39, 1175),
  crossings_per_km = c(0.0417332950314426, 0.0376264483830974, 0.0629701773240193, 0.233927223787345),
  file_id = "CH2"
)
# 
# buffer_data <- data.frame(
#   class = c(3, 2, 1, 0),
#   sum = c(47645.81, 29136.16, 103616.58, 553862.14),
#   count = c(18, 2, 39, 1175),
#   crossings_per_km = c(0.037778768, 0.0068643225, 0.037638764, 0.2121466544),
#   file_id = "Buf"
# )

final_result <- rbind(final_result, o_point_one, ch2_data)

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
  "CH2" = "Conv2"
#  "Buf" = "Buf"
)
final_result$file_id <- unname(file_id_map[final_result$file_id])

# mean crossing rates per class
mean_crossings <- final_result %>%
  group_by(class) %>%
  summarise(mean_crossings_per_km = mean(crossings_per_km, na.rm = TRUE))
# class         mean_crossings_per_km
#      0                0.290 
#      1                0.0603
#      2                0.0223
#      3                0.0414

# make variable for color, keeping in the same theme as other
coul <- brewer.pal(6, "BuPu")

# visualise in scatter plot
ggplot(final_result, aes(x = file_id, y = crossings_per_km, shape = as.factor(class))) +
  geom_point(size = 5, alpha = 0.9, aes(fill = as.factor(class)), 
             color = "black", stroke = 0.1) +
  scale_fill_manual(
    values = c(coul[3], coul[4], coul[5], coul[6]),
    labels = c("Local roads", "Low intensity", "Medium intensity", "High intensity")) +
  scale_shape_manual(values = c(21, 22, 23, 24),
                     labels = c("Local roads", "Low intensity", "Medium intensity", "High intensity")) +
  labs(
    x = "Study area size (concave to convex hull)",
    y = "Crossings per 100m",
    fill = "Road class",
    shape = "Road class"
  ) +
  theme_minimal() +
  theme(
    text = element_text(color = "#303033"),
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
ggsave("crossings_per_km_plot.png", width = 10, height = 6, bg = "#E1DFDB")



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
    text = element_text(color = "#303033"),
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

ggsave("road_buffers_stacked_merged_classes.png", width = 14, height = 8, bg = "#E1DFDB")
  


#--- statistical tests ----

kruskal_test <- kruskal.test(crossings_per_km ~ as.factor(class), data = final_result)
dunn_result <- dunn.test(final_result$crossings_per_km, as.factor(final_result$class), method = "holm", list = T)
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



#--- visualising mean crossing rates ----

road_names <- c(
  "Local roads",
  "Low intensity",
  "Medium & high intensity "
)

mean_crossings <- final_result %>%
  mutate(
    new_class = case_when(
      class == 2 ~ 3,  # merge class 2 into class 3
      TRUE ~ class # keep rest
    ),
    method = "Method 1"
  ) %>%
  group_by(new_class, method) %>%
  summarise(value = mean(crossings_per_km, na.rm = TRUE), .groups = "drop") # summarize to get the merged class

mean_crossings2 <- aggregate(crossings ~ new_class, data = file, mean)

mean_crossings1.2 <- final_result %>%
  filter(file_id == "Conv1") %>%
  mutate(
    new_class = case_when(
      class == 2 ~ 3,  # merge class 2 into class 3
      TRUE ~ class # keep rest
    ),
    method = "Method 1.2"
  ) %>%
  group_by(new_class, method) %>%
  summarise(value = mean(crossings_per_km, na.rm = TRUE), .groups = "drop") # summarize to get the merged class

plot_data <- bind_rows(
  mean_crossings,
  mean_crossings2 %>%
    mutate(method = "Method 2") %>%
    rename(value = crossings),
  mean_crossings1.2
)

ggplot(plot_data, aes(x = factor(new_class), y = value, fill = method)) +
  geom_col(position = "dodge", color = "black", width = 0.7, alpha = 0.9, linewidth = 0.1) +
  scale_fill_manual(
    values = c(coul[3], coul[4], coul[5]),
    labels = c("Method 1 means", "Method 1 just convex", "Method 2")
  ) + 
  scale_x_discrete(labels= road_names) +
  labs(
    x = "Traffic class",
    y = "Mean crossings per 100m",
    fill = "Method"
  ) +
  theme_minimal() +
  theme(
    text = element_text(color = "#303033"),
    plot.title = element_text(size = 18, hjust = 0.5, margin = margin(b = 15)),
    axis.text.x = element_text(size = 14, angle = 20, hjust = 1),
    axis.text.y = element_text(size = 14),
    axis.title.x = element_text(size = 16, margin = margin(t = 10)),
    axis.title.y = element_text(size = 16, margin = margin(r = 15)),
    legend.title = element_text(size = 16),
    legend.text = element_text(size = 14),
    legend.key.size = unit(0.8, "cm"),
    legend.key.spacing.y = unit(0.5, 'cm'))

ggsave("crossings_per_km_all_methods.png", width = 10, height = 6, bg = "lightgrey")

