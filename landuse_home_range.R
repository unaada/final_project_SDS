library(RColorBrewer)
library(tidyverse)
library(scales)
library(viridis)

#--- joining the csv's of individual moose ----

core_folder <- "D:\\Users\\amand\\Documents\\qgis\\masters_qgis\\rasters\\land_use_home_ranges\\core_area\\vector_report"

# get all the filles and create a list to later put the df in
core_landuse <- list.files(core_folder, pattern = "\\.csv$", full.names = TRUE)
landuse_data <- list()

#fill the list with df from each csv 
for (file in core_landuse) {
  df <- read.csv(file)
  df <- df[, c("Code_18", "sum")]
  
  file_name <- tools::file_path_sans_ext(basename(file)) #name by the file name without .csv
  colnames(df)[2] <- file_name #add the name
  
  landuse_data[[file_name]] <- df #add the df to the list
}

merged_data <- reduce(landuse_data, full_join, by = "Code_18") #join the list with full_join to keep all observations
write.csv(merged_data,"core_landuse.csv", row.names = FALSE)



#--- for full home range ----
full_folder <- "D:\\Users\\amand\\Documents\\qgis\\masters_qgis\\rasters\\land_use_home_ranges\\full_home_range"
full_landuse <- list.files(full_folder, pattern = "\\.csv$", full.names = TRUE)
full_data <- list()

for (file in full_landuse) {
  df <- read.csv(file)
  df <- df[, c("Code_18", "sum")]
  
  file_name <- tools::file_path_sans_ext(basename(file))
  colnames(df)[2] <- file_name
  
  full_data[[file_name]] <- df
}

merged_data <- reduce(full_data, full_join, by = "Code_18")
write.csv(merged_data,"full_landuse.csv", row.names = FALSE)


# --- visualising CORE home range ----
core_landuse_long <- core_landuse_prop %>%
  pivot_longer(cols = starts_with("core"), 
               names_to = "Core_Area", 
               values_to = "Proportion")
#filter out the discontinuos urban fabric, because only 1 moose had 0.3% of it in home range
# and does not show up
core_landuse_long <- core_landuse_long %>%
  filter(Code_18 != "112 - Discontinuous urban fabric")
core_landuse_long$Code_18 <- str_wrap(core_landuse_long$Code_18, width = 50) #bcs one is really long


coul <- brewer.pal(5, "BuPu") ## only for report. Will not work on a projector
# different color palette options
#   scale_fill_gradientn(colors = coul) +
#  scale_fill_viridis(option = "D", direction = -1) +
#   scale_fill_viridis(option = "E", direction = 1) +

ggplot(core_landuse_long, aes(x = Core_Area, y = fct_rev(Code_18), fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = coul) +
  labs(x = "Core home range, ID",
       y = "Land use type",
       fill = "Proportion",
       size = 12) +
  theme_minimal() +
  theme(axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "#EDF8FB"),
        axis.text.y = element_text(size = 16, , color = "#EDF8FB"),
        axis.title.y = element_text(margin = margin(r = 15), size = 17, , color = "#EDF8FB"),
        axis.title.x = element_text(size = 17, , color = "#EDF8FB"),
        legend.text = element_text(color = "#EDF8FB", size = 16),
        legend.title =element_text(size=17, color = "#EDF8FB"))

ggsave("core_heatmap.png", width = 14, height = 8, bg = "#747579")



# all moose median box plots
core_landuse_long <- core_landuse_prop %>%
  pivot_longer(cols = starts_with("core"), 
               names_to = "Core_Area", 
               values_to = "Proportion")
core_landuse_long$Code_18 <- str_wrap(core_landuse_long$Code_18, width = 40)

core_landuse_long %>%
  mutate(Code_18 = fct_reorder(Code_18, Proportion, .fun='median')) %>%
  ggplot(aes(x = Code_18, y = Proportion)) +
  geom_boxplot(fill = coul[1], 
               color = coul[4], 
               outlier.color = coul[5], 
               outlier.size = 2,
               outlier.alpha = 0.95) +
  labs(x = "Land use type in core home range",
       y = "Proportion") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 60, hjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(margin = margin(r = 20), size = 17),
    axis.title.x = element_text(size = 17, margin = margin(t = 10))
  )

ggsave("core_boxplot.png", width = 14, height = 8, bg = "lightgrey")








# --- stacked bar charts----
core_landuse_long <- core_landuse %>%
  pivot_longer(cols = starts_with("core"), 
               names_to = "Core_Area", 
               values_to = "Area")

order <- c("core 94 summer", "core 94 winter", "core 88", "core 87", 
           "core 85", "core 90", "core 93", "core 89", "core 91")

core_landuse_long$Core_Area <- factor(core_landuse_long$Core_Area, levels = order)

core_landuse_long <- core_landuse_long %>%
  filter(Code_18 != "total")

# colors from CORINE
land_use_colors <- c(
  "312 - Coniferous forest" = "#00A600",
  "324 - Transitional woodland-shrub" = "#A6F200",
  "231 - Pastures" = "#E6E64D",
  "313 - Mixed forest" = "#4DFF00",
  "412 - Peat bogs" = "#4D4DFF",
  "411 - Inland marshes" = "#A6A6FF",
  "311 - Broad-leaved forest" = "#80FF00",
  "211 - Non-irrigated arable land" = "#FFFFA8",
  "243 - Land principally occupied by agriculture with significant areas of natural vegetation" = "#E6CC4D",
  "131 - Mineral extraction sites" = "#A600CC",
  "112 - Discontinuous urban fabric" = "#FF0000",
  "321 - Natural grasslands" = "#CCF24D",
  "222 - Fruit trees and berry plantations" = "#F2A64D"
)


ggplot(core_landuse_long, aes(x = Core_Area, y = Area, fill = Code_18)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = land_use_colors,
    labels = label_wrap(50)
  ) +
  labs(x = "Core home range, ID", 
       y = "Area, ha", 
       fill = "Land use type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 30, hjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(margin = margin(r = 15), size = 17),
    axis.title.x = element_text(size = 17),
    legend.position = "none"
  )

ggsave("core_stacked_no_legend.png", width = 5, height = 8, bg = "lightgrey")






# --- FULL home range ----

# --- visualising ----
landuse_long <- landuse_prop %>%
  pivot_longer(cols = -Code_18, 
               names_to = "Area", 
               values_to = "Proportion")

landuse_long$Code_18 <- str_wrap(landuse_long$Code_18, width = 50)

landuse_long <- landuse_long %>%
  filter(Code_18 != "112 - Discontinuous urban fabric",
         Code_18 != "121 - Industrial or commercial units")

coul <- brewer.pal(5, "BuPu")

ggplot(landuse_long, aes(x = Area, y = fct_rev(Code_18), fill = Proportion)) +
  geom_tile(color = "white") +
  scale_fill_gradientn(colors = coul) +
  labs(x = "Home range, ID",
       y = "Land use type",
       fill = "Proportion",
       size = 12) +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16, color = "#EDF8FB"),
    axis.text.y = element_text(size = 16, color = "#EDF8FB"),
    axis.title.y = element_text(margin = margin(r = 15), size = 17, color = "#EDF8FB"),
    axis.title.x = element_text(size = 17, color = "#EDF8FB"),
    legend.text = element_text(color = "#EDF8FB", size = 16),
    legend.title = element_text(size = 17, color = "#EDF8FB")
  )

ggsave("heatmap.png", width = 14, height = 8, bg = "#747579")





# all moose median box plots
landuse_long <- landuse_prop %>%
  pivot_longer(cols = -Code_18, 
               names_to = "Area", 
               values_to = "Proportion")

landuse_long$Code_18 <- str_wrap(landuse_long$Code_18, width = 40)

landuse_long %>%
  mutate(Code_18 = fct_reorder(Code_18, Proportion, .fun='median')) %>%
  ggplot(aes(x = Code_18, y = Proportion)) +
  geom_boxplot(fill = coul[1], 
               color = coul[4], 
               outlier.color = coul[5], 
               outlier.size = 2,
               outlier.alpha = 0.95) +
  labs(x = "Land use type in home range",
       y = "Proportion") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 63, hjust = 1, size = 15),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(margin = margin(r = 20), size = 17),
    axis.title.x = element_text(size = 17, margin = margin(t = 10))
  )

ggsave("boxplot.png", width = 14, height = 8, bg = "lightgrey")




# --- stacked bar charts----
landuse_long <- landuse %>%
  pivot_longer(cols = -Code_18, 
               names_to = "Home_range", 
               values_to = "Area")

order <- c("94 summer", "94 winter", "88",
           "85", "87",  "90",  "89", "93","91")


landuse_long$Home_range <- factor(landuse_long$Home_range, levels = order)

landuse_long <- landuse_long %>%
  filter(Code_18 != "total")

land_use_colors <- c(
  "312 - Coniferous forest" = "#00A600",
  "324 - Transitional woodland-shrub" = "#A6F200",
  "231 - Pastures" = "#E6E64D",
  "313 - Mixed forest" = "#4DFF00",
  "412 - Peat bogs" = "#4D4DFF",
  "411 - Inland marshes" = "#A6A6FF",
  "311 - Broad-leaved forest" = "#80FF00",
  "211 - Non-irrigated arable land" = "#FFFFA8",
  "243 - Land principally occupied by agriculture with significant areas of natural vegetation" = "#E6CC4D",
  "131 - Mineral extraction sites" = "#A600CC",
  "112 - Discontinuous urban fabric" = "#FF0000",
  "321 - Natural grasslands" = "#CCF24D",
  "222 - Fruit trees and berry plantations" = "#F2A64D",
  "242 - Complex cultivation patterns" = "#FFE64D",
  "121 - Industrial or commercial units" = "#CC4DF2"
)

ggplot(landuse_long, aes(x = Home_range, y = Area, fill = Code_18)) +
  geom_bar(stat = "identity", position = "stack") +
  scale_fill_manual(
    values = land_use_colors,
    labels = label_wrap(50)
  ) +
  labs(x = "Home range, ID", 
       y = "Area, ha", 
       fill = "Land use type") +
  theme_minimal() +
  theme(
    axis.text.x = element_text(angle = 45, hjust = 1, size = 16),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(margin = margin(r = 15), size = 17),
    axis.title.x = element_text(size = 17),
    legend.key.spacing.y = unit(0.3, "cm"),
    legend.title = element_text(size=17)
  ) +
  guides(
    fill = guide_legend(
      label.theme = element_text(size = 15),
      keywidth = 1,
      keyheight = 1,
      override.aes = list(size = 0.8)
    )
  )

ggsave("stacked_narrow.png", width = 9.5, height = 8, bg = "lightgrey")











# --- study area landuse ----
landuse_colors <- c(
  "313" = "#4DFF00",
  "312" = "#00A600",
  "324" = "#A6F200",
  "321" = "#CCF24D",
  "231" = "#E6E64D",
  "211" = "#FFFFA8",
  "512" = "#4DA6FF",
  "411" = "#A6A6FF",
  "412" = "#4D4DFF",
  "131" = "#A600CC",
  "311" = "#80FF00",
  "243" = "#E6CC4D",
  "242" = "#FFE64D",
  "112" = "#FF0000",
  "121" = "#CC4DF2",
  "222" = "#F2A64D"
)

study_area_landuse <- study_area_landuse %>%
  mutate(Proportion = sum / sum(sum))

ggplot(study_area_landuse, aes(x = 1, y = Proportion, fill = Code_18)) +
  geom_bar(stat = "identity", width = 0.5) +
  scale_fill_manual(values = landuse_colors) +
  scale_y_continuous(labels = scales::percent_format(scale = 100)) +
  labs(
    x = "Study area",
    y = "Percentage of land use type",
    fill = "Land use type"
  ) +
  theme_minimal() +
  theme(
    axis.text.x = element_blank(),
    axis.ticks.x = element_blank(),
    axis.text.y = element_text(size = 16),
    axis.title.y = element_text(size = 17, margin = margin(r = 15)),
    axis.title.x = element_text(size = 17, margin = margin(r = 15)),
    legend.title = element_text(size = 17),
    legend.text = element_text(size = 16),
    legend.key.size = unit(0.8, "cm")
  )
ggsave("study_area_landuse.png", width = 5, height = 8, bg = "lightgrey")
