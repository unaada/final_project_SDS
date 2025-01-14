attach(moose_collars_gps)

# libraries
library(sf) #younger
library(ctmm)
library(dplyr)
library(raster)
library(scales)
library(terra)
library(readxl)


# --- full dataset DONT CHANGE ----
estonian <- '+proj=lcc +lat_0=57.5175539305556 +lon_0=24 +lat_1=59.3333333333333 +lat_2=58 +x_0=500000 +y_0=6375000 +ellps=GRS80 +towgs84=0,0,0,0,0,0,0 +units=m +no_defs +type=crs'

moose_subset_spat <- st_as_sf(moose_collars_gps, coords = c("Lon", "Lat"), crs = 4326)
moose_subset_spat <- st_transform(moose_subset_spat, crs = 32635)

coordinates <- st_coordinates(moose_subset_spat)

moose_subset_spat$Easting <- coordinates[, "X"]
moose_subset_spat$Northing <- coordinates[, "Y"]

moose_df_simple <- data.frame(
  id = as.factor(moose_subset_spat$CollarID),
  UTM.Easting = moose_subset_spat$Easting,
  UTM.Northing = moose_subset_spat$Northing,
  UTM.zone = "35 +north",
  timestamp = as.POSIXct(moose_subset_spat$LMT, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Tallinn")
  
)


moose_df <- na.omit(moose_df)
tel <- as.telemetry(moose_df, drop = FALSE, crs = 32635, projection = estonian, timezone = "Europe/Tallinn")


fits_subset <- ctmm.fit(tel$"39794")
summary(tel)
akde_subset <- akde(tel$"39794", fits_subset)
summary(akde_subset)

plot(akde_subset)
head(akde_subset)






#--- autocorrelation exists INDIVIDUAL ----

tel_individual <- tel$"39794"
guess <- ctmm.guess(tel_individual, interactive = TRUE)
model <- ctmm.select(tel_individual, guess)
summary(model)


vario <- variogram(tel_individual)
plot(vario, CTMM = guess)


akde_result <- akde(tel_individual, CTMM = model)


plot(akde_result, level = 0.95)
plot(tel_individual)











#--- no restriction moose  ----

#  empty lists to store results and plots
results <- list()
akde_plots <- list()
variogram_plots <- list()

individuals <- c("39793", "39790", "39787")


for (id in individuals) {
  try({
    # extract the individual's telemetry data
    tel_individual <- tel[[id]]
    
    # initial model guess
    guess <- ctmm.guess(tel_individual, interactive = FALSE)
    
    # fit the model
    model <- ctmm.select(tel_individual, guess)
    
    # calculate the variogram
    vario <- variogram(tel_individual)
    
    # record and save the variogram plot
    plot(vario, CTMM = guess)
    variogram_plots[[id]] <- recordPlot()
    
    
    # compute the AKDE
    akde_result <- akde(tel_individual, CTMM = model, grid = list(dr=30, align.to.origin = TRUE))
    
    # record and save the AKDE plot
    plot(akde_result, level = 0.95)
    akde_plots[[id]] <- recordPlot()  # Save the plot in memory
    
    # store the results
    results[[id]] <- list(model = model, akde = akde_result)
  }, silent = TRUE)
}


replayPlot(akde_plots[["39793"]])
replayPlot(variogram_plots[["39793"]])


# export
writeVector(results[["39787"]]$akde,"39787_grid.gpkg",filetype="GPKG",convex=FALSE,level.UD=0.95)
writeVector(results[["39790"]]$akde,"39790_grid.gpkg",filetype="GPKG",convex=FALSE,level.UD=0.95)
writeVector(results[["39793"]]$akde,"39793_grid.gpkg",filetype="GPKG",convex=FALSE,level.UD=0.95)

# export core area
writeVector(results[["39787"]]$akde,"39787_50.gpkg",filetype="GPKG",convex=FALSE,level.UD=0.5)
writeVector(results[["39790"]]$akde,"39790_50.gpkg",filetype="GPKG",convex=FALSE,level.UD=0.5)
writeVector(results[["39793"]]$akde,"39793_50.gpkg",filetype="GPKG",convex=FALSE,level.UD=0.5)











#--- roads as barrier ----

study_area_bottom <- st_read("sa_bottom.gpkg", crs = 3301)
study_area_bottom <- st_zm(study_area_bottom, drop=TRUE, what = 'ZM')
study_area_bottom <- as_Spatial(study_area_bottom)


study_area_a5 <- st_read("39789_polygon_boundary.gpkg", crs = 3301)
study_area_a5 <- st_zm(study_area_a5, drop=TRUE, what = 'ZM')
study_area_a5 <- as_Spatial(study_area_a5)



#--- 39789 in the top polygon  ----
tel_39789 <- tel[["39789"]] 

guess_39789 <- ctmm.guess(tel_39789, interactive = FALSE)
model_39789 <- ctmm.select(tel_39789, guess_39789, verbose = FALSE)
akde_result_39789 <- akde(tel_39789, CTMM = model_39789, grid = list(dr=30, align.to.origin = TRUE), SP = study_area_a5)
plot(akde_result_39789, level = 0.95)

writeVector(akde_result_39789,"39789-grid.gpkg",filetype="GPKG",convex=FALSE,level.UD=0.95)
writeVector(akde_result_39789,"39789_50.gpkg",filetype="GPKG",convex=FALSE,level.UD=0.5)

plot(tel_39789, akde_result_39789, level = 0.95)






  #--- the bottom polygon as barrier ----
results_restricted <- list()
akde_plots_restricted <- list()
variogram_plots_restricted <- list()

# List of individuals to process
individuals_to_process <- c("39785", "39788", "39791")

# Loop through the selected moose individuals
for (id in individuals_to_process) {
  try({
    # individual
    tel_individual <- tel[[id]]
    
    #initial model guess for the telemetry data
    guess <- ctmm.guess(tel_individual, interactive = FALSE)
    
    # fit model
    model <- ctmm.select(tel_individual, guess)
    
    # variogram
    vario <- variogram(tel_individual)
    plot(vario, CTMM = guess)
    variogram_plots_restricted[[id]] <- recordPlot()  # Save the plot in memory
    
    # AKDE within the restricted area (Tartu-Tallinn bottom part )
    akde_result <- akde(tel_individual, CTMM = model, grid = list(dr=30, align.to.origin = TRUE), SP = study_area_bottom)
    
    # record AKDE plot
    plot(akde_result, level = 0.95)
    akde_plots_restricted[[id]] <- recordPlot()  # Save the plot in memory
    
    # results
    results_restricted[[id]] <- list(model = model, akde = akde_result)
  }, silent = TRUE)
}

replayPlot(akde_plots_restricted[["39785"]])
replayPlot(variogram_plots_restricted[["39788"]])

#export the new stuff

writeVector(results_restricted[["39785"]]$akde,"39785-grid.gpkg",filetype="GPKG",convex=FALSE,level.UD=0.95)
writeVector(results_restricted[["39788"]]$akde,"39788-grid.gpkg",filetype="GPKG",convex=FALSE,level.UD=0.95)
writeVector(results_restricted[["39791"]]$akde,"39791-grid.gpkg",filetype="GPKG",convex=FALSE,level.UD=0.95)

# export the core area
writeVector(results_restricted[["39785"]]$akde,"39785_50.gpkg",filetype="GPKG",convex=FALSE,level.UD=0.5)
writeVector(results_restricted[["39788"]]$akde,"39788_50.gpkg",filetype="GPKG",convex=FALSE,level.UD=0.5)
writeVector(results_restricted[["39791"]]$akde,"39791_50.gpkg",filetype="GPKG",convex=FALSE,level.UD=0.5)






# --- example of different models  ----
tel_39794 <- tel[["39794"]] 

guess <- ctmm.guess(tel_39794, interactive = FALSE)
models <- ctmm.select(tel_39794, guess,verbose = TRUE)

results_39794 <- list()
akde_plots_39794 <- list()
variogram_plots_39794 <- list()


for (model_name in names(models)) {
  try({
    model <- models[[model_name]]
    
    # compute the variogram and save the plot
    vario <- variogram(tel_39794)
    plot(vario, CTMM = model)
    variogram_plots_39794[[model_name]] <- recordPlot()  # Save the plot
    
    # compute AKDE for this model
    akde_result <- akde(tel_39794, CTMM = model, res = 10)
    
    # store the AKDE result
    results_39794[[model_name]] <- list(model = model, akde = akde_result)
    
    # plot the AKDE result and save
    plot(akde_result, level = 0.95, main = paste("AKDE for Model:", model_name))
    akde_plots_39794[[model_name]] <- recordPlot()  # Save the plot
    
  }, silent = TRUE)
}


replayPlot(akde_plots_39794[["OU"]]) # plot different models
replayPlot(variogram_plots_39794[["OU"]])

lapply(results_39794, function(res) summary(res$akde))
summary(models)


writeVector(results_39794[["OUF"]]$akde, "39794_OUF", filetype = "ESRI Shapefile", convex = FALSE, level.UD = 0.95, level = 0.95)













# --- splitting data for 39794 ----
# import the data
moose_collars_gps <- read_excel("D:/Users/amand/OneDrive - Tartu Ülikool/Masters/Masters_thesis/data/original/moose_collars_gps.xlsx",
                                col_types = c("skip", "skip", "skip", 
                                              "skip", "skip", "skip", "skip", "skip",
                                              "skip", "numeric", "numeric", "skip",
                                              "skip", "skip", "skip", "numeric", 
                                               "skip", "text", "skip", "skip"))
moose_collars_gps$LMT <- as.POSIXct(moose_subset_spat$LMT, format = "%Y-%m-%d %H:%M:%S", tz = "UTC")


#--- attempting to align the grids



#subset the dates when in winter habitat
winter_data_grid <- subset(
  moose_collars_gps,
  CollarID == "39794" & 
    ((LMT > "2018-11-25 00:00:00" & LMT < "2019-02-02 00:00:00") |
       (LMT > "2019-03-12" & LMT < "2019-04-13"))
)

# Correct CRS and projection for winter
winter_sf_grid <- st_transform(st_as_sf(winter_data_grid, coords = c("Lon", "Lat"), crs = 4326), crs = 32635) ## change of CRS
winter_coordinates_grid <- st_coordinates(winter_sf_grid)
winter_sf_grid$Easting <- winter_coordinates_grid[, "X"]
winter_sf_grid$Northing <- winter_coordinates_grid[, "Y"]

# Reformat in the way that as.telemetry accepts
winter_df_grid <- data.frame(
  id = as.factor(winter_sf_grid$CollarID),
  UTM.Easting = winter_sf_grid$Easting,
  UTM.Northing = winter_sf_grid$Northing,
  UTM.zone = "35 +north",
  timestamp = as.POSIXct(winter_sf_grid$LMT, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Tallinn")
)

# Clean the data
winter_df_grid <- na.omit(winter_df_grid)

# Get telemetry
winter_tel_grid <- as.telemetry(winter_df_grid, 
                                drop = FALSE, 
                                crs = 32635, 
                                projection = estonian, 
                                timezone = "Europe/Tallinn") # testing

# Model selection and estimation
guess_winter_grid <- ctmm.guess(winter_tel_grid[["39794"]], interactive = FALSE)
model_winter_grid <- ctmm.select(winter_tel_grid[["39794"]], guess_winter_grid)
akde_result_winter_grid <- akde(winter_tel_grid[["39794"]], 
                                CTMM = model_winter_grid, 
                                grid = list(dr = 30, align.to.origin = TRUE))

# Export results
writeRaster(akde_result_winter_grid,
            "39794_winter_grid.tif",
            filetype = "GTiff",
            level.UD = 0.95,
            DF = "PDF")
#export
writeVector(akde_result_winter_grid,
            "39794_winter_grid.gpkg",
            filetype = "GPKG",
            level.UD = 0.95, 
            overwrite=TRUE)

writeVector(akde_result_winter_grid,
            "39794_winter_50.gpkg",
            filetype = "GPKG",
            level.UD = 0.5, 
            overwrite=TRUE)


###---------- Correct CRS and projection SUMMER 94----




# Subset the dates when in summer habitat
summer_data_grid <- subset(
  moose_collars_gps,
  CollarID == "39794" & 
    ((LMT > "2019-02-04 00:00:00" & LMT < "2019-03-09 00:00:00") |
       (LMT > "2019-04-16" & LMT < "2019-09-02"))
)

# Make as sf to apply coordinates attributes, transform, so units would be meters
summer_sf_grid <- st_transform(st_as_sf(summer_data_grid, coords = c("Lon", "Lat"), crs = 4326), crs = 32635) ## change of crs
summer_coordinates_grid <- st_coordinates(summer_sf_grid)
summer_sf_grid$Easting <- summer_coordinates_grid[, "X"]
summer_sf_grid$Northing <- summer_coordinates_grid[, "Y"]

# Reformat in the way that as.telemetry accepts
summer_df_grid <- data.frame(
  id = as.factor(summer_sf_grid$CollarID),
  UTM.Easting = summer_sf_grid$Easting,
  UTM.Northing = summer_sf_grid$Northing,
  UTM.zone = "35 +north",
  timestamp = as.POSIXct(summer_sf_grid$LMT, format = "%Y-%m-%d %H:%M:%S", tz = "Europe/Tallinn")
)

# Clean the data
summer_df_grid <- na.omit(summer_df_grid)

# Get telemetry
summer_tel_grid <- as.telemetry(summer_df_grid,
                           drop = FALSE,
                           crs = 32635,
                           projection = estonian,
                           timezone = "Europe/Tallinn") #testing

guess_summer_grid <- ctmm.guess(summer_tel_grid[["39794"]], interactive = FALSE)
model_summer_grid <- ctmm.select(summer_tel_grid[["39794"]], guess_summer_grid)
akde_result_summer_grid <- akde(summer_tel_grid[["39794"]],
                           CTMM = model_summer_grid,
                           grid = list(dr=30, align.to.origin = TRUE))

#export
writeVector(akde_result_summer_grid,
            "39794_summer_grid.gpkg",
            filetype = "GPKG",
            level.UD = 0.95, 
            overwrite=TRUE)

writeVector(akde_result_summer_grid,
            "39794_summer_50.gpkg",
            filetype = "GPKG",
            level.UD = 0.5, 
            overwrite=TRUE)


#--- testing different models & evaluation methods ----
tel_39793 <- tel[["39793"]] 

guess <- ctmm.guess(tel_39793, interactive = FALSE)
models <- ctmm.select(tel_39793, guess,verbose = TRUE)















# second period
#!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!!! This needs to be weighted later
#subset the dates when in winter habitat
# first_data <- subset(
#   moose_collars_gps,
#   CollarID == "39791" & 
#     ((LMT > "2018-11-23 00:00:00" & LMT < "2018-12-30 00:00:00") |
#        (LMT > "2019-01-08" & LMT < "2019-03-19") |
#        (LMT > "2019-05-17" & LMT < "2019-09-02"))
# )
# 

