## GOAL: Compare the new Trenord data with the RL OD matrix, both of 2030 and 2020
rm(list = ls())
source("utils.R")
set.seed(24091998)

## Load stations
stations <- read.csv("Data/Trenord/stations_processed.csv")

# Correct some municipality names in the dataset
stations$Nome <- str_to_title(tolower(stations$Nome))
stations$Nome[which(stations$Nome == "Albano S.alessandro")] <- "Albano S. Alessandro"
stations$Nome[which(stations$Nome == "Cassano D'adda")] <- "Cassano D'Adda"
stations$Nome[which(stations$Nome == "Desenzano Del Garda-Sirmione")] <- "Desenzano del Garda - Sirmione"
stations$Nome[which(stations$Nome == "Grumello Del Monte")] <- "Grumello del Monte"
stations$Nome[which(stations$Nome == "Palazzolo Sull'oglio")] <- "Palazzolo sull'Oglio"
stations$Nome[which(stations$Nome == "Peschiera Del Garda")] <- "Peschiera del Garda"
stations$Nome[which(stations$Nome == "Ponte S.pietro")] <- "Ponte S. Pietro"
stations$Nome[which(stations$Nome == "Sesto S.giovanni")] <- "Sesto S. Giovanni"

#### AGGREGATION OF MUNICIPALITIES INTO STATIONS BASINS ----
## GOAL: construct a correspondance between municipalities and stations, defining stations basins

# Load municipalities dataset
comuni_ISTAT <- read_excel("Data/ISTAT/istat_codes_2022.xls")
colnames(comuni_ISTAT) <- make.names(colnames(comuni_ISTAT))
comuni_ISTAT <- comuni_ISTAT |> filter(Denominazione.Regione %in% c("Lombardia","Veneto"))
suppressed_ISTAT_comuni <- read_excel("Data/ISTAT/suppressed_municipalities.xls")

# Load distance dataset
net1 <- read.csv2("Data/ISTAT/Lombardia_distances.txt")
net2 <-  read.csv2("Data/ISTAT/Veneto_distances.txt")
net <- rbind(net1, net2)
rm(net1, net2)
comuni_distances <- distances_cleaned(net, comuni_ISTAT, suppressed_ISTAT_comuni)

if (!dir.exists("Data/ISTAT/Processed")) dir.create("Data/ISTAT/Processed", recursive = TRUE)
write.csv(comuni_distances, "Data/ISTAT/Processed/distances_PROCESSED.csv", row.names = FALSE)
rm(net)

# load stations
load("Data/Trenord/Processed/station_codes.Rdata")

# Creating dataset for stations correspondances
Stations_correspondances <- build_stations_correspondances(comuni_ISTAT, stations, comuni_distances, IS_area)

if (!dir.exists("Data/Trenord/Processed/OD_comparison")) dir.create("Data/Trenord/Processed/OD_comparison", recursive = TRUE)
write.csv(Stations_correspondances, "Data/Trenord/Processed/OD_comparison/stations_correspondances.csv", row.names = FALSE)
rm(comuni_ISTAT, comuni_distances, Stations_correspondances)

#### PREPROCESSING OF RL DATA ----
# Load OD dataset of Regione Lombardia
OD_2020 <- read.csv("Data/RegioneLombardia/matriceOD_lombardia_2020.csv")

# Apply cleaning function to OD
OD_2020 <- OD_Lombardia_cleaned(OD_2020)
if (!dir.exists("Data/RegioneLombardia/Processed")) dir.create("Data/RegioneLombardia/Processed", recursive = TRUE)
write.csv(OD_2020, "Data/RegioneLombardia/Processed/OD_2020_cleaned.csv", row.names = F)
OD <- read.csv("Data/RegioneLombardia/Processed/OD_2020_cleaned.csv")

# Generate correspondances between OD areas and ISTAT municipalities
comuni_ISTAT <- read_excel("Data/ISTAT/istat_codes_2022.xls")
matches <- generate_matches_OD_municipalities(OD, comuni_ISTAT)
rm(comuni_ISTAT)
write.csv(matches, "Data/RegioneLombardia/Processed/matches_municipalities_ODarea.csv", row.names = F)

#### COMPARISON BY MEANS OF CORRELATION AND LINEAR REGRESSION - RL 2020 ----
## GOAL: compare Regione Lombardia and Trenord mobility data

# Load correspondances between OD Regione and comuni
C_Lombardia_comuni <- read.csv("Data/RegioneLombardia/Processed/matches_municipalities_ODarea.csv")

# Load correspondances between comuni and stazioni
C_comuni_stations <- read.csv("Data/Trenord/Processed/OD_comparison/stations_correspondances.csv")

# Load stations codes
load("Data/Trenord/Processed/station_codes.Rdata")

# Join
matches <- C_Lombardia_comuni |> left_join(C_comuni_stations, by = c("ISTAT_code" = "Comune_code")) |>
  dplyr::select(-Comune_name.y)
colnames(matches)[2] <- "Comune_name"

# Selecting only municipalities belonging to a station of my area
# I exclude municipalities whose travel time to the closest station is more than 30 minutes
# I exclude stations in Veneto since I cannot accurately estimate their station's basins and OD RL flows
stations <- stations |> dplyr::filter(Codice %in% c(station_codes))
matches <- matches |> dplyr::filter(Station_ref_name %in% c(stations$Comune, "IS area") & Station_ref_minutes <= 30 &
                                      !(Station_ref_name %in% c("Verona", "Peschiera del Garda")))

## LOAD OD (Lombardia)
OD_RL <- read.csv("Data/RegioneLombardia/Processed/OD_2020_cleaned.csv")

# Select and aggregate data at BreBeMi level
# I aggregate IS area since it has no sense to compare internal cells with RL data, only external comparison is possible
OD_RL <- aggregate_OD_RL_BreBeMi(OD_RL, matches, stations, IS_area)

# Load OD Trenord aggregated
OD_Trenord <- read.csv("Data/Trenord/Processed/IPF/OD_Trenord_IPF.csv")

# Aggregate stations in the same municipality (Bergamo and Treviglio) and save
# Moreover, I aggregate the IS area to have suitable comparison with the RL OD matrix 
OD_Trenord <- aggregate_municipalities_Trenord(OD_Trenord, stations, station_codes, IS_area)
write.csv(OD_Trenord, "Data/Trenord/Processed/OD_comparison/OD_Trenord_municipalities_aggregated.csv", row.names = F)

# Get weeks
weeks <- sort(unique(colnames(OD_Trenord)[-c(1,2)]))
weeks <- substr(weeks, 6, nchar(weeks))

# Perform correlation analysis and save result
# Comparison is made excluding undirect paths
df <- correlation_OD_analysis(OD_Trenord, OD_RL, weeks)
write.csv(df, "Data/Trenord/Processed/OD_comparison/comparison_Trenord_RL_2020.csv", row.names = F)
