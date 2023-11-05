## GOAL: This script estimate dynamical OD matrices from two datasets provided by Trenord (tickets and passenger counts)
##       applying Furness method
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

# Stations belong to 6 train lines
# R1, R2, R4, R14, RE2, RE6
lines = c("R1","R2","R4","R14","RE_2","RE_6")

R1_stations <- c("S01717", "S01716", "S01714", "S01540", "S01539", "S01538", "S01537", "S01536", "S01535",
                 "S01534", "S01533", "S01529")
R2_stations <- c("S01529", "S01612", "S01611", "S01600", "S01608", "S01601", "S01708")
R4_stations <- c("S01717", "S01716", "S01714", "S01713", "S01712", "S01711", "S01710", "S01709", "S01708",
                 "S01707", "S01706", "S01722", "S01705", "S01704", "S01703", "S01701", "S01326")
R14_stations <- c("S01645", "S01326", "S01325", "S01322", "S01510", "S01511", "S01502", "S01503", "S01504",
                  "S01528", "S01530", "S01529")
RE2_stations <- c("S01645", "S01639", "S01700", "S01326", "S01701", "S01703", "S01600", "S01529")
RE6_stations <- c("S01700", "S01701", "S01703", "S01708", "S01711", "S01713", "S01714", "S01717", "S02084", 
                  "S02088", "S02430")

stations_list_lines <- hash() 
stations_list_lines[["R1"]] <- R1_stations
stations_list_lines[["R2"]] <- R2_stations
stations_list_lines[["R4"]] <- R4_stations
stations_list_lines[["R14"]] <- R14_stations
stations_list_lines[["RE_2"]] <- RE2_stations
stations_list_lines[["RE_6"]] <- RE6_stations

station_codes <- unique(c(R1_stations, R2_stations, R4_stations, R14_stations, RE2_stations, RE6_stations))

# Define the code of station Verona Porta Nuova
Verona <- "S02430"
# Define the codes of stations in Milan
Milan_area <- c("S01326", "S01639", "S01645", "S01700", "S01701")
# Define the codes of stations in the Integrated Subscriptions area
IS_area <- c("S01322", "S01325", "S01326", "S01510", "S01511", "S01639", "S01645", "S01700", "S01701", "S01703",
             "S01704", "S01705", "S01706", "S01707", "S01722")

# Save
if (!dir.exists("Data/Trenord/Processed")) dir.create("Data/Trenord/Processed", recursive = TRUE)
save(list = c("station_codes","Milan_area", "IS_area", "Verona"), file = "Data/Trenord/Processed/station_codes.Rdata")

#### ESTIMATION OF TRAVEL TIMES ----
# We build estimates of travel times, considering direct paths and paths requiring at most one change of train, 
# which will be needed for the analyses

## COMPUTE THE MEAN TRAVEL TIME FOR EVERY LINE AND EVERY DIRECT OD PATH
# Load timetable data
train_count <- read.csv("Data/Trenord/counter_data_2022.csv", sep = ";")

# Estimation of times
for (line in lines){
  line_stations <- stations_list_lines[[line]]
  mat_times <- generate_travel_time_matrix_direct(line, line_stations, train_count)
  
  if (!dir.exists("Data/Trenord/Processed/Travel_times")) dir.create("Data/Trenord/Processed/Travel_times", recursive = TRUE)
  write.csv(mat_times, paste0("Data/Trenord/Processed/Travel_times/Mat_times", line, ".csv"), row.names = F)
}
rm(train_count, mat_times)

## CONSIDER THE WHOLE NETWORK AND FIND THE MINIMUN TRAVEL TIME FOR EACH PAIR CONNECTED BY A DIRECT LINE 
# Load all the time matrices for the 6 train lines
mat_times <- NULL 
for (line in lines){
  line_mat_times <- read.csv(paste0("Data/Trenord/Processed/Travel_times/mat_times", line, ".csv"))
  mat_times <- rbind(mat_times, line_mat_times)
}

# For OD pairs connected by more than one line, consider as travel.time the time of the fastest line
direct_times <- mat_times %>%
  group_by(Start, End) %>%
  filter(Mean.travel.time == min(Mean.travel.time)) %>%
  ungroup()
rm(mat_times)

## FOR EVERY OD PAIR NOT CONNECTED BY A LINE, FIND THE OPTIMAL CHANGING STATION AND THE MEAN TRAVEL TIME 
Travel_times <- generate_travel_time_matrix(station_codes, direct_times)
write.csv(Travel_times, "Data/Trenord/Processed/Travel_times/total_travel_times.csv", row.names = F)

# Finally, generate the matrix representing the direct travel paths in the network, to be used in the the next steps
Direct_paths <- generate_direct_paths_matrix(Travel_times, station_codes)
write.csv(Direct_paths, "Data/Trenord/Processed/Travel_times/direct_paths.csv", row.names = F)
rm(Travel_times, Direct_paths)

#### COUNTER DATA AGGREGATION AND ESTIMATION OF MISSING DATA ----
# For each station, I should construct the matrix having boarded and dropped passengers for every week 
# of the study 

# Loading counter data
train_count <- read.csv("Data/Trenord/counter_data_2022.csv", sep = ";")

# Weeks of the study are
weeks <- unique(sort(as.character(format(as.Date(train_count$Data.missione), "%Y_%W"))))
# I remove partial weeks 
weeks <- weeks[-c(1,length(weeks))]

## BUILDING MARGINAL MATRIX
# I count, for every week and stations, the number of boarded and dropped passengers considering only VALID data
# i.e., counter states in (0,1,2,3,4)
Marg <- build_marg(station_codes, train_count, weeks)

# Saving marginal 
if (!dir.exists("Data/Trenord/Processed/Marginals")) dir.create("Data/Trenord/Processed/Marginals", recursive = TRUE)
write.csv(Marg, "Data/Trenord/Processed/Marginals/partial_marginals.csv", row.names = FALSE)
rm(Marg)

## COVERAGE COMPUTATION
# I build a dataframe reporting the fraction of trains having valid data with respect to the total number
# of trains, for each week and station
Coverage <- build_coverage(station_codes, train_count, weeks)

write.csv(Coverage, "Data/Trenord/Processed/Marginals/coverage.csv", row.names = FALSE)
rm(Coverage)

## RESTIMATION OF MISSING COUNTERS DATA
# load Marg data
Marg <- read.csv("Data/Trenord/Processed/Marginals/partial_marginals.csv")

# load Coverage data 
Cov <- read.csv("Data/Trenord/Processed/Marginals/coverage.csv")
rm(train_count)

## DATA PREPARATION
# I prepare the dataset to fill the missing marginal data
to_fill_df <- prepare_filling_dataset(station_codes, weeks, Cov, Marg)
write.csv(to_fill_df, "Data/Trenord/Processed/Marginals/to_fill_marginals.csv", row.names = FALSE)
rm(Cov, Marg)

## FILLING THE MISSING DATA
# Couples station-week are rescaled as Passengers / Coverage to estimate the total number of boarded and dropped passengers
Filled_Marg <- filling_missing_marg_data(to_fill_df, station_codes, weeks)
rm(to_fill_df)

# Saving the result
write.csv(Filled_Marg, "Data/Trenord/Processed/Marginals/marginals_filled.csv", row.names = FALSE)
rm(Filled_Marg)

#### CONVERSION OF TICKETS INTO ESTIMATED TRIPS ----
# Importing ticket datasets
tickets <- read.csv("Data/Trenord/ticket_data_2022.csv", sep=";")

# I preprocess tickets
tickets <- clean_tickets(tickets, station_codes)

# Apply function to generate OD matrices from tickets data 
dates_tickets <- sort(unique(format(tickets$Date, "%Y-%W")))
OD <- OD_ticket_matrix_build(tickets, dates_tickets)
rm(tickets, dates_tickets)

# Applying the postprocessing function which substitutes negative values with 0 and selects relevant station codes
OD <- postprocess_tickets(OD, station_codes)

# Save result
if (!dir.exists("Data/Trenord/Processed/Seeds")) dir.create("Data/Trenord/Processed/Seeds", recursive = TRUE)
write.csv(OD, "Data/Trenord/Processed/Seeds/OD_seeds_step1.csv", row.names = FALSE)
rm(OD)

#### SEPARATION OF TRIPS REQUIRING AT MOST ONE CHANGE OF TRAIN ----
# Load ticket-estimated OD matrix
OD_tickets <- read.csv("Data/Trenord/Processed/Seeds/OD_seeds_step1.csv")

# Load travel times dataset
Travel_times <- read.csv("Data/Trenord/Processed/Travel_times/total_travel_times.csv")

# 1. Remove trips requiring more than one change of train
# 2. Separate the trips requiring one change of train in the optimal station (i.e. station achieving the minimum travel time)
OD_tickets <- separate_trips(OD_tickets, Travel_times)
write.csv(OD_tickets, "Data/Trenord/Processed/Seeds/OD_seeds_step2.csv", row.names = FALSE)
rm(Travel_times, OD_tickets)

#### ESTIMATION OF MISSING TICKET OD DATA THORUGH GRAVITY MODEL ----
# Load data 
Travel_times <- read.csv("Data/Trenord/Processed/Travel_times/total_travel_times.csv")
OD_tickets <- read.csv("Data/Trenord/Processed/Seeds/OD_seeds_step2.csv")
Marg <- read.csv("Data/Trenord/Processed/Marginals/marginals_filled.csv")

# Prepare dataset for the application of the gravity model
df <- build_gravity_model_dataset(OD_tickets, Travel_times, Marg, Verona, IS_area, Milan_area)
rm(OD_tickets, Travel_times, Marg)

# Fit the gravity model and use it to predict missing ticket-estimated OD data 
OD_tickets <- gravity_model_estimation(df)
rm(df)
write.csv(OD_tickets, "Data/Trenord/Processed/Seeds/OD_seeds_step3.csv", row.names = FALSE)

#### FURNESS METHOD ----
# The final step is the application of the Furness method for every week of the study period
# Load ticket OD seed
OD_tickets <- read.csv("Data/Trenord/Processed/Seeds/OD_seeds_step3.csv")

# Load marginals
Marg <- read.csv("Data/Trenord/Processed/Marginals/marginals_filled.csv")

# Load Direct paths data, needed to identify direct paths
Direct_paths <- read.csv("Data/Trenord/Processed/Travel_times/direct_paths.csv")

# The weeks are
weeks <- sort(unique(Marg$Week))

# Apply Furness
X <- build_Furness_OD_matrices(OD_tickets, Marg, Direct_paths, station_codes, weeks)

# Saving the results
if (!dir.exists("Data/Trenord/Processed/IPF")) dir.create("Data/Trenord/Processed/IPF", recursive = TRUE)
write.csv(X$Errors, "Data/Trenord/Processed/IPF/IPF_errors.csv", row.names = FALSE)
write.csv(X$OD_Furness, "Data/Trenord/Processed/IPF/OD_Trenord_IPF.csv", row.names = FALSE)
rm(X, Marg, OD_tickets)
