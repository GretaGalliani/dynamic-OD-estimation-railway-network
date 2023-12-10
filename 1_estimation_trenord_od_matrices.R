## GOAL: This script estimate dynamical OD matrices from two datasets provided by Trenord (tickets and passenger counts)
##       applying Furness method
rm(list = ls())
source("utils.R")
set.seed(24091998)

# Stations belong to 6 train lines
lines = c("L1","L2","L3","L4","L5","L6")

L1_stations <- c("S178", "S240", "S273", "S304", "S366", "S389", "S430", "S506", "S547", "S632", "S744", 
                 "S859")
L2_stations <- c("S070", "S085", "S095", "S106", "S154", "S228", "S276", "S547", "S551", "S635", "S685", "S742")
L3_stations <- c("S320", "S547", "S621", "S648", "S651", "S856", "S875")
L4_stations <- c("S025", "S163", "S304", "S414", "S478", "S549", "S625", "S630", "S632", "S661", "S685",
                 "S760", "S767", "S787", "S820", "S859", "S875")
L5_stations <- c("S179", "S276", "S281", "S320", "S414", "S547", "S549", "S685")
L6_stations <- c("S179", "S221", "S414", "S423", "S478", "S549", "S632", "S799", "S820", "S859", "S875")

stations_list_lines <- hash() 
stations_list_lines[["L1"]] <- L1_stations
stations_list_lines[["L2"]] <- L2_stations
stations_list_lines[["L3"]] <- L3_stations
stations_list_lines[["L4"]] <- L4_stations
stations_list_lines[["L5"]] <- L5_stations
stations_list_lines[["L6"]] <- L6_stations

station_codes <- unique(c(L1_stations, L2_stations, L3_stations, L4_stations, L5_stations, L6_stations))

# Define the code of stations with missing data
missing_data_station <- "S799"
# Define the codes of stations in the same municipality
city_area <- c("S179", "S276", "S281", "S549", "S685")
# Define the codes of stations in the missing data area
missing_data_area <- c("S025", "S070", "S106", "S163", "S179", "S228", "S276", "S281", "S414", "S549",
                       "S661", "S685", "S742", "S760", "S767")

# Save
if (!dir.exists("Data/Processed")) dir.create("Data/Processed", recursive = TRUE)
save(list = c("station_codes","city_area", "missing_data_area", "missing_data_station"), file = "Data/Processed/station_codes.Rdata")

#### ESTIMATION OF TRAVEL TIMES ----
# We build estimates of travel times, considering direct paths and paths requiring at most one change of train, 
# which will be needed for the analyses

## COMPUTE THE MEAN TRAVEL TIME FOR EVERY LINE AND EVERY DIRECT OD PATH
# Load timetable data
train_count <- read.csv("Data/train.csv")
train_count <- train_count |>
  mutate_at(vars(TrainCode, Line, DepartureStation, ArrivalStation, StopStation), as.character)


# Estimation of times
for (line in lines){
  line_stations <- stations_list_lines[[line]]
  mat_times <- generate_travel_time_matrix_direct(line, line_stations, train_count)
  
  if (!dir.exists("Data/Processed/Travel_times")) dir.create("Data/Processed/Travel_times", recursive = TRUE)
  write.csv(mat_times, paste0("Data/Processed/Travel_times/mat_times_", line, ".csv"), row.names = F)
}
rm(train_count, mat_times)

## CONSIDER THE WHOLE NETWORK AND FIND THE MINIMUN TRAVEL TIME FOR EACH PAIR CONNECTED BY A DIRECT LINE 
# Load all the time matrices for the 6 train lines
mat_times <- NULL 
for (line in lines){
  line_mat_times <- read.csv(paste0("Data/Processed/Travel_times/mat_times_", line, ".csv"))
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
write.csv(Travel_times, "Data/Processed/Travel_times/total_travel_times.csv", row.names = F)

# Finally, generate the matrix representing the direct travel paths in the network, to be used in the the next steps
Direct_paths <- generate_direct_paths_matrix(Travel_times, station_codes)
write.csv(Direct_paths, "Data/Processed/Travel_times/direct_paths.csv", row.names = F)
rm(Travel_times, Direct_paths)

#### COUNTER DATA AGGREGATION AND ESTIMATION OF MISSING DATA ----
# For each station, I should construct the matrix having boarded and dropped passengers for every week 
# of the study 

# Loading counter data
train_count <- read.csv("Data/train.csv")

# Weeks of the study are
weeks <- unique(sort(as.character(format(as.Date(train_count$MissionDate), "%Y_%W"))))
# I remove partial weeks 
weeks <- weeks[-c(1,length(weeks))]

## BUILDING MARGINAL MATRIX
# I count, for every week and stations, the number of boarded and dropped passengers considering only VALID data
# i.e., counter states in (1)
Marg <- build_marg(station_codes, train_count, weeks)

# Saving marginal 
if (!dir.exists("Data/Processed/Marginals")) dir.create("Data/Processed/Marginals", recursive = TRUE)
write.csv(Marg, "Data/Processed/Marginals/partial_marginals.csv", row.names = FALSE)
rm(Marg)

## COVERAGE COMPUTATION
# I build a dataframe reporting the fraction of trains having valid data with respect to the total number
# of trains, for each week and station
Coverage <- build_coverage(station_codes, train_count, weeks)

write.csv(Coverage, "Data/Processed/Marginals/coverage.csv", row.names = FALSE)
rm(Coverage)

## ESTIMATION OF MISSING COUNTERS DATA
# load Marg data
Marg <- read.csv("Data/Processed/Marginals/partial_marginals.csv")

# load Coverage data 
Cov <- read.csv("Data/Processed/Marginals/coverage.csv")
rm(train_count)

## DATA PREPARATION
# I prepare the dataset to fill the missing marginal data
to_fill_df <- prepare_filling_dataset(station_codes, weeks, Cov, Marg)
write.csv(to_fill_df, "Data/Processed/Marginals/to_fill_marginals.csv", row.names = FALSE)
rm(Cov, Marg)

## FILLING THE MISSING DATA
# Couples station-week are rescaled as Passengers / Coverage to estimate the total number of boarded and dropped passengers
Filled_Marg <- filling_missing_marg_data(to_fill_df, station_codes, weeks)
rm(to_fill_df)

# Saving the result
write.csv(Filled_Marg, "Data/Processed/Marginals/marginals_filled.csv", row.names = FALSE)
rm(Filled_Marg)

#### CONVERSION OF TICKETS INTO ESTIMATED TRIPS ----
# Importing ticket datasets
tickets <- read.csv("Data/ticket.csv")

# I preprocess tickets
tickets <- clean_tickets(tickets, station_codes)

# Apply function to generate OD matrices from tickets data 
dates_tickets <- sort(unique(format(tickets$Date, "%Y-%W")))
OD <- OD_ticket_matrix_build(tickets, dates_tickets)
rm(tickets, dates_tickets)

# Applying the postprocessing function which substitutes negative values with 0 and selects relevant station codes
OD <- postprocess_tickets(OD, station_codes)

# Save result
if (!dir.exists("Data/Processed/Seeds")) dir.create("Data/Processed/Seeds", recursive = TRUE)
write.csv(OD, "Data/Processed/Seeds/OD_seeds_step1.csv", row.names = FALSE)
rm(OD)

#### SEPARATION OF TRIPS REQUIRING AT MOST ONE CHANGE OF TRAIN ----
# Load ticket-estimated OD matrix
OD_tickets <- read.csv("Data/Processed/Seeds/OD_seeds_step1.csv")

# Load travel times dataset
Travel_times <- read.csv("Data/Processed/Travel_times/total_travel_times.csv")

# 1. Remove trips requiring more than one change of train
# 2. Separate the trips requiring one change of train in the optimal station (i.e. station achieving the minimum travel time)
OD_tickets <- separate_trips(OD_tickets, Travel_times)
write.csv(OD_tickets, "Data/Processed/Seeds/OD_seeds_step2.csv", row.names = FALSE)
rm(Travel_times, OD_tickets)

#### ESTIMATION OF MISSING TICKET OD DATA THORUGH GRAVITY MODEL ----
# Load data 
Travel_times <- read.csv("Data/Processed/Travel_times/total_travel_times.csv")
OD_tickets <- read.csv("Data/Processed/Seeds/OD_seeds_step2.csv")
Marg <- read.csv("Data/Processed/Marginals/marginals_filled.csv")

# Prepare dataset for the application of the gravity model
df <- build_gravity_model_dataset(OD_tickets, Travel_times, Marg, missing_data_station, missing_data_area, city_area)
rm(OD_tickets, Travel_times, Marg)

# Fit the gravity model and use it to predict missing ticket-estimated OD data 
OD_tickets <- gravity_model_estimation(df)
rm(df)
write.csv(OD_tickets, "Data/Processed/Seeds/OD_seeds_step3.csv", row.names = FALSE)

#### FURNESS METHOD ----
# The final step is the application of the Furness method for every week of the study period
# Load ticket OD seed
OD_tickets <- read.csv("Data/Processed/Seeds/OD_seeds_step3.csv")

# Load marginals
Marg <- read.csv("Data/Processed/Marginals/marginals_filled.csv")

# Load Direct paths data, needed to identify direct paths
Direct_paths <- read.csv("Data/Processed/Travel_times/direct_paths.csv")

# The weeks are
weeks <- sort(unique(Marg$Week))

# Apply Furness
X <- build_Furness_OD_matrices(OD_tickets, Marg, Direct_paths, station_codes, weeks)

# Saving the results
if (!dir.exists("Data/Processed/IPF")) dir.create("Data/Processed/IPF", recursive = TRUE)
write.csv(X$Errors, "Data/Processed/IPF/IPF_errors.csv", row.names = FALSE)
write.csv(X$OD_Furness, "Data/Processed/IPF/OD_Trenord_IPF.csv", row.names = FALSE)
rm(X, Marg, OD_tickets)
