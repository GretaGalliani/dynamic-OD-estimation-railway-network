## Auxiliary functions for my analysis - Estimation of Trenord Origin-Destination matrices
library(dplyr)
options(dplyr.summarise.inform = FALSE)
library(tidyr)
library(hash)
library(tibble)
library(ggplot2)
library(spdep)
library(gridExtra)
library(ggpubr)
library(stringr)
library(lubridate)
library(progress)
library(readxl)
library(mipfp)

#### ESTIMATION OF TRAVEL TIMES ----
# Function to compute the mean travel time between two stations s1 (origin) and s2 (destination) of the same line using data collected in train_count
mean_travel_time <- function(s1, s2, line, train_count){
  # Generate travel code keeping account of the day to match trains
  train_count$Mycode <- paste0(as.Date(as.Date(train_count$MissionDate), format = "%d_%m"), train_count$TrainCode)
  
  # 1. Select train starting from s1 and not cancelled
  set1 <- train_count |> filter(StopStation == s1 & Status %in% c(0,1) & Line == line) |>
    select(Mycode, StopStation, ExitTime, StopIndex)
  colnames(set1) <- c("Mycode", "Station.start", "Time.start", "StopIndex.start")
  # 2. Select train ending in s1 and not cancelled
  set2 <- train_count |> filter(StopStation == s2 & Mycode %in% unique(set1$Mycode) & Status %in% c(0,1) & Line == line)  |>
    select(Mycode, StopStation, EntryTime, StopIndex)
  colnames(set2) <- c("Mycode", "Station.end", "Time.end", "StopIndex.end")
  
  # Match trains
  times <- set1 |> filter(Mycode %in% unique(set2$Mycode)) |> full_join(set2, by = "Mycode") |> filter(StopIndex.end > StopIndex.start) |>
    mutate(Travel.time = as.numeric(difftime(as.POSIXct(Time.end, format="%Y-%m-%d %H:%M:%S"), as.POSIXct(Time.start, format="%Y-%m-%d  %H:%M:%S"), units = "mins"))) |>
    # Remove difference in times outside of the IQR range since I want to account for normal situations
    filter(Travel.time <= quantile(Travel.time, 0.95, na.rm = T) & Travel.time >= quantile(Travel.time, 0.05, na.rm = T))
  
  return(mean(times$Travel.time))
}

# Function to generate the matrix of travel times for a line, given its stations line_stations and data about train rides in train_count
generate_travel_time_matrix_direct <- function(line, line_stations, train_count){
  # For all the station pairs in the line
  station_pairs <- expand.grid(Start = line_stations, End = line_stations) %>%
    filter(Start != End)
  
  # Compute the mean travel time using function mean_travel_time
  mat_times <- station_pairs %>%
    mutate(Mean.travel.time = mapply(
      function(s1, s2) mean_travel_time(s1, s2, line, train_count),
      Start, End
    )) |>
    # Eliminate NA values. NOTE: NA happens when some stations are not connected in the line. Lines R4 and RE_2 have some stations with this behavior
    filter(!is.na(Mean.travel.time))
  
  return(mat_times)
}

# Function that finds the optimal changing station for stations s1 (Start) and s2 (End) not connected by a direct train line
optimal_change <- function(s1, s2, direct_times){
  candidates <- direct_times |> filter(Start == s1) |> pull(End)
  candidates <- direct_times |> filter(Start %in% candidates, End == s2) |> pull(Start)
  
  # If there is no possible change with only one changing station, the function stops with an error
  if (length(candidates) == 0){
    # print(paste("There exists no single change station for starting station", s1, "and ending station", s2))
    return(list(travel_time_change = NA, optimal_change_station = NA))
  }
  
  # Otherwise, we find the travel time considering the change for each of the possible candidate change station and summing the direct times
  df <- data.frame(Change.station = candidates) |>
    rowwise() |>
    mutate(
      Mean.travel.time = sum(
        direct_times[direct_times$Start == s1 & direct_times$End == Change.station,"Mean.travel.time"],
        direct_times[direct_times$Start == Change.station & direct_times$End == s2,"Mean.travel.time"]
      )
    )
  
  # The travel time is the minimum
  travel_time_change <- min(df$Mean.travel.time)
  
  # The optimal change station is 
  optimal_change_station <- df |> filter(Mean.travel.time == travel_time_change) |> pull(Change.station)
  
  return(list(travel_time_change = travel_time_change, optimal_change_station = optimal_change_station))
}

# Function to generate the Travel_times dataframe, infering the optimal change station for every path requiring exaclty one 
# change of train 
generate_travel_time_matrix <- function(station_codes, direct_times){
  Travel_times <- as.data.frame(expand.grid(Start = station_codes, End = station_codes) |> 
                                  mutate(Start = as.character(Start), End = as.character(End)) |> 
                                  filter(Start != End) |> 
                                  mutate(Mean.travel.time = NA, Optimal.change.station = NA))
  
  # Create progress bar
  pb <- progress_bar$new(
    format = "  computing [:bar] :percent eta: :eta",
    total = dim(Travel_times)[1], clear = FALSE, width= 60)
  
  for (i in 1:dim(Travel_times)[1]){
    s1 <- Travel_times[i,"Start"]
    s2 <- Travel_times[i,"End"]
    
    # If the two stations are connected by a direct line, I already know the mean travel time
    # I put 0 in the column Optimal.change.station to indicate that the two stations are directly connected
    if (any(direct_times$Start == s1 & direct_times$End == s2)){
      Travel_times[i,"Mean.travel.time"] <- direct_times[direct_times$Start == s1 & direct_times$End == s2, "Mean.travel.time"]
      Travel_times[i,"Optimal.change.station"] <- "0"
    }
    # ELSE, I apply the function to find the optimal change station and travel time
    else{
      r <- optimal_change(s1, s2, direct_times)
      Travel_times[i,"Mean.travel.time"] <- r$travel_time_change
      Travel_times[i,"Optimal.change.station"] <- r$optimal_change_station
    }
    
    pb$tick()
  }
  
  return(Travel_times)
}

# Function to generate the matrix expressing the direct travel paths in the network
generate_direct_paths_matrix <- function(Travel_times, station_codes){
  station_codes <- sort(station_codes)
  
  Direct_paths <- Travel_times
  Direct_paths$Direct <- ifelse(Direct_paths$Optimal.change.station == 0, 1, 0)
  Direct_paths <- Direct_paths |>
    filter(Start %in% station_codes, End %in% station_codes) |>
    select(Start, End, Direct) |>
    pivot_wider(names_from = End, values_from = Direct, values_fill = 0) |>
    column_to_rownames(var = "Start") 
  
  Direct_paths <-  replace(Direct_paths, is.na(Direct_paths), 0)
  
  # Ordering 
  Direct_paths <- Direct_paths[, order(colnames(Direct_paths))]
  Direct_paths <- Direct_paths[order(rownames(Direct_paths)), ]
  
  return(Direct_paths)
}

#### ESTIMATION OF BOARDED AND DROPPED PASSENGERS FROM COUNTER DATA ----
# Function to divide data between valid (V), missing (M), partially cancelled (PM) and cancelled (C)
status_coversion <- function(s) {
  if (s %in% c(1))
    return("V")
  if (s %in% c(0))
    return("C")
  if (s %in% c(-1))
    return("M")
  # if (s %in% c(-3,-4))
  #   return("PC")
  stop('Status not defined')
}

# Function to build a dataset having  a column dividing the states into valid (V), missing (M), partially cancelled (PM) or cancelled (C)
count_states <- function(train_count){
  train_count <- train_count |> mutate(Week = as.character(format(as.Date(MissionDate), "%Y-%W")))
  
  # Generate a column dividing the states into valid (V), missing (M) or canceled (C)
  train_count <- train_count |> rowwise() |> mutate(DATA_status = status_coversion(Status))
  
  # Now I would like to have unique train rides. Train rides are unique between lines and days
  train_count$MissionDate <- as.Date(train_count$MissionDate)
  train_rides <- train_count |> group_by(Line, MissionDate, TrainCode) |> slice(1)
  
  return(train_rides)
}

# Function to aggregate the (partial) counters data
build_marg <- function(total_stat, train_count, weeks){
  # Convert to week
  train_count$MissionDate <- as.character(format(as.Date(train_count$MissionDate), "%Y_%W"))
  
  # Select relevant stations and weeks
  train_count <- train_count |> filter(StopStation %in% total_stat & MissionDate %in% weeks)
  
  # Create Marg dataset
  Marg <- expand.grid(Station = total_stat, Week = weeks)
  Marg <- Marg |> mutate_at(vars(Station, Week), as.character)
  
  # Aggregate counter data for trains having valid data (V = (1)
  N_passengers <- train_count |> filter(Status %in% c(1)) |> select(MissionDate, StopStation, PassengersBoarded, PassengersExited) |>
    group_by(MissionDate, StopStation) |> summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) |> ungroup()
  
  # Join the two datasets
  Marg <- Marg |> left_join(N_passengers, by = c("Station" = "StopStation", "Week" = "MissionDate"))
  
  # Fill the NA (no valid train) with 0
  Marg <- mutate_all(Marg, ~replace(., is.na(.), 0))
  
  colnames(Marg) <- c("Station", "Week", "Passengers_boarded", "Passengers_dropped")
  
  return (Marg)
}

# Function to build the Coverage dataset
build_coverage <- function(total_stat, train_count, weeks){
  # Prepare train_count
  train_count$MissionDate <- format(as.Date(train_count$MissionDate), "%Y_%W")
  
  # Select relevant stations and weeks
  train_count <- train_count |> filter(StopStation %in% total_stat & MissionDate %in% weeks)
  
  # Create Coverage dataset
  Coverage <- expand.grid(Station = total_stat, Week = weeks)
  
  # Compute the number of trains stopping in the couple station-week (i.e., status in (0,1))
  N_trains <- train_count |> filter(Status %in% c(0,1)) |>
    select(MissionDate, StopStation) |>
    group_by(MissionDate, StopStation) |> summarise(N_trains = n()) |> ungroup()
  
  # Compute the number of VALID trains stopping in the couple station-week (i.e., status in (1))
  N_valid_trains <- train_count |> filter(Status %in% c(1)) |>
    select(MissionDate, StopStation) |>
    group_by(MissionDate, StopStation) |> summarise(N_valid_trains = n()) |> ungroup()
  
  # Join the two datasets to build coverage
  Coverage <- Coverage |> left_join(N_trains, by = c("Station" = "StopStation", "Week" = "MissionDate")) |> 
    left_join(N_valid_trains, by = c("Week" = "MissionDate", "Station" = "StopStation"))
  
  # Fill NA with 0 (no train correspond to 0)
  Coverage <- mutate_all(Coverage, ~replace(., is.na(.), 0))
  
  # Compute Coverage percentual
  Coverage <- Coverage |> mutate(Cov_perc = N_valid_trains / N_trains)
  
  # Fill NA with 1 (no train correspond to perfect coverage)
  Coverage <- mutate_all(Coverage, ~replace(., is.na(.), 1))
  
  return (Coverage)
}

# Function to prepare the dataset for estimation of of missing counters data
prepare_filling_dataset <- function(total_stat, weeks, Cov, Marg) {
  # I select only relevant stations and weeks for Marg and Cov, if not already selected
  Marg <- Marg |> filter(Station %in% total_stat & Week %in% weeks)
  Cov <- Cov |> filter(Station %in% total_stat & Week %in% weeks)
  
  # Prepare dataset
  Trains <- Cov |> left_join(Marg, by = c("Station", "Week"))
  
  return(Trains)
}

# Function to estimate and fill the missing counters data 
filling_missing_marg_data <- function(Trains, total_stat, weeks) {
  # I prepare two columns in the Trains dataset to get the filled data 
  Trains <- Trains |> add_column(Passengers_boarded_PRED = 0, Passengers_dropped_PRED = 0)

  # I apply the rescaling to compute the predictions 
  if (any(Trains$Cov_perc == 0))
    stop("Rescaling cannot be applied because of 0 coverage")
  
  # Apply rescaling
  Trains[,"Passengers_boarded_PRED"] = Trains[,"Passengers_boarded"] / Trains[,"Cov_perc"]
  Trains[,"Passengers_dropped_PRED"] = Trains[,"Passengers_dropped"] / Trains[,"Cov_perc"]
  
  # I select only the Marginal matrix and round the result since it corresponds to a number of movements
  Filled_Marg <- Trains |> select(Station, Week, Passengers_boarded_PRED, Passengers_dropped_PRED) |>
    rename(Passengers_boarded = Passengers_boarded_PRED, Passengers_dropped = Passengers_dropped_PRED) |>
    mutate_if(is.numeric, round)
  
  return(Filled_Marg)
}

#### CONVERSION OF TICKETS INTO TRIPS ----
# Function to preprocess tickets
clean_tickets <- function(tickets, station_codes) {
  colnames(tickets) <- c("COD_DES", "COD_ORI", "Date","Type", "Quantity")
  
  # I only need day granularity
  tickets$Date <- as.Date(tickets$Date)
  
  # I ceil ticket quantity to correct fractionary quantity due to combined ATM tickets
  tickets$Quantity <- ceiling(tickets$Quantity)
  
  # Filter only tickets internal to my area
  tickets <- tickets |> filter(COD_DES %in% station_codes & COD_ORI %in% station_codes &
                                 COD_DES != COD_ORI)
  
  # I convert ticket type into factors
  tickets$Type <- as.factor(tickets$Type)
  
  # Eliminate empty rows
  tickets[tickets==""] <- NA
  tickets <- tickets |> drop_na() |> filter()
  
  # Shuffling dataframe to get accurate time progress bar
  tickets <- tickets[sample(1:nrow(tickets)), ]
  
  return(tickets)
}

# Function to add the monthly subscriptions into the matrix
monthly_sub <- function(t, OD, dates){
  
  # I get the month of validity - Current if before the 22, following if after the 22
  month_val <- if (format(t$Date, "%d") < 22) format(t$Date, "%m") else format(ymd(t$Date) %m+% months(1), "%m")
  # Now, starting from the first day of validity, for complete weeks I add 5 A&R trips and for uncomplete weeks
  # I add trips as in the following table
  # days of the uncomplete week -> A&R trips added
  # 1 -> 0
  # 2 -> 1
  # 3-4 -> 2
  # 5 -> 3
  # 6 -> 4
  
  cur = if (format(t$Date, "%d") < 22) t$Date else as.Date(format(ymd(t$Date) %m+% months(1),"%Y-%m-01"))
  
  # while we are in the month of validity 
  while(as.numeric(month_val) == as.numeric(format(cur, "%m"))){
    
    # if the week is complete i.e. starting day is Monday and end of week is within the month, I add 5 A&R trips to the matrix and
    # skip to next Monday
    if (wday(cur, week_start=1) == 1 & (as.numeric((format(cur + 6, "%m"))) <= as.numeric(month_val) + 1 )){
      week <- as.character(format(cur, "%Y-%W"))
      
      # I add the quantity to the matrix - 10 trips per week (round trip)
      if (week %in% dates){
        OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES), week] <- OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES), week] + 5 * t$Quantity
        OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI), week] <- OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI), week] + 5 * t$Quantity
      }
      
      cur = cur + 7
    }
    
    # Other cases handling uncomplete weeks 
    # 6 days of validity -> Tuesday start OR end of the month in 6 days 
    else if (wday(cur, week_start=1) == 2 | (as.numeric((format(cur + 6 , "%m")) >= as.numeric(month_val) + 1) & (as.numeric(format(cur + 5, "%m")) < as.numeric(month_val) + 1))){
      week <- as.character(format(cur, "%Y-%W"))
      
      # I add the quantity to the matrix - 4 trips per week (round trip)
      if (week %in% dates){
        OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES), week] <- OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES), week] + 4 * t$Quantity
        OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI), week] <- OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI), week] + 4 * t$Quantity
      }
      
      cur = cur + 6
    }
    
    # 5 days of validity -> Wedsneday start OR end of the month in 5 days 
    else if (wday(cur, week_start=1) == 3 | ((as.numeric(format(cur + 5, "%m")) >= as.numeric(month_val) + 1) & (as.numeric(format(cur + 4, "%m")) < as.numeric(month_val) + 1))){
      week <- as.character(format(cur, "%Y-%W"))
      
      # I add the quantity to the matrix - 3 trips per week (round trip)
      if (week %in% dates){
        OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES), week] <- OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES), week] + 3 * t$Quantity
        OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI), week] <- OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI), week] + 3 * t$Quantity
      }
      
      cur = cur + 5
    }
    
    # 4 days of validity -> Thursday start OR end of the month in 4 days 
    else if (wday(cur, week_start=1) == 4 | ((as.numeric(format(cur + 4, "%m")) >= as.numeric(month_val) + 1) & (as.numeric(format(cur + 3, "%m")) < as.numeric(month_val) + 1))){
      week <- as.character(format(cur, "%Y-%W"))
      
      # I add the quantity to the matrix - 2 trips per week (round trip)
      if (week %in% dates){
        OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES), week] <- OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES), week] + 2 * t$Quantity
        OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI), week] <- OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI), week] + 2 * t$Quantity
      }
      
      cur = cur + 4
    }
    
    # 3 days of validity -> Friday start OR end of the month in 3 days 
    else if (wday(cur, week_start=1) == 5 | ((as.numeric(format(cur + 3, "%m")) >= as.numeric(month_val) + 1) & (as.numeric(format(cur + 2, "%m")) < as.numeric(month_val) + 1))){
      week <- as.character(format(cur, "%Y-%W"))
      
      # I add the quantity to the matrix - 2 trips per week (round trip)
      if (week %in% dates){
        OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES), week] <- OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES), week] + 2 * t$Quantity
        OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI), week] <- OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI), week] + 2 * t$Quantity
      }
      
      cur = cur + 3
    }
    
    # 2 days of validity -> Saturday start OR end of the month in 2 days 
    else if (wday(cur, week_start=1) == 6 | ((as.numeric(format(cur + 2, "%m")) >= as.numeric(month_val) + 1) & (as.numeric(format(cur + 1, "%m")) < as.numeric(month_val) + 1))){
      week <- as.character(format(cur, "%Y-%W"))
      
      # I add the quantity to the matrix - 1 trips per week (round trip)
      if (week %in% dates){
        OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES), week] <- OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES), week] + 1 * t$Quantity
        OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI), week] <- OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI), week] + 1 * t$Quantity
      }
      
      cur = cur + 2
    }
    
    # 2 days of validity -> Sunday start OR end of the month in 1 day 
    else if (wday(cur, week_start=1) == 7 | ((as.numeric(format(cur + 1, "%m")) >= as.numeric(month_val) + 1) & (as.numeric(format(cur, "%m")) < as.numeric(month_val) + 1))){
      # week <- as.character(format(cur, "%Y-%W"))
      # 
      # # I add the quantity to the matrix - 0 trips per week (round trip)
      # if (week %in% dates) {
      #   OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES), week] <- OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES), week] + 0 * t$Quantity
      #   OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI), week] <- OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI), week] + 0 * t$Quantity
      # }
      
      
      cur = cur + 1
    }
    
  }
  
  return(OD)
}

# Function to add the yearly subscriptions into the matrix
yearly_sub <- function(t, OD, dates){
  # I add 5 A & R trips for every week - uncomplete weeks in the same fashion as monthly sub 
  cur = t$Date
  
  # Last day of validity of the sub
  end_sub = ceiling_date(t$Date%m+% months(12)) - days(1)
  
  while (cur <= end_sub){
    
    # if the week is complete i.e. starting day is Monday and end of sub is within the month, I add 5 A&R trips to the matrix and
    # skip to next Monday
    if (wday(cur, week_start=1) == 1 & (cur + 6 <= end_sub)){
      col <- as.character(format(cur, "%Y-%W"))
      
      # I add the quantity to the matrix - 10 trips per week (round trip)
      if (col %in% dates){
        OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] = OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] + 5*t$Quantity
        OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] = OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] + 5*t$Quantity
        
      }
      
      cur = cur + 7
    }
    
    # Other cases handling uncomplete weeks 
    # 6 days of validity -> Tuesday start OR end of the sub in 6 days 
    else if ((wday(cur, week_start=1) == 2 | (cur + 6 > end_sub) & (cur + 5 <= end_sub))){
      col <- as.character(format(cur, "%Y-%W"))
      
      # I add the quantity to the matrix - 4 trips per week (round trip)
      if (col %in% dates){
        OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] = OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] + 4*t$Quantity
        OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] = OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] + 4*t$Quantity
      }
      
      cur = cur + 6
    }
    
    # 5 days of validity -> Wedsneday start OR end of the sub in 5 days 
    else if (wday(cur, week_start=1) == 3 | ((cur + 5 > end_sub) & (cur + 4 <= end_sub))){
      col <- as.character(format(cur, "%Y-%W"))
      
      # I add the quantity to the matrix - 3 trips per week (round trip)
      if (col %in% dates){
        OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] = OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] + 3*t$Quantity
        OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] = OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] + 3*t$Quantity
      }
      
      cur = cur + 5
    }
    
    # 4 days of validity -> Thursday start OR end of the sub in 4 days 
    else if (wday(cur, week_start=1) == 4 | ((cur + 4 > end_sub) & (cur + 3 <= end_sub))){
      col <- as.character(format(cur, "%Y-%W"))
      
      # I add the quantity to the matrix - 2 trips per week (round trip)
      if (col %in% dates){
        OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] = OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] + 2*t$Quantity
        OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] = OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] + 2*t$Quantity
      }
      
      cur = cur + 4
    }
    
    # 3 days of validity -> Friday start OR end of the sub in 3 days 
    else if (wday(cur, week_start=1) == 5 | ((cur + 3 > end_sub) & (cur + 2 <= end_sub))){
      col <- as.character(format(cur, "%Y-%W"))
      
      # I add the quantity to the matrix - 2 trips per week (round trip)
      if (col %in% dates){
        OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] = OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] + 2*t$Quantity
        OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] = OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] + 2*t$Quantity
      }
      
      cur = cur + 3
    }
    
    # 2 days of validity -> Saturday start OR end of the sub in 2 days 
    else if (wday(cur, week_start=1) == 6 | ((cur + 2 > end_sub) & (cur + 1 <= end_sub))){
      col <- as.character(format(cur, "%Y-%W"))
      
      # I add the quantity to the matrix - 1 trips per week (round trip)
      if (col %in% dates){
        OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] = OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] + t$Quantity
        OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] = OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] + t$Quantity
      }
      
      cur = cur + 2
    }
    
    # 2 days of validity -> Sunday start OR end of the sub in 1 day 
    else if (wday(cur, week_start=1) == 7 | ((cur + 1 > end_sub) & (cur <= end_sub))){
      col <- as.character(format(cur, "%Y-%W"))
      
      # I add the quantity to the matrix - 0 trips per week (round trip)
      #if (col %in% dates){
      # OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] = OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] + t$Quantity
      # OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] = OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] + t$Quantity
      #}
      
      cur = cur + 1
    }
    
    
  }
  
  return(OD)
  
}

# Function to add the carnet subscriptions into the matrix
carnet_sub <- function(t, OD, dates){
  # The assumption is the following: I extract 5 different days in the period (30 days) following the 
  # purchase and suppose A&R trip in the 5 days 
  
  # I have to do a separate extraction for every carnet in the lot 
  for (i in 1:t$Quantity){
    
    days <- sample(0:29, size = 5)
    
    for (d in days){
      date = t$Date + d
      
      # I get the week where to add the sub 
      col <- as.character(format(date, "%Y-%W"))
      
      # I add the quantity to the matrix - 1 A&R trip (round trip)
      if (col %in% dates){
        OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] = OD[(OD$Start == t$COD_ORI) & (OD$End == t$COD_DES),col] + 1
        OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] = OD[(OD$Start == t$COD_DES) & (OD$End == t$COD_ORI),col] + 1
      }
      
    }
  }
  
  return(OD)
}

# Main function to convert tickets into OD matrices
OD_ticket_matrix_build <- function(tickets, dates){
  # Create an empty OD matrix with columns for Start, End, and each week in the dataset
  stations <- unique(c(tickets$COD_ORI, tickets$COD_DES))
  OD <- expand.grid(Start = stations, End = stations)
  OD[, dates] <- 0
  
  # Remove autoloops
  OD <- OD |> filter(Start != End)
  
  # Create progress bar
  pb <- progress_bar$new(
    format = "  computing [:bar] :percent eta: :eta",
    total = dim(tickets)[1], clear = FALSE, width= 60)
  
  # Now I need to read the rows of the matrix ticket one by one and add the trip (or multiple trips) in the corresponding rows
  for (i in 1:nrow(tickets)){
    ticket <- tickets[i, ]
    
    if (ticket$Type %in% c("Ordinary ticket", "Malpensa Express", "Special rates initiative", "Supplementary correction")) {
      # For every one of the tickets, I extract a day of use in the following 7 days
      for (i in 1:ticket$Quantity){
        # Get the week corresponding to the ticket's date
        week <- format(ticket$Date + sample(0:6,1), "%Y-%W")
        
        if (week %in% dates) {
          # Increment the OD matrix with half of the quantity for both O->D and D->O
          OD[(OD$Start == ticket$COD_ORI) & (OD$End == ticket$COD_DES), week] <- OD[(OD$Start == ticket$COD_ORI) & (OD$End == ticket$COD_DES), week] + 0.5 
          OD[(OD$Start == ticket$COD_DES) & (OD$End == ticket$COD_ORI), week] <- OD[(OD$Start == ticket$COD_DES) & (OD$End == ticket$COD_ORI), week] + 0.5 
        }
      }
    }
    
    
    if (ticket$Type == "Weekly subscription") {
      # Determine the week based on the ticket's date - current week if bought between Monday and Wedsneday, else next week 
      week <- ifelse(wday(ticket$Date, week_start = 1) <= 3, format(ticket$Date, "%Y-%W"), format(ticket$Date + 7, "%Y-%W"))
      
      if (week %in% dates) {
        # Increment the OD matrix with 5 times the quantity for both O->D and D->O
        OD[(OD$Start == ticket$COD_ORI) & (OD$End == ticket$COD_DES), week] <- OD[(OD$Start == ticket$COD_ORI) & (OD$End == ticket$COD_DES), week] + 5 * ticket$Quantity
        OD[(OD$Start == ticket$COD_DES) & (OD$End == ticket$COD_ORI), week] <- OD[(OD$Start == ticket$COD_DES) & (OD$End == ticket$COD_ORI), week] + 5 * ticket$Quantity
      }
    }
    
    if (ticket$Type == "Carnet") {
      # Call carnet_sub() function to handle the ticket type
      OD <- carnet_sub(ticket, OD, dates)
    }
    
    if (ticket$Type == "Monthly subscription") {
      # Call monthly_sub() function to handle the ticket type
      OD <- monthly_sub(ticket, OD, dates)
    }
    
    if (ticket$Type == "Annual subscription") {
      # Call yearly_sub() function to handle the ticket type
      OD <- yearly_sub(ticket, OD, dates)
    }
    
    pb$tick()
  }
  
  # Remove empty rows 
  # OD <- OD[rowSums(OD[,3:dim(OD)[2]]) != 0,]
  
  return(OD)
}

# Function to postprocess the OD tickets matrix
postprocess_tickets <- function(OD, real_station_codes) {
  # Put negative values to 0
  OD[,-c(1,2)][OD[, -c(1,2)] < 0] <- 0
  
  # select Start and End belonging to stations of our area
  OD <- OD |> filter(Start %in% real_station_codes & End %in% real_station_codes)
  
  # Changing names 
  weeks <- colnames(OD)[3:dim(OD)[2]]
  names <- paste0("Week_", weeks)
  names <- lapply(names, function(x) {
    gsub("-", "_", x)
  })
  colnames(OD)[3:dim(OD)[2]] <- names
  
  return(OD)
}



#### SEPARATION OF TRIPS REQUIRING AT MOST ONE CHANGE OF TRAIN ----
# Function to separate the ticket-estimated trips requiring exactly one change of train and removing the trips requiring more than one change 
separate_trips <- function(OD_tickets, Travel_times){
  # I remove information about travel times, which I do not need for the separation
  Travel_times <- Travel_times |> select(-"Mean.travel.time")
  
  # 1. Filtering out paths requiring more than one change of train
  # I compute the list of the paths requiring more than one change of train. They have NA in Optimal.change.station
  long_paths <- Travel_times |> filter(is.na(Optimal.change.station)) |> select(Start, End)
  
  # I remove rows in dataframe OD_tickets matching rows in long_paths
  OD_tickets <- OD_tickets |> anti_join(long_paths, by = c("Start", "End"))
  
  # 2. Separating paths requiring exactly one change of train 
  # I select such paths. The paths requiring one change of train are
  change_paths <- Travel_times |> filter(!is.na(Optimal.change.station) & Optimal.change.station != 0)
  
  # I select rows of OD_tickets requiring a change 
  OD_tickets_change <- OD_tickets |> semi_join(change_paths, by = c("Start", "End"))
  
  # I remove those change paths from OD_tickets
  OD_tickets <- OD_tickets |> anti_join(change_paths, by = c("Start", "End"))
  
  # To OD_tickets_change, I add the information about the optimal change station
  OD_tickets_change <- OD_tickets_change |> left_join(Travel_times, by = c("Start", "End"))
  
  # For every row of OD_tickets_change, I separate the trips dividing them into Start->Change and Change->End
  for (i in 1:dim(OD_tickets_change)[1]){
    ori <- OD_tickets_change[i,"Start"]
    change <- OD_tickets_change[i,"Optimal.change.station"]
    dest <- OD_tickets_change[i,"End"]
    
    # Select only the numeric variables
    row <- OD_tickets_change[i,] |> select(-c("Start", "End", "Optimal.change.station"))
    
    # Separate trips
    OD_tickets[OD_tickets$Start == ori & OD_tickets$End == change,-c(1,2)] <- OD_tickets[OD_tickets$Start == ori & OD_tickets$End == change,-c(1,2)] + row
    OD_tickets[OD_tickets$Start == change & OD_tickets$End == dest,-c(1,2)] <- OD_tickets[OD_tickets$Start == change & OD_tickets$End == dest,-c(1,2)] + row
  }
  
  return(OD_tickets)
}

#### ESTIMATION OF MISSING TICKET-ESTIMATED OD DATA THROUGH GRAVITY MODEL ----
# Function to prepare the dataset for the gravity model estimation
build_gravity_model_dataset <- function(OD_tickets, Travel_times, Marg, missing_data_station, missing_data_area, city_area){
  # Preprocess ticket dataset
  OD_tickets <- OD_tickets |> 
    pivot_longer(cols = starts_with("Week_"), names_to = "Week", values_to = "Counts") |>
    mutate(Week = substr(Week, 6, nchar(Week))) |>
    # Removing weeks for which we do not have counter data, i.e., weeks 17:22 and 52
    filter(substr(Week, nchar(Week) - 1, nchar(Week)) %in% "23":"51")
  
  # Preprocessing - selecting only direct paths
  Travel_times <- Travel_times |> filter(Optimal.change.station == 0) |> select(-Optimal.change.station)
  
  # Preprocessing - arranging data to match OD_tickets dataset
  boarded <- Marg |> select(-Passengers_dropped)
  dropped <- Marg |> select(-Passengers_boarded)
  
  # Join datasets 
  df <- OD_tickets|>
    # Join with travel times and direct path data
    full_join(Travel_times, by = c("Start", "End")) |> 
    full_join(boarded, by = c("Start" = "Station", "Week")) |>
    full_join(dropped, by = c("End" = "Station", "Week")) |>
    drop_na()
  
  ## IDENTYFING THE CELLS TO ESTIMATE
  df <- df |> mutate(To_estimate = 0)
  
  # 1. I have to estimate all the paths to/from Verona
  df[df$Start == missing_data_station | df$End == missing_data_station,"To_estimate"] <- 1
  
  # 2. I have to estimate paths Milan-STBIM and STBIM-Milan
  df[df$Start %in% city_area & df$End %in% missing_data_area,"To_estimate"] <- 1
  df[df$Start %in% missing_data_area & df$End %in% city_area,"To_estimate"] <- 1
  
  return(df)
}

# Function to fit the gravity model considering ticket-estimated OD data and predict values for missing data
gravity_model_estimation <- function(df){
  # I save data to be added to OD_tickets 
  OD_tickets <- df |> filter(To_estimate == 0) |> select(Start, End, Week, Counts)
  
  # I remove from df the couple Pozzuolo Martesana and Week 23 since no passengers have boarded or dropped
  to_remove <- df |> filter(Passengers_boarded == 0 | Passengers_dropped == 0)
  df <- df |> filter(!(Passengers_boarded == 0 | Passengers_dropped == 0))
  
  # I separate training and estimation data
  train <- df |> filter(To_estimate == 0)
  to_pred <- df |> filter(To_estimate == 1)
  
  # The model I want to apply is 
  # log(OD_ij) = b1 + b_2*log(O_i) + b_3*log(D_j) + b_4*log(c_ij) + eps_ij
  
  # I substitute 0 cells with 0.01
  # This problem involves dim(train |> filter(Counts == 0))[1] /dim(train)[1] -> 5,6% of my data
  train$Counts[train$Counts == 0] <- 0.01
  
  # I transform data considering the log model
  train <- train |>
    mutate(Counts = log(Counts), Mean.travel.time = log(Mean.travel.time),
           Passengers_boarded = log(Passengers_boarded), Passengers_dropped = log(Passengers_dropped))
  
  ## FITTING THE MODEL
  gravity_model <- lm(Counts ~ Passengers_boarded + Passengers_dropped + Mean.travel.time , data = train)
  print(summary(gravity_model)) # R2 0.5638
  
  
  ## PREDICTION OF MISSING DATA POINTS
  to_pred <- to_pred |>
    mutate(Mean.travel.time = log(Mean.travel.time), 
           Passengers_boarded = log(Passengers_boarded), Passengers_dropped = log(Passengers_dropped))
  
  to_pred$Counts_pred <- exp(predict(gravity_model, newdata = to_pred))
  
  to_add <- to_pred |> select(Start, End, Week, Counts_pred) |> rename(Counts = Counts_pred)
  OD_tickets <- rbind(OD_tickets, to_add)
  
  # I need to recompose the ticket OD matrix
  OD_tickets <- OD_tickets |> pivot_wider(
    names_from = "Week",
    values_from = "Counts"
  )
  
  colnames(OD_tickets)[-c(1,2)] <- paste0("Week_", colnames(OD_tickets)[-c(1,2)])
  
  # Fill missing cells (i.e. the ones relating to Pozzuolo Martesana on week 23) with 0 values
  OD_tickets <- replace(OD_tickets, is.na(OD_tickets), 0)
  
  return(OD_tickets)
}

#### FURNESS METHOD ----
# Function to select the OD matrix only of a certain week
OD_week <- function(OD, w, station_codes){
  OD_week <- OD |>
    arrange(Start, End) |>
    dplyr::filter(Start %in% station_codes, End %in% station_codes) |>
    dplyr::select(Start, End, all_of(paste("Week",w,sep="_"))) |>
    pivot_wider(names_from = End, values_from = all_of(paste("Week",w,sep="_")), values_fill = 0) |>
    column_to_rownames(var = "Start") 
  
  # Ordering
  OD_week <- OD_week[, order(colnames(OD_week))]
  OD_week <- OD_week[order(rownames(OD_week)), ]
  
  return(OD_week)
}

# Function to build and fill the datasets resulting from the Furness method applycated to every week of the study
build_Furness_OD_matrices <- function(OD_tickets, Marg, Direct_paths, station_codes, weeks) {
  # I create the dataset which stores all the OD matrices (one column for each week)
  OD_Furness <- expand.grid(Start = station_codes, End = station_codes, stringsAsFactors = F)
  
  # I create a dataframe containing row and margin errors
  Errors <- data.frame("Week" = weeks, 
                       "Row_error" = rep(NA, length(weeks)),
                       "Col_error" = rep(NA, length(weeks)))
  
  # I apply Furness method for every week
  for (w in weeks){
    # Apply function to the week
    OD_cur <- application_Furness_week(OD_tickets, Marg, Direct_paths, w, station_codes) 
    
    # Fill of the error dataset
    Errors[Errors$Week == w, "Row_error"] = OD_cur$Row_error
    Errors[Errors$Week == w, "Col_error"] = OD_cur$Col_error
    
    # Fill the total matrix
    OD_Furness <- OD_Furness |> left_join(OD_cur$OD_estimated, by = c("Start", "End")) 
  }
  
  # Set names
  dates <- paste("Week", weeks, sep="_")
  colnames(OD_Furness)[3:dim(OD_Furness)[2]] <- dates
  
  return(list("OD_Furness" = OD_Furness, "Errors" = Errors))
}

# Function to perform Furness method for a selected week
application_Furness_week <- function(OD_tickets, Marg, Direct_paths, week, station_codes){
  print(week)
  
  # I sort the station codes to have correspondances between tickets and marginals
  station_codes <- sort(station_codes)
  
  # first I compute the OD of the week for the ticket-estimated seed
  OD_week <- OD_week(OD_tickets, week, station_codes)
  
  # I find the positions where I need to correct the 0 values. These are 0s in cells belonging to direct paths
  OD_zeros_original <- OD_week == 0 & Direct_paths == 1
  
  # I apply the correction of 0 values: 0 values becomes 0.1
  OD_week[OD_zeros_original == TRUE] <- 0.1
  
  # Now we apply the Furness method
  # The seed is given by the ticket-estimated OD matrix
  seed.2d <- as.matrix(OD_week)
  
  # Now I have to build the marginals (start & end, i.e. boarded and dropped passengers)
  # I select only the week
  Marg <- Marg |> filter(Week == week)
  # I order the dataset to match the same order of the ticket OD matrix
  Marg <- Marg[order(Marg$Station),]
  
  # The two targets are
  target.row <- Marg$Passengers_boarded
  target.col <- Marg$Passengers_dropped
  tgt.data.2d <- list(target.row, target.col)
  tgt.list.2d <- list(1,2)
  
  # Application of the Furness method
  res.2d <- Estimate(seed.2d, tgt.list.2d, tgt.data.2d, method = "ipfp", print = FALSE, keep.input = TRUE)
  # print(summary(res.2d))
  
  # Print convergence
  ifelse(res.2d$conv == TRUE, print("Ipfp reached convergence"), print("Ipfp DID NOT REACH convergence"))
  
  # I recover the OD matrix from the probability matrix multiplying by the total number of boarded passengers
  OD_Estimated = res.2d$p*(sum(target.row))
  colnames(OD_Estimated) <- station_codes
  rownames(OD_Estimated) <- station_codes
  
  # Rounding
  OD_Estimated <- as.data.frame(round(as.matrix(OD_Estimated)))
  
  # Converting in the pivot longer format
  OD_Estimated <- OD_Estimated |> rownames_to_column("Start") |> 
    pivot_longer(cols = 2:(dim(OD_Estimated)[2]+1), names_to = "End") 
  
  # Computing row and column errors
  Row_error = max(abs(apply(round(as.matrix(res.2d$p*(sum(target.row)))),1,sum) - target.row))
  Col_error = max(abs(apply(round(as.matrix(res.2d$p*(sum(target.row)))),2,sum) - target.col))
  
  print("______________________________")
  
  return (list("OD_estimated" = OD_Estimated, "Row_error" = Row_error, "Col_error" = Col_error))
}