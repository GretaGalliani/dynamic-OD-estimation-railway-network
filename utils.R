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
  train_count$Mycode <- paste0(as.Date(as.Date(train_count$Data.missione), format = "%d_%m"), train_count$Codice.treno)
  
  # 1. Select train starting from s1 and not cancelled
  set1 <- train_count |> filter(Stazione.fermata == s1 & Stato %in% c(-4,-3,-1,0,1,2,3,4,5,6) & Linea == line) |>
    select(Mycode, Stazione.fermata, Ora.uscita.dalla.stazione, Indice.fermata, Stazione.cancellata)
  colnames(set1) <- c("Mycode", "Stazione.start", "Ora.start", "Indice.start", "Stazione.cancellata.start")
  
  # 2. Select train ending in s1 and not cancelled
  set2 <- train_count |> filter(Stazione.fermata == s2 & Mycode %in% unique(set1$Mycode) & Stato %in% c(-4,-3,-1,0,1,2,3,4,5,6) & Linea == line)  |>
    select(Mycode, Stazione.fermata, Ora.ingresso.in.stazione, Indice.fermata, Stazione.cancellata)
  colnames(set2) <- c("Mycode", "Stazione.end", "Ora.end", "Indice.end", "Stazione.cancellata.end")
  
  # Match trains
  times <- set1 |> filter(Mycode %in% unique(set2$Mycode)) |> full_join(set2, by = "Mycode") |> filter(Stazione.cancellata.end == "false" &
                                                                                                         Stazione.cancellata.start == "false" &
                                                                                                         Indice.end > Indice.start) |>
    mutate(Travel.time = as.numeric(difftime(as.POSIXct(Ora.end, format="%Y/%m/%d %H:%M:%S"), as.POSIXct(Ora.start, format="%Y/%m/%d  %H:%M:%S"), units = "mins"))) |>
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
  if (s %in% c(0,1,2,3,4))
    return("V")
  if (s %in% c(-2,-5,-3,-4))
    return("C")
  if (s %in% c(-1,5,6))
    return("M")
  # if (s %in% c(-3,-4))
  #   return("PC")
  stop('Status not defined')
}

# Function to build a dataset having  a column dividing the states into valid (V), missing (M), partially cancelled (PM) or cancelled (C)
count_states <- function(train_count){
  train_count <- train_count |> mutate(Week = as.character(format(as.Date(Data.missione), "%Y-%W")))
  
  # Generate a column dividing the states into valid (V), missing (M) or canceled (C)
  train_count <- train_count |> rowwise() |> mutate(DATA_status = status_coversion(Stato))
  
  # Now I would like to have unique train rides. Train rides are unique between lines and days
  train_count$Data.missione <- as.Date(train_count$Data.missione)
  train_rides <- train_count |> group_by(Linea, Data.missione, Codice.treno) |> slice(1)
  
  return(train_rides)
}

# Function to aggregate the (partial) counters data
build_marg <- function(total_stat, train_count, weeks){
  # Convert to week
  train_count$Data.missione <- as.character(format(as.Date(train_count$Data.missione), "%Y_%W"))
  
  # Select relevant stations and weeks
  train_count <- train_count |> filter(Stazione.fermata %in% total_stat & Data.missione %in% weeks)
  
  # Create Marg dataset
  Marg <- expand.grid(Station = total_stat, Week = weeks)
  
  # Aggregate counter data for trains having valid data (V = (0,1,2,3,4))
  N_passengers <- train_count |> filter(Stato %in% c(0,1,2,3,4)) |> select(Data.missione, Stazione.fermata, Passeggeri.saliti, Passeggeri.scesi) |>
    group_by(Data.missione, Stazione.fermata) |> summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) |> ungroup()
  
  # Join the two datasets
  Marg <- Marg |> left_join(N_passengers, by = c("Station" = "Stazione.fermata", "Week" = "Data.missione"))
  
  # Fill the NA (no valid train) with 0
  Marg <- mutate_all(Marg, ~replace(., is.na(.), 0))
  
  colnames(Marg) <- c("Station", "Week", "Passengers_boarded", "Passengers_dropped")
  
  return (Marg)
}

# Function to build the Coverage dataset
build_coverage <- function(total_stat, train_count, weeks){
  # Prepare train_count
  train_count$Data.missione <- format(as.Date(train_count$Data.missione), "%Y_%W")
  
  # Select relevant stations and weeks
  train_count <- train_count |> filter(Stazione.fermata %in% total_stat & Data.missione %in% weeks)
  
  # Create Coverage dataset
  Coverage <- expand.grid(Station = total_stat, Week = weeks)
  
  # Compute the number of trains stopping in the couple station-week (i.e., status in (-4,-3,-1,0,1,2,3,4,5,6))
  N_trains <- train_count |> filter(Stato %in% c(-4,-3,-1,0,1,2,3,4,5,6) & Stazione.cancellata == "false") |>
    select(Data.missione, Stazione.fermata) |>
    group_by(Data.missione, Stazione.fermata) |> summarise(N_trains = n()) |> ungroup()
  
  # Compute the number of VALID trains stopping in the couple station-week (i.e., status in (0,1,2,3,4))
  N_valid_trains <- train_count |> filter(Stato %in% c(0,1,2,3,4) & Stazione.cancellata == "false") |>
    select(Data.missione, Stazione.fermata) |>
    group_by(Data.missione, Stazione.fermata) |> summarise(N_valid_trains = n()) |> ungroup()
  
  # Join the two datasets to build coverage
  Coverage <- Coverage |> left_join(N_trains, by = c("Station" = "Stazione.fermata", "Week" = "Data.missione")) |> 
    left_join(N_valid_trains, by = c("Week" = "Data.missione", "Station" = "Stazione.fermata"))
  
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
  tickets$Date <- as.Date(tickets$Date, format = "%d/%m/%Y")
  
  # For Quantity, I need to replace comma with point and then convert to numeric
  tickets$Quantity <- as.numeric(gsub(",", ".", gsub("\\.", "", tickets$Quantity)))
  
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
    
    if (ticket$Type %in% c("BIGLIETTI ORDINARI", "MALPENSA EXPRESS", "INIZIATIVE TARIFFE SPECIALI", "ESAZIONE SUPPLETIVA")) {
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
    
    
    if (ticket$Type == "ABBONAMENTI SETTIMANALI") {
      # Determine the week based on the ticket's date - current week if bought between Monday and Wedsneday, else next week 
      week <- ifelse(wday(ticket$Date, week_start = 1) <= 3, format(ticket$Date, "%Y-%W"), format(ticket$Date + 7, "%Y-%W"))
      
      if (week %in% dates) {
        # Increment the OD matrix with 5 times the quantity for both O->D and D->O
        OD[(OD$Start == ticket$COD_ORI) & (OD$End == ticket$COD_DES), week] <- OD[(OD$Start == ticket$COD_ORI) & (OD$End == ticket$COD_DES), week] + 5 * ticket$Quantity
        OD[(OD$Start == ticket$COD_DES) & (OD$End == ticket$COD_ORI), week] <- OD[(OD$Start == ticket$COD_DES) & (OD$End == ticket$COD_ORI), week] + 5 * ticket$Quantity
      }
    }
    
    if (ticket$Type == "CARNET") {
      # Call carnet_sub() function to handle the ticket type
      OD <- carnet_sub(ticket, OD, dates)
    }
    
    if (ticket$Type == "ABBONAMENTI MENSILI") {
      # Call monthly_sub() function to handle the ticket type
      OD <- monthly_sub(ticket, OD, dates)
    }
    
    if (ticket$Type == "ABBONAMENTI ANNUALI") {
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
build_gravity_model_dataset <- function(OD_tickets, Travel_times, Marg, Verona, IS_area, Milan_area){
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
    full_join(dropped, by = c("End" = "Station", "Week")) 
  
  ## IDENTYFING THE CELLS TO ESTIMATE
  df <- df |> mutate(To_estimate = 0)
  
  # 1. I have to estimate all the paths to/from Verona
  df[df$Start == Verona | df$End == Verona,"To_estimate"] <- 1
  
  # 2. I have to estimate paths Milan-STBIM and STBIM-Milan
  df[df$Start %in% Milan_area & df$End %in% IS_area,"To_estimate"] <- 1
  df[df$Start %in% IS_area & df$End %in% Milan_area,"To_estimate"] <- 1
  
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
  
  # Travel_times <- read.csv("Data/Trenord/Processed/NEW/2022/Travel_times/Total_travel_times.csv")
  # X <- OD_tickets |> left_join(Travel_times, by = c("Start", "End")) # CHECK -> Only 0, so no indirect paths
  
  # X <- OD_tickets |> full_join(Travel_times, by = c("Start", "End"))
  # Y <- X |> filter(Optimal.change.station == 0 & is.na(Counts)) # CHECK -> empty, so all the direct paths have a number in OD tickets
  # # OK, I checked that OD tickets now contains ONLY direct paths
  
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
    filter(Start %in% station_codes, End %in% station_codes) |>
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

#### AGGREGATION OF MILAN AREA ----
# Function to aggregate Milan area tickets
aggregate_Milan_tickets <- function(OD, Milan_area) {
  # Substitute Milan codes with S9999 and aggregate
  OD <- OD |> mutate(Start = replace(Start, Start %in% Milan_area, "S99999"), 
                     End = replace(End, End %in% Milan_area, "S99999")) |> group_by(Start, End) |> 
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) |> ungroup()
  
  return(OD)
}

# Function to aggregate marginal dataset
aggregate_Milan_marg <- function(Marg, Milan_area) {
  # Substitute Milan code and aggregate
  Marg <- Marg |> mutate(Station = replace(Station, Station %in% IS_area, "S99999")) |> group_by(Station, Week) |> 
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) 
  
  return(Marg)
}

#### COMPARISON BETWEEN REGIONE LOMBARDIA AND TRENORD OD MATRICES ----
# Function to clean distances data
distances_cleaned <- function(net, info, suppressed_comunis){
  # The correspondance between the two files is given by "Codice Comune formato alfanumerico"
  # Remove white spaces since they are causing problems 
  colnames(info) <- make.names(names(info))
  
  # To start, keep only net rows having origin AND destination in the provinces of interest
  provinces_of_interest = c("Monza e della Brianza", "Milano", "Bergamo", "Lecco", "Brescia", "Verona")
  
  # List of the codes respectig this condition
  list_codes <- info |> 
    filter(Denominazione.Regione %in% c("Lombardia", "Veneto")) |>
    dplyr::select(Codice.Comune.formato.numerico) |> pull(Codice.Comune.formato.numerico)
  
  # Selecting only edeges having origin AND destination in this list
  net2 <- net |> filter(Origine %in% list_codes & Destinazione %in% list_codes)
  
  ## Defining nodes and edges
  # I want the IDs with names and provinces 
  nodes <- info |> 
    filter(Denominazione.Regione %in% c("Lombardia", "Veneto"))
  nodes <- nodes[,c(16, 7, 12)]
  colnames(nodes) <- c("ID", "Comune", "Province")
  
  edges <- net2 |> dplyr::select(-Name) |> rename(Minuti = Total_Minu, Metri = Total_Mete)
  # Removing edges having same origin and destination
  edges <- edges |> filter(Origine != Destinazione)
  
  ## 25 municipalities are new; copy their data from old municipalities 
  new_codes <- nodes |> filter(!(ID %in% edges$Origine) | !(list_codes %in% edges$Destinazione)) |> pull(ID)
  
  colnames(suppressed_comunis) <- make.names(names(suppressed_comunis))
  
  correspondances <- suppressed_comunis |> 
    filter(as.numeric(Codice.del.Comune.associato.alla.variazione) %in% new_codes) |>
    group_by(Codice.del.Comune.associato.alla.variazione) |>
    filter(row_number()==1) |>
    dplyr::select(Codice.del.Comune.associato.alla.variazione, Codice.Comune)
  colnames(correspondances) = c("New_code", "Old_code")
  correspondances$Old_code <- as.numeric(correspondances$Old_code)
  correspondances$New_code <- as.numeric(correspondances$New_code)
  
  idx <- net$Origine %in% correspondances$Old_code
  net$Origine[idx] <- correspondances$New_code[match(net$Origine[idx], correspondances$Old_code)]
  
  idx <- net$Destinazione %in% correspondances$Old_code
  net$Destinazione[idx] <- correspondances$New_code[match(net$Destinazione[idx], correspondances$Old_code)]
  
  # I handle Torre de' Busi since it has not be suppressed; it changed province
  net[net$Origine == 97080,"Origine"] = 16215
  net[net$Destinazione == 97080,"Destinazione"] = 16215
  
  # Check if there are other problems in codes correspondance
  net2 <- net |> filter(Origine %in% list_codes & Destinazione %in% list_codes)
  edges <- net2 |> dplyr::select(-Name) |> rename(Minuti = Total_Minu, Metri = Total_Mete)
  edges <- edges |> filter(Origine != Destinazione)
  nodes |> filter(!(ID %in% edges$Origine) | !(list_codes %in% edges$Destinazione)) 
  
  ## TWO OTHER CASES 
  # Campione d'Italia -> REMOVE; I CANNOT ESTIMATE TIMES
  nodes <- nodes |> filter(!(ID == 13040))
  list_codes <- setdiff(list_codes, 13040)
  
  # Monte Isola -> ADD TIMES TO GO BY BOAT TO SULZANO - 22 minutes, searched by google maps; 5200 metri 
  # Select net of Sulzano
  Sulzano_code <- info |> filter(Denominazione.in.italiano == "Sulzano") |> pull(Codice.Comune.formato.numerico)
  net_MoIs <- net |> filter(Origine ==  Sulzano_code| Destinazione == Sulzano_code)
  
  # Substitute code with Monte Isola code 
  MoIs_code <- info |> filter(Denominazione.in.italiano == "Monte Isola") |> pull(Codice.Comune.formato.numerico)
  net_MoIs[net_MoIs$Origine == Sulzano_code,"Origine"] = MoIs_code
  net_MoIs[net_MoIs$Destinazione == Sulzano_code,"Destinazione"] = MoIs_code
  
  # remove duplicates
  net_MoIs <- net_MoIs |> filter(!(Origine==Destinazione))
  
  # To every arc, add 22 minutes and 5200 metres
  net_MoIs$Total_Minu = net_MoIs$Total_Minu + 22
  net_MoIs$Total_Mete = net_MoIs$Total_Mete + 5200
  
  # add arc for MoIs-Sulzano $ Sulzano-MoIs
  add_arcs <- data.frame("Name" = c(paste(MoIs_code, Sulzano_code, sep="-"), paste(Sulzano_code, MoIs_code, sep="-")),
                         "Origine" = c(MoIs_code, Sulzano_code), 
                         "Destinazione" = c(Sulzano_code, MoIs_code),
                         "Total_Minu" = c(22,22),
                         "Total_Mete" = c(5200,5200))
  net_MoIs <- rbind(net_MoIs, add_arcs)
  
  # add to net
  net <- rbind(net, net_MoIs)
  
  net$Total_Minu <- as.numeric(net$Total_Minu)
  net$Total_Mete <- as.numeric(net$Total_Mete)
  
  return(net)
}

# Function that assigns each municipality to the closest Trenord station
closest_station <- function(Stations_correspondances, stats_codes, comuni_distances){
  
  # Create progress bar
  pb <- progress_bar$new(
    format = "  computing [:bar] :percent eta: :eta",
    total = dim(Stations_correspondances)[1], clear = FALSE, width= 60)
  
  for (i in 1:dim(Stations_correspondances)[1]){
    act_code <- Stations_correspondances[i,"Code"] |> pull()
    
    # Skip Campione d'Italia since I do not have distances estimates
    if (act_code == 13040){
      Stations_correspondances[i,"Station_ref_code"] <- NA
      Stations_correspondances[i,"Station_distance_meters"] <- NA
      Stations_correspondances[i,"Station_distance_minutes"] <- NA
      next
    }
    
    
    # If the comune has a station, returns the comune
    if(act_code %in% stats_codes){
      Stations_correspondances[i,"Station_ref_code"] <- act_code
      Stations_correspondances[i,"Station_distance_meters"] <- 0
      Stations_correspondances[i,"Station_distance_minutes"] <- 0
      next
    }
    
    # I consider the distances in meters from comune to station. The candidates are
    candidates <- comuni_distances |> filter(Origine == act_code & Destinazione %in% stats_codes)
    station_ISTAT_code <- candidates |> filter(Total_Mete == min(Total_Mete)) |> 
      slice(1)
    
    Stations_correspondances[i,"Station_ref_code"] <- station_ISTAT_code |> pull(Destinazione)
    Stations_correspondances[i,"Station_distance_meters"] <- station_ISTAT_code |> pull(Total_Mete)
    Stations_correspondances[i,"Station_distance_minutes"] <- station_ISTAT_code |> pull(Total_Minu)
    
    pb$tick()
  }
  
  return(Stations_correspondances)
}

# Function that builds the stations correspondances dataset
build_stations_correspondances <- function(comuni_ISTAT, stations, comuni_distances, IS_area) {
  # Prepare the dataset
  Stations_correspondances <- comuni_ISTAT |> filter(Denominazione.Regione %in% c("Lombardia","Veneto")) 
  Stations_correspondances <- Stations_correspondances[,c(16, 7, 12)]
  colnames(Stations_correspondances) <- c("Code", "Name", "Province")
  
  # Get municipalities and stations codes
  stats_comuni <- stations |> filter(Comune %in% Stations_correspondances$Name) |> pull(Comune)
  stats_codes <- comuni_ISTAT |> filter(Denominazione.in.italiano %in% stats_comuni) |> pull(Codice.Comune.formato.numerico)
  
  # Prepare the dataset
  Stations_correspondances <- Stations_correspondances |> add_column(Station_ref_code = NA, 
                                                                     Station_distance_meters = NA, 
                                                                     Station_distance_minutes = NA)
  
  # Apply the function that assigns each municipality to the closest stations
  Stations_correspondances <- closest_station(Stations_correspondances, stats_codes, comuni_distances)
  
  # I add comune and province of the station
  Stations_correspondances <- Stations_correspondances |> left_join(comuni_ISTAT, by = c("Station_ref_code" = "Codice.Comune.formato.numerico")) 
  Stations_correspondances <- Stations_correspondances[,c(1,2,3,4,5,6,13,8)]
  colnames(Stations_correspondances) <- c("Comune_code", "Comune_name", "Comune_province", "Station_ref_code", "Station_ref_meters", "Station_ref_minutes", "Station_ref_name", "Station_ref_province")
  
  # I have to change stations of Milan area into "IS area"
  IS_area_comunis <- stations |> filter(Codice %in% IS_area) |> pull(Comune)
  Stations_correspondances[Stations_correspondances$Station_ref_name %in% IS_area_comunis, "Station_ref_name"] = "IS area"
  Stations_correspondances[which(Stations_correspondances$Station_ref_name == "IS area"), "Station_ref_province"] = "Milano"

  return(Stations_correspondances)
}

# Function to clean OD data of Regione Lombardia
OD_Lombardia_cleaned <- function(OD){
  # Aggregating zones divided into subzones
  OD <- OD %>%
    mutate(
      ZONA_ORIG = str_replace(ZONA_ORIG, " [0-9]+", ""),
      ZONA_DEST = str_replace(ZONA_DEST, " [0-9]+", "")
    )
  
  # Drop fascia oraria and sum by groups
  OD <- OD |> dplyr::select(-(FASCIA_ORARIA)) |> group_by(PROV_ORIG, ZONA_ORIG, PROV_DEST, ZONA_DEST) |>
    summarise(across(where(is.numeric), sum)) |> ungroup()
  
  # Selecting only provinces in Lombardia
  Lombardia_provinces <- c("BG", "BS", "CO", "CR", "LC", "LO", "MB", "MI", "MN", "SO", "VA", "PV")
  
  OD <- OD |> dplyr::filter(PROV_ORIG %in% Lombardia_provinces & PROV_DEST %in% Lombardia_provinces)
  
  ## FIXING SPELLING
  # REM: Municipalities separated by "-" represent a unique comune, while if they are separated by " - " they represent
  #     a group of distant ISTAT comunis
  
  A <- c("ALME'", "BREMBILLA", "CAPRIATE SAN GERVASO", "COSTA DI SERINA", "GARDONE VALTROMPIA", 
         "LONATO", "PUEGNAGO SUL GARDA", "ROE' VOLCIANO", "SALO'", "TEMU'", "TOSCOLANO MADERNO", 
         "TREMOSINE", "VALLIO", "CAGNO", "CANTU'", "CAVALLASCA", "FENEGRO'",
         "DREZZO", "GIRONICO", "LENNO", "MEZZEGRA", "OLTRONA CON SAN MAMETTE", "OSSUCCIO", "PARH",
         "PELLIO INTELVI", "SOLBIATE COMASCO", "CORTENOVA", "GRASSOBIO", "SANT'OMOBONO IMAGNA - VALSECCA",
         "VILLA D'ALME'", "COMEZZANO - CIZZAGO", "RODENGO - SAIANO", "CASASCO D'INTELVI - CASTIGLIONE D'INTELVI - CERANO INTELVI - SAN FEDELE INTELVI",
         "LANZO D'INTELVI - RAMPONIO VERNA", "TREMEZZO", "UGGIATE - TREVANO", "CA' D'ANDREA",
         "DRIZZONA", "GABBIONETA BINANUOVA", "GADESCO PIEVE DELMONA", "GERRE DE'CAPRIOLI", "SAN GIOVANNI INCROCE",
         "BARZANO'", "PEREGO", "ROVAGNATE", "SANTA MARIA HOE'", "VERDERIO INFERIORE", "VERDERIO SUPERIORE", "VIGANO'",
         "BOVISIO MASCIAGO", "MUGGIO'", "CASSINA DE PECCHI", "VERMEZZO", "ZELO SURRIGONE", "BIGARELLO",
         "BORGOFORTE", "BORGOFRANCO SUL PO", "CARBONARA DI PO", "FELONICA", "PIEVE DI CORIANO", "REVERE",
         "SAN GIORGIO DI MANTOVA", "SERMIDE", "VILLA POMA", "VIRGILIO", "BASCAPE'", "CORNALE", 
         "CORTEOLONA", "GAMBOLO'", "MORNICO LOSANNA", "RUINO", "TRAVACO' SICCOMARIO", "ZERBOLO'",
         "BRISSAGO - VALTRAVAGLIA", "CADEGLIANO - VICONAGO", "CADREZZATE - OSMATE", "COCQUIO - TREVISAGO",
         "CUGLIATE - FABIASCO", "GAZZADA - SCHIANNO", "LAVENO MOMBELLO", "TRAVEDONA - MONATE", "VIGGIU'",
         "CAMAIRAGO", "CAVACURTA", "SAN MARTINO IN SCORTE BRUGNATELLA", "TERRANUOVA DEI PASSERINI", "BASTIDA DE' DOSSI",
         "MACCAGNO - PINO SULLA SPONDA DEL LAGO MAGGIORE - TRONZANO LAGO MAGGIORE - VEDDASCA", "MONTESCANO - MONTU' BECCARIA - ZENEVREDO",
         "BASTIDA DE' DOSSI - CASEI GEROLA", "CASALE CREMASCO - VIDOLASCO - CASTEL GABBIANO", "INTROZZO - PAGNONA - TREMENICO",
         "GEROSA - SAN GIOVANNI BIANCO", "BIENNO - PRESTINE", "CIVENNA - MAGREGLIO", "BELLAGIO", "PIADENA - VOLTIDO",
         "CASARGO - MARGNO - VENDROGNO", "BELLANO", "SUEGLIO - VESTRENO", "CANEVINO - MONTECALVO VERSIGGIA - ROCCA DE' GIORGI - VOLPARA",
         "COPIANO - GENZONE", "VALVERDE - VARZI", "GORDONA - MENAROLA"
  )
  B <- c("ALMÈ", "VAL BREMBILLA - SAN GIOVANNI BIANCO", "CAPRIATE SAN GERVASIO", "COSTA SERINA", "GARDONE VAL TROMPIA",
         "LONATO DEL GARDA", "PUEGNAGO DEL GARDA", "ROÈ VOLCIANO", "SALÒ", "TEMÙ", "TOSCOLANO-MADERNO",
         "TREMOSINE SUL GARDA", "VALLIO TERME", "SOLBIATE CON CAGNO", "CANTÙ", "SAN FERMO DELLA BATTAGLIA", "FENEGRÒ",
         "COLVERDE", "COLVERDE", "TREMEZZINA", "TREMEZZINA", "OLTRONA DI SAN MAMETTE", "TREMEZZINA", "COLVERDE",
         "ALTA VALLE INTELVI", "SOLBIATE CON CAGNO", "CORTENUOVA - PARLASCO - TACENO", "GRASSOBBIO", "SANT'OMOBONO TERME",
         "VILLA D'ALMÈ", "COMEZZANO-CIZZAGO", "RODENGO SAIANO", "CERANO D'INTELVI - CENTRO VALLE INTELVI",
         "CERANO D'INTELVI - CENTRO VALLE INTELVI", "TREMEZZINA", "UGGIATE-TREVANO", "TORRE DE' PICENARDI",
         "PIADENA DRIZZONA - VOLTIDO", "GABBIONETA-BINANUOVA", "GADESCO-PIEVE DELMONA", "GERRE DE' CAPRIOLI", "SAN GIOVANNI IN CROCE",
         "BARZANÒ", "LA VALLETTA BRIANZA", "LA VALLETTA BRIANZA", "SANTA MARIA HOÈ", "VERDERIO", "VERDERIO", "VIGANÒ",
         "BOVISIO-MASCIAGO", "MUGGIÒ", "CASSINA DE' PECCHI", "VERMEZZO CON ZELO", "VERMEZZO CON ZELO", "SAN GIORGIO BIGARELLO",
         "BORGO VIRGILIO", "BORGOCARBONARA", "BORGOCARBONARA", "SERMIDE E FELONICA", "BORGO MANTOVANO", "BORGO MANTOVANO",
         "SAN GIORGIO BIGARELLO", "SERMIDE E FELONICA", "BORGO MANTOVANO", "BORGO VIRGILIO", "BASCAPÈ", "CORNALE E BASTIDA - CASEI GEROLA",
         "COPIANO - CORTEOLONA E GENZONE", "GAMBOLÒ", "MORNICO LOSANA", "COLLI VERDI - MONTECALVO VERSIGGIA - ROCCA DE' GIORGI - VOLPARA - VARZI", "TRAVACÒ SICCOMARIO", "ZERBOLÒ",
         "BRISSAGO-VALTRAVAGLIA", "CADEGLIANO-VICONAGO", "CADREZZATE CON OSMATE", "COCQUIO-TREVISAGO",
         "CUGLIATE-FABIASCO", "GAZZADA SCHIANNO", "LAVENO-MOMBELLO", "TRAVEDONA-MONATE", "VIGGIÙ",
         "CASTELGERUNDO", "CASTELGERUNDO", "SAN MARTINO IN STRADA", "TERRANOVA DEI PASSERINI", "CORNALE E BASTIDA - CASEI GEROLA",
         "MACCAGNO CON PINO E VEDDASCA - TRONZANO LAGO MAGGIORE", "MONTESCANO - MONTÙ BECCARIA - ZENEVREDO",
         "CORNALE E BASTIDA - CASEI GEROLA", "CASALE CREMASCO-VIDOLASCO - CASTEL GABBIANO", "SUEGLIO - VALVARRONE - PAGNONA",
         "VAL BREMBILLA - SAN GIOVANNI BIANCO", "BIENNO", "BELLAGIO - MAGREGLIO", "BELLAGIO - MAGREGLIO", "PIADENA DRIZZONA - VOLTIDO",
         "CASARGO - MARGNO - BELLANO", "CASARGO - MARGNO - BELLANO", "SUEGLIO - VALVARRONE - PAGNONA", "COLLI VERDI - MONTECALVO VERSIGGIA - ROCCA DE' GIORGI - VOLPARA - VARZI",
         "COPIANO - CORTEOLONA E GENZONE", "COLLI VERDI - MONTECALVO VERSIGGIA - ROCCA DE' GIORGI - VOLPARA - VARZI", "GORDONA"
  )
  
  # Substitute denominations in vector A in OD with denominations in vector B
  OD <- OD %>%
    mutate(
      ZONA_ORIG = case_when(
        ZONA_ORIG %in% A ~ B[match(ZONA_ORIG, A)],
        TRUE ~ ZONA_ORIG
      ),
      ZONA_DEST = case_when(
        ZONA_DEST %in% A ~ B[match(ZONA_DEST, A)],
        TRUE ~ ZONA_DEST
      )
    )
  
  # Aggregate
  OD <- OD |> group_by(PROV_ORIG, ZONA_ORIG, PROV_DEST, ZONA_DEST) |>
    summarise(across(where(is.numeric), sum)) |> ungroup()
  # Remove self-loops
  OD <- OD |> dplyr::filter(!(ZONA_ORIG == ZONA_DEST))
  
  # Compute the colums summarizing the overall number of movements for each OD couple
  OD <- OD |> mutate(TOT = rowSums(across(where(is.numeric))))
  # Compute the colums summarizing the railway number of movements for each OD couple
  OD <- OD |> mutate(FERRO = rowSums(select(cur_data(), ends_with("FERRO"))))
  
  return(OD)
}

# Function to generate the dataset of correspondances between OD areas and ISTAT municipalities in Lombardia
generate_matches_OD_municipalities <- function(OD, ISTAT){
  # Select only ISTAT municipalities in Lombardia
  ISTAT <- ISTAT |> filter(`Codice Regione` == "03")
  
  # Prepare dataset of correspondances
  # NOTE: I need to select the ISTAT dataset by column indexes because of their naming which causes problems
  Correspondances <- ISTAT[,c(5,7)] |>
    rename(ISTAT_code = 1, Comune_name = 2) |>
    add_column(OD_name = NA)
  
  # FIRST CASE: if the OD matrix contains an OD_area which matches exactly one municipality name, then I add the match
  Correspondances <- Correspondances %>%
    mutate(
      OD_name = ifelse(tolower(Comune_name) %in% tolower(OD$ZONA_ORIG), toupper(Comune_name), OD_name)
    )
  
  # SECOND CASE: when the OD areas are aggregation of municipalities, I separe them and look for the match with an ISTAT municipality
  # This was needed to check if there were unmached municipalities
  unmatched_comunis <- NULL
  # The zones resulting from agregated municipalities can be identified looking for "-" in the name of the zone
  aggr_zones <- unique(OD$ZONA_ORIG)[grep("-", unique(OD$ZONA_ORIG))]
  
  for (act in aggr_zones){
    # I separate the municipalities which constitute the aggregated zone
    municipalities <- unlist(str_split(act, " - "))
    
    for (mun in municipalities){
      # If I have a match with a ISTAT municipality, I add it in the dataset
      if (tolower(mun) %in% tolower(Correspondances$Comune_name))
        Correspondances[tolower(Correspondances$Comune_name) == tolower(mun),"OD_name"] <- act
      # If not, I add it to the unmatched comunis
      else
        unmatched_comunis <- c(unmatched_comunis, tolower(mun))
    }
  }
  
  # Throw an error if I have an unmatched OD name
  if (!is.null(unmatched_comunis))
    stop("There are unmatched OD zones")
  
  # Throw an error if I have an unmatched ISTAT municiplaity
  if (any(is.na(Correspondances$OD_name)))
    stop("There are unmatched municipalities")
  
  return(Correspondances)
}

# Aggregate OD of Regione Lombardia and clean to use at BreBeMi level
aggregate_OD_RL_BreBeMi <- function(OD, matches, stations, IS_area){
  # Compute total movements
  OD <- OD |> mutate(TOT = rowSums(across(where(is.numeric))))
  
  # Compute how many people are moving by railway for each OD couple 
  OD <- OD |> mutate(FERRO = rowSums(across(ends_with('_FERRO'))))
  
  OD <- OD |> dplyr::filter(ZONA_ORIG %in% unique(matches$OD_name) & ZONA_DEST %in% unique(matches$OD_name))
  
  # I also have to aggregate Milan area
  # Comuni in IS area are
  comuni_IS_area <- stations |> filter(Codice %in% IS_area) |> pull(Comune) |> unique()
  matches <- matches |> mutate(Station_ref_name = replace(Station_ref_name, Station_ref_name %in% comuni_IS_area, "IS area"))

  # Pass to stations 
  OD$ZONA_ORIG <-  with(matches, Station_ref_name[match(OD$ZONA_ORIG, OD_name)])
  OD$ZONA_DEST <-  with(matches, Station_ref_name[match(OD$ZONA_DEST, OD_name)])
  
  OD <- OD |> dplyr::select(!c(PROV_ORIG, PROV_DEST)) |> group_by(ZONA_ORIG, ZONA_DEST) |>
    summarise(across(where(is.numeric), sum)) |> na.omit()
}

# Aggregate stations of OD Trenord having the same municipality
aggregate_municipalities_Trenord <- function(OD_Trenord, stations, station_codes, IS_area){
  # Selecting only stations of my study area
  stations <- stations |> filter(Codice %in% station_codes)
  
  # Adding IS area to stations
  # Add IS_area to station
  temp <- NULL
  temp$Codice <- "S99999"
  temp$Nome <- "IS area"
  temp$Comune <- "IS area"
  temp$Provincia <- "MI"
  stations <- rbind(stations, temp)
  rm(temp)
  
  # Aggregating the IS_area
  # Substitute Milan codes with S9999 and aggregate
  OD_Trenord <- OD_Trenord |> mutate(Start = replace(Start, Start %in% IS_area, "S99999"), 
                     End = replace(End, End %in% IS_area, "S99999")) |> group_by(Start, End) |> 
    summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE))) |> ungroup()
  
  # Comunis having more than one station are Bergamo and Treviglio
  # View(stations |> group_by(Comune) |> filter(n()>1) |> summarize(n=n()))
  
  # We have to aggregate Bergamo and Treviglio 
  # Bergamo stations will pass from S01529 and S01530 to S01529
  # Treviglio stations will pass from S01708 and S01601 to S01708
  # I define a dictionary, containing old code and new code
  dict_stations <- data.frame("Old" = stations$Codice, 
                              "New" = stations$Codice)
  dict_stations[dict_stations$Old == "S01530", "New"] = "S01529"  
  dict_stations[dict_stations$Old == "S01601", "New"] = "S01708"  
  # I need to add manually the artificial Milan aggregated stations
  dict_stations <- rbind(dict_stations, data.frame("Old" = "S99999", "New" = "S99999"))

  # Replacing the codes 
  OD_Trenord$Start <- with(dict_stations, New[match(OD_Trenord$Start, Old)])
  OD_Trenord$End <- with(dict_stations, New[match(OD_Trenord$End, Old)])
  
  # Now I replace the station code with the Comune name 
  OD_Trenord$Start <- with(stations, Comune[match(OD_Trenord$Start, Codice)])
  OD_Trenord$End <- with(stations, Comune[match(OD_Trenord$End, Codice)])
  
  # Summing equal rows to aggregate
  OD_Trenord <- OD_Trenord |> group_by(Start, End) |> summarize(across(.cols = where(is.numeric),.fns = sum))
  
  return(OD_Trenord)
}

# Function to perform the correlation analysis between OD data of Trenord and Regione Lombardia
correlation_OD_analysis <- function(OD_Trenord, OD_RL, weeks){
  # Join and fix NA
  df <- dplyr::full_join(OD_RL, OD_Trenord, by = c("ZONA_ORIG" = "Start", "ZONA_DEST" = "End"))
  df <- df |> group_by(ZONA_ORIG, ZONA_DEST) |> slice(1) |> ungroup()
  # Remove self loops
  df <- df |> dplyr::filter(ZONA_ORIG != ZONA_DEST)
  
  # Remove undirect loops since I do not want to compare them 
  df <- df |> mutate(SUM = rowSums(select(df, starts_with("Week")))) |>
    filter(SUM != 0) |> select(-SUM)
  
  # Remove Peschiera and Verona (stations in Veneto for which we cannot reconstruct matches)
  df <- df |> dplyr::filter(!(ZONA_ORIG %in% c("Peschiera del Garda", "Verona")) & !(ZONA_DEST %in% c("Peschiera del Garda", "Verona")))
  
  # Replace all the na with 0
  df <- df |> ungroup() |> mutate_all(~replace(., is.na(.), 0))
  
  # COMPUTE CORRELATION between OD Lombardia and every week of OD Trenord
  cor_p <- NULL
  cor_s <- NULL
  coeff <- NULL
  R2 <- NULL
  for (w in weeks){
    col <- paste0("Week_",w)
    removed_stats <- setdiff(unique(df$ZONA_ORIG), unique(df[which(df[,col] > 0),"ZONA_ORIG"]) |> pull())
    df_cur <- df |> dplyr::filter(!(ZONA_ORIG %in% removed_stats) & !(ZONA_DEST %in% removed_stats))
    cor_p <- c(cor_p, cor(df_cur$FERRO, df_cur[,col], method = "pearson"))
    cor_s <- c(cor_s, cor(df_cur$FERRO, df_cur[,col], method = "spearman"))
    A <- lm(as.numeric(df_cur[,col] |> pull()) ~ df_cur$FERRO - 1)
    s <- summary(A)
    R2 <- c(R2, s$r.squared)
    coeff <- c(coeff, A$coefficients[1])
  }
  
  df2 <- as.data.frame(cbind(weeks, cor_p, cor_s, coeff, R2))
  colnames(df2) <- c("Week", "Correlation_p", "Correlation_s", "Coefficient", "Rsquared")
  weeks <- gsub("_","-", weeks) 
  df2$Week <- as.Date(unlist(lapply(weeks, function(x) {
    if(str_sub(x,-2) == "00")
      return(as.character(as.Date(paste(substr(x,1,4),1, 1, sep="-"))))
    return(as.character(as.Date(paste(x, 1, sep="-"), "%Y-%U-%u")))})))
  df2[,2:5] <- apply(df2[,2:5], 2, as.numeric)
  
  return(df2)
}

# Function to produce the geographical dataset of BreBeMi area aggregated into basins
BreBeMi_geodf <- function(matches){
  # Load geographical dataset of Italy
  italy_geo <- st_read("Data/ISTAT/General_data/Limiti01012022_g/Com01012022_g")
  
  italy_geo <- italy_geo |> dplyr::filter(COMUNE %in% matches$Comune_name) |> 
    left_join(matches, by = c("COMUNE" = "Comune_name"))
  
  # Aggregation 
  stat_agg <- italy_geo %>%
    group_by(Station_ref_name) %>% 
    summarize(geometry = st_union(geometry))
  rm(italy_geo)
  
  # Remove Verona and Peschiera
  stat_agg <- stat_agg |> dplyr::filter(!(Station_ref_name %in% c("Verona", "Peschiera del Garda")))
  
  return(stat_agg)
}

