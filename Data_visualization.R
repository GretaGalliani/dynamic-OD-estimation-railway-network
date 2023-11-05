## GOAL: Exploration of the data produced in estimating Trenord OD matrices
library(dbplyr)
library(ggplot2)
library(lubridate)
library(tidyverse)
library(RColorBrewer)
library(grid)
library(gridExtra)
library(ggpubr)
library(stringr)
library(ggrepel)
library(scales)

rm(list = ls())
set.seed(24091998)

# Prepare palettes
pal <- brewer.pal(12, "Set3")
palx <- c(pal,pal)
display.brewer.pal(12, "Set3")

load("Data/Trenord/Processed/station_codes.Rdata")
source("utils.R")

if (!dir.exists("Plots")) dir.create("Plots", recursive = TRUE)

# Load stations 
stations <- read.csv("Data/Trenord/stations_processed.csv")

# Rename
stations$Nome <- str_to_title(tolower(stations$Nome))
stations$Nome[which(stations$Nome == "Albano S.alessandro")] <- "Albano S. Alessandro"
stations$Nome[which(stations$Nome == "Cassano D'adda")] <- "Cassano D'Adda"
stations$Nome[which(stations$Nome == "Desenzano Del Garda-Sirmione")] <- "Desenzano del Garda - Sirmione"
stations$Nome[which(stations$Nome == "Grumello Del Monte")] <- "Grumello del Monte"
stations$Nome[which(stations$Nome == "Palazzolo Sull'oglio")] <- "Palazzolo sull'Oglio"
stations$Nome[which(stations$Nome == "Peschiera Del Garda")] <- "Peschiera del Garda"
stations$Nome[which(stations$Nome == "Ponte S.pietro")] <- "Ponte S. Pietro"
stations$Nome[which(stations$Nome == "Sesto S.giovanni")] <- "Sesto S. Giovanni"

#### 1. OD ESTIMATION PLOTS ----
if (!dir.exists("Plots/OD_estimation")) dir.create("Plots/OD_estimation", recursive = TRUE)
#### 1.1. TICKET DATA PLOTS ----
if (!dir.exists("Plots/OD_estimation/Tickets")) dir.create("Plots/OD_estimation/Tickets", recursive = TRUE)
#### Tickets by week divided by types ----
# Importing ticket datasets
tickets <- read.csv("Data/Trenord/Original/ticket_data_2022.csv", sep=";")

# I preprocess tickets
tickets <- clean_tickets(tickets, station_codes)

# Convert date format to week
tickets$Date <- format(tickets$Date, "%Y-%W")

tickets$Type <- as.character(tickets$Type)

# Fixing factors name
tickets[tickets$Type == "ABBONAMENTI ANNUALI", "Type"] = "Annual subscription"
tickets[tickets$Type == "ABBONAMENTI MENSILI", "Type"] = "Monthly subscription"
tickets[tickets$Type == "ABBONAMENTI SETTIMANALI", "Type"] =  "Weekly subscription"
tickets[tickets$Type == "ALTRO OPERATORE", "Type"] = "Other operator"
tickets[tickets$Type == "BIGLIETTI ORDINARI", "Type"] = "Ordinary ticket"
tickets[tickets$Type == "CARNET", "Type"] = "Carnet"
tickets[tickets$Type == "ESAZIONE SUPPLETIVA", "Type"] = "Supplementary correction"
tickets[tickets$Type == "INIZIATIVE TARIFFE SPECIALI", "Type"] = "Special rates initiative"
tickets[tickets$Type == "MALPENSA EXPRESS", "Type"] = "Malpensa Express"
tickets[tickets$Type == "RESTI NON CORRISPOSTI", "Type"] = "Unpaid remains"
tickets[tickets$Type == "SANZIONI", "Type"] = "Sanctions"
tickets[tickets$Type == "SUPPLEMENTI", "Type"] = "Supplements"

# Set palette
paln <- c(pal[11], pal[10], pal[2], pal[1], pal[8], pal[9], pal[7], pal[3], pal[4],
          pal[5], pal[12])

# Substitute every week with its first day
tickets$Date <- as.Date(unlist(lapply(tickets$Date, function(x) {
  if(str_sub(x,-2) == "00")
    return(as.character(as.Date(paste("2019-12-30"))))
  return(as.character(as.Date(paste(x, 1, sep="-"), "%Y-%U-%u")))})))

# Compute total
tickets <- tickets |> group_by(Date, Type) |> summarise(Total = sum(Quantity))

# Plot
p2 <- ggplot(tickets, aes(x = Date, y = Total, fill = Type)) + 
  geom_bar(stat = "identity") + theme_bw() + scale_fill_manual(values = paln) +
  ylab("Tickets") + ggtitle("Number of purchased tickets stratified by types") + xlab("Date") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B") + theme(axis.text.x = element_text(angle = 50, hjust=0.95), axis.title.x = element_blank())
p2

if (!dir.exists("Plots/OD_estimation/Tickets")) dir.create("Plots/OD_estimation/Tickets", recursive = TRUE)
ggsave("Plots/OD_estimation/Tickets/Tickets_number_types.png", p2, width = 20, height = 12, units = "cm", dpi = 320)
rm(tickets, p2, paln)

#### OD ticket-estimated matrices - step 1 (after tickets conversion into estimated trips) ----
# Load tickets
OD_tickets <- read.csv("Data/Trenord/Processed/Seeds/OD_seeds_step1.csv", header=TRUE)

# Generate weeks
weeks <- colnames(OD_tickets)[3:dim(OD_tickets)[2]]
weeks <- str_sub(weeks, 6)

if (!dir.exists("Plots/OD_estimation/Seeds/Step1")) dir.create("Plots/OD_estimation/Seeds/Step1", recursive = TRUE)

# Produce plots
for (w in weeks){
  print(w)
  
  OD_cur <- OD_week(OD_tickets, w, station_codes)
  
  # Recovering stations names
  temp <- NULL
  temp <- as.data.frame(matrix(0, nrow = length(station_codes), ncol = 1))
  colnames(temp) <- "Station"
  temp$Station <- sort(station_codes)
  temp <- temp |> dplyr::left_join(stations, by = c("Station" = "Codice"))
  
  rownames(OD_cur) <- with(temp, Nome[match(rownames(OD_cur), Station)])
  colnames(OD_cur) <- with(temp, Nome[match(colnames(OD_cur), Station)])
  
  OD_cur_p <-  OD_cur %>%
    as.data.frame() %>%
    rownames_to_column("Station_start") %>%
    pivot_longer(-c(Station_start), names_to = "Station_end", values_to = "Counts")
  OD_cur_p$Station_start <- factor(OD_cur_p$Station_start, levels = temp$Nome)
  OD_cur_p$Station_end <- factor(OD_cur_p$Station_end, levels = temp$Nome)
  
  if(str_sub(w, start= -2) == "00"){
    p1 <- OD_cur_p %>%
      ggplot(aes(x=Station_end, y=Station_start, fill=Counts)) +
      geom_raster() +
      scale_fill_gradient(low='white', high='dodgerblue4', limits = c(0,1000), na.value = 'dodgerblue4') +
      # geom_raster(data = subset(OD_cur_p, Counts == 0), fill = "yellow") + # To highlight in yellow the 0 cells for analyses
      ggtitle(paste("Seed OD matrix step1 - week from", as.Date(paste("2021_52", 1, sep="_"), "%Y_%U_%u"), "to", as.Date(paste(w, 1, sep="_"), "%Y_%U_%u")-1)) +
      labs(x ="End station", y = "Start station") +
      theme(text = element_text(size = 13), axis.text.x = element_text(angle = 50, hjust=0.95, size = 9),
            axis.text.y = element_text(size = 9)) +
      theme(plot.title = element_text(hjust = + 0.5, vjust=2.12)) 
    
    p1
  } else {
    # To move title to the left, increase theme -> plot.title -> elemnt_text -> hjust
    p1 <- OD_cur_p %>%
      ggplot(aes(x=Station_end, y=Station_start, fill=Counts)) +
      geom_raster() +
      scale_fill_gradient(low='white', high='dodgerblue4', limits = c(0,1000), na.value = 'dodgerblue4') +
      # geom_raster(data = subset(OD_cur_p, Counts == 0), fill = "yellow") + # To highlight in yellow the 0 cells for analyses
      ggtitle(paste("Seed OD matrix step1 - week from", as.Date(paste(w, 1, sep="_"), "%Y_%U_%u"), "to", as.Date(paste(w, 1, sep="_"), "%Y_%U_%u")+6)) +
      labs(x ="End station", y = "Start station") +
      theme(text = element_text(size = 13), axis.text.x = element_text(angle = 50, hjust=0.95, size = 9),
            axis.text.y = element_text(size = 9)) +
      theme(plot.title = element_text(hjust = + 0.5, vjust=2.12)) 
    
    p1
  }
  
  ggsave(paste("Plots/OD_estimation/Seeds/Step1/Seed_",w,".png", sep = ""), p1,
         width = 32, height = 25, units = "cm", dpi = 320)
}
rm(OD_cur, OD_cur_p, OD_tickets, p1, temp)


#### OD ticket-estimated matrices - step 2 (after separation of trips) ----
# Load tickets
OD_tickets <- read.csv("Data/Trenord/Processed/Seeds/OD_seeds_step2.csv", header=TRUE)

# Generate weeks
weeks <- colnames(OD_tickets)[3:dim(OD_tickets)[2]]
weeks <- str_sub(weeks, 6)

if (!dir.exists("Plots/OD_estimation/Seeds/Step2")) dir.create("Plots/OD_estimation/Seeds/Step2", recursive = TRUE)

# Produce plots
for (w in weeks){
  print(w)
  
  OD_cur <- OD_week(OD_tickets, w, station_codes)
  
  # Recovering stations names
  temp <- NULL
  temp <- as.data.frame(matrix(0, nrow = length(station_codes), ncol = 1))
  colnames(temp) <- "Station"
  temp$Station <- sort(station_codes)
  temp <- temp |> dplyr::left_join(stations, by = c("Station" = "Codice"))
  
  rownames(OD_cur) <- with(temp, Nome[match(rownames(OD_cur), Station)])
  colnames(OD_cur) <- with(temp, Nome[match(colnames(OD_cur), Station)])
  
  OD_cur_p <-  OD_cur %>%
    as.data.frame() %>%
    rownames_to_column("Station_start") %>%
    pivot_longer(-c(Station_start), names_to = "Station_end", values_to = "Counts")
  OD_cur_p$Station_start <- factor(OD_cur_p$Station_start, levels = temp$Nome)
  OD_cur_p$Station_end <- factor(OD_cur_p$Station_end, levels = temp$Nome)
  
  if(str_sub(w, start= -2) == "00"){
    p1 <- OD_cur_p %>%
      ggplot(aes(x=Station_end, y=Station_start, fill=Counts)) +
      geom_raster() +
      scale_fill_gradient(low='white', high='dodgerblue4', limits = c(0,1000), na.value = 'dodgerblue4') +
      # geom_raster(data = subset(OD_cur_p, Counts == 0), fill = "yellow") + # To highlight in yellow the 0 cells for analyses
      ggtitle(paste("Seed OD matrix step2 - week from", as.Date(paste("2019_52", 1, sep="_"), "%Y_%U_%u"), "to", as.Date(paste(w, 1, sep="_"), "%Y_%U_%u")-1)) +
      labs(x ="End station", y = "Start station") +
      theme(text = element_text(size = 13), axis.text.x = element_text(angle = 50, hjust=0.95, size = 9),
            axis.text.y = element_text(size = 9)) +
      theme(plot.title = element_text(hjust = + 0.5, vjust=2.12)) 
    
    p1
  } else {
    # To move title to the left, increase theme -> plot.title -> elemnt_text -> hjust
    p1 <- OD_cur_p %>%
      ggplot(aes(x=Station_end, y=Station_start, fill=Counts)) +
      geom_raster() +
      scale_fill_gradient(low='white', high='dodgerblue4', limits = c(0,1000), na.value = 'dodgerblue4') +
      # geom_raster(data = subset(OD_cur_p, Counts == 0), fill = "yellow") + # To highlight in yellow the 0 cells for analyses
      ggtitle(paste("Seed OD matrix step2 - week from", as.Date(paste(w, 1, sep="_"), "%Y_%U_%u"), "to", as.Date(paste(w, 1, sep="_"), "%Y_%U_%u")+6)) +
      labs(x ="End station", y = "Start station") +
      theme(text = element_text(size = 13), axis.text.x = element_text(angle = 50, hjust=0.95, size = 9),
            axis.text.y = element_text(size = 9)) +
      theme(plot.title = element_text(hjust = + 0.5, vjust=2.12)) 
    
    p1
  }
  
  # MODIFY PATH FOR THESIS
  ggsave(paste("Plots/OD_estimation/Seeds/Step2/Seed",w,".png", sep = ""), p1,
         width = 32, height = 25, units = "cm", dpi = 320)
}
rm(OD_cur, OD_cur_p, OD_tickets, p1, temp)

#### OD ticket-estimated matrices - step 3 (after estimation of missing trips) ----
# Load tickets
OD_tickets <- read.csv("Data/Trenord/Processed/Seeds/OD_seeds_step3.csv", header=TRUE)

# Generate weeks
weeks <- colnames(OD_tickets)[3:dim(OD_tickets)[2]]
weeks <- str_sub(weeks, 6)

if (!dir.exists("Plots/OD_estimation/Seeds/Step3")) dir.create("Plots/OD_estimation/Seeds/Step3", recursive = TRUE)


# Produce plots
for (w in weeks){
  print(w)
  
  OD_cur <- OD_week(OD_tickets, w, station_codes)
  
  # Recovering stations names
  temp <- NULL
  temp <- as.data.frame(matrix(0, nrow = length(station_codes), ncol = 1))
  colnames(temp) <- "Station"
  temp$Station <- sort(station_codes)
  temp <- temp |> dplyr::left_join(stations, by = c("Station" = "Codice"))
  
  rownames(OD_cur) <- with(temp, Nome[match(rownames(OD_cur), Station)])
  colnames(OD_cur) <- with(temp, Nome[match(colnames(OD_cur), Station)])
  
  OD_cur_p <-  OD_cur %>%
    as.data.frame() %>%
    rownames_to_column("Station_start") %>%
    pivot_longer(-c(Station_start), names_to = "Station_end", values_to = "Counts")
  OD_cur_p$Station_start <- factor(OD_cur_p$Station_start, levels = temp$Nome)
  OD_cur_p$Station_end <- factor(OD_cur_p$Station_end, levels = temp$Nome)
  
  if(str_sub(w, start= -2) == "00"){
    p1 <- OD_cur_p %>%
      ggplot(aes(x=Station_end, y=Station_start, fill=Counts)) +
      geom_raster() +
      scale_fill_gradient(low='white', high='dodgerblue4', limits = c(0,1000), na.value = 'dodgerblue4') +
      # geom_raster(data = subset(OD_cur_p, Counts == 0), fill = "yellow") + # To highlight in yellow the 0 cells for analyses
      ggtitle(paste("Seed OD matrix step3- week from", as.Date(paste("2019_52", 1, sep="_"), "%Y_%U_%u"), "to", as.Date(paste(w, 1, sep="_"), "%Y_%U_%u")-1)) +
      labs(x ="End station", y = "Start station") +
      theme(text = element_text(size = 13), axis.text.x = element_text(angle = 50, hjust=0.95, size = 9),
            axis.text.y = element_text(size = 9)) +
      theme(plot.title = element_text(hjust = + 0.5, vjust=2.12)) 
    
    p1
  } else {
    # To move title to the left, increase theme -> plot.title -> elemnt_text -> hjust
    p1 <- OD_cur_p %>%
      ggplot(aes(x=Station_end, y=Station_start, fill=Counts)) +
      geom_raster() +
      scale_fill_gradient(low='white', high='dodgerblue4', limits = c(0,1000), na.value = 'dodgerblue4') +
      # geom_raster(data = subset(OD_cur_p, Counts == 0), fill = "yellow") + # To highlight in yellow the 0 cells for analyses
      ggtitle(paste("Seed OD matrix step3 - week from", as.Date(paste(w, 1, sep="_"), "%Y_%U_%u"), "to", as.Date(paste(w, 1, sep="_"), "%Y_%U_%u")+6)) +
      labs(x ="End station", y = "Start station") +
      theme(text = element_text(size = 13), axis.text.x = element_text(angle = 50, hjust=0.95, size = 9),
            axis.text.y = element_text(size = 9)) +
      theme(plot.title = element_text(hjust = + 0.5, vjust=2.12)) 
    
    p1
  }
  
  # MODIFY PATH FOR THESIS
  ggsave(paste("Plots/OD_estimation/Seeds/Step3/Seed_",w,".png", sep = ""), p1,
         width = 32, height = 25, units = "cm", dpi = 320)
}
rm(OD_cur, OD_cur_p, OD_tickets, p1, temp)


#### 1.2. COUNTER DATA PLOTS ----
if (!dir.exists("Plots/OD_estimation/Counters")) dir.create("Plots/OD_estimation/Counters", recursive = TRUE)

# load counter data 
train_count <- read.csv("Data/Trenord/Original/counter_data_2022.csv", sep = ";")

# Adjust dates
train_count$Data.missione <- as.Date(train_count$Data.missione, "%Y/ %m/ %d")

#### Number of trains by line ----
TC <- train_count |> group_by(Data.missione, Linea, Codice.treno) |> filter(!(Stato %in% c(-5, -2))) |> slice(1)

# Adjust to weeks
TC$Data.missione <- format(TC$Data.missione, "%Y-%W")

# Computing counts
TC <- TC |> group_by(Data.missione, Linea) |> summarise(N_trains = n())
colnames(TC) <- c("Data.missione", "Line", "N_trains")

TC$Line <- recode_factor(TC$Line, R1 = "R1", R2 = "R2", R4 = "R4", R14 = "R14", RE_2 = "RE 2", RE_6 = "RE 6")

TC$Data.missione <- as.Date(unlist(lapply(TC$Data.missione, function(x) {
  if(str_sub(x,-2) == "00")
    return(as.character(as.Date(paste(substr(x,1,4),1, 1, sep="-"))))
  return(as.character(as.Date(paste(x, 1, sep="-"), "%Y-%U-%u")))})))

p1 <- ggplot(TC, aes(x = Data.missione, y = N_trains, fill = Line)) + 
  geom_bar(stat = "identity") + theme_bw() + scale_fill_brewer(palette="Set3") +
  ylab("Trains") + ggtitle("Number of trains per week stratified by lines") + xlab("Date") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B") + theme(axis.text.x = element_text(angle = 50, hjust=0.95), axis.title.x = element_blank())
p1

ggsave("Plots/OD_estimation/Counters/Trains_number_lines.png", p1, width = 20, height = 12, units = "cm", dpi = 320)
rm(TC, p1)

#### States distribution over the year ----
# Apply the function to generate a new column describing the outcome of the counter state
TC <- train_count |> group_by(Data.missione, Linea, Codice.treno) |> slice(1)
TC$Data.missione <- format(TC$Data.missione, "%Y-%W")
TC <- TC |> rowwise() |> mutate(DATA_status = status_coversion(Stato))

# Counting states (not divided by line)
aux <- TC |> group_by(Data.missione, DATA_status) |> 
  summarise(cnt = n()) |>
  mutate(freq = round(cnt / sum(cnt), 3))


aux$Data.missione <- as.Date(unlist(lapply(aux$Data.missione, function(x) {
  if(str_sub(x,-2) == "00")
    return(as.character(as.Date(paste(substr(x,1,4),1, 1, sep="-"))))
  return(as.character(as.Date(paste(x, 1, sep="-"), "%Y-%U-%u")))})))

palx = c(pal[9], pal[4], "gray48", pal[7])
p2 <- ggplot(aux, aes(x = Data.missione, y = cnt, fill = DATA_status)) + 
  geom_bar(stat = "identity") + theme_bw() + scale_fill_manual(values = palx, 
                                                               name = "Status",
                                                               labels = c("Cancelled", "Missing data", "Valid")) +
  ylab("Trains") + ggtitle("Number of trains per week stratified by APC states") + xlab("Date") + 
  labs(fill='Counters states') +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B") + theme(axis.text.x = element_text(angle = 50, hjust=0.95), axis.title.x = element_blank())
p2

ggsave("Plots/OD_estimation/Counters/Trains_number_states.png", p2, width = 20, height = 12, units = "cm", dpi = 320)
rm(aux, p2, TC)

#### Partial valid boarded and dropped passengers ----
# Load partial marginals
Marg <- read.csv("Data/Trenord/Processed/Marginals/partial_marginals.csv")

Marg$Week <- as.Date(unlist(lapply(Marg$Week, function(x) {
  if(str_sub(x,-2) == "00")
    return(as.character(as.Date(paste(substr(x,1,4),1, 1, sep="-"))))
  return(as.character(as.Date(paste(x, 1, sep="_"), "%Y_%U_%u")))})))

p3 <- ggplot(Marg, aes(x = Week, y = Passengers_boarded)) + 
  geom_bar(stat = "identity", color=pal[6], fill = pal[6]) + theme_bw() +
  ylab("Passengers") + ggtitle("Number of boarded passengers from valid counters data") + xlab("Date") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B") + theme(axis.text.x = element_text(angle = 50, hjust=0.95), text = element_text(size=15), axis.title.x = element_blank())
p3
p4 <- ggplot(Marg, aes(x = Week, y = Passengers_dropped)) + 
  geom_bar(stat = "identity", color=pal[5], fill = pal[5]) + theme_bw() +
  ylab("Passengers") + ggtitle("Number of dropped passengers from valid counters data") + xlab("Date") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B") + theme(axis.text.x = element_text(angle = 50, hjust=0.95), text = element_text(size=15), axis.title.x = element_blank())
p4

ggsave("Plots/OD_estimation/Counters/Total_boarded_passengers.png", p3)
ggsave("Plots/OD_estimation/Counters/Total_dropped_passengers.png", p4)
rm(p3, p4, Marg)

#### APC states waffle plot ----
TC <- train_count |> mutate_at(c('Codice.treno', 'Stato', 'Linea'), as.factor)

# Generate a new column stratifying counter states in valid (V), missing (M), partially cancelled (PC) and cancelled (C)
TC <- TC |> rowwise() |> mutate(DATA_status = status_coversion(Stato))

# Palette
palx = c(pal[9], pal[4], "gray48", pal[7])

lines <- levels(TC$Linea)
lines_names <- c("R1","R14", "R2", "R4", "RE2", "RE6")

# Producing 6 plots, one for every line
# I order train by estimated departure time in the day
TC$Ora.partenza <- as.POSIXct(TC$Ora.partenza, format = "%Y/%m/%d %H:%M:%S")

# Extract hour and minute
TC$Ora.partenza <- format(TC$Ora.partenza, format = "%H:%M")

for (i in 1:6){
  line <- lines[i]
  line_name <- lines_names[i]
  df_line <- TC |> filter(Linea == line) |> dplyr::select(Data.missione, Ora.partenza, DATA_status) |> 
    group_by(Data.missione, Ora.partenza)  |> slice(1)
  
  p <- ggplot(df_line, aes(x = Data.missione, y = Ora.partenza, fill = DATA_status)) +
    geom_tile(linewidth = 0.01, color = "white") +
    labs(title=paste("APC states distribution -", line_name, "line", sep=" ")) + scale_fill_manual(values = palx, 
                                                                                                   name = "Status",
                                                                                                   labels = c("Cancelled", "Missing data", "Partially cancelled", "Valid")) + 
    xlab("Date") + ylab("Departure time") + theme_bw() +
    theme(axis.title.x = element_blank(), axis.text.y = element_text(size = 3),
          axis.ticks.y = element_line(), text = element_text(size = 10))
  
  assign(paste0("p", line), p)
}
ggarrange(pR1, pR2, pR4, pR14, pRE_2, pRE_6, ncol=2, nrow=3, common.legend = TRUE, legend="bottom")

ggarrange(pR1, pR2, pR4, pR14, pRE_2, pRE_6, ncol=2, nrow=3, common.legend = TRUE, legend="bottom") |> ggexport(filename = "Plots/OD_estimation/Counters/APC_states.png", width = 2480,
                                                                                                                height = 3508,
                                                                                                               res = 300)
rm(TC, p, df_line, pR1, pR14, pR2, pR4, pRE_2, pRE_6, train_count, line, line_name, lines, lines_names)

#### Coverage graph ----
# Load Coverage dataset
Coverage <- read.csv("Data/Trenord/Processed/Marginals/coverage.csv")

# Categorization
breakpoints <- seq(0, 1, 0.1)

# Create a new column "Category" in the "Coverage" dataframe
Coverage <- Coverage |> mutate(Perc_cat = cut(Coverage$Cov_perc, breakpoints, include.lowest = TRUE, labels = FALSE))

# Convert the "Category" column to factor and add appropriate labels
Coverage$Perc_cat <- as.factor(Coverage$Perc_cat)
levels(Coverage$Perc_cat) <- paste((breakpoints[-length(breakpoints)])*100, "-", (breakpoints[-1])*100, "%")[7:10]

# pal
# palx <- c("#FF0000", "#FF0000", "#FFAA00", "#FFAA00", "#FFFF00", "#8AFF00", "#2FFF00", "#00FF7F", "#00FFD4", "#3700A3")
palx <- c("#2FFF00", "#00FF7F", "#00FFD4", "#3700A3")

# Fixing date format
Coverage$Week <- as.Date(unlist(lapply(Coverage$Week, function(x) {
  if(str_sub(x,-2) == "00")
    return(as.character(as.Date(paste(substr(x,1,4),1, 1, sep="-"))))
  return(as.character(as.Date(paste(x, 1, sep="_"), "%Y_%U_%u")))})))

# Fixing stations to show name
Coverage$Station <- with(stations, Nome[match(Coverage$Station, Codice)])

p <- ggplot(Coverage, aes(x = Week, y = Station, fill = Perc_cat)) +
  geom_tile(linewidth = 0.01, color = "white") +
  labs(title=paste("Coverage")) + scale_fill_manual(values = palx) + 
  xlab("Date") + ylab("Station") + theme_bw() +
  theme(axis.title.x = element_blank(),
        axis.ticks.y = element_line(), text = element_text(size = 10)) +
  labs(fill = "Percentual coverage")
p

ggsave("Plots/OD_estimation/Counters/Coverage.png", p, width = 20, height = 12, units = "cm", dpi = 320)
rm(p, Coverage, breakpoints)



#### Marginals model filled vs partial true by station ----
# Load partial marginal dataset
Marg_partial <- read.csv("Data/Trenord/Processed/Marginals/partial_marginals.csv")
Marg_partial <- Marg_partial |> rename(Boarded_partial = Passengers_boarded, 
                                       Dropped_partial = Passengers_dropped)

# Load the model-filled dataset
Marg_filled <- read.csv("Data/Trenord/Processed/Marginals/marginals_filled.csv")
Marg_filled <- Marg_filled |> rename(Boarded_model = Passengers_boarded, 
                                     Dropped_model = Passengers_dropped)

# Load Coverage
Cov <- read.csv("Data/Trenord/Processed/Marginals/coverage.csv")

# Get the needed dataset
df <- Marg_partial |> left_join(Marg_filled, by = c("Station", "Week")) |> left_join(Cov, by = c("Station", "Week"))
  
df$Week <- as.Date(unlist(lapply(df$Week, function(x) {
  if(str_sub(x,-2) == "00")
    return(as.character(as.Date(paste(substr(x,1,4),1, 1, sep="-"))))
  return(as.character(as.Date(paste(x, 1, sep="_"), "%Y_%U_%u")))})))

# Palette
pal <- brewer.pal(8, "Set3")
palx <- c("Boarded_partial" = pal[7], "Dropped_partial" = pal[7],
         "Boarded_model" = pal[1], "Dropped_model" = pal[1])

weeks <- unique(sort(Cov$Week))

if (!dir.exists("Plots/OD_estimation/Marginals_filled")) dir.create("Plots/OD_estimation/Marginals_filled", recursive = TRUE)

# Plot station by station
for (st in station_codes){
  df_loc <- df |> filter(Station == st)
  
  name <- toString(stations |> filter(Codice == st) |> dplyr::select(Nome))
  
  # Highlight points where my model comes into play
  v <- rep(0,length(weeks))
  v[which(df_loc$Cov_perc < 0.85)] <- 1
  inds <- diff(c(0, v))
  start <- df_loc$Week[inds == 1]
  end <- df_loc$Week[inds == -1]
  if (length(start) > length(end)) 
    end <- c(end, tail(df_loc$Week, 1))
  
  # highlight region data
  rects <- data.frame(start=start, end=end, group=seq_along(start))
  
  # The case of perfect Coverage must be handled differently
  if (min(df_loc$Cov_perc > 0.85)){
    p1 <- ggplot(df_loc, aes(Week)) +
      geom_line(aes(y = Boarded_partial, color = "Boarded_partial")) + geom_point(aes(y = Boarded_partial, color = "Boarded_partial"), linewidth = 1) +
      geom_line(aes(y = Boarded_model, color = "Boarded_model")) + geom_point(aes(y = Boarded_model, color = "Boarded_model"), linewidth = 1) +
      labs(title = paste(name, "Passengers boarded", sep = " - "))  + scale_color_manual(values = palx, name = "Type", 
                                                                                         labels = c("Rescaling estimates", "Partial valid")) +
      theme_bw() + labs(x ="Date", y = "Boarded passengers") + theme(text = element_text(size = 14), axis.title = element_blank()) 
    
    p2 <- ggplot(df_loc, aes(Week)) +
      geom_line(aes(y = Dropped_partial, color = "Dropped_partial")) + geom_point(aes(y = Dropped_partial, color = "Dropped_partial"), linewidth = 1) +
      geom_line(aes(y = Dropped_model, color = "Dropped_model")) + geom_point(aes(y = Dropped_model, color = "Dropped_model"), linewidth = 1) +
      labs(title = paste(name, "Passengers dropped", sep = " - "))  + scale_color_manual(values = palx, name = "Type", 
                                                                                         labels = c("Rescaling estimates", "Partial valid")) +
      labs(x ="Date", y = "Dropped passengers") + 
      theme_bw()  + theme(text = element_text(size = 14), axis.title = element_blank()) 
  } else {
    p1 <- ggplot(df_loc, aes(Week)) +
      geom_line(aes(y = Boarded_partial, color = "Boarded_partial")) + geom_point(aes(y = Boarded_partial, color = "Boarded_partial"), linewidth = 1) +
      geom_line(aes(y = Boarded_model, color = "Boarded_model")) + geom_point(aes(y = Boarded_model, color = "Boarded_model"), linewidth = 1) +
      labs(title = paste(name, "Passengers boarded", sep = " - "))  + scale_color_manual(values = palx, name = "Type", 
                                                                                         labels = c("Rescaling estimates", "Partial valid")) +
      theme_bw() + geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=min(c(df_loc$Boarded_partial, df_loc$Boarded_model)),
                                                                ymax=max(c(df_loc$Boarded_partial, df_loc$Boarded_model)), 
                                                                group=group), color="transparent", fill=pal[7], alpha=0.2) +
      labs(x ="Date", y = "Boarded passengers")  + theme(text = element_text(size = 14), axis.title = element_blank()) 
    
    p2 <-  ggplot(df_loc, aes(Week)) +
      geom_line(aes(y = Dropped_partial, color = "Dropped_partial")) + geom_point(aes(y = Dropped_partial, color = "Dropped_partial"), linewidth = 1) +
      geom_line(aes(y = Dropped_model, color = "Dropped_model")) + geom_point(aes(y = Dropped_model, color = "Dropped_model"), linewidth = 1) +
      labs(title = paste(name, "Passengers dropped", sep = " - "))  + scale_color_manual(values = palx, name = "Type", 
                                                                                         labels = c("Rescaling estimates", "Partial valid")) +
      theme_bw() + geom_rect(data=rects, inherit.aes=FALSE, aes(xmin=start, xmax=end, ymin=min(c(df_loc$Dropped_partial, df_loc$Dropped_model)),
                                                                ymax=max(c(df_loc$Dropped_true, df_loc$Dropped_model)), 
                                                                group=group), color="transparent", fill=pal[7], alpha=0.2) +
      labs(x ="Date", y = "Dropped passengers")  + theme(text = element_text(size = 14), axis.title = element_blank()) 
  }

  pgrid <- ggarrange(p1, p2, nrow = 1, ncol = 2, common.legend = TRUE, legend="bottom")
  ggsave(paste("Plots/OD_estimation/Marginals_filled/Marginals_",name,".png", sep = ""), pgrid, 
         width = 30, height = 12, units = "cm", dpi = 320)
}

rm(p1, p2, v, start, name, inds, end, rects, pgrid, Marg_partial, Marg_filled, df, df_loc, Cov)

#### 1.3. IPF MATRICES ----
if (!dir.exists("Plots/OD_estimation/IPF")) dir.create("Plots/OD_estimation/IPF", recursive = TRUE)

#### Furness matrices  ----
# Load Furness
OD_Trenord <- read.csv("Data/Trenord/Processed/IPF/OD_Trenord_IPF.csv")

load("Data/Trenord/Processed/station_codes.Rdata")
total_stat <- station_codes

total_stat = sort(total_stat)

weeks <- colnames(OD_Trenord)[3:dim(OD_Trenord)[2]]
weeks <- str_sub(weeks, 6)

stations_x <- stations |> filter(Codice %in% total_stat) 
stations_x <- stations_x[order(stations_x$Codice),]

# Producing plots of weekly OD tickets matrix
for (w in weeks){
  print(w)
  
  # Getting the week OD matrix
  OD_cur <- OD_week(OD_Trenord, w, total_stat)
  
  # Recovering stations names
  rownames(OD_cur) <- with(stations, Nome[match(rownames(OD_cur), Codice)])
  colnames(OD_cur) <- with(stations, Nome[match(colnames(OD_cur), Codice)])
  
  OD_cur_p <-  OD_cur %>%
    as.data.frame() %>%
    rownames_to_column("Station_start") %>%
    pivot_longer(-c(Station_start), names_to = "Station_end", values_to = "Counts")
  
  # To match Code order
  OD_cur_p$Station_start <- factor(OD_cur_p$Station_start, levels = stations_x$Nome)
  OD_cur_p$Station_end <- factor(OD_cur_p$Station_end, levels = stations_x$Nome)
  
  if(str_sub(w, start= -2) == "00"){
    # To move title to the left, increase theme -> plot.title -> elemnt_text -> hjust
    p1 <- OD_cur_p %>%
      ggplot(aes(x=Station_end, y=Station_start, fill=Counts)) +
      geom_raster() +
      scale_fill_gradient(low='white', high='dodgerblue4', limits = c(0,2000), na.value = 'dodgerblue4') +
      ggtitle(paste("IPF estimated OD matrix - week from", as.Date(paste(substr(w,1,4), 01, 01, sep="_"), "%Y_%m_%d"), "to", as.Date(paste(w, 1, sep="_"), "%Y_%U_%u")-1)) +
      labs(x ="End station", y = "Start station") +
      theme(text = element_text(size = 13), axis.text.x = element_text(angle = 50, hjust=0.95, size = 11),
            axis.text.y = element_text(size = 11)) +
      theme(plot.title = element_text(hjust = + 0.5, vjust=2.12))
    
    p1
  } else {
    # To move title to the left, increase theme -> plot.title -> elemnt_text -> hjust
    p1 <- OD_cur_p %>%
      ggplot(aes(x=Station_end, y=Station_start, fill=Counts)) +
      geom_raster() +
      scale_fill_gradient(low='white', high='dodgerblue4', limits = c(0,2000), na.value = 'dodgerblue4') +
      ggtitle(paste("IPF estimated OD matrix - week from", as.Date(paste(w, 1, sep="_"), "%Y_%U_%u"), "to", as.Date(paste(w, 1, sep="_"), "%Y_%U_%u")+6)) +
      labs(x ="End station", y = "Start station") +
      theme(text = element_text(size = 13), axis.text.x = element_text(angle = 50, hjust=0.95, size = 11),
            axis.text.y = element_text(size = 11)) +
      theme(plot.title = element_text(hjust = + 0.5, vjust=2.12))
    
    p1
  }
  
  # MODIFY PATH FOR THESIS
  ggsave(paste("Plots/OD_estimation/IPF/IPF_",w,".png", sep = ""), p1,
         width = 30, height = 25, units = "cm", dpi = 320)
}

rm(OD_cur, OD_cur_p, OD_Trenord, p1, stations_x, total_stat)

#### Margins error ----
# Load errors
Errors <- read.csv("Data/Trenord/Processed/IPF/IPF_errors.csv")

pal <- brewer.pal(12, "Set3")
p1 <- ggplot(Errors, aes(x=Row_error)) + geom_histogram(binwidth=1, color = pal[5], fill = pal[5]) + theme_bw() +
  xlab("Row margin error") + ylab("Count") + ggtitle("Histogram of row margin errors")
p2 <- ggplot(Errors, aes(x=Col_error)) + geom_histogram(color = pal[5], fill = pal[5]) + theme_bw() +
  xlab("Col margin error") + ylab("Count") + ggtitle("Histogram of column margin errors")

pgrid <- grid.arrange(p1, p2, nrow = 1, ncol = 2, widths = c(500,500), heights = 500)
ggsave(paste("Plots/OD_estimation/IPF/Margins_errors.png", sep = ""), pgrid, 
       width = 25, height = 10, units = "cm", dpi = 320)
rm(Errors, p1, p2, pgrid)

#### 2. COMPARISON OD TRENORD RL ----
if (!dir.exists("Plots/OD_comparison")) dir.create("Plots/OD_comparison", recursive = TRUE)

# Perform correlation analysis and save result
df <- read.csv("Data/Trenord/Processed/OD_comparison/comparison_Trenord_RL_2020.csv")
df$Week <- as.Date(df$Week)

#### Rsquared ----
col <-hue_pal()(3)[2]
p4 <- ggplot(NULL) + 
  geom_point(data=df, aes(Week, Rsquared), col = col) + 
  geom_line(data=df, aes(Week, Rsquared), col = col) +
  theme_bw() + #labs(title = expression(paste(R^{2},' coefficient between dynamic Trenord data and Regione Lombardia data through 2020'))) +
  ylab(expression(paste(R^{2}))) + xlab("2022") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B") + theme(axis.text.x = element_text(angle = 50, hjust=0.95))
p4
ggsave("Plots/OD_comparison/Rsquared.png", p4, width = 20 , height = 12 , units = "cm")
rm(p4, col)

#### Regression coefficient ----
p3 <- ggplot(df, aes(Week, Coefficient)) + 
  geom_point(color = "dodgerblue3") + 
  geom_line(color = "dodgerblue3") +
  theme_bw() + labs(title = "Coefficient between dynamic Trenord data and Regione Lombardia data") +
  scale_x_date(date_breaks = "1 month",
               date_labels = "%B") + theme(axis.text.x = element_text(angle = 50, hjust=0.95), axis.title.x = element_blank())

ggsave("Plots/OD_comparison/Linear_regression_coefficient.png", p3, width = 20 , height = 12 , units = "cm")
rm(p3, df)

#### 3. DYNAMIC NETWORK ANALYSIS ----
## TODO: Testare da qui
if (!dir.exists("Plots/Dynamic_network_analysis")) dir.create("Plots/Dynamic_network_analysis", recursive = TRUE)

#### MSE ----
Diff_norm <- read.csv("Data/Trenord/Processed/Dynamic_network_analysis/global_MSE.csv")
Diff_norm$Week <- as.Date(Diff_norm$Week)

# Adding relevant events in the network
strikes <- data.frame(Week = as.Date(c("2022-07-04", "2022-09-05", "2022-10-03", "2022-11-28"))) |> 
  left_join(Diff_norm, by = 'Week')
network_interventions <- data.frame(Week = as.Date(c("2022-11-07", "2022-11-21"))) |> 
  left_join(Diff_norm, by = 'Week')
holidays <- data.frame(Week = as.Date(c("2022-08-15", "2022-10-31", "2022-12-05", "2022-12-19"))) |> 
  left_join(Diff_norm, by = 'Week')

g <- ggplot(Diff_norm, aes(Week, RMSE, group = 1)) +
  geom_line() + geom_point() + theme_bw() + theme(axis.text.x = element_text(angle = 50, hjust=0.95)) +
  ylab("MSE") +
  labs(title = "MSE for the differences between weekly dynamic Trenord OD matrices considering the previous week") +
  scale_x_date(date_breaks = "1 week") + 
  geom_point(data = strikes, aes(x = Week, y = RMSE, color = "Strikes")) +
  geom_point(data = network_interventions, aes(x = Week, y = RMSE, color = "Network interventions")) +
  geom_point(data = holidays, aes(x = Week, y = RMSE, color = "Holidays")) +
  scale_color_manual(values = c("Strikes" = "red", "Network interventions" = "blue", "Holidays" = "green")) +
  labs(color = "Events") 
ggsave("Plots/Dynamic_network_analysis/MSE.png", g, width = 25, height = 8, 
       units = 'cm')
rm(g, Diff_norm, strikes, network_interventions, holidays)

#### Mean strength ----
Strengths_mean_net <- read.csv("Data/Trenord/Processed/Dynamic_network_analysis/mean_strength.csv")
Strengths_mean_net$Week <- as.Date(Strengths_mean_net$Week)

# Adding relevant events in the network
strikes <- data.frame(Week = as.Date(c("2022-07-04", "2022-09-05", "2022-10-03", "2022-11-28"))) |> 
  left_join(Strengths_mean_net, by = 'Week')
network_interventions <- data.frame(Week = as.Date(c("2022-11-07", "2022-11-21"))) |> 
  left_join(Strengths_mean_net, by = 'Week')
holidays <- data.frame(Week = as.Date(c("2022-08-15", "2022-10-31", "2022-12-05", "2022-12-19"))) |> 
  left_join(Strengths_mean_net, by = 'Week')

g <- ggplot(Strengths_mean_net, aes(Week, Strength_all)) +
  geom_line() + geom_point() + theme_bw() + theme(axis.text.x = element_text(angle = 50, hjust=0.95)) +
  labs(title = "Mean strength in the network") +
  scale_x_date(date_breaks = "1 week") + 
  geom_point(data = strikes, aes(x = Week, y = Strength_all, color = "Strikes")) +
  geom_point(data = network_interventions, aes(x = Week, y = Strength_all, color = "Network interventions")) +
  geom_point(data = holidays, aes(x = Week, y = Strength_all, color = "Holidays")) +
  scale_color_manual(values = c("Strikes" = "red", "Network interventions" = "blue", "Holidays" = "green")) +
  labs(color = "Events") 
ggsave("Plots/Dynamic_network_analysis/Mean_strength_global.png", g, width = 20, height = 12, 
       units = 'cm')
rm(g, Strengths_mean_net, strikes, network_interventions, holidays)

##### Local strength ----
strength <- read.csv("Data/Trenord/Processed/Dynamic_network_analysis/strength.csv")
strength$Week <- as.Date(strength$Week)

g <- ggplot(strength, aes(Week, Strength_all_norm, color = Station)) +
  geom_line() + theme_bw() + theme(axis.text.x = element_text(angle = 50, hjust=0.95),
                                                  legend.position = "none") +
  labs(title = "Strengths for all stations nodes") +
  scale_x_date(date_breaks = "1 week")
g
ggsave("Plots/Dynamic_network_analysis/Local_strength.png", g, width = 20, height = 12, 
       units = 'cm')
rm(g, strength)

#### Smoothed local strength ----
df <- read.csv("Data/Trenord/Processed/Dynamic_network_analysis/local_strength_function.csv")
df$Week <- as.Date(df$Week)

# Plotting
g <- ggplot(df, aes(x = Week, y = y, color = Station)) + 
  geom_line() +
  labs(title = "Smoothed normalized strengths by station") +
  ylab("Normalized strength") +
  ylim(0,0.1) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, hjust=0.95),
        legend.position = "none") +
  scale_x_date(date_breaks = "2 week")
g
ggsave("Plots/Dynamic_network_analysis/Strength_norm_smoothed_stations.png", g, width = 20, height = 12, 
       units = 'cm')
rm(g, df)

#### Functional outliers ----
df <- read.csv("Data/Trenord/Processed/Dynamic_network_analysis/functional_outliers.csv")
df$Week <- as.Date(df$Week)

g <- ggplot(df, aes(x = Week, y = y, group = Station, color = as.factor(out_group))) + 
  geom_line() +
  labs(title = "Functional outliers", color = "Type") +
  ylab("Normalized strength") +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 50, hjust=0.95)) +
  scale_x_date(date_breaks = "2 week") +
  scale_color_manual(values = c('Normal' = "black", 'R4' = "blue", 'R14' = "green", 'Milano PG' = 'red'))
g
ggsave("Plots/Dynamic_network_analysis/Strength_functional_outliers.png", g, width = 20, height = 15, 
       units = 'cm')
rm(g, df)
