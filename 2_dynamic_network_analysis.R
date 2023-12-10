## GOAL: Analyze the Trenord dynamic OD matrices using network analysis and FDA techniques
rm(list = ls())
source("utils.R")
set.seed(24091998)

library(igraph)
library(fda)
library(fields)
library(roahd)
library(fdacluster)

# Load network
OD <- read.csv("Data/Processed/IPF/OD_Trenord_IPF.csv")

# Load stations codes
load("Data/Processed/station_codes.Rdata")

if (!dir.exists("Data/Processed/Dynamic_network_analysis")) dir.create("Data/Processed/Dynamic_network_analysis", recursive = TRUE)

####  1. GLOBAL METRICS ----
#### 1.1. Global comparison between matrices through RMSE-----
weeks <- colnames(OD)[-c(1,2)]
weeks <- substring(weeks,6)

# Preparing dataset
Diff_norm <- data.frame("Week" = weeks, 
                        "RMSE" = rep(NA, length(weeks)))

for (i in 2:length(weeks)){
  # Select needed weeks for the comparison
  w_prev <- weeks[i-1]
  w_cur <- weeks[i]
  
  # selecting the OD matrices for the two needed weeks in the whole dataset
  OD_prev <- OD_week(OD, w_prev, station_codes)
  OD_cur <- OD_week(OD, w_cur, station_codes)
  
  # Computing the RMSE
  Diff_norm[Diff_norm$Week == w_cur, "RMSE"] <- sqrt(sum((OD_cur - OD_prev)^2) / length(OD_cur))
}

Diff_norm$Week <- as.Date(unlist(lapply(Diff_norm$Week, function(x) {
  if(str_sub(x,-2) == "00")
    return(as.character(as.Date(paste(substr(x,1,4),1, 1, sep="-"))))
  return(as.character(as.Date(paste(x, 1, sep="_"), "%Y_%W_%w")))})))
  
# Saving the result
write.csv(Diff_norm, "Data/Processed/Dynamic_network_analysis/global_MSE.csv", row.names = FALSE)

#### 1.2. Mean Strength -----
strength <- expand.grid(Week = weeks, Station = station_codes) |> 
  add_column(Strength_in = NA, Strength_out = NA, Strength_all = NA)

for (w in weeks){
  # Get the OD matrix 
  OD_cur <- OD_week(OD, w, station_codes)
  
  # Convert to a weighted directed network object
  net <- graph_from_adjacency_matrix(as.matrix(OD_cur), mode = "directed", weighted = TRUE)
  
  # Get local strength in/out/all
  strength[strength$Week == w, "Strength_all"] <- strength(net, vids = station_codes, mode = "all")
  strength[strength$Week == w, "Strength_in"] <- strength(net, vids = station_codes, mode = "in")
  strength[strength$Week == w, "Strength_out"] <- strength(net, vids = station_codes, mode = "out")
  
}

# Fixing date format
strength$Week <- as.Date(unlist(lapply(strength$Week, function(x) {
  if(str_sub(x,-2) == "00")
    return(as.character(as.Date(paste(substr(x,1,4),1, 1, sep="-"))))
  return(as.character(as.Date(paste(x, 1, sep="_"), "%Y_%W_%w")))})))

strength <- strength |>
  group_by(Station) |>
  mutate(Total_all = sum(Strength_all),
         Total_in = sum(Strength_in),
         Total_out = sum(Strength_out)) |>
  ungroup() |>
  # Divide each Streghts value by the corresponding Total
  mutate(Strength_all_norm = Strength_all / Total_all,
         Strength_in_norm = Strength_in / Total_in,
         Strength_out_norm = Strength_out / Total_out) |>
  dplyr::select(-c(Total_all, Total_in, Total_out))
write.csv(strength, "Data/Processed/Dynamic_network_analysis/strength.csv", row.names = FALSE)

# I compute the mean streght in the network
mean_strength <- strength |> dplyr::select(-c(Strength_all_norm,Strength_in_norm,Strength_out_norm))|> group_by(Week) |> 
  summarise(across(where(is.numeric), ~ sum(.x, na.rm = TRUE)))

# Saving the result
write.csv(mean_strength, "Data/Processed/Dynamic_network_analysis/mean_strength.csv", row.names = FALSE)


#### 2. LOCAL METRICS  ----
# Strength can also be viewed as a local metric

#### 2.1. FDA ON STRENGTH VALUES ----
## GOAL: Analysis on total strength using FDA techniques
# I consider strength, normalized dividing by the total strength for each station through the period considered

#### 2.1.1. FDA - SMOOTHING ----
# Transform dataset in the format needed to apply FDA 
st_df <- strength |> 
  dplyr::select(Week, Station, Strength_all_norm) |>
  pivot_wider(names_from = Week, values_from = Strength_all_norm)
colnames(st_df)[2:30] <- paste0("Week_", colnames(st_df)[2:30])

# SMOOTHING - Cubic splines
row.names(st_df) <- st_df$Station
x=t(st_df[,2:30])
t=rep(1:29) 
degree <- 3  #spline degree
m=degree+1  #spline order

# Select nbasis -> compute gcv
nb = as.data.frame(st_df[,1])
for (i in 1:46){
  nb[i,2]=0
}

for(j in 1:46){
  x1=x[,j]
  nbasis <- 4:20  
  gcv <- numeric(length(nbasis))
  for (i in 1:length(nbasis)){
    basis <- create.bspline.basis(rangeval=c(1,29),nbasis[i],m)
    gcv[i] <- smooth.basis(t, x1, basis)$gcv
  }
  par(mfrow=c(1,1))
  plot(nbasis,gcv)
  nb[j,2]=nbasis[which.min(gcv)]
}

print(mean(nb[,2]))
# I round this quantity and select nbasis = 4
nbasis=4

# Create b-spline basis
basis <- create.bspline.basis(rangeval=c(1,29), nbasis=nbasis, norder=m)
Xsp <- smooth.basis(argvals=t, y=x, fdParobj=basis)
Xsp0bis <- eval.fd(t, Xsp$fd)

# Adjusting functional data
st_func_df <- as.data.frame(Xsp0bis)
colnames(st_func_df) <- st_df$Station
st_func_df$Week <- weeks
# Fixing date format
st_func_df$Week <- as.Date(unlist(lapply(st_func_df$Week, function(x) {
  if(str_sub(x,-2) == "00")
    return(as.character(as.Date(paste(substr(x,1,4),1, 1, sep="-"))))
  return(as.character(as.Date(paste(x, 1, sep="_"), "%Y_%W_%w")))})))
st_func_df <- st_func_df |> pivot_longer(cols = colnames(st_func_df)[1:46], names_to = 'Station', values_to = 'y')

# Saving the result
write.csv(st_func_df, "Data/Processed/Dynamic_network_analysis/local_strength_function.csv", row.names = FALSE)

# Plot the functional mean
fd_strength <- Data2fd(y = t(as.matrix(st_df[,-1])),argvals = t,basisobj = basis)
plot.fd(fd_strength, titles = st_df$Station)
lines(mean.fd(fd_strength),lwd=3)

# Plot the functional covariance 
eval <- eval.fd(t,fd_strength)
image.plot(t,t,(cov(t(eval))[1:29,])) 
# It makes sense: weeks near each other are more similar than weeks far in time

#### 2.1.2. FDA - OUTLIERS ----
# Consider data
grid=seq(1,29)
fData=fData(grid, t(eval))
plot(fData)
matplot(Xsp0bis, lty=1, type='l',main='Mobility data smoothed - Provinces',ylab='Densities of strength',xlab='Weeks')

# a. Functional boxplot
fb = fbplot(fData, xlab='Week', ylab='Mobility densities', main='Functional Boxplot')
fb
# b. Outliergram
pr=outliergram(fData)
# fbplot finds some outliers while outliergram finds none !!

# The outliers are
outliers = st_df$Station[fb$ID_outliers] 
outliers

# Outliers interpreted by station
st_func_df$out_group <- 'Normal'
st_func_df[st_func_df$Station %in% outliers, "out_group"] <- 'Outlier'

write.csv(st_func_df, "Data/Processed/Dynamic_network_analysis/functional_outliers.csv", row.names = F)
