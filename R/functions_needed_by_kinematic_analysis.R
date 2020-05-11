#' Functions, not needed by the user, but needed by the script
#'
#' Contains functions which are used by the \link{kinematic_analysis} function.
#'
#' @param size_vect is a vector containing all the cell sizes.
#' @param pos_vect is a vector containing all the positions at which the cell sizes were taken.
#' @param cumm_cell_no is a vector containing all cummulative cell numbers.
#'
#' @author Jonas Bertels
#'
#' @import tidyr
#' @import tibble
#' @import dplyr
#' @import tools
#'

# Calculate cummulative cell number and return it as a vector
cumm_cell_no <- function(size_vect, pos_vect){
  resultsvector <- vector(mode = "numeric", length = length(size_vect))
  set_interval <- pos_vect[2] - pos_vect[1] #by subtracting the second position from the first position, we obtain the interval size
  for (x in 2:length(size_vect)) {
    resultsvector[x] <- resultsvector[x-1] + ((set_interval*10000)/mean(c(size_vect[x-1], size_vect[x]))) #position is in cm, thus also interval. Since cell size in µmm the interval is set to µm to calculate how many cells fit in there. For this, average cell size is taken of the current cell size and previous cell size.
  }
  return(resultsvector)
}

# Calculate 95 perc value
perc_95 <- function(size_vect){
  resultsvector <- vector(mode = "numeric", length = length(size_vect))
  for (x in 1:length(size_vect)) {
    resultsvector[x] <-  0.95 * mean(size_vect[x:length(size_vect)])
  }
  return(resultsvector)
}

# Find growth zone size
growth_zone <- function(pos_vect, size_vect, perc95_vect){
  resultsvector <- size_vect > perc95_vect
  location <- min(which(resultsvector == TRUE))
  return(pos_vect[location] * 10)
}

# Mature cell length
mat_cell_length <- function(growth_zone_size, pos_vect, cell_lengths){
  start_of_mature_zone <- which(as.character(pos_vect) == as.character(growth_zone_size/10)) + 1 # plus one, because the protocol of Sprangers et al. 2016 (JOVE) did also start from this value.
  last_measurement <- length(cell_lengths)
  return( mean( cell_lengths[ start_of_mature_zone : last_measurement  ] ) )
}

# Cell production rate
cell_prod_rate <- function(LER, mat_cell_length){
  return(LER/(mat_cell_length/1000))
}

# Function to obtain a value at the end of the meristem.
# Can be used for number of cells in the meristem and length of the cells leaving the meristem.
value_at_end_of_mer <- function(mer_size, value, pos_vect){
  set_interval <- pos_vect[2] - pos_vect[1] # by subtracting the second position from the first position, we obtain the interval size in cm
  if ((mer_size %% (set_interval*10000)) == 0) {  # check whether meristem size fits perfectly with the choosen interval size.
    location1 <- which(as.character(pos_vect) == as.character((mer_size/10000)))
    return(value[location1])
  } else {  # if meristem size is between two positions, meaning that the value needs to be calculated between two positions
    location1 <- which(as.character(pos_vect) == as.character(((mer_size %/% 1000)/10)))
    location2 <- location1 + 1
    return(value[location1] + ((value[location2] - value[location1]) * (mer_size %% 1000)/1000))
  }
}

# number of cells in the growth zone
growth_zone_cell_no <- function(pos_vect, cum_cell_no, growth_zone_size){
  location <- which(as.character(pos_vect) == as.character(growth_zone_size/10))
  return(cum_cell_no[location])
}

# number of cells in the elongation zone
elong_zone_cell_no <- function(growth_zone_cell_no, mer_cell_no){
  return(growth_zone_cell_no - mer_cell_no)
}

# Average cell division rate
avg_cell_div_rate <- function(cell_prod_rate, mer_cell_no){
  return(cell_prod_rate/mer_cell_no)
}

# Cell cycle duration
cell_cycle_duration <- function(avg_cell_div_rate){
  return(log(2)/avg_cell_div_rate)
}

# Time in elongation zone
time_in_elo_zone <- function(elong_zone_cell_no, cell_prod_rate){
  return(elong_zone_cell_no/cell_prod_rate)
}

# Time in division zone
time_in_div_zone <- function(mer_cell_no, cell_cycle_duration){
  return(log2(mer_cell_no) * cell_cycle_duration )
}

# Average cell expansion rate
avg_cell_exp_rate <- function(mat_cell_length, mer_cell_length, time_in_elo_zone){
  return((log(mat_cell_length)-log(mer_cell_length))/time_in_elo_zone)
}
