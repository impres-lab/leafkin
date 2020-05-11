#' Calculate leaf elongation rate
#'
#' Calculates the leaf elongation rate (LER) for each plant in the provided dataset.
#'
#' This function takes in leaf growth data, where the first column are
#' plant_id's and the following columns are leaf length measurements, where the
#' column headers are in the following date-time format: yyyy/mm/dd hh:mm or
#' yyyy/mm/dd hh:mm:ss
#' The provided leaf growth data must be in a data.frame or tibble. By default,
#' it uses the two first LERs are used for the calculation of the LER mean. This
#' means that at least three leaf length measurements are needed, allowing a
#' minimum of two growth intervals to be calculated.
#'
#' @param leaf_length_data data.frame or tbl / tbl (tibble) containing the leaf length data.
#' @param n_LER_for_mean specifies the number of calculated LERs which must be used to calculate
#' the mean LER.
#' @param output specifies what the the funtion should return. By default, a tibble of the calculated
#' mean LERs is returned ("means"). When selecting "tidy_LER", a tidy tibble containing all calculated
#' LERs will be returned. "wide_LER" will return a wide, more human readable tibble of the calculated LERs.
#'
#' @author Jonas Bertels
#'
#' @import tidyr
#' @import tibble
#' @import dplyr
#' @import tools
#' @importFrom  lubridate interval
#' @importFrom  lubridate ymd_hms
#' @export
#'
#' @return A tibble containing LERs. Depending on specified output parameter, LER output varies.
#'

calculate_LER <- function(leaf_length_data,
                          n_LER_for_mean = 2,
                          output = c("means", "tidy_LER", "wide_LER")) {

  # use first option of output variable by default
  output <- match.arg(output)

  # check whether the user has provided a data.frame or tbl (tibble)
  if(sum(class(leaf_length_data) %in% c("data.frame", "tbl")) == 0) { # If file does not exist
    stop("\n ##### \n Data is not provided as data.frame or tbl (tibble).\n #####")
  }

  # Perform checks on the data_file
  # check column name of first column. Needs to be plant_id.
  if(names(leaf_length_data)[1] != "plant_id") { # Check whether the first column is named plant_id
    stop("\n ##### \n The first column should contain plant_id's and should be named exactly plant_id.\n #####")
  }
  # check whether all plant_id's are unique
  if(length(leaf_length_data$plant_id) != length(unique(leaf_length_data$plant_id))){
    stop("\n ##### \n There are duplicate plant_id's. Please use unique plant_id's for each plant.\n #####")
  }

  # Tidy data
  # Create one data_and_hour_column and one leaf_length column
  tidy_leaf_length_data <- gather(leaf_length_data,
                          "date_and_hour", "leaf_length",
                          2:ncol(leaf_length_data))

  # Check whether leaf_length is all nummeric
  if(!is.numeric(tidy_leaf_length_data$leaf_length)) {
    stop("\n ##### \n Please check the entered leaf lengths. Not all of them seem to be numeric.\n #####")
  }

  # *** convert data_and_hour strings column to date-time POSIXct format
  tidy_leaf_length_data$date_and_hour <- lubridate::ymd_hms(tidy_leaf_length_data$date_and_hour, truncated = 1)

  if(anyNA(tidy_leaf_length_data$date_and_hour)) {
    stop("\n ##### \n Please check the dates in the provided file. They should be in the format yyyy/mm/dd hh:mm. \n #####")
  }

  # *** Order based on plant and timepoint to allow me to calculate time and growth and eventually LER
  # When the data is ordered, you can loop over the data and calculate differences between consecutive rows.
  sorted_tidy_leaf_length_data <- tidy_leaf_length_data[order(tidy_leaf_length_data$plant_id, tidy_leaf_length_data$date_and_hour),]


  # *** Create DF for calculation
  # Copy sorted_tidy_leaf_length_data and make new columns for time_hours, growth_mm and
  # LER These columns will be used to perform calculations with, the results will
  # be stored in the new dataframe, using these columns. This is because each
  # plant is filtered out individually for each iteration in the for loop. In
  # each iteration, these columns of the filtered plant will be used, but not
  # stored The result of each iteration will be saved in the LER_calculations_df,
  # created below.
  sorted_tidy_leaf_length_data_calculations <- sorted_tidy_leaf_length_data
  sorted_tidy_leaf_length_data_calculations$time_hours <- as.numeric(NA)
  sorted_tidy_leaf_length_data_calculations$growth_mm <- as.numeric(NA)
  sorted_tidy_leaf_length_data_calculations$LER <- as.numeric(NA)

  # Initiate new df to collect all calculation results
  LER_calculations_df <- sorted_tidy_leaf_length_data_calculations[0,]

  # Create vectors to store mean LER's
  # Make a vector to collect all sample id's
  plant_id_vect <- levels(as.factor(sorted_tidy_leaf_length_data_calculations$plant_id))
  # Make a vector to collect all LER means
  mean_plant_LER <- vector(mode = "numeric",
                           length = length(levels(as.factor(sorted_tidy_leaf_length_data_calculations$plant_id))))
  # Make new tibble with plants and their mean LER
  tibble_LERs <- tibble(plant_id_vect, mean_plant_LER)
  tibble_LERs$mean_plant_LER <- NA
  tibble_LERs

  # Add info
  # Rename column, so both tables have same ID
  colnames(tibble_LERs)[colnames(tibble_LERs) == "plant_id_vect"] <- "plant_id"

  # For loop to calculate all LERs
  # Will generate two warnings for each plant with no data
  for(plant in levels(as.factor(sorted_tidy_leaf_length_data_calculations$plant_id))){
    # filter plant out of dataset
    filtered_plant <- sorted_tidy_leaf_length_data_calculations %>% filter(plant_id == plant)

    # take rows needed
    first_row <- min(which(!is.na(filtered_plant$leaf_length) == TRUE)) # take integer of first row where you have a measurement
    last_row <- max(which(!is.na(filtered_plant$leaf_length) == TRUE)) # take integer of last row where you have a measurement
    if ( first_row == Inf || last_row == -Inf || first_row == last_row ) {
      # If there are no measurements for a plant, do nothing. Two warning messages will be generated for each plant.
      # If there is only one measurement for a plant, do nothing.
    } else {
      filtered_plant <- filtered_plant[first_row:last_row,] # only keep the measurements

      # calculate time between dates (EXPRESSED IN NUMBER OF HOURS) and add this in the time_hours column
      for (rownumber in 2:nrow(filtered_plant)) { # omit the very first row, since there is not a previous day on row 1
        filtered_plant$time_hours[rownumber] <- as.numeric(interval(filtered_plant$date_and_hour[rownumber-1], filtered_plant$date_and_hour[rownumber]))/3600
      }

      # calculate growth between measurements
      # for LER, it is possible that on a certain day/time, a plant was not measured by accident
      for (rownumber in 2:nrow(filtered_plant)) { # omit the very first row, since you cannot calculate a difference with a previous measurement there
        if ( is.na(filtered_plant$leaf_length[rownumber]) || sum(!is.na(filtered_plant$leaf_length[1:rownumber])) < 2  ) {
          # If the current row has no measurement, then you cannot calculate growth OR if there are no previous measurements
          # Do nothing.
          # Reminder: The first row has a measurement, since this row was selected using the "first_row" variable
        } else {
          filtered_plant$growth_mm[rownumber] <- filtered_plant$leaf_length[rownumber] - filtered_plant$leaf_length[max(which(!is.na(filtered_plant$leaf_length[1:rownumber - 1]) == TRUE))]
          # the last part of the formula above selects the last measurement, skipping NA measurements
          # it first collects all the leaf length measurements, except for the current one
          # then, using !is.na(), it returns TRUE for rows with measurements
          # then, using which, it returns the rownumber for the rows which returned TRUEs
          # then, using max, it returns the highest rownumber, thus the last previous measurement
        }
      }

      # calculate LER
      for (rownumber in 1:nrow(filtered_plant)) {
        if ( is.na(filtered_plant$growth_mm[rownumber]) ) { # if there is no growth (i.e. NA), then skip. The first row will never have a growth value, thus will always be skipped here.
        } else {
          time_interval <- as.numeric(interval(filtered_plant$date_and_hour[max(which(!is.na(filtered_plant$leaf_length[1:rownumber - 1]) == TRUE))], filtered_plant$date_and_hour[rownumber]))/3600
          # calculate time interval inbetween the two length measurements, skipping days which have no measurements
          # it uses the same technique as the calculation for calculating growth inbetween measurements
          filtered_plant$LER[rownumber] <- filtered_plant$growth_mm[rownumber] / time_interval
        }
      }

      LER_calculations_df <- rbind(LER_calculations_df, filtered_plant) # add the calculations of the current plant to the dataframe "LER_calculations_df"

      all_LERs_of_plant <- na.omit(filtered_plant$LER)
      tibble_LERs$mean_plant_LER[which(tibble_LERs$plant_id == plant)] <- mean(all_LERs_of_plant[1:n_LER_for_mean], na.rm = TRUE)
    }

  }

  message("\n ##### \n Analysis finished.\n #####")
  message("\n ##### \n For every plant, not containing any growth measurements, two warnings concerning min and max value are printed below.\n If nothing is printed, all plants contained measurements. \n ##### \n")

  if (output == "means") {
    return(tibble_LERs)
  } else if (output == "tidy_LER") {
    return(LER_calculations_df)
  } else {
    return(spread(select(LER_calculations_df, plant_id, date_and_hour, LER), date_and_hour, LER))
  }
}

