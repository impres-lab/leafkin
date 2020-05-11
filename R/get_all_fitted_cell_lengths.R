#' Get all fitted cell lengths: Calculates for each plant all cell lengths
#'
#' Produces, for each plant, all cell fitted lenghts over for a certain interval.
#'
#' This function takes in cell length data. The provided cell length data must
#' be in a data.frame or tbl / tbl_df (tibble).
#' The first column are the plant IDs and are used in the LER analysis (header =
#' plant_id) .
#' The second column contains the positions of the cell length measurements in
#' the leaf's growth zone (in cm) (header = position).
#' The third column contains the cell length measurements (header = cell_length).
#'
#' Using the interval_in_cm variable, you indicate how often (i.e. at which
#' interval) a cell length should be calculated over the entire growth zone. By
#' default, it is set to 0.1 cm.
#'
#' The bw_multiplier can be used to multiply the bandwidth. Usually, the
#' calculated bandwidth is OK to use. Multiplying it by a number between 0 and 1
#' will follow the profile more strict. Multiplying the bandwidth with a number
#' larger than 1 will make the fit more loose. Making the multiplier to small
#' could result in an error.
#'
#' An alternative bandwidth can be specified if, for some plants, no bandwidth
#' could be estimated. Our advise is to use the mean bandwidth of bandwidths
#' which were calculated. You can use the
#' \link{get_pdf_with_cell_length_fit_plots} function to extract all these
#' bandwidths and use them to calculate the mean bandwidth. When not specified,
#' the alternative bandwidth is set to 0.5 Normally, the cell lengths are
#' returned in a tidy format. When tidy_cell_lengths is set to \code{FALSE}, a
#' human readable wide table is returned.
#'
#' @param cell_length_data data.frame or tbl / tbl_df (tibble) containing the cell length data.
#' @param interval_in_cm specifies the inteval at which cell lengths should be calculated.
#' @param bw_multiplier multiplies the calculated bandwidth with the provided number.
#' @param alternative_bw sets an alternative bandwidth if no band width could be calculated for the plant.
#' @param tidy_cell_lengths \code{TRUE} by default. When \code{FALSE}, the function will return cell lengths as a human readable table.
#'
#' @author Jonas Bertels
#'
#' @import tidyr
#' @import tibble
#' @import dplyr
#' @import tools
#' @import scales
#' @import KernSmooth
#'
#' @return A tidy tibble, containing all fitted cell lengths for each plant.
#' @export
#'



get_all_fitted_cell_lengths <- function(cell_length_data,
                                        interval_in_cm = 0.1,
                                        bw_multiplier = 1,
                                        alternative_bw = 0.5,
                                        tidy_cell_lengths = TRUE) {

  # check whether the user has provided a data.frame or tbl (tibble)
  if(sum(class(cell_length_data) %in% c("data.frame", "tbl", "tbl_df")) == 0) { # If file does not exist
    stop("\n ##### \n Data is not provided as data.frame or tbl (tibble).\n #####")
  }

  # Check column headers
  if(names(cell_length_data)[1] != "plant_id") { # Check whether the first column is named plant_id
    stop(paste("First column should be named 'plant_id'.", sep = ""))
  }

  if(names(cell_length_data)[2] != "position") { # Check whether the second column is named position
    stop(paste("Second column should be named 'position'.", sep = ""))
  }

  if(names(cell_length_data)[3] != "cell_length") { # Check whether the thrird column is named cell_length
    stop(paste("Third column should be named 'cell_length'.", sep = ""))
  }

  # Check whether the positions are numeric
  if(!is.numeric(cell_length_data$position)) {
    stop(paste("Positions are not all numeric.", sep = ""))
  }

  # Check whether the cell_lengths are numeric
  if(!is.numeric(cell_length_data$cell_length)) {
    stop(paste("Cell lengths are not all numeric.", sep = ""))
  }

  # Create a data frame to collect all cell lengths
  position <- vector(mode = "numeric")
  cell_length <- vector(mode = "numeric")
  plant_id <- vector(mode = "character")
  df_of_fit <- as.data.frame(cbind(position, cell_length, plant_id))

  # Create new dpill function to catch errors
  # Coded with the help of: https://stackoverflow.com/questions/12193779/how-to-write-trycatch-in-r
  new_dpill <- function(positions, cell_lengths) {
    out <- tryCatch(
      {
        KernSmooth::dpill(positions, cell_lengths)
      },
      error = function(cond) {
        # message(paste("The following plant generated an error:", plant_ID))
        # message("Here's the original error message:")
        # message(cond)
        return(NA)
      },
      warning = function(cond) {
        # message(paste("The following plant generated a warning:", plant_ID))
        # message("Here's the original warning message:")
        # message(cond)
        return(NA)
      }
    )
    return(out)
  }

  for (selected_plant_id in unique(cell_length_data$plant_id)) {
    # dpill is sometimes unable to calculate bandwidth.
    # Set calculated_bandwidth to NA each time.
    # A check is perfomed whether bandwidth is then calculated or not.
    calculated_bandwidth <- NA

    #1. FILTER THE DATA
    profile <- dplyr::filter(cell_length_data, plant_id == selected_plant_id)

    # Store position and cell_length
    x <- profile$position
    y <- profile$cell_length

    #2. CALCULATION OF OPTIMIZED BANDWIDTH FOR FITTING
    calculated_bandwidth <- new_dpill(x, y) #this is the calculation of the optimized bandwidth for the kernel as in KernSmooth package

    #3. DETERMINE THE ENDPOINT
    endpoint_in_cm <- ( (max(profile$position) %/% interval_in_cm) * interval_in_cm ) + interval_in_cm

    #3. CALCULATON OF THE GRIDSIZE FOR WHICH CELL LENGTHS ARE ESTIMATED
    datagrid <- (endpoint_in_cm / interval_in_cm) + 1

    # If the bandwidth can be calculated, make plot the polynomials. If not calculated, skip plotting.
    if (!is.na(calculated_bandwidth)) {
      fit <- locpoly(x, y,
                     degree = 2,
                     bandwidth = bw_multiplier*calculated_bandwidth,
                     gridsize = datagrid,
                     range.x = c(0, endpoint_in_cm))
    } else if (alternative_bw == 0.5) {
      message("Unable to calculate bandwidth. Alternative bandwidth used. Unless specified, alternative_bw is set to 0.5 by default.")
      message("Advised to extract the bandwidths, using get_pdf_with_different_bw_fits(), with output_bw_tibble = TRUE.")
      message("The extracted bandwidth can be use to calculate the average bandwidth.")
      fit <- locpoly(x,y,
                     degree=2,
                     bandwidth=bw_multiplier*alternative_bw,
                     gridsize=datagrid,
                     range.x=c(0, endpoint_in_cm))
    } else {
      message(paste("Using the set alternative_bw of ", alternative_bw, " for plant ", selected_plant_id, " .", sep = ""))
      fit <- locpoly(x,y,
                     degree=2,
                     bandwidth=bw_multiplier*alternative_bw,
                     gridsize=datagrid,
                     range.x=c(0, endpoint_in_cm))
    }

    temp_df <- as.data.frame(cbind(fit[[1]], fit[[2]]))
    colnames(temp_df) <- c("position", "cell_length")
    temp_df$plant_id <- selected_plant_id

    df_of_fit <- rbind(df_of_fit, temp_df)

  }

  message("\n ##### \n Analysis finished. A tibble with cell lengths was returned by the function.\n #####")

  tibble_of_df <- as_tibble(df_of_fit)
  tibble_of_df <- tibble_of_df %>% select(plant_id, position, cell_length)

  if (tidy_cell_lengths == TRUE) {
    return(tibble_of_df)
  } else {
    return(spread(tibble_of_df, position, cell_length))
  }

}
