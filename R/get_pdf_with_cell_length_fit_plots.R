#' Cell lengths: Make a pdf containing the cell length fits.
#'
#' Produces a pdf file in the work directory, containing all the graphs of the
#' fitted cell lengths and their derivatives.
#'
#' This function takes in cell length data. The provided cell length data must
#' be in a data.frame or tbl / tbl_df (tibble).
#' The first column are the plant IDs and are used in the LER analysis (header =
#' plant_id) .
#' The second column contains the positions of the cell length measurements in
#' the leaf's growth zone (in cm) (header = position).
#' The third column contains the cell length measurements (header = cell_length).
#'
#' Important: All cell length measurements are thus entered directly underneath
#' eachother.
#'
#' Concerning the variables:
#' Using the interval_in_cm variable, you indicate how often (i.e. at which
#' interval) a cell length should be calculated over the entire growth zone. By
#' default, it is set to 0.1 cm.
#'
#' The bw_multiplier can be used to multiply the bandwidth. Usually, the
#' calculated bandwidth is OK to use. Multiplying it by a number between 0 and 1
#' will follow the profile more strict. Multiplying the bandwidth with a number
#' larger than 1 will make the fit more loose. Making the multiplier to small or
#' to big could result in an error.
#'
#' The function will always produce a pdf file with plots in the working
#' directory, also indicating the multiplier in the file name. If you set
#' output_bw_tibble to \code{TRUE}, then the function will also return all
#' calculated bandwidths as a tibble.
#'
#' @param cell_length_data data.frame or tbl / tbl_df (tibble) containing the cell length data.
#' @param interval_in_cm specifies the inteval at which cell lengths should be calculated.
#' @param bw_multiplier multiplies the calculated bandwidth with the provided number.
#' @param output_bw_tibble \code{FALSE} by default. When \code{TRUE}, the function will return all calculated bandwidths as a tibble.
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
#' @return Normally, only a pdf is produced in the work directory. See additional output when using \code{output_bw_tibble}.
#' @export
#'


# Cell lengths: Make a pdf of the fits for the different band widths
get_pdf_with_cell_length_fit_plots <- function(cell_length_data,
                                               interval_in_cm = 0.1,
                                               bw_multiplier = 1,
                                               output_bw_tibble = FALSE) {

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

  if(names(cell_length_data)[3] != "cell_length") { # Check whether the third column is named cell_length
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

  # Tibble to collect bandwidths
  collected_bandwidths <- vector(mode = "numeric", length = length((unique(cell_length_data$plant_id))))
  collected_bandwidths <- NA
  bandwidth_tibble <- tibble(unique(cell_length_data$plant_id), collected_bandwidths)
  colnames(bandwidth_tibble) <- c("plant_id", "collected_bandwidths")

  # FIT CELL LENGTHS AND PLOT THEM IN A PDF FILE
  pdf_name <- paste("fit_plots_using_bandwidth_multiplier_", bw_multiplier, ".pdf", sep = "")
  pdf(pdf_name)

  for (selected_plant_id in unique(cell_length_data$plant_id)) {
    # dpill is sometimes unable to calculate bandwidth. Set calculated_bandwidth to NA each time. A check is perfomed whether bandwidth is then calculated or not.
    calculated_bandwidth <- NA

    #1. FILTER THE DATA
    profile <- dplyr::filter(cell_length_data, plant_id == selected_plant_id)

    # Store position and cell_length
    x <- profile$position
    y <- profile$cell_length

    #2. CALCULATION OF OPTIMIZED BANDWIDTH FOR FITTING
    calculated_bandwidth <- new_dpill(x, y) #this is the calculation of the optimized bandwidth for the kernel as in KernSmooth package
    bandwidth_tibble$collected_bandwidths[which(bandwidth_tibble$plant_id == selected_plant_id)] <- calculated_bandwidth

    #3. DETERMINE THE ENDPOINT
    endpoint_in_cm <- ( (max(profile$position) %/% interval_in_cm) * interval_in_cm ) + interval_in_cm

    #3. CALCULATON OF THE GRIDSIZE FOR WHICH CELL LENGTHS ARE ESTIMATED
    datagrid <- (endpoint_in_cm / interval_in_cm) + 1

    # If the bandwidth can be calculated, make plot the polynomials. If not calculated, skip plotting.
    if (!is.na(calculated_bandwidth)) {
      #4. FIT AND PLOT POLYNOMIALS USING BANDWIDTH (AND DESIRED MULTIPLES FOR MORE SMOOTHNESS) CALCULATED IN STEP 2
      fit <- locpoly(x, y,
                     degree = 2,
                     bandwidth = bw_multiplier*calculated_bandwidth,
                     gridsize = datagrid,
                     range.x = c(0, endpoint_in_cm))
      plot(x, y,
           xlab = "position [cm from leaf base]",
           ylab = "cell length [micrometer]",
           main = paste("Cell length fit of: ", selected_plant_id, sep = ""))
      lines(fit, col=alpha("blue", 1), type="l", lwd=3)

      #5. CALCULATION AND PLOT OF CORRESPONDING DERIVATIVES
      der <- locpoly(x, y,
                     drv = 1,
                     degree = 2,
                     bandwidth = bw_multiplier*calculated_bandwidth,
                     gridsize = datagrid,
                     range.x = c(0, endpoint_in_cm))
      plot(der,
           col = alpha("blue", 1),
           xlab = "position [cm from leaf base]",
           ylab = "dl/dx",
           type = "l", lwd=3,
           main = paste("Derivative of: ", selected_plant_id, sep = ""))
      abline(h = 0)
    } else {
      # When there is no calculated BW, do:
      plot(x, y,
           xlab = "position [cm from leaf base]",
           ylab = "cell length [micrometer]",
           main = paste("Plant: ", selected_plant_id, " did not allow for bandwidth calculation."))
    }
  }
  # The function to catch errors, puts only an NA for the first error.
  # For the remaining errors, it puts NAN. Therefore, I manually replace NANs
  # with NA in the following line:
  bandwidth_tibble$collected_bandwidths[is.nan(bandwidth_tibble$collected_bandwidths)] <- NA

  # Create barplot with all bandwidths:
  barplot(bandwidth_tibble$collected_bandwidths,
          ylab = "bandwidth",
          xlab = "all plants (labels not printed)",
          main = paste("Mean bandwidth is: ",
                       mean(bandwidth_tibble$collected_bandwidths,
                            na.rm = TRUE),
                       sep = ""))

  # stop pdf
  dev.off()


  # Test whether there are NAs. If so, print the message below.
  if(anyNA(bandwidth_tibble$collected_bandwidths)) {
    message(paste("There was/were",
                  sum(is.na(bandwidth_tibble$collected_bandwidths)),
                  "plant(s) for which no bandwidth could be calculated (error or warning was generated).",
                  "You could check the data. Perhaps there are not enough measurements.",
                  "We advice to use the mean of the succesfully calculated bandwidths as the alternative bandwidth.",
                  "You can extract the bandwidths using this function again while setting its output_bw_tibble paramater to TRUE."
                  ))
  }

  # Print analysis finished message.
  if (output_bw_tibble == TRUE) {
    message("\n ##### \n Analysis finished. PDF created in work directory and a tibble with the bandwidths was returned by the function.\n #####")
    return(bandwidth_tibble)
  } else {
    message("\n ##### \n Analysis finished. PDF created in work directory.\n #####")
  }

}
