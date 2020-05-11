#' leafkin: A package to perform the kinematic analysis of monocotyl leaves.
#'
#' This package provides functions to perform a kinematic analysis of monocotyledonous leaves
#' in R.
#'
#' To do the kinematic analysis, you will need following functions:
#' \itemize{
#' \item \link{calculate_LER}: calculates the leaf elongation rates using leaf length measurements
#' \item \link{get_pdf_with_cell_length_fit_plots}: creates a pdf with cell length fit plots. Use this pdf to check whether cell lengths are appropriately fitted.
#' \item \link{get_all_fitted_cell_lengths}: obtains all fitted cell length profiles for each plant.
#' \item \link{kinematic_analysis}: performs the kinematic analysis, using leaf elongation rates, cell length profiles and meristem measurements.
#' }
#'
#' @author Jonas Bertels and Gerrit T.S. Beemster
#' @docType package
#' @name leafkin
"_PACKAGE"
