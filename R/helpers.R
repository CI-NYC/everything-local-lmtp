# -------------------------------------
# Script: helpers.R
# Author: Nick Williams
# Updated:
# Purpose: Functions for repeat tasks
# Notes:
# -------------------------------------

#' Open an arrow dataset for the inpatient hospital files
open_iph <- function(path = "/mnt/processed-data/disability") {
  list.files(path, 
             pattern = "TAFIPH_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}

#' Open an arrow dataset for the other services files
open_oth <- function(path = "/mnt/processed-data/disability") {
  list.files(path, 
             pattern = "TAFOTH\\d+_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}

open_otl <- function(path = "/mnt/processed-data/disability/") {
  list.files(path, 
             pattern = "TAFOTL\\d+_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}

open_rxl <- function(path = "/mnt/processed-data/disability/") {
  list.files(path, 
             pattern = "TAFRXL_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}
