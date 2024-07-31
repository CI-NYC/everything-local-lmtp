# -------------------------------------
# Script: helpers.R
# Author: Nick Williams
# Updated:
# Purpose: Functions for repeat tasks
# Notes:
# -------------------------------------

library(arrow)

write_data <- function(data, file, dir = "/mnt/general-data/disability/everything-local-lmtp") {
  write_fst(data, file.path(dir, file))
}

load_data <- function(file) {
  dir <- "/mnt/general-data/disability/everything-local-lmtp"
  read_fst(file.path(dir, file))
}

#' Open an arrow dataset for the dates files
open_dedts <- function(path = "/mnt/processed-data/disability") {
  list.files(path, 
             pattern = "TAFDEDTS_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}

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

open_rxh <- function(path = "/mnt/processed-data/disability/") {
  list.files(path, 
             pattern = "TAFRXH_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}

open_demo <- function(path = "/mnt/processed-data/disability/") {
  list.files(path, 
             pattern = "TAFDEBSE_\\d+\\.parquet", 
             recursive = TRUE) |> 
    (\(files) file.path(path, files))() |> 
    open_dataset()
}
