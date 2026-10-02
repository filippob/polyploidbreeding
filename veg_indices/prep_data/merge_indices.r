
## script to merge together all index files:
## i) RGB
## ii) multispectral
## iii) thermal
## iv) dem

library("purrr")
library("dplyr")
library("tidyverse")
library("data.table")

# INPUT CONFIGURATION MANAGEMENT ------------------------------------------
args = commandArgs(trailingOnly=TRUE)
if (length(args) == 1){
  #loading the parameters
  source(args[1])
} else {
  #this is the default configuration, used for development and debug
  writeLines('Using default config')
  
  #this dataframe should be always present in config files, and declared
  #as follows
  config = NULL
  config = rbind(config, data.frame(
    base_folder = "~/Documents/polyploid_breeding/",
    input_folder = "drone_phenotyping/vegetation_indices/barley/indices/",
    outdir = 'drone_phenotyping/Analysis/merged_data',
    species = "barley",
    force_overwrite = FALSE
  ))
  
}
## -------------------------------------- ##

path_to_folder = file.path(config$base_folder, config$input_folder)

list_of_files <- list.files(path = path_to_folder,
           recursive = TRUE,
           pattern = "*.csv",
           full.names = TRUE)

print(paste("CROP SPECIES:", config$species))
print("Files from the input folder")
print(paste(list_of_files, collapse = ","))

## RGB
writeLines(" - reading RGB indices")
fname = list_of_files[grepl("RGB", x = list_of_files)]
rgb = fread(fname)
rgb <- rgb |> select(dataset, gid, VARIrgb_mean, GLI_mean, BGI_mean, HUE_mean)
rgb$dataset = gsub("_.*$","",rgb$dataset)

## MULTISPECTRAL
writeLines(" - reading Multispectral indices")
fname = list_of_files[grepl("multi", x = list_of_files)]
multi = fread(fname)
multi <- multi |> select(dataset, gid, NDVI_mean, GNDVI_mean, NDRE_mean, CVI_mean, 
                         EVI_mean, CIG_mean, PSRI_mean, RVI_mean, TVI_triangular_mean)
multi$dataset = gsub("_.*$","",multi$dataset)

## thermal
writeLines(" - reading Thermal indices")
fname = list_of_files[grepl("term", x = list_of_files)]
therm = fread(fname)
therm <- therm |> select(dataset, gid, temperature_mean)
therm$dataset = gsub("_.*$","",therm$dataset)

## dem
writeLines(" - reading DEM indices")
fname = list_of_files[grepl("Dem", x = list_of_files)]
dem = fread(fname)
dem <- dem |> select(dataset, gid, altezza_mean, summation)
dem$dataset = gsub("_.*$","",dem$dataset)

writeLines(" - merging datasets ... ")
temp <- purrr::reduce(list(rgb, multi, therm, dem), dplyr::left_join, by = c("gid","dataset"))

nrec = nrow(temp)

## removing columns (indices) with more than 50% missing data
vec = (100*(colSums(is.na(temp))/nrec) < 50)
temp <- temp[, vec, with=FALSE]

writeLines(" - writing out merged file")
fname = paste("merged_indices_", config$species, ".csv", sep="")
fwrite(x = temp, 
       file = file.path(config$base_folder, config$outdir, fname),
       sep = ","
       )

print("DONE!")
