
## script to merge together all index files:
## i) RGB
## ii) multispectral
## iii) thermal
## iv) dem

library("purrr")
library("dplyr")
library("writexl")
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
    base_folder = "~/Documents/polyploid_breeding/drone_phenotyping/vegetation_indices",
    input_folder = "wheat/data/indices",
    outdir = 'Analysis/merged_data',
    species = "wheat",
    missing_threshold = 0.75, ## a quarter of the data is missing
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
rgb <- rgb |> select(dataset, gid, VARIrgb_mean, VARIrgb_median, VARIrgb_std, VARIrgb_min, VARIrgb_max, 
                     GLI_mean, GLI_median, GLI_std, GLI_max, GLI_min, 
                     BGI_mean, BGI_median, BGI_std, BGI_max, BGI_min,
                     HUE_mean, HUE_median, HUE_std, HUE_max, HUE_min)
rgb$dataset = gsub("_.*$","",rgb$dataset)

vec = (colSums(is.na(rgb))/nrow(rgb) < config$missing_threshold)
rgb <- rgb[, vec, with = FALSE]

## MULTISPECTRAL
writeLines(" - reading Multispectral indices")
fname = list_of_files[grepl("multi", x = list_of_files)]
multi = fread(fname)
multi <- multi |> 
  select(-c(type, ortho_files, shapes_file, channels, centroid_x, centroid_y, threshold, pixels, pixels_after_threshold))
multi$dataset = gsub("_.*$","",multi$dataset)

vec = (colSums(is.na(multi))/nrow(multi) < config$missing_threshold)
multi <- multi[, vec, with = FALSE]

## thermal
writeLines(" - reading Thermal indices")
fname = list_of_files[grepl("term", x = list_of_files)]
therm = fread(fname)
therm <- therm |> select(dataset, gid, temperature_mean, temperature_median, temperature_std, temperature_max, temperature_min)
therm$dataset = gsub("_.*$","",therm$dataset)

vec = (colSums(is.na(therm))/nrow(therm) < config$missing_threshold)
therm <- therm[, vec, with = FALSE]

## dem
writeLines(" - reading DEM indices")
fname = list_of_files[grepl("Dem|dem", x = list_of_files)]
dem = fread(fname)
dem <- dem |> select(dataset, gid, altezza_mean, altezza_median, altezza_std, altezza_max, altezza_min, summation)
dem$dataset = gsub("_.*$","",dem$dataset)

vec = (colSums(is.na(dem))/nrow(dem) < config$missing_threshold)
dem <- dem[, vec, with = FALSE]

########## MERGING ####################
writeLines(" - merging datasets ... ")
temp <- purrr::reduce(list(rgb, multi, therm, dem), dplyr::left_join, by = c("gid","dataset"))
nrec = nrow(temp)

## removing columns (indices) with more than 50% missing data
vec = (100*(colSums(is.na(temp))/nrec) < 100*config$missing_threshold)
temp <- temp[, vec, with=FALSE]

writeLines(" - writing out merged file")
# fname = paste("merged_indices_", config$species, ".csv", sep="")
# fwrite(x = temp, 
#        file = file.path(config$base_folder, config$outdir, fname),
#        sep = ","
#        )

fname = paste("merged_indices_", config$species, ".xlsx", sep="")
write_xlsx(x = temp, path = file.path(config$base_folder, config$outdir, fname))

print("DONE!")
