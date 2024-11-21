#per calcolare altezza pianta
library("tidyverse")
library("data.table")


## PARAMETERS
args = commandArgs(trailingOnly=TRUE)
if (length(args) >= 1) {
  
  #loading the parameters
  if (file_ext(args[1]) %in% c("r","R")) {
    
    source(args[1])
    # source("Analysis/hrr/config.R")
  } else {
    
    load(args[1])
  }
  
} else {
  #this is the default configuration, used for development and debug
  writeLines('Using default config')
  
  #this dataframe should be always present in config files, and declared
  #as follows
  config = NULL
  config = rbind(config, data.frame(
    repo = "Documents/polyploid_breeding/polyploidbreeding",
    prjfolder = "Documents/polyploid_breeding/drone_phenotyping/vegetation_indices",
    analysis_folder = "veg_indexes",
    input_file = "drone2report/indexes_F_Dem.csv", ## from drone2report
    support_file = "processed_input/indexes_F_Dem_solo_240219.csv", ## plant height from first flight (reference)
    outdir = "processed_input",
    pattern = "_dem",
    force_overwrite = FALSE
  ))
}

## -- 
HOME <- Sys.getenv("HOME")
repo = file.path(HOME, config$repo)
prjfolder = file.path(HOME, config$prjfolder)
outdir = file.path(prjfolder,config$analysis_folder, config$outdir)

## read data file
fname = file.path(prjfolder, config$analysis_folder, config$input_file)
dem = fread(fname)

dem$date = gsub(config$pattern,"",dem$dataset)
dem$date = as.Date(dem$date, "%y%m%d")

# temp <- dem |>
#   group_by(dataset) |>
#   summarise(avg_height = mean(altezza_mean),
#             avg_sum = mean(summation))

dates = sort(unique(dem$date))
print(paste("Selecting", dates[1], "as baseline for plant height"))

## get baseline data
dem_baseline <- dem |>
  filter(date == dates[1])

dem$baseline_height = dem_baseline$altezza_mean[match(dem$gid, dem_baseline$gid)]
dem <- dem |>
  mutate(altezza_mean = altezza_mean - baseline_height)

#calcolo volume pianta
dem <- dem |>
  group_by(gid) |>
  mutate(max_px = max(pixels), coef_px = max_px / pixels)

dem <- dem |> 
  mutate(summation_norm = coef_px*summation)

dem_baseline <- dem_baseline |>
  group_by(gid) |>
  mutate(max_px = max(pixels), coef_px = max_px / pixels, summation_norm = coef_px*summation)

dem$baseline_volume = dem_baseline$summation_norm[match(dem$gid, dem_baseline$gid)]

dem <- dem |>
  mutate(summation = summation_norm - baseline_volume)

# temp2 <- dem |>
#   group_by(dataset) |>
#   summarise(avg_height_norm = mean(altezza_mean),
#             avg_sum_norm = mean(summation))
# 
# temp <- temp |> inner_join(temp2, by = "dataset")
# fwrite(x = temp, file = "barley/dem_stats.csv")

dem <- dem |> select(-c(baseline_height, baseline_volume, date, summation_norm, coef_px, max_px))

fname = file.path(outdir, "normalised_hight_and_volume.csv")
fwrite(x = dem, file = fname)
