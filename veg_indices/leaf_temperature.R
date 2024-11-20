## input : output of drone2report for temperature from thermal camera imagesa

library("tidyverse")

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
    input_file = "indexes_term_F.csv",
    support_file = "CNR_Riepilogo Temperature voli.xlsx",
    outdir = "processed_input",
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
tdata = fread(fname)

## read support file with climate data
fname = file.path(prjfolder, config$analysis_folder, config$support_file)
temp = readxl::read_xlsx(fname, sheet = 1)

tdata$env_temp = temp$TEMP[match(tdata$dataset, temp$dataset)]
tdata <- mutate(tdata, leaf_temperature = temperature_mean-env_temp)

tdata |>
  group_by(dataset) |>
  summarise(avgT = mean(temperature_mean), avg_t_env = mean(env_temp), avg_leaf = mean(leaf_temperature)) |>
  print()

fname = file.path(outdir, "leaf_temperature.csv")

fwrite(x = tdata, file = fname)

