
library("ggpubr")
library("ggplot2")
library("ggthemes")
library("reshape2")
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
    input_file = "processed_input/normalised_hight_and_volume.csv",
    label = "avg_volume",
    outdir = "barley",
    pattern = "_dem", ## pattern to remove from the dataset column in order to get flight date
    index_column = "summation",
    force_overwrite = FALSE
  ))
}

## -- 
HOME <- Sys.getenv("HOME")
repo = file.path(HOME, config$repo)
prjfolder = file.path(HOME, config$prjfolder)
outdir = file.path(prjfolder,config$outdir)


#plot 
fname = file.path(prjfolder, config$analysis_folder, config$input_file)
vegidx <- fread(fname)


vegidx$dataset = gsub(config$pattern,"",vegidx$dataset)
vegidx$dataset = as.Date(vegidx$dataset, "%y%m%d")

p <- ggplot(vegidx, aes(x = as.factor(dataset), y = .data[[config$index_column]], group=1)) + geom_jitter(aes(color = as.factor(dataset))) + 
  geom_smooth(method = "loess", color="blue", linewidth=1, se = TRUE) + 
  labs(color = "date") + theme_hc() + ylab(config$label) + 
  theme(axis.text.x = element_blank(), axis.title.x = element_blank(), axis.ticks.x = element_blank(), legend.position = "none")

q <- ggplot(vegidx, aes(x = as.factor(dataset), y = .data[[config$index_column]], colour = gid, group = gid)) +
  geom_line() + geom_point() + labs(color = "genetic line") + 
  xlab("date") + ylab(config$label) + theme_hc() +
  theme(axis.text.x = element_text(angle=90))

g <- ggarrange(p, q, ncol = 1)

dir.create(outdir, showWarnings = FALSE)
temp = paste(config$label, config$outdir, sep="_")
fname = file.path(outdir, paste(temp,".png", sep=""))

ggsave(filename = fname, plot = g, device = "png", width = 8, height = 10)

