
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
    input_file = "processed_input/indexes_term_calcolati_temperature_per_plot.csv",
    label = "avg_leaf_temperature",
    outdir = "barley",
    pattern = "_thermal", ## pattern to remove from the dataset column in order to get flight date
    index_column = "t_fogliare",
    force_overwrite = FALSE
  ))
}

## -- 
HOME <- Sys.getenv("HOME")
repo = file.path(HOME, config$repo)
prjfolder = file.path(HOME, config$prjfolder)
outdir = file.path(prjfolder,config$outdir)


#plot NDVI Fiorenzuola
fname = file.path(prjfolder, config$analysis_folder, config$input_file)
vegidx <- fread(fname)


vegidx$dataset = gsub(config$pattern,"",vegidx$dataset)
vegidx$dataset = as.Date(vegidx$dataset, "%y%m%d")

p <- ggplot(vegidx, aes(x = as.factor(dataset), y = .data[[config$index_column]], group=1)) + geom_jitter(aes(color = as.factor(dataset))) + 
  geom_smooth(method = "loess", color="blue", size=1, se = TRUE) + 
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

#################################


#plot temperatura fogliare
data_t<-read.csv("indexes_term_calcolati_temperature_per_plot.csv", header = TRUE, dec = ".")
plot_t<-ggplot(data_t, aes(x=dataset,y=t_fogliare, group=gid)) + 
  #geom_point(size=2, shape=21, na.rm = TRUE,color="black", fill="orange",se=FALSE)+ 
  geom_smooth(method = "loess",color="blue", size=0.2) +
  geom_text(label=data_t$gid,vjust=2, size=2) +
  theme(text = element_text(color = "black"),plot.title = element_text(size = 26, color = 'black'),axis.title = element_text(size = 16, color = 'black'),axis.title.y = element_text(angle = 90, color="black", size = 12, vjust=3),axis.text.x=element_text(angle=45, hjust=1,vjust=1, color = "black", size=10),plot.caption = element_text(size = 11,hjust=0, color="black"))+
  #geom_hline(aes(yintercept =10, colour = "red"))+
  #geom_hline(aes(yintercept =20, colour = "red"))+
  theme_hc()
plot_t

#plot altezza pianta 

data_a<-read.csv("indexes_calcolati_altezza_per_plot.csv", header = TRUE, dec = ".")
plot_a<-ggplot(data_a, aes(x=dataset,y=altezza_pianta, group=gid)) + 
  #geom_point(size=2, shape=21, na.rm = TRUE,color="black", fill="orange",se=FALSE)+ 
  geom_smooth(method = "loess",color="blue", size=0.2) +
  geom_text(label=data_a$gid,vjust=2, size=2) +
  theme(text = element_text(color = "black"),plot.title = element_text(size = 26, color = 'black'),axis.title = element_text(size = 16, color = 'black'),axis.title.y = element_text(angle = 90, color="black", size = 12, vjust=3),axis.text.x=element_text(angle=45, hjust=1,vjust=1, color = "black", size=10),plot.caption = element_text(size = 11,hjust=0, color="black"))+
  #geom_hline(aes(yintercept =10, colour = "red"))+
  #geom_hline(aes(yintercept =20, colour = "red"))+
  theme_hc()
plot_a

#plot volume pianta 

data_v<-read.csv("calcolati_index_dem_summation_per_plot.csv", header = TRUE, dec = ".")
plot_v<-ggplot(data_v, aes(x=dataset,y=volume_pianta, group=gid)) + 
  #geom_point(size=2, shape=21, na.rm = TRUE,color="black", fill="orange",se=FALSE)+ 
  geom_smooth(method = "loess",color="blue", size=0.2) +
  geom_text(label=data_v$gid,vjust=2, size=2) +
  theme(text = element_text(color = "black"),plot.title = element_text(size = 26, color = 'black'),axis.title = element_text(size = 16, color = 'black'),axis.title.y = element_text(angle = 90, color="black", size = 12, vjust=3),axis.text.x=element_text(angle=45, hjust=1,vjust=1, color = "black", size=10),plot.caption = element_text(size = 11,hjust=0, color="black"))+
  #geom_hline(aes(yintercept =10, colour = "red"))+
  #geom_hline(aes(yintercept =20, colour = "red"))+
  theme_hc()
plot_v
