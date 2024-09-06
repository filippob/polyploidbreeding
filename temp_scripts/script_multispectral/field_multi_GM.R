rm(list=ls())
### Necessary packages ###
requiredPackages = c("tidyr","devtools","raster","ggplot2","DescTools","lme4","emmeans","reshape2","car","plyr","factoextra","ggrepel","agricolae","corrplot","RStoolbox","gridExtra","readxl", "terra", "rgdal")
for(p in requiredPackages){
  if(!require(p,character.only = TRUE)) install.packages(p)
  library(p,character.only = TRUE)
}
##
setwd("C:/Users/giuliamoscatelli/Desktop/poly_r")
#install.packages('rgdal_1.6-7.tar.gz', repos = NULL, type = 'source')
##
require(FIELDimageR)
require(FIELDimageR.Extra)
require(cleanRfield)

##cor
source("/Users/giuliamoscatelli/Downloads/fieldIndexRedEdgeMXdual.R")
source("/Users/giuliamoscatelli/Downloads/fieldInfoRedEdgeMXdual.R")

library(spMC)
setCores(1)
# Reading raster
EX1 <- stack("/Users/giuliamoscatelli/Desktop/phenomics/drone_phenotyping/Fiorenzula/finali_qgis/clippata_240221_F_MULTI.tif")# all bands
EX1
# Removing soil to avoid bias 
EX1.RemSoil <- fieldMask(mosaic = rast(EX1), Red = 1, Green = 2, Blue = 3, index = "HUE", cropValue=0,cropAbove=T,plot=F)
library(sf)
library(terra)
library(rgdal)
#Shapefile
ShapeFile <- rgdal::readOGR(dsn="/Users/giuliamoscatelli/Desktop/phenomics/drone_phenotyping/Fiorenzula/240219_21/Fiorenzuola.shp")
#crs(ShapeFile)
ShapeFile
plot(ShapeFile, border="green",add=T)
index_list<- c("BI","BIM","SCI","GLI", "HI", "NDVI","GNDVI","RVI","NDRE", "NGRDI","SI","VARI","HUE","BGI","PSRI", "TVI","CVI","CIG","CIRE","DVI","EVI")
my_summary <- "function(x, na.rm) c(mean_mysum = mean(x, na.rm=na.rm), sd_mysum = sd(x, na.rm = na.rm), min_mysum = min(x, na.rm=na.rm), max_mysum = max(x, na.rm=na.rm), median_mysum = median(x,na.rm = na.rm), q2_mysum = quantile(x,probs=0.2,na.rm=na.rm), q25_mysum = quantile(x,probs=0.25,na.rm=na.rm), q5_mysum = quantile(x,probs=0.5,na.rm=na.rm), q75_mysum = quantile(x,probs=0.75,na.rm=na.rm), q98_mysum = quantile(x,probs=0.98,na.rm=na.rm))"
EX1.Indices1<- fieldIndexRedEdgeMXdual(mosaic = EX1.RemSoil$newMosaic, 
                                       coastalBlue = 1, Blue = 2, Green531 = 3, Green = 4, 
                                       Red650 = 5, Red = 6, RedEdge705 = 7, RedEdge = 8, RedEdge740 = 9, NIR = 10,
                                       myIndex = NULL,) # "(Blue/Red)" plot = T) 

Ex1.Info1 <- fieldInfoRedEdgeMXdual(mosaic = EX1.Indices1, fieldShape = ShapeFile, n.core = 1) 
df_idx <- Ex1.Info1$Index_metric
for (i in index_list) {
  a<- fieldIndexRedEdgeMXdual(mosaic = EX1.RemSoil$newMosaic,
                              index = i,
                              myIndex = NULL, # "(Blue/Red)"
                              plot = FALSE) #
  b <- fieldInfoRedEdgeMXdual(mosaic = a, fieldShape = ShapeFile, n.core = 1) 
  df1<- b$Index_metric
  df_idx[ ,ncol(df_idx)+1 ] <-  df1[, ncol(df1)]
  names(df_idx)[length(names(df_idx))]<- i
}
write.csv(df_idx, file = "/Users/giuliamoscatelli/Desktop/phenomics/drone_phenotyping/Fiorenzula/Risultati_nuovi_multi_240219_final.csv", row.names = FALSE)
#pulisce dai file temporanei 
tmp_dir <- tempdir()
tmp_dir
list.files(tmp_dir)
tmp_file <- tempfile()
files <- list.files(tmp_dir, full.names = T, pattern = "^file")
files
file.remove(files)
multi <- read.csv("/Users/giuliamoscatelli/Desktop/phenomics/drone_phenotyping/Fiorenzula/Risultati_nuovi_multi_240219_final.csv")
rgb <- read.csv("/Users/giuliamoscatelli/Desktop/phenomics/drone_phenotyping/Fiorenzula/Risultati_nuovi_rgb_24021_final.csv")
library(corrplot)
rgb_clean <- rgb[,!names(rgb) %in% c("area_name", "name", "area", "TRIAL", "Date", "VARI")]
multi_clean <- multi[,!names(multi) %in% c("area_name", "name", "area", "coastalBlue", "VARI", "Green531", "Red650", "RedEdge705", "RedEdge", "RedEdge740", "NIR", "BIM", "HI", "NDVI", "GNDVI", "RVI", "NDRE", "PSRI", "TVI", "CVI", "CIG", "CIRE", "DVI", "EVI", "HUE.1", "BI.1", "BI.2", "BIM.1", "SCI.1", "GLI.1", "HI.1", "NDVI.1", "GNDVI.1", "RVI.1", "NDRE.1", "NGRDI.1", "SI.1", "VARI.1", "HUE.1", "HUE.2", "BGI.1", "PSRI.1", "TVI.1", "CVI.1", "CIG.1", "CIRE.1", "DVI.1", "EVI.1")]
rgb_clean_rc <- subset(rgb_clean,metric=='mean' )
multi_clean_rc <- subset(multi_clean,metric=='mean' )
multi_clean_final <- multi_clean_rc[,!names(multi_clean_rc) %in% c("gid", "metric")]
rgb_clean_final <- rgb_clean_rc[,!names(rgb_clean_rc) %in% c("gid", "metric")]
#rgb_clean_final_numeric <- as.numeric(unlist(rgb_clean_final))
#multi_clean_final_numeric <- as.numeric(unlist(multi_clean_final))
df<-cor(rgb_clean_final, multi_clean_final, use='complete.obs')
corrplot(cor(df))
heatmap(df)
#recode excel file
#install.packages("xlsx")
library("xlsx")
write.xlsx(rgb,file = "/Users/giuliamoscatelli/Desktop/phenomics/drone_phenotyping/Fiorenzula/Filtrati_paper_rgb_240219.xlsx")
write.xlsx(multi,file = "/Users/giuliamoscatelli/Desktop/phenomics/drone_phenotyping/Fiorenzula/Filtrati_paper_multi_240219.xlsx")
