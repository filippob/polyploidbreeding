

# rm(list=ls())
# 
# 
# requiredPackages = c("tidyr","devtools","raster","terra","ggplot2","rgdal","DescTools",
#                      "lme4","emmeans","reshape2","car","plyr","factoextra","ggrepel",
#                      "agricolae","corrplot","RStoolbox","gridExtra","readxl", "terra")
# for(p in requiredPackages){
#   if(!require(p,character.only = TRUE)) install.packages(p)
#   library(p,character.only = TRUE)
# }
#library=c("tidyr","devtools","raster","terra","ggplot2","rgdal","DescTools","lme4","emmeans","reshape2","car","plyr","factoextra","ggrepel","agricolae","corrplot","RStoolbox","gridExtra","readxl", "terra", "tidyverse")
#install.packages('rgdal_1.6-7.tar.gz', repos = NULL, type = 'source')
#BiocManager::install("EBImage")
#devtools::install_github("OpenDroneMap/FIELDimageR")
#devtools::install_github("filipematias23/FIELDimageR.Extra")

library("sf")
library("spMC")
library("stars")
library("terra")
library("mapview")
require("cleanRfield")
library("FIELDimageR")
require("FIELDimageR.Extra")


basefolder = "/home/filippo/Documents/polyploid_breeding/drone_phenotyping/analisi_immagini/giulia_example/"

source("analisi_immagini/giulia_example/script_RGB//support_scripts/fieldIndexRedEdgeMXdual_RGB.R")
source("analisi_immagini/giulia_example/script_RGB//support_scripts/fieldInfoRedEdgeMXdual_RGB.R")


#' -----------------------------------------------------------------------------
#'  Flights characteristics - new output columns


#trial <- "Fiorenzuola"
#date <- "05-15-2024" # month / day / year

#' -----------------------------------------------------------------------------
#'  Example: DUS orthomosaic subset
#'  https://tilemill-project.github.io/tilemill/docs/guides/joining-data/ 


setCores(4)

## 1) Reading raster, uploading the mosaic
# (Giulia: se non va, togli pacchetto raster e metti terra, poi rimetti raster)
inpfile = "clippata_RGB_3_row_fiorenzuola.tif"
fname = file.path(basefolder, inpfile)
EX1.dus <- rast(fname)
EX1.dus

## 2) remove background (e.g. soil)
EX1.RemSoil <- fieldMask(mosaic = EX1.dus, Red = 1, Green = 2, Blue = 3, 
                         index = "HUE", ## Filippo: what is this?
                         cropValue=0, 
                         cropAbove=TRUE, 
                         plot=FALSE)


## 3) Reading the shapefile
inpfile = "Shapefile_3_rows_final.shp"
fname = file.path(basefolder, inpfile)
ShapeFile <- rgdal::readOGR(dsn = fname) # Option 01
#ShapeFile <- terra::vect("./SHAPEFILE.shp") # Option 02
#crs(ShapeFile)
ShapeFile

# plot
plotRGB(FIELDimageR:::RGB.rescale(EX1.dus,1))
plot(ShapeFile, border="green",add=T) 

#### 
index_list <- c("BI","SCI","GLI","SI","VARI","HUE","BGI", "NGRDI")
my_summary <- "function(x, na.rm) c(mean = mean(x, na.rm=na.rm), sd = sd(x, na.rm = na.rm), 
min = min(x, na.rm=na.rm), max = max(x, na.rm=na.rm), 
median = median(x,na.rm = na.rm), q2=quantile(x,probs=0.2,na.rm=na.rm), 
q25=quantile(x,probs=0.25,na.rm=na.rm), q5=quantile(x,probs=0.5,na.rm=na.rm), 
q75=quantile(x,probs=0.75,na.rm=na.rm), q98=quantile(x,probs=0.98,na.rm=na.rm))" # quantiles = (0.2, 0.25, 0.5, 0.75, 0.98)"


EX1.Indices1<- fieldIndexRedEdgeMXdual_RGB(mosaic = EX1.RemSoil$newMosaic, 
                                       Red = 1, Green = 2, Blue = 3,
                                       index = index_list,
                                       myIndex = NULL, # "(Blue/Red)"
                                       plot = TRUE) #

Ex1.Info1 <- fieldInfoRedEdgeMXdual(mosaic = EX1.Indices1, fun = my_summary, fieldShape = ShapeFile, n.core = 1) 
df_idx <- Ex1.Info1$Index_metric

#crs(EX1.Indices1)
#df_idx <- as.data.frame(Ex1.Info1)

for (i in index_list) {
 a<- fieldIndex(mosaic = EX1.RemSoil$newMosaic, 
                                         index = i,
                                         myIndex = NULL, # "(Blue/Red)"
                                         plot = FALSE) #
  b <- fieldInfo_extra(mosaic = a,  fieldShape = ShapeFile) 
  df1<- as.data.frame(b)
  df_idx[ ,ncol(df_idx)+1 ] <-  df1[, ncol(df1)]
  names(df_idx)[length(names(df_idx))]<- i
  }

df_idx2 <- cbind.data.frame(TRIAL = trial, Date = date, df_idx)

# save .RData
#save(EX1.Indices, Ex1.Info, file = "E:/Index_output.RData")
# write .csv
write.csv(df_idx2, file = "/~/Desktop/phenomics/drone_phenotyping/Fiorenzula/risultatiRGB_complete.csv", row.names = FALSE)

tmp_dir <- tempdir()
tmp_dir
list.files(tmp_dir)
tmp_file <- tempfile()
files <- list.files(tmp_dir, full.names = T, pattern = "^file")
files
file.remove(files)
data <- read.table('/~/Desktop/phenomics/drone_phenotyping/Fiorenzula/risultatiRGB_complete.csv', sep=",", header = 1)
print(data)


