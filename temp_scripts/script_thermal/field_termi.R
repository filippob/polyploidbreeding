install.packages("fieldimageR")
install.packages("terra")
install.packages("ggplot2")
require(FIELDimageR)
library(terra)
library(ggplot2)
# Carica l'immagine termica
thermal_image <- rast("thermal_image.tif")

# Convertire l'immagine termica in un dataframe per `fieldimageR`
thermal_df <- as.data.frame(thermal_image, xy = TRUE)

# Utilizzare la funzione di segmentazione per rimuovere il suolo
thermal_df_clean <- fieldMask(thermal_df, threshold = "Otsu", band = "layer")

# Convertire il dataframe pulito in un oggetto raster
thermal_image_clean <- rast(thermal_df_clean)

# Temperature di riferimento
Ta <- 30.0  # Temperatura dell'aria in gradi Celsius
Td <- 40.0  # Temperatura di una superficie asciutta in gradi Celsius
Tw <- 25.0  # Temperatura di una superficie bagnata in gradi Celsius

# Calcolare il CWSI per ogni pixel
CWSI <- (thermal_image_clean - Ta) / (Td - Tw)

# Convertire il raster in un dataframe per la visualizzazione con ggplot2
CWSI_df <- as.data.frame(CWSI, xy = TRUE)

# Visualizzare il CWSI
ggplot(CWSI_df, aes(x = x, y = y, fill = layer)) +
  geom_raster() +
  scale_fill_viridis_c() +
  labs(title = "Crop Water Stress Index (CWSI)",
       fill = "CWSI") +
  theme_minimal()
