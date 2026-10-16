library("dplyr")
library("readxl")
library("ggplot2")
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
    species = "barley",
    basefolder = "~/Documents/polyploid_breeding/drone_phenotyping/vegetation_indices",
    index_file = "Analysis/merged_data/merged_indices_barley.xlsx",
    phenotype_file = "Analysis/merged_data/phenotypes_gid_barley.xlsx",
    outdir = 'Analysis/correlations',
    flight = "all", ## this can be all
    method = "pearson", ## pearson, spearman, kendall
    missing_threshold = 0.5, ## a quarter of the data is missing
    force_overwrite = FALSE
  ))
  
}
## -------------------------------------- ##

print(paste("CROP SPECIES:", config$species))

dir.create(file.path(config$basefolder, config$outdir, config$species), showWarnings = FALSE, recursive = TRUE)

writeLines(" - reading index file")
fname = file.path(config$basefolder, config$index_file)
print(fname)
indices = readxl::read_xlsx(fname)

writeLines(" - reading phenotype file")
fname = file.path(config$basefolder, config$phenotype_file)
print(fname)
phenotypes = readxl::read_xlsx(fname)

if (config$flight != "all") {
  
  indices = filter(indices, dataset == config$flight)
  
  intersection = intersect(indices$gid, phenotypes$gid)
  phenotypes <- filter(phenotypes, gid %in% intersection)
  indices <- filter(indices, gid %in% intersection)
  
  # Reorder df2 to match the order of df1
  indices <- indices[match(phenotypes$gid, indices$gid), ]
  
  # Verify that the IDs now match
  stopifnot(identical(phenotypes$gid, indices$gid))
  
  X1 <- select(indices, -c(dataset,gid)) |> mutate(across(everything(), as.numeric))
  X2 <- select(phenotypes, -c(accession,gid)) |> select(where(is.numeric))
  
  cormatrix <- cor(X1, X2, method = "pearson", use = "pairwise.complete.obs")
  
  # Convert the correlation matrix to long format
  cor_long <- as.data.frame(cormatrix) %>%
    tibble::rownames_to_column("var1") %>%
    pivot_longer(
      cols = -var1,
      names_to = "var2",
      values_to = "correlation"
    )
  
  # Plot
  g <- ggplot(cor_long, aes(x = var2, y = var1, fill = correlation)) +
    geom_tile(color = "white") +
    scale_fill_gradient2(
      low = "blue",
      mid = "white",
      high = "red",
      midpoint = 0,
      limits = c(-1, 1),
      name = "Correlation"
    ) +
    theme_minimal() +
    theme(
      axis.text = element_text(size = 6),
      axis.text.x = element_text(angle = 45, hjust = 1),
      panel.grid = element_blank()
    ) +
    labs(x = NULL, y = NULL, title = "Cross-correlation matrix") +
    coord_fixed() + 
    guides(fill = guide_legend(override.aes = list(size = 0.5))) + 
    theme(legend.title = element_text(size = 8), 
          legend.text = element_text(size = 8))
  
  
  fname = paste(config$method,"_correlations_", config$flight, ".png", sep="")
  fname = file.path(config$basefolder, config$outdir, config$species, fname)
  ggsave(filename = fname, plot = g, device = "png", width = 7.5, height = 8)
} else if (config$flight == "all") {
  
  for (w in unique(indices$dataset)) {
    
    print(w)
    temp = filter(indices, dataset == w)
    
    intersection = intersect(temp$gid, phenotypes$gid)
    phenotypes <- filter(phenotypes, gid %in% intersection)
    temp <- filter(temp, gid %in% intersection)
    
    # Reorder df2 to match the order of df1
    temp <- temp[match(phenotypes$gid, temp$gid), ]
    
    # Verify that the IDs now match
    stopifnot(identical(phenotypes$gid, temp$gid))
    
    X1 <- select(temp, -c(dataset,gid)) |> mutate(across(everything(), as.numeric)) |>
      select(where(~ n_distinct(.x, na.rm = TRUE) > 1))
    X2 <- select(phenotypes, -c(accession,gid)) |> select(where(is.numeric)) |>
      select(where(~ n_distinct(.x, na.rm = TRUE) > 1))
    
    cormatrix <- cor(X1, X2, method = "pearson", use = "pairwise.complete.obs")
    
    # Convert the correlation matrix to long format
    cor_long <- as.data.frame(cormatrix) %>%
      tibble::rownames_to_column("var1") %>%
      pivot_longer(
        cols = -var1,
        names_to = "var2",
        values_to = "correlation"
      )
    
    # Plot
    g <- ggplot(cor_long, aes(x = var2, y = var1, fill = correlation)) +
      geom_tile(color = "white") +
      scale_fill_gradient2(
        low = "blue",
        mid = "white",
        high = "red",
        midpoint = 0,
        limits = c(-1, 1),
        name = "Correlation"
      ) +
      theme_minimal() +
      theme(
        axis.text = element_text(size = 6),
        axis.text.x = element_text(angle = 45, hjust = 1),
        panel.grid = element_blank()
      ) +
      labs(x = NULL, y = NULL, title = "Cross-correlation matrix") +
      coord_fixed() + 
      guides(fill = guide_legend(override.aes = list(size = 0.5))) + 
      theme(legend.title = element_text(size = 8), 
            legend.text = element_text(size = 8))
    
    
    fname = paste(config$method,"_correlations_", w, ".png", sep="")
    fname = file.path(config$basefolder, config$outdir, config$species, fname)
    ggsave(filename = fname, plot = g, device = "png", width = 7.5, height = 8)
  }
}




