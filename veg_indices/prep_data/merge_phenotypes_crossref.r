
library("tools")
library("dplyr")
library("readxl")
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
    basefolder = "~/Documents/polyploid_breeding/drone_phenotyping/vegetation_indices",
    phenotype_file = "wheat/data/Phenotypic_data_SIS_Crop_season23-24_unito.xlsx",
    cross_ref_file = "wheat/data/GD_ID-Code_SL.csv",
    outdir = 'Analysis/merged_data',
    species = "wheat",
    missing_threshold = 0.75, ## a quarter of the data is missing
    force_overwrite = FALSE
  ))
  
}
## -------------------------------------- ##

print(paste("CROP SPECIES:", config$species))

#import files
writeLines(" - reading the phenotype file")
fname = file.path(config$basefolder, config$phenotype_file)
extension = tools::file_ext(fname)

if (extension == "csv") {
  phenotypes <- fread(fname)
} else if (extension == "xlsx") {
  phenotypes <- readxl::read_xlsx(fname, sheet = 1)
} else stop("NOT VALID FILE EXTENSION")


print("Remove unnecessary columns: LOCATION, trial, TESI, REP, GENOTYPE CODE")
if (config$species == "wheat") {
  
  phenotypes <- phenotypes |> select(-c(LOCATION, trial, TESI, REP, `GENOTYPE CODE`, ...20, ...19))
  phenotypes <- phenotypes |> rename(accession = code)
} else if (config$species == "barley") {
  
  phenotypes <- phenotypes |> select(-c(Row, Column, rep))
}

writeLines(" - reading the cross reference file")
fname = file.path(config$basefolder, config$cross_ref_file)
extension = tools::file_ext(fname)

if (extension == "csv") {
  crossref <- fread(fname)
} else if (extension == "xlsx") {
  crossref <- readxl::read_xlsx(fname, sheet = 1)
} else stop("NOT VALID FILE EXTENSION")

if (config$species == "wheat") crossref <- crossref |> select(-field) |> rename(accession = code)

###########################
## DUPLICATES
###########################
writeLines(" - DUPLICATES")
## phenotype files
phenotype_accessions = phenotypes$accession
if (sum(duplicated(phenotype_accessions)) == 0) {
  print("No duplicated accession codes in the file of phenotypes: OK")
} else {
  
  print("There are DUPLICATED accession codes in the file of phenotypes")
  print("The duplicated lines are:")
  dupls = pull(unique(phenotypes[duplicated(phenotype_accessions),"accession"]))
  cat(dupls, sep ="\n")
  print("These will be removed from the phenotype file")
  phenotypes <- phenotypes |> filter(!accession %in% dupls)
}

phenotype_accessions = phenotypes$accession

## cross reference file
cross_accessions = crossref$accession
dupl_codes = crossref[duplicated(cross_accessions),"accession"] |> pull()
ndupl = length(unique(dupl_codes))
duplicates <- filter(crossref, accession %in% dupl_codes) |> arrange(accession)

print(paste("In the cross-reference file there are", ndupl, "duplicated accession codes, for a total of", nrow(duplicates), "records"))
print("The duplicated accessions are:")
cat(unique(dupl_codes), sep = ",")
cat("\n")

## Now we take unique accession codes and calculate intersections and differences
writeLines(" - Working with unique accession codes")
phenotype_accessions = unique(phenotype_accessions)
cross_accessions = unique(cross_accessions)

print(paste("There are", length(phenotype_accessions), "unique accession codes in the phenotype file"))
print(paste("There are", length(cross_accessions), "unique accession codes in the cross-reference file"))

intersection = intersect(phenotype_accessions, cross_accessions)
print(paste("The accession codes in common between the phenotype file and the cross-reference file are:", length(intersection)))

print("Records from the phenotype file whose accession code is in the intersection (has at least one gid):")
filter(phenotypes, accession %in% intersection) |> nrow()
print("Records from the cross-reference file whose accession code is in the intersection (has a phenotype):")
filter(crossref, accession %in% intersection) |> nrow()

#############################
## MERGE DATASETS
#############################
writeLines(" - inner join of the two datasets")
merged_dataset <- phenotypes |> inner_join(crossref, by = "accession")

ndiff = (nrow(merged_dataset) - length(intersection))
print(paste("The merged datasets has", nrow(merged_dataset), "this is", ndiff, "more than the intersection"))
vec = duplicates$accession[duplicated(merged_dataset$accession)]
dupl_recs = merged_dataset |> filter(accession %in% vec) |> nrow()

print(paste("This difference corresponds to", length(vec), "duplicated accession codes, for a total of", dupl_recs, "records"))

if (length(vec) > 0 & dupl_recs > 0) print("These are records with duplicated accession codes but different gid's; the phenotypic values are duplicated")

if(sum(vec %in% dupl_codes) == 0) {
  print("No duplicates")
} else if(sum(vec %in% dupl_codes) == length(vec)) { 
  cat("All duplicated accessions from the merged dataset are included
  in the original duplicated accessions from the cross-reference file\n") 
} else print("Warning: not all duplicated accessions after merging seem to be in the cross-reference file")

ndupl_other = filter(phenotypes, accession %in% setdiff(dupl_codes,vec)) |> nrow()

if(ndupl_other == 0) {
  cat("All other duplicated accessions from the cross-reference file
        do not have a phenotypic record\n")
}

writeLines(" - writing our results")

if (config$species == "wheat") {
  
  for (w in unique(merged_dataset$species)) {
    print(w)
    temp <- merged_dataset |> filter(species == w)
    fname = paste("phenotypes_gid_", w, ".xlsx", sep="")
    write_xlsx(x = temp, path = file.path(config$basefolder, config$outdir, fname))
    }
} else {
  
  fname = paste("phenotypes_gid_", config$species, ".xlsx", sep="")
  write_xlsx(x = merged_dataset, path = file.path(config$basefolder, config$outdir, fname))
}




