
library("dplyr")
library("readxl")
library("writexl")
library("tidyverse")
library("data.table")

basefolder = "/home/filippo/Documents/polyploid_breeding/drone_phenotyping/vegetation_indices"
phenotype_file = "wheat/data/Phenotypic_data_SIS_Crop_season23-24_unito.xlsx"
cross_ref_file = "wheat/data/GD_ID-Code_SL.csv"
outdir = "Analysis/merged_data"

#import files
fname = file.path(basefolder, phenotype_file)
SL_fenotipi <- readxl::read_xlsx(fname, sheet = 1)
print("Remove unnecessary columns: LOCATION, trial, TESI, REP, GENOTYPE CODE")
SL_fenotipi <- SL_fenotipi |> select(-c(LOCATION, trial, TESI, REP, `GENOTYPE CODE`))

fname = file.path(basefolder, cross_ref_file)
SL_gid_code <- fread(fname)
SL_gid_code <- SL_gid_code |> select(-field)

###########################
## DUPLICATES
###########################
writeLines(" - DUPLICATES")
## phenotype files
pheno_codes = SL_fenotipi$code
if (sum(duplicated(pheno_codes)) == 0) {
  print("No duplicated accession codes in the file of phenotypes")
} else print("There are DUPLICATED accession codes in the file of phenotypes")


## cross reference file
cross_codes = SL_gid_code$code
dupl_codes = SL_gid_code[duplicated(cross_codes),"code"] |> pull()
ndupl = length(unique(dupl_codes))
duplicates <- filter(SL_gid_code, code %in% dupl_codes) |> arrange(code)

print(paste("In the cross-reference file there are", ndupl, "duplicated accession codes, for a total of", nrow(temp), "records"))

## Now we take unique accession codes and calculate intersections and differences
writeLines(" - Working with unique accession codes")
pheno_codes = unique(pheno_codes)
cross_codes = unique(cross_codes)

print(paste("There are", length(pheno_codes), "unique accession codes in the phenotype file"))
print(paste("There are", length(cross_codes), "unique accession codes in the cross-reference file"))

intersection = intersect(pheno_codes, cross_codes)
print(paste("The accession codes in common between the phenotype file and the cross-reference file are:", length(intersection)))

print("Records from the phenotype file whose accession code is in the intersection (has at least one gid):")
filter(SL_fenotipi, code %in% intersection) |> nrow()
print("Records from the cross-reference file whose accession code is in the intersection (has a phenotype):")
filter(SL_gid_code, code %in% intersection) |> nrow()

#############################
## MERGE DATASETS
#############################
writeLines(" - inner join of the two datasets")
merged_dataset <- SL_fenotipi |> inner_join(SL_gid_code, by = "code")

ndiff = (nrow(merged_dataset) - length(intersection))
print(paste("The merged datasets has", nrow(merged_dataset), "this is", ndiff, "more than the intersection"))
vec = temp$code[duplicated(merged_dataset$code)]
dupl_recs = merged_dataset |> filter(code %in% vec) |> nrow()

print(paste("This difference corresponds to", length(vec), "duplicated accession codes, for a total of", dupl_recs, "records"))
print("These are records with duplicated accession codes but different gid's; the phenotypic values are duplicated")

if(sum(vec %in% dupl_codes) == length(vec)) { 
  cat("All duplicated accessions from the merged dataset are included
  in the original duplicated accessions from the cross-reference file") 
} else print("Warning: not all duplicated accessions after merging seem to be in the cross-reference file")

ndupl_other = filter(SL_fenotipi, code %in% setdiff(dupl_codes,vec)) |> nrow()

if(ndupl_other == 0) {
  cat("All other duplicated accessions from the cross-reference file
        do not have a phenotypic record")
}

writeLines(" - writing our results")
fname = file.path(basefolder, outdir, "phenotypes_gid.xlsx")
write_xlsx(x = merged_dataset, path = fname)



