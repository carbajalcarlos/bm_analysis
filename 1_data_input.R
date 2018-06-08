# ----- Initialisation -----
# Loading required libraries
require(bibliometrix)

# Loading input data
di_raw <- readFiles("0_input/di_wos_001.bib", "0_input/di_wos_002.bib", "0_input/di_wos_003.bib", "0_input/di_wos_004.bib",
                    "0_input/di_wos_005.bib", "0_input/di_wos_006.bib", "0_input/di_wos_007.bib", "0_input/di_wos_008.bib",
                    "0_input/di_wos_009.bib", "0_input/di_wos_010.bib", "0_input/di_wos_011.bib", "0_input/di_wos_012.bib",
                    "0_input/di_wos_013.bib", "0_input/di_wos_014.bib", "0_input/di_wos_015.bib", "0_input/di_wos_016.bib",
                    "0_input/di_wos_017.bib", "0_input/di_wos_018.bib", "0_input/di_wos_019.bib", "0_input/di_wos_020.bib",
                    "0_input/di_wos_021.bib", "0_input/di_wos_022.bib", "0_input/di_wos_023.bib", "0_input/di_wos_024.bib",
                    "0_input/di_wos_025.bib", "0_input/di_wos_026.bib")

# Conversion to dataframe
#ptm <- proc.time()
di <- convert2df(file = di_raw, dbsource = "isi", format = "bibtex")
#proc.time() - ptm

# ----- Creating output files -----
# Removing temporary files
if(is.element(el = "ptm", set = ls())) { rm(ptm) }

# # Saving the rare.comics subset
save(file = "0_process/di_raw.Rdata", list = "di_raw")
save(file = "0_process/di_df.Rdata", list = "di")
