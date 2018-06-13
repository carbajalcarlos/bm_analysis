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

# ----- Filtering interest articles -----
# # Extracting unique data
# entry.type <- as.data.frame(table(di$DT), stringsAsFactors = FALSE)
# entry.details <- as.data.frame(table(di$DT2), stringsAsFactors = FALSE)#; entry.details

# 
# 
# write.csv(x = sub_cat, file = "0_process/subjects.csv")
# 
# # Generation of the list <-
# index <- grep(pattern = "innov", x = di)

# Filtering entry type
lista <- c("ARTICLE", "ARTICLE; BOOK CHAPTER", "ARTICLE; PROCEEDINGS PAPER", "BOOK", 
          "EDITORIAL MATERIAL", "PROCEEDINGS PAPER")
di_fil <- di[which(is.element(el = di$DT2, set = lista)), ]
# Filtering by title
index.a <- grep(pattern = "innov", x = di_fil$TI, ignore.case = TRUE)
index.b <- grep(pattern = "digit", x = di_fil$TI, ignore.case = TRUE)
index <- index.a[is.element(el = index.a, set = index.b)]
# Filtering by abstract
index.a <- grep(pattern = "innov", x = di_fil$AB, ignore.case = TRUE)
index.b <- grep(pattern = "digit", x = di_fil$AB, ignore.case = TRUE)
index <- unique(c(index, index.a[is.element(el = index.a, set = index.b)]))
# Filtering by author keywords
index.a <- grep(pattern = "innov", x = di_fil$DE, ignore.case = TRUE)
index.b <- grep(pattern = "digit", x = di_fil$DE, ignore.case = TRUE)
index <- unique(c(index, index.a[is.element(el = index.a, set = index.b)]))
# Filtering by aggregator keywords
index.a <- grep(pattern = "innov", x = di_fil$ID, ignore.case = TRUE)
index.b <- grep(pattern = "digit", x = di_fil$ID, ignore.case = TRUE)
index <- unique(c(index, index.a[is.element(el = index.a, set = index.b)]))
# Filtering by subject categories
index.a <- grep(pattern = "innov", x = di_fil$SC, ignore.case = TRUE)
index.b <- grep(pattern = "digit", x = di_fil$SC, ignore.case = TRUE)
index <- unique(c(index, index.a[is.element(el = index.a, set = index.b)]))
di_fil <- di_fil[index, ]

#save.set <- di_fil
#di_fil <- save.set

# Replacing general categories 
categories <- read.csv(file = "0_process/subjects.csv", header = TRUE, stringsAsFactors = FALSE)
for (i in 1:nrow(categories)) {
  index <- grep(pattern = categories$unique[i], x = di_fil$SC, fixed =  TRUE, ignore.case = TRUE)
  di_fil$SC[index] <- gsub(pattern = categories$unique[i], replacement = categories$global[i], x = di_fil$SC[index],
                           fixed = TRUE, ignore.case = TRUE)
}


# Extracting subject categories
sub_cat <- as.data.frame(table(trimws(unlist(strsplit(x = di_fil$SC, split = ";")), which = "both")), stringsAsFactors = FALSE)
sub_cat <- sub_cat[order(sub_cat$Freq, decreasing = TRUE), ]

# ----- Creating output files -----
# Removing temporary files
if(is.element(el = "ptm", set = ls())) { rm(ptm) }

# # Saving the rare.comics subset
save(file = "0_process/di_raw.Rdata", list = "di_raw")
save(file = "0_process/di_df.Rdata", list = "di")
save(file = "0_process/di_fil.Rdata", list = "di_fil")
