# ----- Initialisation -----
# Loading required libraries
require(bibliometrix)

# Loading input data
raw <- readFiles("0_input/di_wos_001.bib", "0_input/di_wos_002.bib", "0_input/di_wos_003.bib", "0_input/di_wos_004.bib",
                 "0_input/di_wos_005.bib", "0_input/di_wos_006.bib", "0_input/di_wos_007.bib", "0_input/di_wos_008.bib",
                 "0_input/di_wos_009.bib", "0_input/di_wos_010.bib", "0_input/di_wos_011.bib", "0_input/di_wos_012.bib",
                 "0_input/di_wos_013.bib", "0_input/di_wos_014.bib", "0_input/di_wos_015.bib", "0_input/di_wos_016.bib",
                 "0_input/di_wos_017.bib", "0_input/di_wos_018.bib", "0_input/di_wos_019.bib", "0_input/di_wos_020.bib",
                 "0_input/di_wos_021.bib", "0_input/di_wos_022.bib", "0_input/di_wos_023.bib", "0_input/di_wos_024.bib",
                 "0_input/di_wos_025.bib", "0_input/di_wos_026.bib")

categories <- read.csv(file = "0_input/jcr_categories.csv",
                       header = TRUE, stringsAsFactors = FALSE)
categories <- categories[1:234, ]
journals <- read.csv(file = "0_input/jcr_journals.csv",
                     header = TRUE, stringsAsFactors = FALSE)
journals <- journals[1:12120, ]
mjl <- read.csv(file = "0_input/mjl_index.csv",
                header = TRUE, stringsAsFactors = FALSE)
for (i in 1:ncol(mjl)) {
  index <- grep(pattern = ">", x = mjl[, i])
  if (length(index) > 0) {
    mjl[index, i] <- gsub(pattern = ">", replacement = ",", x = mjl[index, i])
  }
}

# ----- Parsing the data -----
# Listing individual entries 
index <- c(grep(pattern = "^@[[:alpha:]]+\\{", x = raw)-1, length(raw)+1)
list_raw <- list()
for (i in 1:(length(index)-1)) {
  temp <- list(x = raw[(index[i]+1):(index[i+1]-1)])
  list_raw <- c(list_raw, temp)
}

# Retrieving all the fields used in the list
fields <- character()
for (i in 1:length(list_raw)) {
  index <- grep(pattern = " = {" , x = list_raw[[i]], fixed = TRUE)
  temp <- list_raw[[i]][index]
  temp <- trimws(gsub(pattern = "(^.*)= \\{+(.*)",replacement = "\\1", x = temp), which = "both")
  temp <- tolower(trimws(x = temp, which = "both"))
  fields <- c(fields, temp)
}
fields <- as.data.frame(table(fields), stringsAsFactors = FALSE)
fields <- fields[order(fields$fields, decreasing = FALSE), ]
fields <- c("entry-type", fields$fields)

# Creation of bibliography dataframe
rare <- data.frame(matrix(data = NA, ncol = length(fields), nrow = length(list_raw)), 
                    stringsAsFactors = FALSE)
colnames(rare) <- make.names(fields)
for (i in 1:length(list_raw)) {
  index <- grep(pattern = " = {" , x = list_raw[[i]], fixed = TRUE)
  fields <- list_raw[[i]][index]
  fields <- gsub(pattern = "(^.*) = \\{+(.*)",replacement = "\\1", x = fields)
  fields <- make.names(tolower(trimws(x = fields, which = "both")))
  # Filling individual attributes
  rare$entry.type[i] <- gsub(pattern = "^@(.*)\\{(.*)",replacement = "\\1", x = list_raw[[i]][1])
  index <- c(index, length(list_raw[[i]]))
  for (j in 1:length(fields)) {
    pat <- paste(c("^", fields[j], "$"), collapse = "")
    column <- grep(pattern = pat, x = colnames(rare))
    clean <- c("abstract", "author")
    if (is.element(el = fields[j], set = clean)) {
      temp <- paste(list_raw[[i]][index[j]:(index[j+1]-1)], collapse = "")
      temp <- gsub(pattern = "[[:space:]]+", replacement = " ", x = temp)
    } else {
      temp <- paste(list_raw[[i]][index[j]:(index[j+1]-1)], collapse = ";")
    }
    if (fields[j] == "author") {
      temp <- gsub(pattern = ".*\\{(.*)\\}.*",replacement = "\\1", x = temp)
    } else  {
      temp <- gsub(pattern = ".*\\{{2}(.*)\\}{2}.*",replacement = "\\1", x = temp)
    }
    rare[i,column] <- tolower(temp)
  }
}

lista <- unique(rare$research.areas)
column <- grep(pattern = ";   ", x = lista)
lista[column] <- gsub(pattern = ";   ", replacement = " ", x = lista[column])
column <- grep(pattern = "\\\\&", x = lista)
lista[column] <- gsub(pattern = "\\\\&", replacement = "&", x = lista[column])

lista <- trimws(unlist(strsplit(na.omit(lista), split = ";")), which = "both")
lista <- toupper(sort(unique(lista)))
temp <- is.element(el = lista, set = index)

head(rare$research.areas, n = 15)


# Invert order to comply with chronographical order ascending.
rare <- rare[nrow(rare):1, ]


# ----- Closing project -----
# Insert name and store of the interest dataframe
di_rare <- rare
save(file = "0_process/di_rare.Rdata", list = "di_rare")

# Removing and storing information 
rm(raw)
rm(list_raw)
rm(rare)
rm(clean)
rm(column)
rm(fields)
rm(i)
rm(j)
rm(index)
rm(pat)
rm(temp)

# # Saving the working space
save.image(file = "0_process/ws_manual.Rdata")
