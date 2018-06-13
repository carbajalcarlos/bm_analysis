# Semi process to extract the list of categories
lista <- unique(rare$research.areas)
column <- grep(pattern = ";   ", x = lista)
lista[column] <- gsub(pattern = ";   ", replacement = " ", x = lista[column])
column <- grep(pattern = "\\\\&", x = lista)
lista[column] <- gsub(pattern = "\\\\&", replacement = "&", x = lista[column])

lista <- trimws(unlist(strsplit(na.omit(lista), split = ";")), which = "both")
lista <- toupper(sort(unique(lista)))
temp <- is.element(el = lista, set = index)

head(rare$research.areas, n = 15)