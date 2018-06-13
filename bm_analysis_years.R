# ----- Initialisation -----
# Loading required libraries
require(bibliometrix)

# Loading required datasets
load(file = "0_process/di_fil.Rdata")
# Split by years
set.a <- subset(x = di_fil, subset = di_fil$PY <= 2009)
set.b <- subset(x = di_fil, subset = di_fil$PY > 2009 & di_fil$PY <= 2013)
set.c <- subset(x = di_fil, subset = di_fil$PY > 2013 & di_fil$PY <= 2016)
set.d <- subset(x = di_fil, subset = di_fil$PY > 2016)

# ----- Bibliometric analysis and comparison -----
# bibliometric analysis
bma <- biblioAnalysis(di_fil, sep = ";")
bma.a <- biblioAnalysis(set.a, sep = ";")
bma.b <- biblioAnalysis(set.b, sep = ";")
bma.c <- biblioAnalysis(set.c, sep = ";")
bma.d <- biblioAnalysis(set.d, sep = ";")
# bibliometric summaries
temp <- summary(object = bma, k = 12, pause = FALSE)
temp <- summary(object = bma.a, k = 6, pause = FALSE)
temp <- summary(object = bma.b, k = 6, pause = FALSE)
temp <- summary(object = bma.c, k = 6, pause = FALSE)
temp <- summary(object = bma.d, k = 6, pause = FALSE)
# ploting analysis
plot(x = bma, k = 12, pause = FALSE)
plot(x = bma.a, k = 6, pause = FALSE)
plot(x = bma.b, k = 6, pause = FALSE)
plot(x = bma.c, k = 6, pause = FALSE)
plot(x = bma.d, k = 6, pause = FALSE)
# metatag extraction
#bm <- metaTagExtraction(M = bm, Field = "AU_CO", sep = ";")

# ----- Networks plotting ------
## Authors's keywords
### Set A 1975-2009
au_kw.a <- biblioNetwork(M = set.a, analysis = "co-occurrences", network = "author_keywords", ";")
set.seed(70)
networkPlot(NetMatrix = au_kw.a, normalize = "association", weighted = TRUE, n = 20,
            Title = "Author's keywords co-ocurrences - Set A 1975-2009",
            remove.multiple = TRUE, remove.isolates = TRUE, halo = TRUE,
            curved = FALSE, cluster = "walktrap",
            type = "fruchterman", size = TRUE, labelsize = 2.5, edgesize = 5)
### Set B 2010-2013
au_kw.b <- biblioNetwork(M = set.b, analysis = "co-occurrences", network = "author_keywords", ";")
set.seed(70)
networkPlot(NetMatrix = au_kw.b, normalize = "association", weighted = TRUE, n = 20,
            Title = "Author's keywords co-ocurrences - Set B 2010-2013",
            remove.multiple = TRUE, remove.isolates = TRUE, halo = TRUE,
            curved = FALSE, cluster = "walktrap",
            type = "fruchterman", size = TRUE, labelsize = 2.5, edgesize = 5)
### Set C 2014-2016
au_kw.c <- biblioNetwork(M = set.c, analysis = "co-occurrences", network = "author_keywords", ";")
set.seed(69)
networkPlot(NetMatrix = au_kw.c, normalize = "association", weighted = TRUE, n = 20,
            Title = "Author's keywords co-ocurrences - Set C 2014-2016",
            remove.multiple = TRUE, remove.isolates = TRUE, halo = TRUE,
            curved = FALSE, cluster = "walktrap",
            type = "fruchterman", size = TRUE, labelsize = 2.5, edgesize = 5)
### Set D 2017-2018
au_kw.d <- biblioNetwork(M = set.d, analysis = "co-occurrences", network = "author_keywords", ";")
set.seed(69)
networkPlot(NetMatrix = au_kw.d, normalize = "association", weighted = TRUE, n = 20,
            Title = "Author's keywords co-ocurrences - Set D 2017-2018",
            remove.multiple = TRUE, remove.isolates = TRUE, halo = TRUE,
            curved = FALSE, cluster = "walktrap",
            type = "fruchterman", size = TRUE, labelsize = 2.5, edgesize = 5)

## Aggregator's keywords
### Set A 1975-2009
db_kw.a <- biblioNetwork(M = set.a, analysis = "co-occurrences", network = "keywords", ";")
set.seed(70)
networkPlot(NetMatrix = db_kw.a, normalize = "association", weighted = TRUE, n = 20,
            Title = "Aggregators's keywords co-ocurrences - Set A 1975-2009",
            remove.multiple = TRUE, remove.isolates = TRUE, halo = TRUE,
            type = "fruchterman", size = TRUE, labelsize = 2.5, edgesize = 5)
### Set B 2010-2013
db_kw.b <- biblioNetwork(M = set.b, analysis = "co-occurrences", network = "keywords", ";")
set.seed(68)
networkPlot(NetMatrix = db_kw.b, normalize = "association", weighted = TRUE, n = 20,
            Title = "Aggregators's keywords co-ocurrences - Set B 2010-2013",
            remove.multiple = TRUE, remove.isolates = TRUE, halo = TRUE,
            type = "fruchterman", size = TRUE, labelsize = 2.5, edgesize = 5)
### Set C 2014-2016
db_kw.c <- biblioNetwork(M = set.c, analysis = "co-occurrences", network = "keywords", ";")
set.seed(69)
networkPlot(NetMatrix = db_kw.c, normalize = "association", weighted = TRUE, n = 18,
            Title = "Aggregators's keywords co-ocurrences - Set C 2014-2016",
            remove.multiple = TRUE, remove.isolates = TRUE, halo = TRUE,
            type = "fruchterman", size = TRUE, labelsize = 2.5, edgesize = 5)
### Set D 2017-2018
db_kw.d <- biblioNetwork(M = set.d, analysis = "co-occurrences", network = "keywords", ";")
set.seed(70)
networkPlot(NetMatrix = db_kw.d, normalize = "association", weighted = TRUE, n = 20,
            Title = "Aggregators's keywords co-ocurrences - Set D 2017-2018",
            remove.multiple = TRUE, remove.isolates = TRUE, halo = TRUE,
            type = "fruchterman", size = TRUE, labelsize = 2.5, edgesize = 5)

## Subject category
### masking the subject category under  author's keywords
set.a2 <- set.a
index.akw <- which(names(set.a2) == "DE")
index.sc <- which(names(set.a2) == "SC")
names(set.a2)[index.akw] <- "DE2"
names(set.a2)[index.sc] <- "DE"
set.b2 <- set.b
index.akw <- which(names(set.b2) == "DE")
index.sc <- which(names(set.b2) == "SC")
names(set.b2)[index.akw] <- "DE2"
names(set.b2)[index.sc] <- "DE"
set.c2 <- set.c
index.akw <- which(names(set.c2) == "DE")
index.sc <- which(names(set.c2) == "SC")
names(set.c2)[index.akw] <- "DE2"
names(set.c2)[index.sc] <- "DE"
set.d2 <- set.d
index.akw <- which(names(set.d2) == "DE")
index.sc <- which(names(set.d2) == "SC")
names(set.d2)[index.akw] <- "DE2"
names(set.d2)[index.sc] <- "DE"

## Subject categories
### Set A 1975-2009
sc.a <- biblioNetwork(M = set.a2, analysis = "co-occurrences", network = "author_keywords", ";")
set.seed(68)
networkPlot(NetMatrix = sc.a, normalize = "association", weighted = TRUE, n = 25,
            Title = "Subject categories's co-ocurrences - Set A 1975-2009",
            remove.multiple = TRUE, remove.isolates = TRUE, halo = TRUE,
            curved = FALSE, cluster = "walktrap",
            type = "fruchterman", size = TRUE, labelsize = 2.5, edgesize = 5)
### Set B 2010-2013
sc.b <- biblioNetwork(M = set.b2, analysis = "co-occurrences", network = "author_keywords", ";")
set.seed(70)
networkPlot(NetMatrix = sc.b, normalize = "association", weighted = TRUE, n = 25,
            Title = "Subject categories's co-ocurrences - Set B 2010-2013",
            remove.multiple = TRUE, remove.isolates = TRUE, halo = TRUE,
            curved = FALSE, cluster = "walktrap",
            type = "fruchterman", size = TRUE, labelsize = 2.5, edgesize = 5)
### Set C 2014-2016
sc.c <- biblioNetwork(M = set.c2, analysis = "co-occurrences", network = "author_keywords", ";")
set.seed(72)
networkPlot(NetMatrix = sc.c, normalize = "association", weighted = TRUE, n = 25,
            Title = "Subject categories's co-ocurrences - Set C 2014-2016",
            remove.multiple = TRUE, remove.isolates = TRUE, halo = TRUE,
            curved = FALSE, cluster = "walktrap",
            type = "fruchterman", size = TRUE, labelsize = 2.5, edgesize = 5)
### Set D 2017-2018
sc.d <- biblioNetwork(M = set.d2, analysis = "co-occurrences", network = "author_keywords", ";")
set.seed(72)
networkPlot(NetMatrix = sc.d, normalize = "association", weighted = TRUE, n = 25,
            Title = "Subject categories's co-ocurrences - Set D 2017-2018",
            remove.multiple = TRUE, remove.isolates = TRUE, halo = TRUE,
            curved = FALSE, cluster = "walktrap",
            type = "fruchterman", size = TRUE, labelsize = 2.5, edgesize = 5)

