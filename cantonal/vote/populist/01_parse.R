source("~/swissinfo/_helpers/helpers.R")

############################################################################################
###   SETTINGS
############################################################################################

infile <- 'concatenate_popu.csv'


# Load xls sheet
df <- read.csv(infile, row.names = 1)




## MAP canton name to 2 letters abbrevation
iso2c <- canton_namesStrict(rownames(df))
rownames(df) <- iso2c

# reorder df by iso2 canton abbreviations
iorder <- match(canton_names$iso2, rownames(df))
df <- df[iorder,]

write.csv(df, "popuInitiatives.csv")


















iorder <- match(canton_names$iso2, colnames(df))

data <- df[,iorder]

# deal with some duplicated indicators!
rown <- gsub(" {2,}", " ", paste(df[,2], df[,1]))
rown[which(duplicated(rown))] <- paste(rown[which(duplicated(rown))], "2")

rownames(data) <- rown

row.sub <- c('Population Habitants en milliers', 'Population par km2', 'Population Etrangers en %', "Population 0–19", "Population 20–64", "Population 65 ou plus", "Population Population urbaine en %",
	"Langue principale en % Allemand" , "Appartenance à une religion en % Catholiques romains"  , "Appartenance à une religion en % Evangéliques réformés" ,
	"Surface en km2 Surfaces d'habitat et d'infrastructure en %"  , "Travail Taux de chômage en % (selon le SECO)", "Economie Produit intérieur brut par habitant, en francs",
	"Mobilité Voitures de tourisme pour 1000 habitants", "Logements Taux de logements vacants", "Niveau de formation (dès 25 ans) en % Sans formation postobligatoire",
	"Niveau de formation (dès 25 ans) en % Degré secondaire II", "Niveau de formation (dès 25 ans) en % Degré tertiaire" ,
	"Protection sociale Taux d'aide sociale ")

write.csv(data[row.sub,], file = "cantonal_sub.csv")



