source("~/swissinfo/_helpers/helpers.R")
font <- "Alegreya Sans"



table.all <- read.csv("forfaitFiscaux_recettesFiscales.csv", stringsAsFactors = F)

colIdx <- !grepl("note", colnames(table.all), ignore.case = T)
table <- table.all[,colIdx]
colnames(table)<- gsub("\\.", "", colnames(table))

# reorder canton factor in function of the number of forfaits
table[,1] <- factor(table[,1], levels = table[order(table[,3], na.last = F, decreasing = F),1])

# compute percentage forfait fiscaux out of the total revenues
table$forfaitRecetteCantonale <- (table$impôtcantonal / table$Recettesfiscalescantonales) * 100
table$bin <- cut(table$forfaitRecetteCantonale, breaks = seq(0, ceiling(max(table$forfaitRecetteCantonale, na.rm = T)), 0.5))
forfaitRecetteCantonale.mean <- mean(table$forfaitRecetteCantonale, na.rm = T)

p1 <- ggplot(table, aes(x = Canton, y =  Nombredeforfaits)) + geom_bar(stat = "identity", fill = "#CE4A21") +
	coord_flip(ylim = c(0, max(table$Nombredeforfaits, na.rm = T) + 200)) + ggtheme +
	theme(text=element_text(family=font , size=12), axis.ticks.y = element_blank()) + xlab("") +
	geom_text(data=table,aes(label=as.character(Nombredeforfaits)), hjust=-0.2, size = 2.5, color = "darkgrey") +
	ylab("Nombre de contribuables soumis au forfait fiscal en 2012")

tableM <- melt(table[,c(1, 4:7)], id = 1)
tableM.1 <- tableM[tableM$variable != 'totaledesrecettesfiscales',]
tableM.2 <- tableM[tableM$variable == 'totaledesrecettesfiscales',]
p2 <- ggplot(data = tableM.1 , aes(x = Canton, y =  value)) + geom_bar(aes(fill = variable)) +
	coord_flip(ylim = c(0, max(tableM.2$value, na.rm = T) + 30)) + ggtheme +
	geom_text(data=tableM.2 ,aes(label=as.character(value)), hjust=-0.2, size = 2.5, color = "darkgrey") +
	theme(text=element_text(family=font , size=12), axis.ticks.y = element_blank(), legend.title=element_blank(),
	legend.position=c(.8, .3)) +
	xlab("") + scale_fill_brewer(palette="Reds") +
	ylab("Recettes fiscales des forfaits fiscaux en 2012")


p3 <- ggplot(table, aes(x = Canton, y =  forfaitRecetteCantonale)) +
	geom_hline(yintercept = forfaitRecetteCantonale.mean, color = alpha("grey", 2/3), size = 0.5) +
	geom_bar(stat = "identity", aes(fill = bin))  +
	coord_flip(ylim = c(0, max(table$forfaitRecetteCantonale, na.rm = T) + 0.3)) + ggtheme +
	geom_text(data=table,aes(label=format(forfaitRecetteCantonale, digits = 1)), hjust=-0.2, size = 2.5, color = "darkgrey") +
	theme(text=element_text(family=font , size=12), axis.ticks.y = element_blank(), legend.title=element_blank(),
	legend.position=c(.8, .3)) + xlab("") +
	ylab("Contribution [%] des forfait fiscaux aux recettes cantonales totales") + scale_fill_brewer(palette = "Oranges")


pdf("forfait1.pdf", width = 8, height = 6)
grid.arrange(p1, p2, ncol=2)
dev.off()





require(rgdal)
require(rgeos)

# available layers: municipalities , cantons, districts
layer <- 'cantons'

setwd(swissMapShp.path)


ch <- readOGR(".", layer = layer)
lake <- readOGR(".", layer = "lakes")
country <- readOGR(".", layer = "country")

# convert to data frame for plotting with ggplot - takes a while
ch.df <- formatShp(ch)

# rename cantons to 2 letters code
cantonISO <- read.csv("~/swissinfo/_helpers/CantonCH_iso.csv")
cantonsRenamed <- sapply(levels(ch.df$NAME), function(ct) {
	nrow <- which(ct == cantonISO, T)
	if(length(nrow) > 1) as.character(cantonISO[nrow[1,1],1]) else NULL
})
levels(ch.df$NAME) <- cantonsRenamed

lake.df <- formatShp(lake)


# assign value
ch.df$bin <- table[match(ch.df$NAME, table[,'Canton']), 'bin']


p4 <- ggplot(ch.df, aes(x = long, y = lat, group = group)) +
    geom_polygon(colour = alpha("black", 0.9), size = 0.1, aes(fill = bin)) + scale_fill_brewer(palette = "Oranges") +
	geom_polygon(data = country, fill = NA, colour = alpha("black", 1), size = 0.15) +
	geom_polygon(data = lake.df, fill = alpha("lightgrey", alpha = 1), colour = alpha("lightgrey", alpha = 1)) + ggtheme2

pdf("forfait2.pdf", width = 8, height = 6)
grid.arrange(p3, ncol=2)
dev.off()

p4
ggsave("forfait3.pdf")




