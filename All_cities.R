############################
## Laurence Hendry
## R version used: RStudio 0.99.892
## Behavioural Economics with Prof. Traxler
############################

#NB.
#naming convention for shapes file is PLZ99 for PLZ with 0 in front, PLZ99_N with no 0 in front, and PLTZORT99 provides city names
#library(doParallel)
library(shapefiles)
library(rgdal)
library(RColorBrewer)
library(dplyr)
library(leaflet)

#full firepower
#cl <- makeCluster(4)
#registerDoParallel(cl)

#Directory and taking correct columns from datasets
try(setwd("/Users/laurencehendry/GoogleDrive/MPP Hertie/2nd Year/Behavioral Economics & Experimental Policy Analysis/Groupwork/plz"),silent=TRUE)
try(setwd("C:/Users/Christopher/Google Drive/GitHub/Behavioural_Economics/Data"),silent=TRUE)

sub <- read.csv("rct.csv")
sub <- sub[c(2, 3, 4, 5, 6, 7, 12)]

#renaming
#names(sub)[1] <-'treatment.group'
#names(sub)[2] <-'unique.ID'
#names(sub)[3] <-'rent'
names(sub)[7] <-'PLZ99'

#Remove all observation with missing PLZs (NAs)
sub <- sub[complete.cases(sub$plz), ]

#correct leipzig and dresden missing 0s
sub$PLZ <- if(city = "lei"){paste0("0",sub$PLZ)}
?paste0

# loading maps with readORG
Shapes <- readOGR(dsn = ".", layer = "post_pl") # Load Kreise shapefile

#isolate cities
#cities_shapes <- Shapes[Shapes$PLZORT99 == "nchen",]
#plot(cities_shapes)

#create arab/german name indicator
cities_indicator_german <- subset(sub, sub$treat <= 2)
cities_indicator_arab <- subset(sub, sub$treat >= 3)

plz_names <- data.frame(Shapes)

cities_indicator_german <- merge(cities_indicator_german, plz_names, by="PLZ99", all = TRUE)
cities_indicator_german <- cities_indicator_german[complete.cases(cities_indicator_german$rr2), ]
cities_indicator_german <- cities_indicator_german[complete.cases(cities_indicator_german$PLZORT99), ]
cities_indicator_german <- aggregate(cities_indicator_german$rr2 ~ cities_indicator_german$PLZORT99, FUN = mean)

cities_indicator_arab <- merge(cities_indicator_arab, plz_names, by="PLZ99", all = TRUE)
cities_indicator_arab <- cities_indicator_arab[complete.cases(cities_indicator_arab$rr2), ]
cities_indicator_arab <- cities_indicator_arab[complete.cases(cities_indicator_arab$PLZORT99), ]
cities_indicator_arab <- aggregate(cities_indicator_arab$rr2 ~ cities_indicator_arab$PLZORT99, FUN = mean)

names(cities_indicator_german)[1] <-'PLZORT99'
names(cities_indicator_german)[2] <-'german.rr2'

names(cities_indicator_arab)[1] <-'PLZORT99'
names(cities_indicator_arab)[2] <-'arab.rr2'

#cities_plot
cities_plot_german <- aggregate(cities_indicator_german$german.rr2 ~ cities_indicator_german$PLZORT99, FUN = mean)
cities_plot_arab <- aggregate(cities_indicator_arab$arab.rr2 ~ cities_indicator_arab$PLZORT99, FUN = mean)

#create a polygon dataframe including shapes and rents for cities
cities_plot_german <- merge(Shapes, cities_indicator_german, by="PLZORT99", all = TRUE)
cities_plot_german <- cities_plot_german[complete.cases(cities_plot_german$german.rr2), ]
#cities_plot_german <- cities_plot_german[!duplicated(cities_plot_german),]

cities_plot_arab <- merge(Shapes, cities_indicator_arab, by="PLZORT99", all = TRUE)
cities_plot_arab <- cities_plot_arab[complete.cases(cities_plot_arab$arab.rr2), ]
#cities_plot_arab <- cities_plot_arab[!duplicated(cities_plot_arab),]

names(cities_plot_arab)[1] <-'PLZORT99'
names(cities_plot_arab)[2] <-'arab.rr2'

cities_plot <- merge(cities_plot_german, cities_plot_arab, by="PLZORT99")
View(cities_plot)
cities_plot$difference <- cities_plot$german.rr2 - cities_plot$arab.rr2


??aes
plot(cities_plot, 
     border = "darkgrey"

?geom+subplot2d
myplot  <- p+geom_subplot2d(aes(Shapes, 
                                  subplot = geom_bar(aes(x = cities_plot$PLZORT99, 
                                                       y = cities_plot$german.rr2, 
                                                       fill = cities_plot$german.rr2, 
                                                       width=1), 
                                                   position = "identity"), 
                            ref = NULL, 
                            data = Shapes)
??joinCountryData2Map
#myplot  <- p+geom_subplot2d(aes(long, 
                                lat, 
                                subplot = geom_bar(aes(x = categ, y = myvar, fill = categ, width=1), 
                                                   position = "identity")), 
                            ref = NULL, 
                            data = simdat)
print(myplot)



# Assign colors
colours <- brewer.pal(14, "Greens") # Pick color palette
?brewer.pal
#remove NAs for PLZs with no data 
cities_plot <- cities_plot[complete.cases(cities_plot$rr2), ]

#Add a colour profile for rent in cities
View(cities_plot)
cities_plot$indicator_cut <- cut(cities_plot$rr2, 
                        seq(min(cities_plot$rr2), 
                            max(cities_plot$rr2), 
                            100))
                        #right = FALSE)
#labels = c("200-220", "220-240", "240-260", "260-280")
labels = levels(cities_plot$rr2_cut)


# Plot the shapefiles colored
plot(cities_plot, 
     border = "darkgrey", 
     col = colours[cities_plot$rr2])

legend("right", 
       fill = colours, 
       legend = labels, 
       title = "Rent by PLZ:", 
       cex = 0.5)

####### Christopher Leaflet Map
cities_plot <- cities_plot[complete.cases(cities_plot$difference), ]

cities_label <- ~colorFactor(heat.colors(5), 
                             cities_plot$difference, 
                             na.color = "#808080")

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = cities_plot,
              color = ~colorFactor(heat.colors(5), 
                                   cities_plot$difference, 
                                   na.color = "#808080")
              (difference))




cities_plot$rr2.difference <- cities_plot$rr2_german_cut + cities_plot$rr2_arab_cut
cities_plot$rr2.difference <- cities_plot$german.rr2 - cities_plot$arab.rr2

cities_plot$rr2_german_cut <- as.character(cities_plot$german.rr2)
cities_plot$rr2_arab_cut <- as.character(cities_plot$arab.rr2)
cities_plot$rr2_cut_numeric <- as.character(cities_plot$arab.rr2)



cities_labels <- cities_labels[complete.cases(unique(cities_plot$rr2_cut_numeric))]
View(cities_labels)

cities_label <- ~colorFactor(heat.colors(5), 
                             cities_plot$rr2.difference, 
                             na.color = "#808080")

leaflet() %>% 
  addTiles() %>% 
  addPolygons(data = cities_plot,
              color = ~colorFactor(heat.colors(5), 
                                   cities_plot$rr2.difference, 
                                   na.color = "#808080")
              (cities_plot$rr2.difference)) #%>% #Which can also be colored if you want to 

  addLegend(position = 'bottomright',
            colors = ~colorFactor(heat.colors(5), 
                          cities_plot$rr2, 
                          na.color = "#808080"),
            labels = palette(),
            title = 'feedback')


#View(cities_plot)
#labels <- data.frame(c(1,2,3,4,5))
#View(cities_plot$)
?colorFactor
?heat.colors
?addLegend
?colorFactor
?topo.colors
?heat.colors
?leaflet
?addTiles
