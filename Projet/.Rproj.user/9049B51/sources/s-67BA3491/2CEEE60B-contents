library(shiny)

library(dplyr)

library(leaflet)

library(DT)

library(data.table)

library("RColorBrewer")

library(shinyWidgets)

library(glmnet)

library(caret)

library(plotly)

setwd("~/Documents/M2/Cours_Agro_J1_Programmation_R/archive/projet")
testdata <- fread("./AB_NYC_2019.csv", header=T,stringsAsFactors = T)
# apres verif des prix sur airbnb correction
testdata[25434,10] <- 55
testdata[25795,10] <- 40
testdata[25796,10] <- 35
testdata[25797,10] <- 40

#pour ceux qui ne sont pas corrigibles suppression
testdata <- testdata[-c(23162,25635,25754,25779,26260,26842,26867),]

# creation de sous jeu de donnees par type de chambre
Apartment=testdata[ which(testdata$room_type =="Entire home/apt"), ]
Private_room=testdata[ which(testdata$room_type =="Private room"), ]
Shared_room=testdata[ which(testdata$room_type =="Shared room"), ]

# on selectionne les valeurs extremes pour la categorie appartement
apt_out= boxplot(Apartment$price, plot=F)$out
# on les enleve
outdf_apt=Apartment[Apartment$price %in% apt_out,]
# dim(outdf_apt) # 1871 donnees
no_out_apt=Apartment[-which(Apartment$price %in% apt_out),]

# extreme pour les 2 autres
shd_out=boxplot(Shared_room$price, plot=F)$out
pvt_out=boxplot(Private_room$price, plot=F)$out

outdf_shd=Shared_room[Shared_room$price %in% shd_out,]
outdf_pvt=Private_room[Private_room$price %in% pvt_out,]

# on les enleve
no_out_shd=Shared_room[-which(Shared_room$price %in% shd_out),]
no_out_pvt=Private_room[-which(Private_room$price %in% pvt_out),]

rbnb <- rbind(no_out_apt,no_out_pvt,no_out_shd)
# dim(rbnb)

# remplacer vides par null ou 0
rbnb$reviews_per_month[is.na(rbnb$reviews_per_month)] <- 0

# abandonner les colonnes ininteressantes (noms d'hote... )
rbnb <- rbnb[,-c(1:4,12:14)]

rbnb$neighbourhood_group <- as.factor(rbnb$neighbourhood_group)
rbnb$neighbourhood <- as.factor(rbnb$neighbourhood)
rbnb$room_type <- as.factor(rbnb$room_type)

# separation des variables en numerique/categoriel
cont_vars1 = names(rbnb)[sapply(rbnb, is.numeric)]
cat_vars1 = names(rbnb)[sapply(rbnb, is.factor)]

rbnb_cont = rbnb[,..cont_vars1]
rbnb_cat = rbnb[,..cat_vars1]

## transformation des variables categorielle en dummy variables

room <- caret::dummyVars(~ room_type, data = rbnb_cat)
dummy_room_type = data.frame(predict(room, rbnb_cat))

neighb <- caret::dummyVars(~ neighbourhood, data = rbnb_cat)
dummy_neighbourhood = data.frame(predict(neighb, rbnb_cat))

group <- caret::dummyVars(~ neighbourhood_group, data = rbnb_cat)
dummy_group = data.frame(predict(group, rbnb_cat))

# # on concatene les 2 tableaux
new_rbnb= cbind(rbnb_cont, dummy_room_type,
                dummy_group, dummy_neighbourhood)


## separation entre un jeu de donnees d'apprentissage et de test
n = nrow(new_rbnb)                   # Sample size
trainIndex <- createDataPartition(new_rbnb$price, p = .7,times = 1,list=FALSE)

new_rbnb_train <- new_rbnb[ trainIndex,]
x_train <- as.matrix(new_rbnb_train[,-c(1:3,6)])
y_train <- as.matrix(new_rbnb_train[,3])


## regression Lasso

## Decreasing sequence of log-lambda values
loglambda = seq(10,-10,length=100)

rbnb_cvlasso = cv.glmnet(x_train,y_train,type.measure="mse",lambda=exp(loglambda))
rbnb_lasso = glmnet(x_train,y_train,lambda=exp(loglambda))


# Define server logic required to draw a histogram
names_x_test <- c( "minimum_nights",                          "calculated_host_listings_count",
                   "room_type.Entire.home.apt",               "room_type.Private.room",
                   "room_type.Shared.room",                   "neighbourhood_group.Bronx",
                   "neighbourhood_group.Brooklyn",            "neighbourhood_group.Manhattan",
                   "neighbourhood_group.Queens",              "neighbourhood_group.Staten.Island",
                   "neighbourhood.Allerton",                  "neighbourhood.Arden.Heights",
                   "neighbourhood.Arrochar",                  "neighbourhood.Arverne",
                   "neighbourhood.Astoria",                   "neighbourhood.Bath.Beach",
                   "neighbourhood.Battery.Park.City",         "neighbourhood.Bay.Ridge",
                   "neighbourhood.Bay.Terrace",               "neighbourhood.Bay.Terrace..Staten.Island",
                   "neighbourhood.Baychester",                "neighbourhood.Bayside",
                   "neighbourhood.Bayswater",                 "neighbourhood.Bedford.Stuyvesant",
                   "neighbourhood.Belle.Harbor",              "neighbourhood.Bellerose",
                   "neighbourhood.Belmont",                   "neighbourhood.Bensonhurst",
                   "neighbourhood.Bergen.Beach",              "neighbourhood.Boerum.Hill",
                   "neighbourhood.Borough.Park",              "neighbourhood.Breezy.Point",
                   "neighbourhood.Briarwood",                 "neighbourhood.Brighton.Beach",
                   "neighbourhood.Bronxdale",                 "neighbourhood.Brooklyn.Heights",
                   "neighbourhood.Brownsville",               "neighbourhood.Bull.s.Head",
                   "neighbourhood.Bushwick",                  "neighbourhood.Cambria.Heights",
                   "neighbourhood.Canarsie",                  "neighbourhood.Carroll.Gardens",
                   "neighbourhood.Castle.Hill",               "neighbourhood.Castleton.Corners",
                   "neighbourhood.Chelsea",                   "neighbourhood.Chinatown",
                   "neighbourhood.City.Island",               "neighbourhood.Civic.Center",
                   "neighbourhood.Claremont.Village",         "neighbourhood.Clason.Point",
                   "neighbourhood.Clifton",                   "neighbourhood.Clinton.Hill",
                   "neighbourhood.Co.op.City",                "neighbourhood.Cobble.Hill",
                   "neighbourhood.College.Point",             "neighbourhood.Columbia.St",
                   "neighbourhood.Concord",                   "neighbourhood.Concourse",
                   "neighbourhood.Concourse.Village",         "neighbourhood.Coney.Island",
                   "neighbourhood.Corona",                    "neighbourhood.Crown.Heights",
                   "neighbourhood.Cypress.Hills",             "neighbourhood.DUMBO",
                   "neighbourhood.Ditmars.Steinway",          "neighbourhood.Dongan.Hills",
                   "neighbourhood.Douglaston",                "neighbourhood.Downtown.Brooklyn",
                   "neighbourhood.Dyker.Heights",             "neighbourhood.East.Elmhurst",
                   "neighbourhood.East.Flatbush",             "neighbourhood.East.Harlem",
                   "neighbourhood.East.Morrisania",           "neighbourhood.East.New.York",
                   "neighbourhood.East.Village",              "neighbourhood.Eastchester",
                   "neighbourhood.Edenwald",                  "neighbourhood.Edgemere",
                   "neighbourhood.Elmhurst",                  "neighbourhood.Eltingville",
                   "neighbourhood.Emerson.Hill",              "neighbourhood.Far.Rockaway",
                   "neighbourhood.Fieldston",                 "neighbourhood.Financial.District",
                   
                   "neighbourhood.Flatbush",                  "neighbourhood.Flatiron.District",
                   "neighbourhood.Flatlands",                 "neighbourhood.Flushing",
                   "neighbourhood.Fordham",                   "neighbourhood.Forest.Hills",
                   "neighbourhood.Fort.Greene",               "neighbourhood.Fort.Hamilton",
                   "neighbourhood.Fort.Wadsworth",            "neighbourhood.Fresh.Meadows",
                   "neighbourhood.Glendale",                  "neighbourhood.Gowanus",
                   "neighbourhood.Gramercy",                  "neighbourhood.Graniteville",
                   "neighbourhood.Grant.City",                "neighbourhood.Gravesend",
                   "neighbourhood.Great.Kills",               "neighbourhood.Greenpoint",
                   "neighbourhood.Greenwich.Village",         "neighbourhood.Grymes.Hill",
                   "neighbourhood.Harlem",                    "neighbourhood.Hell.s.Kitchen",
                   "neighbourhood.Highbridge",                "neighbourhood.Hollis",
                   "neighbourhood.Holliswood",                "neighbourhood.Howard.Beach",
                   "neighbourhood.Howland.Hook",              "neighbourhood.Huguenot",
                   "neighbourhood.Hunts.Point",               "neighbourhood.Inwood",
                   "neighbourhood.Jackson.Heights",           "neighbourhood.Jamaica",
                   "neighbourhood.Jamaica.Estates",           "neighbourhood.Jamaica.Hills",
                   "neighbourhood.Kensington",                "neighbourhood.Kew.Gardens",
                   "neighbourhood.Kew.Gardens.Hills",         "neighbourhood.Kingsbridge",
                   "neighbourhood.Kips.Bay",                  "neighbourhood.Laurelton",
                   "neighbourhood.Lighthouse.Hill",           "neighbourhood.Little.Italy",
                   "neighbourhood.Little.Neck",               "neighbourhood.Long.Island.City",
                   "neighbourhood.Longwood",                  "neighbourhood.Lower.East.Side",
                   "neighbourhood.Manhattan.Beach",           "neighbourhood.Marble.Hill",
                   "neighbourhood.Mariners.Harbor",           "neighbourhood.Maspeth",
                   "neighbourhood.Melrose",                   "neighbourhood.Middle.Village",
                   "neighbourhood.Midland.Beach",             "neighbourhood.Midtown",
                   "neighbourhood.Midwood",                   "neighbourhood.Mill.Basin",
                   "neighbourhood.Morningside.Heights",       "neighbourhood.Morris.Heights",
                   "neighbourhood.Morris.Park",               "neighbourhood.Morrisania",
                   "neighbourhood.Mott.Haven",                "neighbourhood.Mount.Eden",
                   "neighbourhood.Mount.Hope",                "neighbourhood.Murray.Hill",
                   "neighbourhood.Navy.Yard",                 "neighbourhood.Neponsit",
                   "neighbourhood.New.Brighton",              "neighbourhood.New.Dorp",
                   "neighbourhood.New.Dorp.Beach",            "neighbourhood.New.Springville",
                   "neighbourhood.NoHo",                      "neighbourhood.Nolita",
                   "neighbourhood.North.Riverdale",           "neighbourhood.Norwood",
                   "neighbourhood.Oakwood",                   "neighbourhood.Olinville",
                   "neighbourhood.Ozone.Park",                "neighbourhood.Park.Slope",
                   "neighbourhood.Parkchester",               "neighbourhood.Pelham.Bay",
                   "neighbourhood.Pelham.Gardens",            "neighbourhood.Port.Morris",
                   "neighbourhood.Port.Richmond",             "neighbourhood.Prince.s.Bay",
                   
                   "neighbourhood.Prospect.Heights",          "neighbourhood.Prospect.Lefferts.Gardens",
                   "neighbourhood.Queens.Village",            "neighbourhood.Randall.Manor",
                   "neighbourhood.Red.Hook",                  "neighbourhood.Rego.Park",
                   "neighbourhood.Richmond.Hill",             "neighbourhood.Richmondtown",
                   "neighbourhood.Ridgewood",                 "neighbourhood.Riverdale",
                   "neighbourhood.Rockaway.Beach",            "neighbourhood.Roosevelt.Island",
                   "neighbourhood.Rosebank",                  "neighbourhood.Rosedale",
                   "neighbourhood.Rossville",                 "neighbourhood.Schuylerville",
                   "neighbourhood.Sea.Gate",                  "neighbourhood.Sheepshead.Bay",
                   "neighbourhood.Shore.Acres",               "neighbourhood.Silver.Lake",
                   "neighbourhood.SoHo",                      "neighbourhood.Soundview",
                   "neighbourhood.South.Beach",               "neighbourhood.South.Ozone.Park",
                   "neighbourhood.South.Slope",               "neighbourhood.Springfield.Gardens",
                   "neighbourhood.Spuyten.Duyvil",            "neighbourhood.St..Albans",
                   "neighbourhood.St..George",                "neighbourhood.Stapleton",
                   "neighbourhood.Stuyvesant.Town",           "neighbourhood.Sunnyside",
                   "neighbourhood.Sunset.Park",               "neighbourhood.Theater.District",
                   "neighbourhood.Throgs.Neck",               "neighbourhood.Todt.Hill",
                   "neighbourhood.Tompkinsville",             "neighbourhood.Tottenville",
                   "neighbourhood.Tremont",                   "neighbourhood.Tribeca",
                   "neighbourhood.Two.Bridges",               "neighbourhood.Unionport",
                   "neighbourhood.University.Heights",        "neighbourhood.Upper.East.Side",
                   "neighbourhood.Upper.West.Side",           "neighbourhood.Van.Nest",
                   "neighbourhood.Vinegar.Hill",              "neighbourhood.Wakefield",
                   "neighbourhood.Washington.Heights",        "neighbourhood.West.Brighton",
                   "neighbourhood.West.Farms",                "neighbourhood.West.Village",
                   "neighbourhood.Westchester.Square",        "neighbourhood.Westerleigh",
                   "neighbourhood.Whitestone",                "neighbourhood.Williamsbridge",
                   "neighbourhood.Williamsburg",              "neighbourhood.Willowbrook",
                   "neighbourhood.Windsor.Terrace",           "neighbourhood.Woodhaven",
                   "neighbourhood.Woodlawn",                  "neighbourhood.Woodrow",
                   "neighbourhood.Woodside")


x_test1 <- matrix(data=0,ncol=231)
colnames(x_test1) <-  names_x_test
# create a color paletter for category type in the data file
pal <- colorFactor(pal = c("#330033", "#33FF00", "#FF6600", "#990000", "#33FFFF"), domain = c("Brooklyn", "Manhattan", "Bronx", "Queens","Staten Island"))

