shinyServer(function(input, output, session) {
    
    observeEvent(input$run, {
        
        x_test1[,c(paste("room_type.",input$room,sep=""))] <- 1
        x_test1[,c(paste("neighbourhood.",input$quartier1,sep=""))] <- 1
        x_test1[,c(paste("neighbourhood_group.",input$arr,sep=""))] <- 1
        x_test1[,c("minimum_nights")] <- input$night
        x_test1[,c("calculated_host_listings_count")] <- input$appart
        
        cvpredictions = round(predict(rbnb_lasso,newx=x_test1)[,which.min(rbnb_cvlasso$cvm)],2)
        
        output$selected_var <- renderText({
            paste("Votre bien est estime a",cvpredictions,"dollars par nuit.")
        })
        
    })
    
# MAP
    filtered_data <- reactive({
        quartier = input$quartier
        logement = input$logement
        price_min = input$price[1]
        price_max = input$price[2]
        rbnb %>%
            select(neighbourhood_group,room_type,longitude,latitude,price) %>%
            filter(neighbourhood_group == quartier & room_type == logement & price >= price_min & price <= price_max)
    

        })
    
    pal <- colorFactor("viridis", rbnb$room_type)
    
    output$map <- renderLeaflet({
        leaflet(rbnb)%>% 
            setView(lng = -73.9059, lat = 40.7128, zoom = 11) %>% 
            addTiles() %>% 
            addCircleMarkers(lng = filtered_data()$longitude, 
                             lat = filtered_data()$latitude,
                             radius = 3,
                             color = ~pal(filtered_data()$room_type),
                             popup = ~rbnb$name,
                             stroke = FALSE, fillOpacity = 0.8) %>%
            addLegend(position = "topright",
                      pal=pal, 
                      values=rbnb$room_type,
                      opacity=1,
                      title = "Type de logement",
                      group = "circles")
    })
    
    filtered_dataPlot <- rbnb %>%
        select(neighbourhood_group, room_type, price)
    
    output$plot <- renderPlot({
        ggplot(filtered_dataPlot %>% filter(neighbourhood_group == input$quartier),aes(x = price,fill = room_type)) + geom_density() +  coord_cartesian(xlim = c(-0, 500)) + labs(title = "Distribution des prix par type de logement")
    })
    
   observe (if (input$quartier == "Bronx"){ 
                print(output$bronx<-renderText("Vous voulez etre berces par le rap et le rnb, choisissez le Bronx."))}
   )
        
    observe( if (input$quartier == "Brooklyn"){
                print(output$brooklyn<-renderText("Prenez le fameux pont de Brooklyn et plongez vous dans l'ambiance branchee avec une magnifique vue sur la skyline de Manhattan "))}
            )
    observe(  if (input$quartier == "Manhattan"){
                print(output$manhattan<-renderText("Des buildings qui chatouillent le ciel aux larges pelouses de Central Park, des kilometres de musees aux enseignes dedies au capitalisme : voila ce qui vous attend a Manhattan !"))}         
            )
    observe (   if (input$quartier == "Queens"){
               print(output$queens<-renderText("Vous cherchez la proximite avec l'un des plus grands aeroports du monde : le queens est fait pour vous.  Cosmopolite, avant-gardiste, riche culturellement et gastronomiquement, ce quartier est tres interessant !"))}
    )
    observe (if (input$quartier == "Staten Island"){
               print(output$staten<-renderText("Souvent delaisse des touristes, ce quartier offre une experience depaysante avec son fort militaire et son jardin chinois"))}
       )
    
    
    
    output$quartier <- renderUI({
        if (input$arr=="Bronx"){
            selectInput('quartier1', 'Quartier', choices=c( "Allerton","Bath.Beach","Baychester","Bedford.Stuyvesant","Belmont",
                                                           "Bronxdale","Bushwick","Castle.Hill","City.Island","Claremont.Village","Clason.Point",
                                                           "Concourse","Concourse.Village","Co.op.City","East.Morrisania", "Eastchester","Edenwald","Fieldston","Fordham",
                                                           "Highbridge","Hunts.Point","Kingsbridge","Marble.Hill",
                                                           "Longwood","Melrose","Morris.Heights","Morris.Park","Morrisania","Mott.Haven","Mount.Eden",
                                                           "North.Riverdale","Norwood","Olinville","Parkchester","Pelham.Bay","Pelham.Gardens",
                                                           "Port.Morris","Riverdale","Soundview","Spuyten.Duyvil",
                                                           "Schuylerville","Throgs.Neck","Tremont","Unionport","University.Heights","Van.Nest",
                                                           "Wakefield","West.Farms","Westchester.Square","Williamsbridge","Woodlawn"))
        }
        
        else if (input$arr=="Brooklyn"){
            selectInput('quartier1', 'Quartier', choices =  c("Brighton.Beach",
                                                             "Boerum.Hill", "Bay.Ridge","Bensonhurst","Bergen.Beach","Borough.Park","Brooklyn.Heights","Brownsville","Canarsie",
                                                             "Clinton.Hill","Carroll.Gardens","Cobble.Hill", "Columbia.St","Cypress.Hills", "Coney.Island","Crown.Heights","Downtown.Brooklyn","Dyker.Heights","DUMBO","East.Flatbush","East.New.York",
                                                             "Flatbush","Flatlands","Fort.Greene","Fort.Hamilton","Gravesend","Greenpoint","Gowanus","Kensington",
                                                             "Midtown","Midwood","Mill.Basin","Navy.Yard","Park.Slope","Prospect.Lefferts.Gardens","Red.Hook",
                                                             "Sea.Gate","South.Slope",
                                                             "Sheepshead.Bay","Sunset.Park","Prospect.Heights","Vinegar.Hill","West.Brighton","Williamsburg",
                                                             "Windsor.Terrace"))
            
        }
        
        else if (input$arr=="Manhattan"){
            selectInput('quartier1', 'Quartier',choices=c("Battery.Park.City",
                                                         "Chelsea",
                                                         "Chinatown",
                                                         "Civic.Center",
                                                         "East.Harlem",
                                                         "East.Village",
                                                         "Financial.District",
                                                         "Flatiron.District",
                                                         "Gramercy",
                                                         "Greenwich.Village", 
                                                         "Harlem",
                                                         "Hell.s.Kitchen",
                                                         "Inwood",
                                                         "Kips.Bay",
                                                         "Little.Italy",
                                                         "Lower.East.Side",
                                                         "Manhattan.Beach", 
                                                         "Morningside.Heights",
                                                         "Murray.Hill",
                                                         "NoHo",
                                                         "Nolita",
                                                         "Roosevelt.Island",
                                                         "SoHo",
                                                         "Stuyvesant.Town",
                                                         "Theater.District",
                                                         "Tribeca",
                                                         "Two.Bridges",
                                                         "Upper.East.Side",
                                                         "Upper.West.Side", 
                                                         "Washington.Heights",
                                                         "West.Village"))
        }
        
        else if (input$arr=="Queens"){
            selectInput('quartier1', 'Quartier',choices=c( 
                "Arverne",
                "Astoria",
                "Bayswater",
                "Belle.Harbor",
                "Breezy.Point",
                "Bayside",
                "Bellerose",
                "Briarwood",
                "Cambria.Heights", 
                "College.Point",
                "Corona",
                "Ditmars.Steinway",
                "Douglaston",
                "East.Elmhurst",
                "Edgemere",                              
                "Elmhurst",
                "Far.Rockaway",
                "Flushing",
                "Fresh.Meadows",
                "Forest.Hills",
                "Glendale",
                "Hollis",
                "Holliswood",
                "Howard.Beach",
                "Jackson.Heights",
                "Jamaica",
                "Jamaica.Estates",
                "Jamaica.Hills",
                "Kew.Gardens",
                "Laurelton",
                "Little.Neck",
                "Kew.Gardens.Hills",
                "Long.Island.City",
                "Maspeth",
                "Middle.Village",
                "Neponsit", 
                "Ozone.Park",
                "Queens.Village",
                "Rego.Park",
                "Richmond.Hill",
                "Ridgewood",
                "Rockaway.Beach",
                "Rosedale",
                "South.Ozone.Park",
                "Springfield.Gardens",                                
                "St..Albans",
                "Whitestone",
                "Woodhaven",                                
                "Woodside"
            ) )
        }
        
        else if (input$arr=="Staten.Island"){
            selectInput('quartier1', 'Quartier',choices= c("Arden.Heights","Arrochar","Bay.Terrace","Bay.Terrace..Staten.Island",
                                                          "Bull.s.Head","Castleton.Corners","Clifton","Concord","Dongan.Hills",
                                                          "Eltingville","Emerson.Hill","Fort.Wadsworth","Graniteville",
                                                          "Grant.City","Great.Kills","Grymes.Hill","Huguenot","Lighthouse.Hill",
                                                          "Mariners.Harbor","Midland.Beach","New.Brighton","New.Dorp",
                                                          "New.Dorp.Beach","New.Springville","Oakwood","Port.Richmond",
                                                          "Prince.s.Bay","Randall.Manor","Richmondtown","Rosebank",
                                                          "Rossville","Shore.Acres", "Silver.Lake","South.Beach",
                                                          "Stapleton","Sunnyside","Todt.Hill","Tompkinsville",
                                                          "Tottenville","Willowbrook","St..George","Westerleigh", 
                                                          "Woodrow") )
        }
    })
   
    
})

