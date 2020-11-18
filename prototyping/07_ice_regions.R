library(tidyverse)
library(tigris)
library(sf)
library(leaflet)

plot_tx <- TRUE

state_names <- states(class = "sf") %>%
    as_tibble() %>%
    select(STATEFP, STATENAME = NAME)

county_shape <- counties(class = "sf") %>%
    select(STATEFP, GEOID, NAME, geometry) %>%
    left_join(state_names, by = "STATEFP")

# http://www.apsanlaw.com/law-263.a-complete-list-of-ice-field-offices.html
state_offices <- bind_rows(
    tibble(
        office = "Phoenix",
        STATENAME = "Arizona"),
    tibble(
        office = "Atlanta",
        STATENAME = c("Georgia", "North Carolina", "South Carolina")),
    tibble(
        office = "Newark",
        STATENAME = "New Jersey"),
    tibble(
        office = "Baltimore",
        STATENAME = "Maryland"),
    tibble(
        office = "Denver",
        STATENAME = c("Colorado", "Wyoming")),
    tibble(
        office = "New Orleans",
        STATENAME = c(
            "Alabama", "Arkansas", "Louisiana", "Mississippi", "Tennessee")),
    tibble(
        office = "Boston",
        STATENAME = c(
            "Connecticut", "Maine", "Massachusetts", "New Hampshire",
            "Rhode Island", "Vermont")),
    tibble(
        office = "Chicago",
        STATENAME = c(
            "Illinois", "Indiana", "Wisconsin", "Missouri", "Kentucky",
            "Kansas")),
    tibble(
        office = "Philadelphia",
        STATENAME = c("Delaware", "Pennsylvania", "West Virginia")),
    tibble(
        office = "Dallas",
        STATENAME = c("Oklahoma")),
    tibble(
        office = "Salt Lake City",
        STATENAME = c("Utah", "Idaho", "Montana", "Nevada")),
    tibble(
        office = "Detroit",
        STATENAME = c("Michigan", "Ohio")),
    tibble(
        office = "El Paso",
        STATENAME = c("New Mexico")),
    tibble(
        office = "San Francisco",
        STATENAME = c("Hawaii", "Guam")),
    tibble(
        office = "Seattle",
        STATENAME = c("Alaska", "Oregon", "Washington")),
    tibble(
        office = "St Paul",
        STATENAME = c(
            "Iowa", "Minnesota", "Nebraska", "North Dakota", "South Dakota")),
    tibble(
        office = "Miami",
        STATENAME = c(
            "Florida", "United States Virgin Islands", "Puerto Rico")),
    tibble(
        office = "Washington",
        STATENAME = c("District of Columbia", "Virginia")),
    
)

county_offices <- bind_rows(
    tibble(
        office = "Los Angeles",
        STATENAME = "California",
        NAME = c(
            "Los Angeles", "Orange", "Riverside", "San Bernardino",
            "Ventura", "Santa Barbara", "San Luis Obispo")
    ),
    tibble(
        office = "San Diego",
        STATENAME = "California",
        NAME = c("San Diego", "Imperial")
    ),
    tibble(
        office = "New York",
        STATENAME = "New York",
        NAME = c(
            "Duchess", "Nassau", "Putnam", "Suffolk", "Sullivan", "Orange",
            "Rockland", "Ulster", "Westchester", "Richmond", "Queens",
            "New York", "Kings", "Bronx")
    ),
    tibble(
        office = "San Francisco",
        STATENAME = "California",
        NAME = county_shape %>%
            as_tibble() %>%
            filter(STATENAME == "California") %>%
            pull(NAME) %>%
            setdiff(
                c(
                    "San Diego", "Santa Barbara", "Imperial", "San Bernardino",
                    "Los Angeles", "Ventura", "Orange", "San Luis Obispo",
                    "Riverside"
                    ))
    ),
    tibble(
        office = "Buffalo",
        STATENAME = "New York",
        NAME = county_shape %>%
            as_tibble() %>%
            filter(STATENAME == "New York") %>%
            pull(NAME) %>%
            setdiff(
                c(
                    "Duchess", "Nassau", "Putnam", "Suffolk", "Sullivan",
                    "Orange", "Rockland", "Ulster", "Westchester", "Richmond",
                    "Queens", "New York", "Kings", "Bronx"
                ))
    )
)

# https://www.ice.gov/doclib/about/offices/ero/pdf/eroFieldOffices.pdf
county_offices_tx_short <- bind_rows(
    tibble(
        office = "El Paso",
        STATENAME = "Texas",
        NAME = c(
            "Terrell", "Pecos", "Crane", "Ector", "Andrews", "Winkler", "Ward",
            "Loving", "Reeves", "Culberson", "Jeff Davis", "Brewster",
            "Presidio", "Hudspeth", "El Paso", "Upton", "Midland", "Martin")),
    tibble(
        office = "Houston",
        STATENAME = "Texas",
        NAME = c(
            "Live Oak", "Jim Wells", "Brooks", "Kenedy", "Kleberg", "Nueces",
            "San Patricio", "Bee", "Refugio", "Goliad", "Victoria", "Aransas",
            "Jackson", "Calhoun", "DeWitt", "Lavaca", "Lee", "Fayette", "Polk",
            "Walker", "Shelby", "Nacogdoches", "Houston", "Angelina", "Trinity",
            "San Augustine", "Milam", "Robertson", "Leon", "Madison", "Brazos",
            "Burleson", "Washington", "Austin", "Colorado", "Wharton", "Newton",
            "Matagorda", "Sabine", "Jasper", "Tyler", "Orange", "Hardin",
            "Jefferson", "Liberty", "Montgomery", "Grimes", "Waller", "Harris",
            "San Jacinto", "Chambers", "Galveston", "Brazoria", "Fort Bend"
            )
    ),
    tibble(
        office = "San Antonio",
        STATENAME = "Texas",
        NAME = c(
            "Val Verde", "Edwards", "Cameron", "Hidalgo", "Willacy", "Starr",
            "Zapata", "Jim Hogg", "Webb", "Duval", "Kinney", "Maverick",
            "Dimmit", "La Salle", "McMullen", "Karnes", "Gonzales", "Atascosa",
            "Frio", "Zavala", "Wilson", "Bexar", "Medina", "Uvalde", "Real",
            "Bandera", "Guadalupe", "Caldwell", "Bastrop", "Williamson", "Hays",
            "Travis", "Comal", "McLennan", "Bell", "Falls", "McCulloch",
            "Mason", "Kimble", "San Saba", "Llano", "Kerr", "Gillespie", "Hill",
            "Kendall", "Blanco", "Somervell", "Bosque", "Coryell", "Hamilton",
            "Burnet", "Lampasas", "Limestone", "Freestone"
        )))


county_offices_tx <- bind_rows(
    county_offices_tx_short,
    tibble(
        office = "Dallas",
        STATENAME = "Texas",
        NAME = county_shape %>%
            as_tibble() %>%
            filter(STATENAME == "Texas") %>%
            pull(NAME) %>%
            setdiff(county_offices_tx_short$NAME))
)

if(plot_tx){
    sub_us <- county_shape %>%
        filter(STATENAME == "Texas") %>%
        left_join(county_offices_tx, by = c("NAME", "STATENAME")) %>%
        st_transform("+proj=longlat +datum=WGS84")
    
    pal <- colorFactor(
        palette="Spectral",
        domain=c("Dallas", "El Paso", "Houston", "San Antonio"))
    
    map1<-leaflet() %>%
        addProviderTiles("OpenStreetMap.Mapnik") %>%
        addPolygons(
            data=sub_us, fillColor=~pal(office), popup = ~NAME,
            fillOpacity=0.5, weight=.5, smoothFactor=0.2, stroke = TRUE,
            color = "black") %>%
        addLegend(
            "bottomright", pal=pal, values=sub_us$office, title="", opacity=1)
    map1
}

county_office_sf <- county_shape %>%
    left_join(state_offices, by = "STATENAME") %>%
    left_join(
        bind_rows(county_offices, county_offices_tx) %>%
            rename(office2 = office),
        by = c("STATENAME", "NAME")) %>%
    mutate(office = ifelse(is.na(office), office2, office)) %>%
    select(-office2) %>%
    filter(!is.na(office))

no_plot <- c(
    "Hawaii", "Alaska", "Guam", "United States Virgin Islands", "Puerto Rico")

county_office_sf %>%
    filter(!(STATENAME %in% no_plot)) %>%
    ggplot(aes(fill = office)) +
    geom_sf() +
    theme_void()

no_collapse <- c(
    "Guam", "United States Virgin Islands", "Puerto Rico")

office_sf <- do.call(sf:::rbind.sf, lapply(
    unique(county_office_sf$office), function(o){
        county_office_sf %>%
            filter(office == o & !(STATENAME %in% no_collapse)) %>%
            st_union() %>%
            {st_sf(office = o, geometry = .)}
}))

office_sf %>%
    ggplot(aes(fill = office)) +
    geom_sf() +
    theme_void()

# st_write(
#     county_office_sf, dsn = "county_office.shp",
#     layer = "county_office.shp", driver = "ESRI Shapefile")
# 
# st_write(
#     office_sf, dsn = "office.shp",
#     layer = "office.shp", driver = "ESRI Shapefile")

