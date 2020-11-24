#load Tidyverse packages
if (!require(tidyverse)) {
  install.packages("tidyverse")
  require(tidyverse)
}

#display sig figs for lat and long data
options(pillar.sigfig = 6)

#reading in Bestandteile_Fahrzeuge_OEM1_Typ11 and filtering for only those which have the K1DI1 engine
#this gives us a correlation between ID_Motor and ID_Fahrzeug

cars_with_engine <- read_delim("Data/Fahrzeug/Bestandteile_Fahrzeuge_OEM1_Typ11.csv", delim = ";") %>% 
  filter(str_detect(ID_Motor, "K1DI1") == TRUE) %>%
  select(ID_Motor, ID_Car = ID_Fahrzeug)

#problem with the ID_Motor: the first 3 numbers of the 3rd section of the ID should match the 2nd section
#assume that the manufacturer is correct and the factory number is not
#change factory number to match manufacturer by splitting, fixing, and recombining ID_Motor

cars_with_engine_clean <- cars_with_engine %>% 
  separate(ID_Motor, into = c("engine", "manufacturer", "factory", "iden"), sep = "-") %>%
  mutate(factory = paste(manufacturer, str_sub(factory, start = -1), sep = "")) %>%
  unite(ID_Motor, c("engine", "manufacturer", "factory", "iden"), sep = "-")

#remove extra data

rm(cars_with_engine)

#reading in Komponente_K1DI1, selecting relevant columns, remove the rows with no ID nor date
#filter for dates of interest
#this gives the production date of the engine

motor_date <- read_csv("Data/Komponente/Komponente_K1DI1.csv") %>% 
  select(ID_Motor = ID_Motor.x, Production_date_motor = Produktionsdatum.x) %>%
  filter(str_detect(ID_Motor, "NA") == FALSE) %>%
  filter(Production_date_motor >= "2010-09-21" & Production_date_motor <= "2012-09-04")

#same problem with ID_Motor: the first 3 numbers of the 3rd section of the ID should match the 2nd section
#assume that the manufacturer is correct and the factory number is not
#change factory number to match manufacturer by splitting, fixing, and recombining ID_Motor

motor_date_clean <- motor_date %>% 
  separate(ID_Motor, into = c("engine", "manufacturer", "factory", "iden"), sep = "-") %>%
  mutate(factory = paste(manufacturer, str_sub(factory, start = -1), sep = "")) %>%
  unite(ID_Motor, c("engine", "manufacturer", "factory", "iden"), sep = "-")

rm(motor_date)

#combining to correlate the production date of engine to the car ID

faulty <- left_join(motor_date_clean, cars_with_engine_clean, by = "ID_Motor")

#notice that there are 19,948 ID_Motor without ID_Car
#we conclude that these were installed into models other than the Type 11 and can be ignored for our analysis

faulty <- faulty %>% filter(!is.na(ID_Car))

rm(cars_with_engine_clean, motor_date_clean)

#reading in registrations, joining 

register <- read_csv2("Data/Zulassungen/Zulassungen_alle_Fahrzeuge.csv") %>%
  select(ID_Car = IDNummer, municipality = Gemeinden, reg_date = Zulassung)

faulty <- left_join(faulty, register, by = "ID_Car")

rm(register)

#reading in geodata 

plz_lat_long <- read_csv2("Data/Geodaten/Geodaten_Gemeinden_v1.2_2017-08-22_TrR.csv") %>%
  select(plz = Postleitzahl, municipality = Gemeinde, longitude = Laengengrad, latitude = Breitengrad)

#join geodata to affected vehicles, keeps only the relevant municipalities

faulty <- left_join(faulty, plz_lat_long, by = "municipality")

rm(plz_lat_long)

#define function degrees to radians for distance calculation

d_to_r <- function(d){
  (d * pi) / 180
}

#define distance to Berlin Reichstagsgebaeude in km
#use Haversine formula to account for curvature of the Earth

distance_to_Berlin <- function(lat, long){
  ber_lat = 52.518623 
  ber_long = 13.376198 #coordinates of Berlin
  radius = 6371 #radius of the Earth
  d_lat = d_to_r(lat - ber_lat)
  d_long = d_to_r(long - ber_long)
  h = sin(d_lat / 2) * sin(d_lat / 2) +
    cos(d_to_r(lat)) * cos(d_to_r(ber_lat)) *
    sin(d_long / 2) * sin(d_long / 2) #haversine
  d = 2 * radius * asin(sqrt(h))
  return(d)
}

#reordering, adding distance, dropping irrelevant columns

faulty <- faulty %>%
  mutate(municipality = str_to_title(municipality)) %>%
  select(ID_Car, municipality, plz, latitude, longitude)

#list each municipality, get coordinates, and calculate distance to Berlin

muni_dist <- faulty %>%
  select(municipality, latitude, longitude) %>%
  distinct() %>%
  mutate(distance_Berlin = distance_to_Berlin(latitude, longitude))

#count affected cars by municipality

cars_by_muni <- faulty %>%
  group_by(municipality) %>%
  summarise(count = n())

#combine for final dataframe to use for mapping

cars_by_muni <- cars_by_muni %>%
  left_join(muni_dist, by = "municipality")

#remove extra dataframes
rm(muni_dist, faulty)
