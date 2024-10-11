#### upload packages ####
source('R-script/upload_packages.R')
#### upload functions ####
source('R-script/functions.R')

#### upload original data sets ####

prof <- data.table::fread(file = "profiles-anon.txt",na.strings = "*")
links <- data.table::fread(file = "relations-anon.txt")

#### clean location strings in the data set ####
recode_profile_output = recode_profile(prof)

#### convert coordinates into text strings ####

data_recoded = read_coordinates(recode_profile_output)


# drop unneccesary characters from coordintes strings

data_recoded$birth_coord = sub(":.*", "", data_recoded$birth_coord_country)
data_recoded$death_coord = sub(":.*", "", data_recoded$death_coord_country)
data_recoded$burial_coord = sub(":.*", "", data_recoded$burial_coord_country)
data_recoded$baptism_coord = sub(":.*", "", data_recoded$baptism_coord_country)
data_recoded[data_recoded$gender=="unknown"] = NA

# parents data set

parents = parents_col(links,data_var_red)

save(parents,file='Cleaned_Datasets/parents.RData')




#### Assign final birth and death locations to the top 20 countries with the most vital events ####

#### UK ####

country_codes_uk = c("uk","en","eng","scot","GBR","xuk",
                     "xeng","xen","x-en","x-uk","x-eng",
                     "xengland","xunitedkingdom",
                     "x-wales",
                     "x-england","x-scotland",
                     "english","england",
                     "unitedkingdom","greatbritain","britain",
                     "united kingdom", "great britain",
                     "welsh","scotland")

regex_codes_uk = c("xengland","xunitedkingdom",
                   "x-england","x-scotland",
                   "english","england","wales",
                   "unitedkingdom","greatbritain","britain",
                   "united kingdom", "great britain",
                   "welsh","scotland","\bunited.kingdom\b|bukb|\bbritain\b|\bbritish\b|\bengland\b|\bscottland\b|\bwales\b|\bscottish\b|\bwelsh\b|\bbriton\b|\buk\b")



data_uk = filter_country_profiles(data_recoded,links,
                                  country_codes = country_codes_uk,
                                  regex_codes_uk,
                                  country_name ="UK")

profiles_uk = data_uk %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "UK",NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "UK",NA))

profiles_uk_formatted = format_profiles(profiles_uk,links)


#### USA ####

country_codes_usa = c("us","usa","unitedstates",
                      "united states",
                      "xusa","xus","xunitedstates",
                      "x-united states of america",
                      "x-unitedstatesofamerica",
                      "united states of america",
                      "america","republic of america",
                      "massachusetts","connecticut",
                      "illinois","new york","north carolina",
                      "south carolina","new hampshire","kentucky",
                      "rhodeisland","colonialamerica","provinceofvirginia",
                      "texas","ohio","newengland","california","nebraska")

regex_codes_usa = c("unitedstates",
                    "united states",
                    "xunitedstates",
                    "x-united states of america",
                    "x-unitedstatesofamerica",
                    "united states of america",
                    "america","republic of america",
                    "massachusetts","connecticut",
                    "illinois","new york","north carolina",
                    "south carolina","new hampshire","kentucky",
                    "rhodeisland","colonialamerica","provinceofvirginia",
                    "texas","ohio","newengland","california","nebraska","\bunited.states\b|\busa\b")





data_usa = filter_country_profiles(data_recoded,links,
                                   country_codes = country_codes_usa,
                                   regex_codes_usa,
                                   country_name ="USA")

profiles_usa = data_usa %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "USA",NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "USA",NA))

profiles_usa_formatted = format_profiles(profiles_usa,links)


#### SWEDEN ####

country_codes_sweden <- c("SE", "SWEDEN", "SVERIGE", "SCHWEDEN"
                          , "SUECIA", 'RUOTSI')


regex_codes_sweden <- c("SWEDEN", "SVERIGE", "SCHWEDEN"
                        , "SUECIA", 'RUOTSI',"\bsweden\b|\bswede\b|\bswedish\b")




data_sweden = filter_country_profiles(data_recoded,links,
                                      country_codes = country_codes_sweden,
                                      regex_codes_sweden,
                                      country_name ="SWEDEN")

profiles_sweden = data_sweden %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "SWEDEN",NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "SWEDEN",NA))


profiles_sweden_formatted = format_profiles(profiles_sweden,links)



#### NORWAY ####

country_codes_norway <- c("NO", "NORWAY", "NORWEGEN", "NORGE"
                          , "NORUEGA", "NORJA")

regex_codes_norway <- c( "NORWAY", "NORWEGEN", "NORGE"
                         , "NORUEGA", "NORJA","\bnorway\b|\bnordmann\b|\bnorwegian\b")


data_norway = filter_country_profiles(data_recoded,links,
                                      country_codes = country_codes_norway,
                                      regex_codes_norway,
                                      country_name ="NORWAY")

profiles_norway = data_norway %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "NORWAY",NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "NORWAY",NA))

profiles_norway_formatted = format_profiles(profiles_norway,links)



#### FINLAND ####

country_codes_finland <- c("FI", "FINLAND", "SUOMI", "SOOMLANE"
                           , "FINLANDIA", "FINLANDE")

regex_codes_finland <- c("FINLAND", "SUOMI", "SOOMLANE"
                         , "FINLANDIA", "FINLANDE","\bfinland\b|\bfinn\b|\bfinnic\b|\bfinnish\b")



data_finland = filter_country_profiles(data_recoded,links,
                                       country_codes = country_codes_finland,
                                       regex_codes_finland,
                                       country_name ="FINLAND")

profiles_finland = data_finland %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "FINLAND",NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "FINLAND",NA))

profiles_finland_formatted = format_profiles(profiles_finland,links)


#### DENMARK ####


country_codes_denmark <- c("DK", "DENMARK", "DANMARK", "DANEMARK"
                           , "DINAMARCA", "DNK")


regex_codes_denmark <- c( "DENMARK", "DANMARK", "DANEMARK"
                          , "DINAMARCA","\bdenmark\b|\bdane\b|\bdanish\b|\bdansker\b")


data_denmark = filter_country_profiles(data_recoded,links,
                                       country_codes = country_codes_denmark,
                                       regex_codes_denmark,
                                       country_name ="DENMARK")

profiles_denmark = data_denmark %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "DENMARK",NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "DENMARK",NA))

profiles_denmark_formatted = format_profiles(profiles_denmark,links)



#### FRANCE #####

country_codes_france <- c("FRA", "FRANCE", "FRANCIA", "FRANKRIKE"
                          , "FRANKREICH")

regex_codes_france <- c("\bfrance\b|\bfrench\b|\bfrenchman\b|\bfrenchwoman\b", "FRANCE", "FRANCIA", "FRANKRIKE"
                        , "FRANKREICH")


data_france = filter_country_profiles(data_recoded,links,
                                      country_codes = country_codes_france,
                                      regex_codes_france,
                                      country_name ="FRANCE")

profiles_france = data_france %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "FRANCE",NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "FRANCE",NA))

profiles_france_formatted = format_profiles(profiles_france,links)

#### NETHERLANDS ####

country_codes_netherlands = c("NL", "Netherlands","Holland")


regex_codes_netherlands = c("\bnetherlands\b|\bdutch\b|\bhollander\b|\bnetherlander\b|\bnetherlandic\b","Holland", "Nederland","Netherlands")



data_netherlands = filter_country_profiles(data_recoded,links,
                                           country_codes = country_codes_netherlands,
                                           regex_codes_netherlands,
                                           country_name ="NETHERLANDS")

profiles_netherlands = data_netherlands %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "NETHERLANDS",NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "NETHERLANDS",NA))

profiles_netherlands_formatted = format_profiles(profiles_netherlands,links)

#### GERMANY ####


country_codes_germany <- c(
  "DE", "x-East-Germany", "x-West-Germany", "X-East-Prussia", "X-Prussia"
  , "Imperial Germany", "Second Reich", "German Empire"
  
) 


regex_codes_germany <-c("\bgermany\b|\bgerman\b" ,"Germany", "Deutschland", "Prussia",  "Preussen", "Weimar"
                        , "Silesia", "Posen", "Pomerania", "Brandenburg", "Saxony"
                        , "Thuringian", "Bavaria", "Schlesweig", "Holstein", "Wuerttemberg"
                        , "Baden", "Hesse")



data_germany = filter_country_profiles(data_recoded,links,
                                       country_codes = country_codes_germany,
                                       regex_codes_germany,
                                       country_name ="GERMANY")

profiles_germany = data_germany %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "GERMANY",NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "GERMANY",NA))

profiles_germany_formatted = format_profiles(profiles_germany,links)


#### CANADA ####

country_codes_canada <- c("CA","CANADA", "KANADA")

regex_codes_canada <- c("\bcanada\b|\bcanadian\b|\bcanadien\b|\bcanadienne\b|\bcanuck\b","CANADA", "KANADA")





data_canada = filter_country_profiles(data_recoded,links,
                                      country_codes = country_codes_canada,
                                      regex_codes_canada,
                                      country_name ="CANADA")

profiles_canada = data_canada %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "CANADA",NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "CANADA",NA))

profiles_canada_formatted = format_profiles(profiles_canada,links)


#### ESTONIA ####

country_codes_estonia <- c("EE", "ESTONIA", "EESTI")

regex_codes_estonia <- c("\\bestonia\\b|\\bestonian\\b|\\bestonie\\b|\\besti\\b", "ESTONIA", "EESTI")




data_estonia = filter_country_profiles(data_recoded,links,
                                      country_codes = country_codes_estonia,
                                      regex_codes_estonia,
                                      country_name ="ESTONIA")

profiles_estonia = data_estonia %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "ESTONIA",NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "ESTONIA",NA))

profiles_estonia_formatted = format_profiles(profiles_estonia,links)



#### AUSTRALIA ####

country_codes_australia <- c("AU", "AUS", "AUSTRALIA",'AUSTRALIA:TASMANIA', 
  'NEW SOUTH WALES', 
  'AUST', 
  'WESTERN AUSTRALIA', 
  'AUSTRAILIA', 
  'AUSTRALIS', 
  'NSW', 
  'PORT MELBOURNE', 
  'PORTSEA', 
  'SYDNEY NSW', 
  'TASMANIA'
)


regex_codes_australia <- c("\\baustralia\\b|\\baustralian\\b|\\baussie\\b", "AUSTRALIA", "AUS")


data_australia = filter_country_profiles(data_recoded,links,
                                       country_codes = country_codes_australia,
                                       regex_codes_australia,
                                       country_name ="AUSTRALIA")

profiles_australia = data_australia %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "AUSTRALIA",NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "AUSTRALIA",NA))

profiles_australia_formatted = format_profiles(profiles_australia,links)












#### SOUTH AFRICA ####

country_codes_south_africa <- c('ZA','SOUTH AFRICA','SUID AFRIKA','CAPE OF GOOD HOPE', 
  'CAPE COLONY', 'S AFRICA', 'CAPE COLONY (SOUTH AFRICA)', 'REP SOUTH AFRICA', 'SOUTH AFRICA', 
  'SOUTH AFICA', 'SOUTH AFRICA.', 'UNION OF SOUTH AFRICA')

regex_codes_south_africa <- c("\\bsouth africa\\b|\\bsouth african\\b|\\bsuid afrika\\b|\\brsa\\b|\\bs africa\\b", "SOUTH AFRICA", "RSA")

profiles_south_africa = data_south_africa %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "SOUTH AFRICA",NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "SOUTH AFRICA",NA))

profiles_south_africa_formatted = format_profiles(profiles_south_africa,links)


#### SWITZERLAND ####

country_codes_switzerland <- c('CH', 'SWITZERLAND', 'SWISS CONFEDERATION', 'SCHWEIZ', 
                               'SUISSE', 'SVIZZERA', 'HELVETIA', 'CONFEDERATIO HELVETICA', 'SWISS', 'CH.', 'CONFOEDERATIO HELVETICA')

regex_codes_switzerland <- c("\\bswitzerland\\b|\\bswiss\\b|\\bch\\b", "SWITZERLAND", "CH")


profiles_switzerland = data_switzerland %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "SWITZERLAND",NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "SWITZERLAND",NA))

profiles_switzerland_formatted = format_profiles(profiles_switzerland,links)


#### INDIA ####

country_codes_india <- c('IN', 'INDIA', 'BHARAT', 'HINDUSTAN', 
                         'REPUBLIC OF INDIA', 'INDIAN', 'BHARATIYA')

regex_codes_india <- c("\\bindia\\b|\\bindian\\b|\\bbharat\\b|\\bhindustan\\b", "INDIA", "IN")

profiles_india = data_india %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "INDIA", NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "INDIA", NA))

profiles_india_formatted = format_profiles(profiles_india,links)


#### BELGIUM ####

country_codes_belgium <- c('BE', 'BELGIUM', 'KINGDOM OF BELGIUM', 
                           'BELGIAN', 'BELGIE', 'BELGISCH', 'BELGIO', 
                           'KONINKRIJK BELGIE', 'BELGIAN')


regex_codes_belgium <- c("\\bbelgium\\b|\\bbelgian\\b|\\bbelgie\\b|\\bbelgisch\\b|\\bbelgio\\b", "BELGIUM", "BE")

profiles_belgium = data_belgium %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "BELGIUM", NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "BELGIUM", NA))

profiles_belgium_formatted = format_profiles(profiles_belgium,links)

#### RUSSIA ####

country_codes_russia <- c('RU', 'RUSSIA', 'RUSSIAN FEDERATION', 
                          'ROSSIYA', 'RUS', 'RUSSIAN', 'RU.', 
                          'ROSSIYSKAYA FEDERATSIYA', 'RUSSIAN SFSR')


regex_codes_russia <- c("\\brussia\\b|\\brussian\\b|\\brossiya\\b|\\bru\\b", "RUSSIA", "RU")

profiles_russia = data_russia %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "RUSSIA", NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "RUSSIA", NA))


profiles_russia_formatted = format_profiles(profiles_russia,links)

#### POLAND ####

country_codes_poland <- c('PL', 'POLAND', 'REPUBLIC OF POLAND', 
                          'POLSKA', 'POLISH', 'POLSKI', 
                          'POLAND.', 'RZECZPOSPOLITA POLSKA')


regex_codes_poland <- c("\\bpoland\\b|\\bpolska\\b|\\bpolish\\b|\\bpl\\b", "POLAND", "PL")



profiles_poland = data_poland %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "POLAND", NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "POLAND", NA))

profiles_poland_formatted = format_profiles(profiles_poland,links)


#### SPAIN ####

country_codes_spain <- c('ES', 'SPAIN', 'KINGDOM OF SPAIN', 
                         'ESPANA', 'SPANISH', 'HISPANIA', 
                         'SPAIN.', 'REINO DE ESPAÑA')

regex_codes_spain <- c("\\bspain\\b|\\bespana\\b|\\bspanish\\b|\\bes\\b", "SPAIN", "ES")



profiles_spain = data_spain %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "SPAIN", NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "SPAIN", NA))


profiles_spain_formatted = format_profiles(profiles_spain,links)


#### IRELAND ####
country_codes_ireland <- c('IE', 'IRELAND', 'IRELAND', 'REPUBLIC OF IRELAND', 
                           'EIRE', 'BYDONEY, TYRONE, IRELAND', 
                           'IRELAND, UK', 'UK (IRELAND)', 
                           'IRELAND', 'IRELAND (EIRE)', 
                           'IRELAND.', 'OR IRELAND', 'KILKENNY', 
                           'TIPPERARY', 'WATERFORD', 'ULSTER', 
                           'GALWAY', 'DOWN', 'CARLOW')


regex_codes_ireland <- c("\\bireland\\b|\\beire\\b|\\birish\\b|\\bie\\b", "IRELAND", "IE")


profiles_ireland = data_ireland %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_code))|
                                        !(is.na(country_birth_regex)) |
                                        !(is.na(country_birth_coord_based)),
                                      "IRELAND", NA),
         country_death_final = ifelse(
           !(is.na(country_death_code))|
             !(is.na(country_death_regex)) |
             !(is.na(country_death_coord_based)),
           "IRELAND", NA))

profiles_ireland_formatted = format_profiles(profiles_ireland,links)


#### Final selection for the country of birth ####

countries_birth_profiles = rbind(profiles_canada_formatted, 
                                 profiles_france_formatted, 
                                 profiles_sweden_formatted,
                                 profiles_finland_formatted,
                                 profiles_norway_formatted,
                                 profiles_germany_formatted,
                                 profiles_denmark_formatted,
                                 profiles_usa_formatted,
                                 profiles_netherlands_formatted,
                                 profiles_uk_formatted,
                                 profies_australia_formatted,
                                 profiles_india_formatted,
                                 profiles_estonia_formatted,
                                 profiles_ireland_formatted,
                                 profiles_spain_formatted,
                                 profiles_russia_formatted,
                                 profiles_switzerland_formatted,
                                 profiles_poland_formatted,
                                 profiles_germany_formatted,
                                 profiles_belgium_formatted,
                                 profiles_south_africa_formatted) %>%
  filter(!(is.na(country_birth_final)))

countries_birth = list(profiles_canada_formatted %>%
                         filter(!(is.na(country_birth_final))) %>% 
                         select(profileid,country_birth_canada=country_birth_final),
                       profiles_france_formatted %>% 
                         filter(!(is.na(country_birth_final))) %>%
                         select(profileid,country_birth_france = country_birth_final),
                       profiles_sweden_formatted %>% 
                         filter(!(is.na(country_birth_final))) %>% 
                         select(profileid,country_birth_sweden=country_birth_final),
                       profiles_finland_formatted %>% 
                         filter(!(is.na(country_birth_final))) %>%
                         select(profileid,country_birth_finaland=country_birth_final),
                       profiles_norway_formatted %>% 
                         filter(!(is.na(country_birth_final))) %>% 
                         select(profileid,country_birth_norway=country_birth_final),
                       profiles_germany_formatted %>% 
                         filter(!(is.na(country_birth_final))) %>%
                         select(profileid,country_birth_germany=country_birth_final),
                       profiles_denmark_formatted %>% 
                         filter(!(is.na(country_birth_final))) %>%
                         select(profileid,country_birth_denmark=country_birth_final),
                       profiles_usa_formatted%>% 
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_usa=country_birth_final),
                       profiles_uk_formatted%>% 
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_uk=country_birth_final),
                       profiles_netherlands_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_netherlands=country_birth_final),
                       profiles_estonia_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_estonia=country_birth_final),
                       profiles_australia_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_australia=country_birth_final),
                       profiles_south_africa_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_south_africa=country_birth_final),
                       profiles_belgium_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_belgium=country_birth_final),
                       profiles_russia_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_russia=country_birth_final),
                       profiles_poland_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_poland=country_birth_final),
                       profiles_ireland_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_ireland=country_birth_final),
                       profiles_spain_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_spain=country_birth_final),
                       profiles_switzerland_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_switzerlands=country_birth_final),
                       profiles_india_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_india=country_birth_final))

result_birth = Reduce(function(x, y) full_join(x, y, by="profileid"), countries_birth ) 



country_birth_codes_correction = result_birth[rowSums(is.na(result_birth))<9] %>%
  left_join(countries_birth_profiles %>% select(starts_with("country_birth"),profileid),by="profileid") %>%
  select(starts_with("country_birth"),profileid) %>%
  group_by(profileid) %>%
  dplyr::mutate(V = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = profileid,
    names_from = V,
    values_from =  !c(profileid, V),
    values_fill = NA) %>%
  filter(!(is.na(country_birth_code_1)) |
           !(is.na(country_birth_code_2)) |
           !(is.na(country_birth_code_3))) %>%
  filter(!((!(is.na(country_birth_code_1)) & !(is.na(country_birth_code_2))) |
             (!(is.na(country_birth_code_1)) & !(is.na(country_birth_code_3))) |
             (!(is.na(country_birth_code_2)) & !(is.na(country_birth_code_3))))) %>%
  mutate(country_birth_final_correct = case_when(!(is.na(country_birth_code_1)) ~ country_birth_code_1,
                                                 !(is.na(country_birth_code_2)) ~ country_birth_code_2,
                                                 !(is.na(country_birth_code_3)) ~ country_birth_code_3))

country_birth_regex_correction = result_birth[rowSums(is.na(result_birth))<9 & !(result_birth$profileid %in% country_birth_codes_correction$profileid)] %>%
  left_join(countries_birth_profiles %>% select(starts_with("country_birth"),profileid),by="profileid") %>%
  select(starts_with("country_birth"),profileid) %>%
  group_by(profileid) %>%
  dplyr::mutate(V = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = profileid,
    names_from = V,
    values_from =  !c(profileid, V),
    values_fill = NA) %>%
  filter(!(is.na(country_birth_regex_1)) |
           !(is.na(country_birth_regex_2)) |
           !(is.na(country_birth_regex_3))) %>%
  filter(!((!(is.na(country_birth_regex_1)) & !(is.na(country_birth_regex_2))) |
             (!(is.na(country_birth_regex_1)) & !(is.na(country_birth_regex_3))) |
             (!(is.na(country_birth_regex_2)) & !(is.na(country_birth_regex_3))))) %>%
  mutate(country_birth_final_correct = case_when(!(is.na(country_birth_regex_1)) ~ country_birth_regex_1,
                                                 !(is.na(country_birth_regex_2)) ~ country_birth_regex_2,
                                                 !(is.na(country_birth_regex_3)) ~ country_birth_regex_3))







duplicates_birth = rbind(country_birth_codes_correction,
                         country_birth_regex_correction) %>%
  select(profileid,country_birth_final_correct) %>%
  left_join(countries_birth_profiles,by="profileid") %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_final_correct)),
                                      country_birth_final_correct,
                                      country_birth_final)) %>%
  select(-country_birth_final_correct)


duplicates_birth = duplicates_birth[!duplicated(duplicates_birth$profileid),]




countries_birth_final = rbind(result_birth[rowSums(is.na(result_birth))==9] %>%
                                select(profileid) %>%
                                left_join(countries_birth_profiles,by="profileid"),
                              duplicates_birth)



#### Final selection for the country of birth ####


countries_birth = list(profiles_canada_formatted %>%
                         filter(!(is.na(country_birth_final))) %>% 
                         select(profileid,country_birth_canada=country_birth_final),
                       profiles_france_formatted %>% 
                         filter(!(is.na(country_birth_final))) %>%
                         select(profileid,country_birth_france = country_birth_final),
                       profiles_sweden_formatted %>% 
                         filter(!(is.na(country_birth_final))) %>% 
                         select(profileid,country_birth_sweden=country_birth_final),
                       profiles_finland_formatted %>% 
                         filter(!(is.na(country_birth_final))) %>%
                         select(profileid,country_birth_finaland=country_birth_final),
                       profiles_norway_formatted %>% 
                         filter(!(is.na(country_birth_final))) %>% 
                         select(profileid,country_birth_norway=country_birth_final),
                       profiles_germany_formatted %>% 
                         filter(!(is.na(country_birth_final))) %>%
                         select(profileid,country_birth_germany=country_birth_final),
                       profiles_denmark_formatted %>% 
                         filter(!(is.na(country_birth_final))) %>%
                         select(profileid,country_birth_denmark=country_birth_final),
                       profiles_usa_formatted%>% 
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_usa=country_birth_final),
                       profiles_uk_formatted%>% 
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_uk=country_birth_final),
                       profiles_netherlands_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_netherlands=country_birth_final),
                       profiles_estonia_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_estonia=country_birth_final),
                       profiles_australia_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_australia=country_birth_final),
                       profiles_south_africa_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_south_africa=country_birth_final),
                       profiles_belgium_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_belgium=country_birth_final),
                       profiles_russia_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_russia=country_birth_final),
                       profiles_poland_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_poland=country_birth_final),
                       profiles_ireland_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_ireland=country_birth_final),
                       profiles_spain_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_spain=country_birth_final),
                       profiles_switzerland_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_switzerlands=country_birth_final),
                       profiles_india_formatted%>%
                         filter(!(is.na(country_birth_final)))%>%
                         select(profileid,country_birth_india=country_birth_final)))

                  

result_birth = Reduce(function(x, y) full_join(x, y, by="profileid"), countries_birth ) 



country_birth_codes_correction = result_birth[rowSums(is.na(result_birth))<9] %>%
  left_join(countries_birth_profiles %>% select(starts_with("country_birth"),profileid),by="profileid") %>%
  select(starts_with("country_birth"),profileid) %>%
  group_by(profileid) %>%
  dplyr::mutate(V = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = profileid,
    names_from = V,
    values_from =  !c(profileid, V),
    values_fill = NA) %>%
  filter(!(is.na(country_birth_code_1)) |
           !(is.na(country_birth_code_2)) |
           !(is.na(country_birth_code_3))) %>%
  filter(!((!(is.na(country_birth_code_1)) & !(is.na(country_birth_code_2))) |
             (!(is.na(country_birth_code_1)) & !(is.na(country_birth_code_3))) |
             (!(is.na(country_birth_code_2)) & !(is.na(country_birth_code_3))))) %>%
  mutate(country_birth_final_correct = case_when(!(is.na(country_birth_code_1)) ~ country_birth_code_1,
                                                 !(is.na(country_birth_code_2)) ~ country_birth_code_2,
                                                 !(is.na(country_birth_code_3)) ~ country_birth_code_3))


country_birth_regex_correction = result_birth[rowSums(is.na(result_birth))<9 & !(result_birth$profileid %in% country_birth_codes_correction$profileid)] %>%
  left_join(countries_birth_profiles %>% select(starts_with("country_birth"),profileid),by="profileid") %>%
  select(starts_with("country_birth"),profileid) %>%
  group_by(profileid) %>%
  dplyr::mutate(V = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = profileid,
    names_from = V,
    values_from =  !c(profileid, V),
    values_fill = NA) %>%
  filter(!(is.na(country_birth_regex_1)) |
           !(is.na(country_birth_regex_2)) |
           !(is.na(country_birth_regex_3))) %>%
  filter(!((!(is.na(country_birth_regex_1)) & !(is.na(country_birth_regex_2))) |
             (!(is.na(country_birth_regex_1)) & !(is.na(country_birth_regex_3))) |
             (!(is.na(country_birth_regex_2)) & !(is.na(country_birth_regex_3))))) %>%
  mutate(country_birth_final_correct = case_when(!(is.na(country_birth_regex_1)) ~ country_birth_regex_1,
                                                 !(is.na(country_birth_regex_2)) ~ country_birth_regex_2,
                                                 !(is.na(country_birth_regex_3)) ~ country_birth_regex_3))



duplicates_birth = rbind(country_birth_codes_correction,
                         country_birth_regex_correction) %>%
  select(profileid,country_birth_final_correct) %>%
  left_join(countries_birth_profiles,by="profileid") %>%
  mutate(country_birth_final = ifelse(!(is.na(country_birth_final_correct)),
                                      country_birth_final_correct,
                                      country_birth_final)) %>%
  select(-country_birth_final_correct)


duplicates_birth = duplicates_birth[!duplicated(duplicates_birth$profileid),]




countries_birth_final = rbind(result_birth[rowSums(is.na(result_birth))==9] %>%
                                select(profileid) %>%
                                left_join(countries_birth_profiles,by="profileid"),
                              duplicates_birth)







#### Final selection for the coutry of death ####


countries_death_profiles = rbind(profiles_canada_formatted, 
                                 profiles_france_formatted, 
                                 profiles_sweden_formatted,
                                 profiles_finland_formatted,
                                 profiles_norway_formatted,
                                 profiles_germany_formatted,
                                 profiles_denmark_formatted,
                                 profiles_usa_formatted,
                                 profiles_netherlands_formatted,
                                 profiles_uk_formatted,
                                 profies_australia_formatted,
                                 profiles_india_formatted,
                                 profiles_estonia_formatted,
                                 profiles_ireland_formatted,
                                 profiles_spain_formatted,
                                 profiles_russia_formatted,
                                 profiles_switzerland_formatted,
                                 profiles_poland_formatted,
                                 profiles_germany_formatted,
                                 profiles_belgium_formatted,
                                 profiles_south_africa_formatted) %>%
  filter(!(is.na(country_death_final)))







countries_death = list(profiles_canada_formatted %>%
                         filter(!(is.na(country_death_final))) %>% 
                         select(profileid,country_death_canada=country_death_final),
                       profiles_france_formatted %>% 
                         filter(!(is.na(country_death_final))) %>%
                         select(profileid,country_death_france = country_death_final),
                       profiles_sweden_formatted %>% 
                         filter(!(is.na(country_death_final))) %>% 
                         select(profileid,country_death_sweden=country_death_final),
                       profiles_finland_formatted %>% 
                         filter(!(is.na(country_death_final))) %>%
                         select(profileid,country_death_finaland=country_death_final),
                       profiles_norway_formatted %>% 
                         filter(!(is.na(country_death_final))) %>% 
                         select(profileid,country_death_norway=country_death_final),
                       profiles_germany_formatted %>% 
                         filter(!(is.na(country_death_final))) %>%
                         select(profileid,country_death_germany=country_death_final),
                       profiles_denmark_formatted %>% 
                         filter(!(is.na(country_death_final))) %>%
                         select(profileid,country_death_denmark=country_death_final),
                       profiles_usa_formatted%>% 
                         filter(!(is.na(country_death_final)))%>%
                         select(profileid,country_death_usa=country_death_final),
                       profiles_uk_formatted%>% 
                         filter(!(is.na(country_death_final)))%>%
                         select(profileid,country_death_uk=country_death_final),
                       profiles_netherlands_formatted%>%
                         filter(!(is.na(country_death_final)))%>%
                         select(profileid,country_death_netherlands=country_death_final),
                       profiles_estonia_formatted%>%
                         filter(!(is.na(country_death_final)))%>%
                         select(profileid,country_death_estonia=country_death_final),
                       profiles_australia_formatted%>%
                         filter(!(is.na(country_death_final)))%>%
                         select(profileid,country_death_australia=country_death_final),
                       profiles_south_africa_formatted%>%
                         filter(!(is.na(country_death_final)))%>%
                         select(profileid,country_death_south_africa=country_death_final),
                       profiles_belgium_formatted%>%
                         filter(!(is.na(country_death_final)))%>%
                         select(profileid,country_death_belgium=country_death_final),
                       profiles_russia_formatted%>%
                         filter(!(is.na(country_death_final)))%>%
                         select(profileid,country_death_russia=country_death_final),
                       profiles_poland_formatted%>%
                         filter(!(is.na(country_death_final)))%>%
                         select(profileid,country_death_poland=country_death_final),
                       profiles_ireland_formatted%>%
                         filter(!(is.na(country_death_final)))%>%
                         select(profileid,country_death_ireland=country_death_final),
                       profiles_spain_formatted%>%
                         filter(!(is.na(country_death_final)))%>%
                         select(profileid,country_death_spain=country_death_final),
                       profiles_switzerland_formatted%>%
                         filter(!(is.na(country_death_final)))%>%
                         select(profileid,country_death_switzerlands=country_death_final),
                       profiles_india_formatted%>%
                         filter(!(is.na(country_death_final)))%>%
                         select(profileid,country_death_india=country_death_final))

result_death = Reduce(function(x, y) full_join(x, y, by="profileid"), countries_death ) 



country_death_codes_correction = result_death[rowSums(is.na(result_death))<9] %>%
  left_join(countries_death_profiles %>% select(starts_with("country_death"),profileid),by="profileid") %>%
  select(starts_with("country_death"),profileid) %>%
  group_by(profileid) %>%
  dplyr::mutate(V = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = profileid,
    names_from = V,
    values_from =  !c(profileid, V),
    values_fill = NA) %>%
  filter(!(is.na(country_death_code_1)) |
           !(is.na(country_death_code_2)) |
           !(is.na(country_death_code_3))) %>%
  filter(!((!(is.na(country_death_code_1)) & !(is.na(country_death_code_2))) |
             (!(is.na(country_death_code_1)) & !(is.na(country_death_code_3))) |
             (!(is.na(country_death_code_2)) & !(is.na(country_death_code_3))))) %>%
  mutate(country_death_final_correct = case_when(!(is.na(country_death_code_1)) ~ country_death_code_1,
                                                 !(is.na(country_death_code_2)) ~ country_death_code_2,
                                                 !(is.na(country_death_code_3)) ~ country_death_code_3))

country_death_regex_correction = result_death[rowSums(is.na(result_death))<9 & !(result_death$profileid %in% country_death_codes_correction$profileid)] %>%
  left_join(countries_death_profiles %>% select(starts_with("country_death"),profileid),by="profileid") %>%
  select(starts_with("country_death"),profileid) %>%
  group_by(profileid) %>%
  dplyr::mutate(V = row_number()) %>%
  ungroup() %>%
  pivot_wider(
    id_cols = profileid,
    names_from = V,
    values_from =  !c(profileid, V),
    values_fill = NA) %>%
  filter(!(is.na(country_death_regex_1)) |
           !(is.na(country_death_regex_2)) |
           !(is.na(country_death_regex_3))) %>%
  filter(!((!(is.na(country_death_regex_1)) & !(is.na(country_death_regex_2))) |
             (!(is.na(country_death_regex_1)) & !(is.na(country_death_regex_3))) |
             (!(is.na(country_death_regex_2)) & !(is.na(country_death_regex_3))))) %>%
  mutate(country_death_final_correct = case_when(!(is.na(country_death_regex_1)) ~ country_death_regex_1,
                                                 !(is.na(country_death_regex_2)) ~ country_death_regex_2,
                                                 !(is.na(country_death_regex_3)) ~ country_death_regex_3))







duplicates_death = rbind(country_death_codes_correction,
                         country_death_regex_correction) %>%
  select(profileid,country_death_final_correct) %>%
  left_join(countries_death_profiles,by="profileid") %>%
  mutate(country_death_final = ifelse(!(is.na(country_death_final_correct)),
                                      country_death_final_correct,
                                      country_death_final)) %>%
  select(-country_death_final_correct)


duplicates_death = duplicates_death[!duplicated(duplicates_death$profileid),]


countries_death_final = rbind(result_death[rowSums(is.na(result_death))==9] %>%
                                select(profileid) %>%
                                left_join(countries_death_profiles,by="profileid"),
                              duplicates_death)


#### Focal data set #####

countries_final_birth_death = countries_birth_final %>%
  select(profileid,country_birth_final) %>%
  full_join(countries_death_profiles %>%
              select(profileid,country_death_final),by="profileid")


data_countries_overall = rbind(profiles_canada_formatted,profiles_norway_formatted,
                               profiles_denmark_formatted,profiles_france_formatted,
                               profiles_finland_formatted,profiles_sweden_formatted,
                               profiles_germany_formatted,profiles_netherlands_formatted,
                               profiles_uk_formatted,profiles_usa_formatted,profies_australia_formatted,
                               profiles_india_formatted,profiles_estonia_formatted,profiles_ireland_formatted,
                               profiles_spain_formatted,profiles_russia_formatted,profiles_switzerland_formatted,
                               profiles_poland_formatted,profiles_germany_formatted,profiles_belgium_formatted,
                               profiles_south_africa_formatted) %>%
  select(-country_birth_final,-country_death_final) 



data_countries_overall = data_countries_overall[!(duplicated(data_countries_overall$profileid)),]


data_focal  = countries_final_birth_death %>%
  left_join(data_countries_overall,by="profileid") %>%
  left_joint(parents %>%
              select(m=mother,f=father,p_unknown=parent_sex_unknown)) # link to parents


save(data_focal,file='Cleaned_Datasets/data_focal.RData')


# read strings referring to coordinates as text string

data_red_var = read_coordinates(prof)

# set unreasonable birth and death years to NA

# unreasonable ages at death to NA

# match id to the parents id

data_red_var = format_profiles_complete(data_red_var,link)


# Select only variables that are needed to run the analyses

data_red_var = prof %>%
  dplyr::select(profileid,gender,birth_year,
         birth_month,birth_day,death_year,
         death_month,death_day,
         birth_country=birth_coord_country,
         death_country=death_coord_country,
         age_death,century) %>%
  mutate(age_death_cl = case_when(age_death>=0 & age_death<5 ~ '0-4',
                                  age_death>=5 & age_death<15 ~ '5-14',
                                  age_death>=15 & age_death<30 ~ '15-29',
                                  age_death>=30 & age_death<45 ~ '30-44',
                                  age_death>=45 & age_death<60 ~ '45-59',
                                  age_death>=60 & age_death<75 ~ '60-74',
                                  age_death>=75 & age_death<90 ~ '75-89',
                                  age_death>=90 ~ '90+'))


save(data_red_var,file='Cleaned_Datasets/data_red_var.RData')





