##Author: Carla Canovi 
## 09.26.2024
#load packages
install_github('OSGeo/gdal')
install_github("r-lib/devtools")
pacman::p_load(MASS, readxl, tseries, car, tile, simcf, # data refinement
               survival, spaMM, plm, coda, fields, # data analysis
               sf, sp, tmap, raster, spdep, geos,
               ggplot2, ggpubr, shiny, shinyjs, tmaptools, graph4lg, # spatial 
               tidyverse )
setwd("~/R/local-ordinance-model/MIT Replication")

########################### Data Refinement ##################################
#import county list
data <- read_excel("county_list.xlsx")
#import sabin center data
sabin_import <- read_excel("Sabin_Center_RELDI_data__June2024.xlsx")
sabin <- sabin_import[,c(2,3,6,7,10,11,17:23)] %>% 
  filter(`Local Restriction`==1) %>%
  separate_wider_regex(County, c(county_name = ".*", " ", var2 = ".*"),too_few = "align_start")

ordinances <- sabin[,-c(3,6)] 
colnames(ordinances) <- c("state_name","county_name","solar", "wind", "year", "setback", "height", "noise", "wildlife", "agriculture", "visual_impact", "litigation")

#need to fix years
ordinances <- ordinances %>% drop_na(year)
ordinances$year[ordinances$year=='"2019"'] <- 2019
ordinances$year <- as.double(ordinances$year)

# County-level covariates, 2010-2023

year <- c(rep(2010,3142), rep(2011,3142), rep(2012,3142),
          rep(2013,3142), rep(2014,3142), rep(2015,3142),
          rep(2016,3142), rep(2017,3142), rep(2018,3142),
          rep(2019,3142), rep(2020,3142), rep(2021,3142),
          rep(2022,3142), rep(2023,3142))

fulldata <- rbind(data,data,data,data,data,
                  data,data,data,data,data,
                  data,data,data,data)
fulldata$year <- year

windord <- ordinances %>% filter(wind == 1)
solarord <-  ordinances %>% filter(solar == 1)
windord_unique <- windord %>% select(state_name, county_name, year) %>% unique()
windord_unique$windord <- 1 # 241 counties with wind ordinances 
solarord_unique <- solarord %>% select(state_name, county_name, year) %>% unique()
solarord_unique$solarord <- 1 # 188 counties with solar ordinances 

fulldata <- left_join(fulldata, windord_unique, by=c("state_name","county_name","year"))
sum(windord_unique$windord); sum(fulldata$windord, na.rm=T) #39 not transferred 

fulldata <- left_join(fulldata, solarord_unique, by=c("state_name","county_name","year"))
sum(solarord_unique$solarord); sum(fulldata$solarord, na.rm=T) #39 not transferred 

write_csv(fulldata,"test.csv")
fulldata <- fulldata %>% arrange(state_name, county_name, year)

#filter if have year of restriction - anything that has a 1 in contested project filter out - should leave us with straight forward list and colomns f through i  colomns o-3 are also interesting to us - also colomn w is there litigation happening 

###### county-level partisanship####
# county-level presidential republican vote share, 2008, 2012, 2016, 2020
# https://electionlab.mit.edu/data, https://dataverse.harvard.edu/file.xhtml?fileId=6689930&version=11.0 

county_partisan <- read.table("countypres_2000-2020.tab", fill=NA, header=T)
for (i in 1:nrow(county_partisan)){
  county_partisan$county_fips[i][unlist(str_split(county_partisan$county_fips[i], ""))[1]=="0"] <- paste(unlist(str_split(county_partisan$county_fips[i], ""))[2:5], collapse="")
}

county_partisan <- county_partisan %>% 
  select(county_fips, year, party, candidatevotes, totalvotes) %>% 
  filter(party=="REPUBLICAN") %>% 
  group_by(county_fips, year) %>% 
  summarize(repvote=sum(candidatevotes), totalvotes=totalvotes) %>% unique() %>% 
  summarize(repshare=100*repvote/totalvotes)

county_partisan$year[county_partisan$year==2008] <- 2010



fulldata$county_fips <- as.character(fulldata$county_fips)
fulldata <- left_join(fulldata, county_partisan, by=c("county_fips","year"))
fips_list <- unique(fulldata$county_fips)

for (i in 1:length(fips_list)) { # the most recent presidential election year
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2011] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2010] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2013] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2012] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2014] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2012] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2015] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2012] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2017] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2016] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2018] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2016] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2019] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2016] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2021] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2020] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2022] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2020] 
  fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2023] <- fulldata$repshare[fulldata$county_fips==fips_list[i] & fulldata$year==2020] 
  }

#####per capita income and employment##### 

econ <- read.csv("CAINC4__ALL_AREAS_1969_2023.csv")
pcincome1 <- econ[,c(1,7, 50:63)] %>% filter(Description=="Per capita personal income (dollars) 4/") %>% select(-Description)
colnames(pcincome1)[1] <- c("county_fips")
pcincome1 <- pcincome1 %>% gather(year, pcincome1, X2010:X2023, factor_key=T)
pcincome1$year <- gsub("X","",pcincome1$year) %>% as.numeric()

fulldata <- left_join(fulldata, pcincome1, by=c("county_fips","year"))

####### unemployment rate#####
# https://www.bls.gov/lau/tables.htm#cntyaa
unemp <- data.frame(NULL)

for (k in 1:14){
  iter_data <- read_excel(paste0("laucnty",k+9,".xlsx"))[-c(1:5),c(2,3,5,10)]
  colnames(iter_data) <- c("state_code","county_code","year","unemprate")
  iter_data$county_fips <- NA
  for (i in 1:nrow(iter_data)) {
    iter_data$county_fips[i] <- paste0(iter_data$state_code[i], iter_data$county_code[i])
    iter_data$county_fips[i][unlist(str_split(iter_data$county_fips[i], ""))[1]=="0"] <- paste(unlist(str_split(iter_data$county_fips[i], ""))[2:5], collapse="")
  }
  unemp <- rbind(unemp, iter_data)
}
unemp <- unemp[,-c(1:2)]
unemp$year <- as.numeric(unemp$year)
unemp$unemprate <- as.numeric(unemp$unemprate)

#rawdata <- fulldata
fulldata <- left_join(fulldata, unemp, by=c("county_fips","year"))

# state-level covariates
####RPS####
#source: https://www.ncsl.org/energy/state-renewable-portfolio-standards-and-goals
unique(fulldata$state_name)
fulldata$state_rps <- 0
fulldata$state_rps[fulldata$state_name=="Arizona"] <- 1 #since 2006
fulldata$state_rps[fulldata$state_name=="California"] <- 1 #since 2002
fulldata$state_rps[fulldata$state_name=="Colorado"] <- 1 #since 2004
fulldata$state_rps[fulldata$state_name=="Connecticut"] <- 1 #since 1998
fulldata$state_rps[fulldata$state_name=="Delaware"] <- 1 #since 2005
fulldata$state_rps[fulldata$state_name=="Hawaii"] <- 1 #since 2001
fulldata$state_rps[fulldata$state_name=="Illinois"] <- 1 #since 2007
fulldata$state_rps[fulldata$state_name=="Indiana" & fulldata$year>=2011] <- 1 #since 2011
fulldata$state_rps[fulldata$state_name=="Iowa"] <- 1 #since 1983
fulldata$state_rps[fulldata$state_name=="Kansas"] <- 1 #since 2009
fulldata$state_rps[fulldata$state_name=="Maine"] <- 1 #since 1999
fulldata$state_rps[fulldata$state_name=="Maryland"] <- 1 #since 2004
fulldata$state_rps[fulldata$state_name=="Massachusetts"] <- 1 #since 1997
fulldata$state_rps[fulldata$state_name=="Michigan"] <- 1 #since 2008
fulldata$state_rps[fulldata$state_name=="Minnesota"] <- 1 #since 2007
fulldata$state_rps[fulldata$state_name=="Missouri"] <- 1 #since 2007
fulldata$state_rps[fulldata$state_name=="Nevada"] <- 1 #since 1997
fulldata$state_rps[fulldata$state_name=="New Hampshire"] <- 1 #since 2007
fulldata$state_rps[fulldata$state_name=="New Jersey"] <- 1 #since 1991
fulldata$state_rps[fulldata$state_name=="New Mexico"] <- 1 #since 2002
fulldata$state_rps[fulldata$state_name=="New York"] <- 1 #since 2004
fulldata$state_rps[fulldata$state_name=="North Carolina"] <- 1 #since 2007
fulldata$state_rps[fulldata$state_name=="North Dakota"] <- 1 #since 2007
fulldata$state_rps[fulldata$state_name=="Ohio"] <- 1 #since 2008
fulldata$state_rps[fulldata$state_name=="Oklahoma"] <- 1 #since 2010
fulldata$state_rps[fulldata$state_name=="Oregon"] <- 1 #since 2007
fulldata$state_rps[fulldata$state_name=="Pennsylvania"] <- 1 #since 2004
fulldata$state_rps[fulldata$state_name=="Rhode Island"] <- 1 #since 2004
#fulldata$state_rps[fulldata$state_name=="South Carolina" & fulldata$year>=2014] <- 1 #since 2014 (but voluntary)
fulldata$state_rps[fulldata$state_name=="South Dakota"] <- 1 #since 2008
fulldata$state_rps[fulldata$state_name=="Texas"] <- 1 #since 1999
fulldata$state_rps[fulldata$state_name=="Utah"] <- 1 #since 2008
fulldata$state_rps[fulldata$state_name=="Vermont" & fulldata$year>=2015] <- 1 #since 2015
fulldata$state_rps[fulldata$state_name=="Virginia" & fulldata$year>=2020] <- 1 #since 2020
fulldata$state_rps[fulldata$state_name=="Washington"] <- 1 #since 2006
fulldata$state_rps[fulldata$state_name=="West Virginia" & fulldata$year<=2015] <- 1 #2009-2015
fulldata$state_rps[fulldata$state_name=="Wisconsin"] <- 1 #since 1998
fulldata$state_rps[fulldata$state_name=="District of Columbia"] <- 1 #since 2005


###generator-level wind and solar energy capacity, by county by year #####
# https://www.eia.gov/electricity/data/eia860/

gen2010 <- read_excel("gen2010.xls") %>% filter(ENERGY_SOURCE_1=="SUN" | ENERGY_SOURCE_1=="WND")
gen2011 <- read_excel("gen2011.xlsx", skip=1) %>% filter(ENERGY_SOURCE_1=="SUN" | ENERGY_SOURCE_1=="WND")
gen2012 <- read_excel("gen2012.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2013 <- read_excel("gen2013.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2014 <- read_excel("gen2014.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2015 <- read_excel("gen2015.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2016 <- read_excel("gen2016.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2017 <- read_excel("gen2017.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2018 <- read_excel("gen2018.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2019 <- read_excel("gen2019.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2020 <- read_excel("gen2020.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2021 <- read_excel("gen2021.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2022 <- read_excel("gen2022.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")
gen2023 <- read_excel("gen2023.xlsx", skip=1) %>% filter(`Energy Source 1`=="SUN" | `Energy Source 1`=="WND")

gen2010$year <- 2010; gen2011$year <- 2011; gen2012$year <- 2012; gen2013$year <- 2013
gen2014$year <- 2014; gen2015$year <- 2015; gen2016$year <- 2016; gen2017$year <- 2017
gen2018$year <- 2018; gen2019$year <- 2019; gen2020$year <- 2020; gen2021$year <- 2021;
gen2022$year <- 2022; gen2023$year <- 2023

gen2010 <- gen2010 %>% 
  select("UTILITY_ID","UTILITY_NAME","PLANT_CODE","PLANT_NAME","GENERATOR_ID","STATE","COUNTY","year","NAMEPLATE","ENERGY_SOURCE_1")
gen2011 <- gen2011 %>% 
  select("UTILITY_ID","UTILITY_NAME","PLANT_CODE","PLANT_NAME","GENERATOR_ID","STATE","COUNTY","year","NAMEPLATE","ENERGY_SOURCE_1")
gen2012 <- gen2012 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2013 <- gen2013 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2014 <- gen2014 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2015 <- gen2015 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2016 <- gen2016 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2017 <- gen2017 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2018 <- gen2018 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2019 <- gen2019 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2020 <- gen2020 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2021 <- gen2021 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2022 <- gen2022 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")
gen2023 <- gen2023 %>% 
  select("Utility ID","Utility Name","Plant Code","Plant Name","Generator ID","State","County","year","Nameplate Capacity (MW)","Energy Source 1")


varlist <- c("utility_ID", "utility_name", "plant_code", "plant_name", "generator_ID",
             "state_abbv","county_name", "year","capacity", "source")

colnames(gen2010) <- colnames(gen2011) <- colnames(gen2012) <- colnames(gen2013) <- 
  colnames(gen2014) <- colnames(gen2015) <- colnames(gen2016) <- colnames(gen2017) <- 
  colnames(gen2018) <- colnames(gen2019) <- colnames(gen2020) <- colnames(gen2021) <- 
  colnames(gen2022) <- colnames(gen2023)<- varlist

gentotal <- rbind(gen2010, gen2011, gen2012, gen2013, gen2014, gen2015, gen2016,
                  gen2017, gen2018, gen2019, gen2020, gen2021, gen2022, gen2023)

gentotal$county_name <- tolower(gentotal$county_name); fulldata$county_name <- tolower(fulldata$county_name)
# gentotal data has some upper case county names; just use lower case for all datasets for easy merging

mean(gentotal$capacity[gentotal$source=="WND"])
mean(gentotal$capacity[gentotal$source=="SUN"])

windcapa <- gentotal %>% filter(source=="WND") %>% 
  group_by(state_abbv, county_name, year) %>% 
  summarize(capacity=sum(capacity))

solarcapa <- gentotal %>% filter(source=="SUN") %>% 
  group_by(state_abbv, county_name, year) %>% 
  summarize(capacity=sum(capacity))

#rawdata <- fulldata
fulldata <- left_join(fulldata, windcapa, by=c("state_abbv","county_name","year"))
colnames(fulldata)[12] <- "windcapa"

fulldata <- left_join(fulldata, solarcapa, by=c("state_abbv","county_name","year"))
colnames(fulldata)[13] <- "solarcapa"

fulldata$windcapa[is.na(fulldata$windcapa)] <- 0
fulldata$solarcapa[is.na(fulldata$solarcapa)] <- 0


##### geospatial component: adjacency matrix########
geo <- st_read("tl_2019_us_county.shp")
E <- nb2mat(poly2nb(geo), style = "B", zero.policy = T)

# save centroids for potential use
geo$id <- 1:nrow(geo)
geo_sp <- vect(geo)
centr <- centroids(geo_sp, TRUE)
coord <- as.data.frame(centr@coords)
coord$county_fips <- geo$GEOID10
coord$id <- geo$id
for (i in 1:nrow(coord)) {
  coord$county_fips[i][unlist(str_split(coord$county_fips[i], ""))[1]=="0"] <- paste(unlist(str_split(coord$county_fips[i], ""))[2:5], collapse="")
}

rawdata <- fulldata
fulldata <- left_join(fulldata, coord, by="county_fips")

################## index of relative ruralness#####
irr <- read_excel("IRR_2000_2010.xlsx", sheet=2)

irr <- irr[,-c(4,5)]
colnames(irr) <- c("county_fips","county","irr")
irr$county_fips <- as.character(irr$county_fips)
fulldata <- left_join(fulldata, irr, by="county_fips")


