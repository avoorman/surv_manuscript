##########################################
# Purpose: To generate surveillance flags for the Surveillance Flags Manuscript
# Maintainers: Sara Khan, Kristin VanderEnde, Arie Voorman
##########################################

source("./sourcefiles/Packages_Functions_Colors.R", local=T)

### Bring in Packages =====

library(tidyverse)
library(lubridate)
library(rgdal)
library(broom)
library(geojsonio)
library(readxl)


##########################################
## Data reading * manual step! =====
##########################################
# Read data from POLIS. Save this file from the WHO POLIS dataset
# for ALL data from 2015-current, and save it as a csv called "world.surind.01

world.surind.01<- readr::read_csv2("data/world.surind.01.csv")

##########################################
# DATA PROCESSING ========================
##########################################

# dplyr::rename the 7columns with spaces (e.g. "Paralysis Onset Date" to "Paralysis.Onset.Date")
names(world.surind.01)<-make.names(names(world.surind.01))


# only have 2015 onwards cases. Rename Cote D'Ivoire
world.surind.01 <- world.surind.01 %>%
  filter(Date.Onset.Year>=2014) %>% # use read.fst to read the FST file in
  mutate(Place.Admin.0=ifelse(Admin0GUID=="927ba47a-d87d-43f9-8642-d19230811e15", # Cote D'Ivoire has character encoding problem, changed it here
                              "CIV",Place.Admin.0)) 

world.surind.02 = world.surind.01 %>%  
  mutate(adm0guid=Admin0GUID)

# Some provinces are brought it twice - to avoid this, just pick the first
# EPID and which province it falls into
world.surind.02<-world.surind.02[!duplicated(world.surind.02$AnonymousEPID),]
## Other data processing steps

world.surind.02 <- world.surind.02 %>% 
  dplyr::rename(id=AnonymousEPID, ctry=Place.Admin.0) %>%
  mutate(dateonset=dmy(Paralysis.Onset.Date), datenotify=dmy(Notification.Date),
         dateinvest=dmy(Investigation.Date), datestool1=dmy(Stool.1.Collection.Date),
         datestool2=dmy(Stool.2.Collection.Date), yronset=year(dateonset),
         ontostool2=as.numeric(datestool2-dateonset),
         age.months=as.numeric(CalculatedAgeInMonth),
         ontonot=as.numeric(datenotify-dateonset),
         ontoinvest=as.numeric(dateinvest-dateonset)
  ) %>%
  mutate(ontostool2.02=if_else(ontostool2>=60, 60, ontostool2),
         ontostool1=datestool1-dateonset,
         ontostool2=datestool2-dateonset,
         stool2tostool1=datestool2-datestool1,
         stool1tostool2=as.numeric(datestool2-datestool1),
         timeliness=NA,
         timeliness=if_else((ontostool1<=13 & ontostool1>=0 & 
                               ontostool2<=14 & ontostool2>=1 & stool2tostool1>=1),
                            1, 0),
         timeliness=if_else((is.na(datestool1) | is.na(datestool2)), 0, timeliness),
         
         adequacy=NA,
         adequacy=if_else((ontostool1<=13 & ontostool1>=0 & 
                             ontostool2<=14 & ontostool2>=1 & stool2tostool1>=1
                           & Stool.1.Condition == "Good" & Stool.2.Condition == "Good" ),
                          1, 0),
         adequacy=if_else((is.na(datestool1) | is.na(datestool2)), 0, adequacy),
         
         yrday=yday(dateonset),
         wkday=week(dateonset)
  ) %>%
  mutate(compatible=NA,
         compatible=ifelse(
           Classification=="Compatible",
           "compatible", compatible),
         compatible=ifelse(
           Classification!="Compatible",
           "other", compatible)
  ) %>%
  mutate(stool2missing=1) %>%
  mutate(stool2missing=replace(stool2missing, is.na(datestool2), 0)) %>%
  mutate(stool1missing=1) %>%
  mutate(stool1missing=replace(stool1missing, is.na(datestool1), 0)) %>%
  mutate(stoolmissing=NA) %>%
  mutate(stoolmissing=if_else(stool2missing==0 | stool1missing==0, 0, 1)) %>%
  mutate(ontostool2.03=NA,
         ontostool2.03=ifelse(ontostool2>14, ">14", ontostool2.03),
         ontostool2.03=ifelse(ontostool2<=14, "<=14", ontostool2.03)) %>%
  mutate(ontonot.60=NA,
         ontonot.60=ifelse(ontonot>60, ">60", ontonot.60),
         ontonot.60=ifelse(ontonot<=60, "<=60", ontonot.60)) %>% 
  mutate(ontonot.14=NA,
         ontonot.14=ifelse(ontonot>14, ">14", ontonot.14),
         ontonot.14=ifelse(ontonot<=14, "<=14", ontonot.14)) %>%
  mutate(CDC.Classification = f.npclass01(Classification, ClassificationVdpv)) %>% 
  mutate( ontoinvest=as.numeric(dateinvest-dateonset), 
          nottoinvest=as.numeric(dateinvest-datenotify),
          investtostool1=as.numeric(datestool1-dateinvest)
  )


#source("./sourcefiles/Colors.R", local=T)

yr <- 2017
startyr <- yr-2


##########################################
#  SHAPEFILES ============================
##########################################

## Getting POLIS shapefiles ====
# source: http://polioboundaries-who.hub.arcgis.com/

who_00_url<-"https://opendata.arcgis.com/datasets/6b44e83b8efe45c89e22d2721ec824bb_6.geojson"
utf8 <- readr::read_lines(who_00_url)
global.01.adm0 <- enc2native(utf8)
writeLines(global.01.adm0, 'global.01.adm0.geojson')
global.01.adm0 <- geojson_read(x = "global.01.adm0.geojson", what = "sp")  

#Country specific ADM0 file from global admin0 spatial polygon dataframe
country.who.ctry <- global.01.adm0[global.01.adm0@data$WHO_REGION %in% c("SEARO","AFRO", "EMRO"),] 

ctry.id.01 <- country.who.ctry@data  %>%
  select(ADM0_NAME, GUID, ISO_3_CODE) %>% 
  dplyr::rename(ctry=ADM0_NAME, adm0guid=GUID)  %>% 
  distinct(ctry, adm0guid, ISO_3_CODE) 

country.who.plot.ctry <- f.adm03(country.who.ctry, ctry.id.01)


#### Data flags ======

world.surind.02<-world.surind.02 %>% filter(WhoRegion %in% c("AFRO", 
                                                             "SEARO", "EMRO")) %>% 
  filter(yronset %in% 2015:2017)


afp.ctry.01.all <- world.surind.02 %>% #filter(yronset %in% c(2017)) %>%
  # filter(CDC.Classification=='NPAFP', age.months <180
  # ) %>%
  group_by( ctry, adm0guid, Admin0Iso3Code, WhoRegion) %>%
  dplyr::summarise(AFPcases=n() )

afp.ctry.01 <- world.surind.02 %>%
  # filter(CDC.Classification=='NPAFP', age.months <180
  # ) %>%
  group_by( ctry, adm0guid, Admin0Iso3Code) %>%
  dplyr::summarise(AFPcases=n(), lastnotify=max(datenotify, na.rm=TRUE)) %>% 
  filter(AFPcases>=250)

## joining world.surind.02 with afp.ctry.01 (not just NPAFP)
world.surind.02=world.surind.02 %>% inner_join(afp.ctry.01 %>% select(ctry))

## for now, eventually incorporate into original world.surind.02 creation ###
world.surind.02 <- world.surind.02 %>%
  mutate(doses.missing=1) %>%
  mutate(doses.missing=replace(doses.missing, is.na(Doses.Total), 0)) %>%
  mutate(no.doses=1) %>%
  mutate(no.doses=replace(no.doses, Doses.Total>0, 0),
         no.doses=replace(no.doses, is.na(Doses.Total), NA)) %>%
  mutate(full.doses=1) %>%
  mutate(full.doses=replace(full.doses, Doses.Total<3, 0), 
         full.doses=replace(full.doses, is.na(Doses.Total), NA)) %>%
  rowwise() %>%
  mutate(total.dose.na = f.total.doses.na(Doses.Total)) %>%
  rowwise() %>%
  mutate(total.dose = f.total.doses(Doses.Total))


##########################################
### Country-level data creation  #########
##########################################

### All countries past 3 years, > 250 cases for each flag

## dataframe creation for missing second stool flag

tab.all.countries =  world.surind.02 %>%
  f.miss.stool(., ctry, adm0guid, Admin0Iso3Code)

### dataframe creation second stool > 14 days, past 3 years, countries with > 250 cases 

tab.all.countries.onset =  world.surind.02 %>%
  f.stool.14days(., ctry, adm0guid, Admin0Iso3Code)

### dataframe creation onset to notification > 14 days, last 3 years, countries with > 250 cases 

tab.all.countries.not.14 =  world.surind.02 %>%
  f.14days.notify(., ctry, adm0guid, Admin0Iso3Code)


### data frame for onset to investigation > 60 days, last 3 years, countries with > filteron cases 
### Note - for countries with 0 cases > 60 days, added 1 case to numerator and denominator to calculate proportion. 

tab.all.countries.not = world.surind.02 %>%
  f.60days.notify(., ctry, adm0guid, Admin0Iso3Code)

tab.compare <- f.merge.notify(tab.all.countries.not, tab.all.countries.not.14)

#### dataframe creation for ratio under 5 to over 5 but under 15 . 

#countries

age.5 <- world.surind.02 %>%
  f.age.5(., ctry, adm0guid, Admin0Iso3Code)

age.15 <- world.surind.02 %>%
  f.age.15(., ctry, adm0guid, Admin0Iso3Code)

compare.age.5.15 <- f.merge.age(age.5, age.15) %>% 
  mutate(ratio.1=paste(round(ratio, digits=2), "*", sep=" ")) 

# ## dataframe creation for missing dose history ## (under development)
# 
# doses.all.ctry.1 <-  world.surind.02 %>%
#   f.miss.dose(., ctry, adm0guid, Admin0Iso3Code) %>% filter(percent.miss.dose !=100) 
# 
# #doses.all.ctry.1$missing_dose_cat <- f.missing.dose.percent(doses.all.ctry.1$percent.miss.dose)
# 
# doses.all.ctry.2 =  world.surind.02 %>%
#   f.no.dose(., ctry, adm0guid, Admin0Iso3Code) 


## Merge datasets - Country level - all flags except high NPAFP flag
## Note - do not include total N in this merge, as may be slightly different across flags. 

## for use with new surveillance flags 

flag.world  <- f.merge.all(tab.all.countries, tab.compare, 
                           tab.all.countries.onset, compare.age.5.15)
#flag.world  <- f.merge.all.new(tab.all.countries, tab.compare, tab.all.countries.onset, compare.age.5.15, doses.all.ctry.1)

flag.world2 <- flag.world %>% mutate(flagged=ifelse(flag.total==0,
                                                    "Not Flagged", "Flagged"))

flag.world3 <- flag.world2%>% left_join(afp.ctry.01) %>% 
  mutate(percent.timeliness_c = ifelse(flag.timeliness==1, paste0(as.character(round(percent.late.onset,2)), "*"),as.character(round(percent.late.onset,2))),
         prop.late.note_c = ifelse(flag.late.notification==1, paste0(as.character(round(prop.60.14,2)), "*"), as.character(round(prop.60.14,2))),
         percent.missing_c = ifelse(flag.missing==1, paste0(as.character(round(percent.missing,2)), "*"), as.character(round(percent.missing,2))),
         ratio_c = ifelse(flag.age==1, paste0(as.character(round(ratio,2)), "*"), as.character(round(ratio, 2)))) %>% #left_join instead of full join, so only countries with flags included.
  filter(!is.na(percent.timeliness_c) &
           !is.na(prop.late.note_c) &
           !is.na(percent.missing_c) & 
           !is.na(ratio_c)) ### including only countries with data for all four flags


flag.world3 = flag.world3 %>% 
  # select(ctry, adm0guid, Admin0Iso3Code, npAFPcases, percent.14_c, prop.60.14_c, 
  #        percent.missing_c,  ratio_c, flag.total) %>% 
  right_join(afp.ctry.01.all) #left_join instead of full join, so only countries with flags included.


write.csv(flag.world3, "flag.world3.csv")


## This is for the table that is exported out into an excel file
#- it has all countries in the regions selected


flag.world4 = flag.world3 %>% select(ctry,AFPcases, percent.timeliness_c, prop.late.note_c,
                                     percent.missing_c,ratio_c, flag.total)


write.csv(flag.world4, "flag.world4.csv")


## Kristin added below, comes from Data_Flags #
#########################################
#### Creating dataframes for geomtext ####
#########################################

#################
### Country-level
#################

# Creating dataframe for onset to stool 2 > 14 days for including in geom_text

world.14days <- f.stool.14.1(world.surind.02, ctry, adm0guid, ontostool2.03) %>%
  filter(ontostool2.03=="<=14") %>%
  mutate(percent2=paste(round(100 - percent1, digits=2), "% *", sep=" ")) %>% 
  full_join( # add in all of the provinces that did not make the cut
    world.surind.02 %>% 
      select(ctry, adm0guid) %>% ### since only using in geomtext, took out grouping by province. 
      unique()
  ) 


# Creating dataframe for onset to notification > 60 days for including in geom_text

world.60days <- f.stool.60.1(world.surind.02, ctry, adm0guid, ontonot.60) %>%
  mutate(percent60.2=paste(round(100 - percent60, digits=2), "% **", sep="")) %>% 
  full_join( # add in all of the provinces that did not make the cut
    world.surind.02 %>% 
      select(ctry, adm0guid) %>% ### since only using in geomtext, took out grouping by province. 
      unique()
  ) 

# Creating dataframe for onset to notification > 14 days for including in geom_text

world.14days.not <- f.stool.14.2(world.surind.02, ctry, adm0guid, ontonot.14) %>%
  mutate(percent14.1=paste(round(100 - percent14, digits=2), "% *", sep="")) %>% 
  full_join( # add in all of the provinces that did not make the cut
    world.surind.02 %>% 
      select(ctry, adm0guid) %>%
      unique()
  ) 

## Creating dataframe for missing stools

stool.missing <- f.missing.1(world.surind.02, ctry, adm0guid, stoolmissing) %>%
  mutate(percent2=paste(round(percent1, digits=2), "%", sep="")) %>% 
  full_join( # add in all of the provinces that did not make the cut
    world.surind.02 %>% 
      select(ctry, adm0guid) %>%  ## since only using for geom text, took out grouping by province. 
      unique()
  ) 

stool.missing$stoolmissing <- factor(stool.missing$stoolmissing, levels= c(0, 1), labels=c("missing", "not missing"))

#### Surveillance flag output for specific countries ###


flag.world.plot <- flag.world3 %>%
  mutate(prop.2=paste(round(prop.60.14, digits=2), "***", sep=" "))

# x <- paste("\n# ", ctryselect01, "\n", sep="" )
# cat(x)

p01  <- ggplot(data=world.surind.02 #%>% filter(ctry=="BANGLADESH"))
               ,
               aes(x=ontostool2)) +
  geom_bar(aes(y=..prop..)) +
  geom_vline(xintercept = 14) +
  #facet_wrap_paginate(~ctry, ncol = 2, nrow = 4, page=1) +
  scale_x_continuous('Days after onset',limits = c(-1,60)) +
  scale_y_continuous(limits = c(0.0,0.25)) +
  geom_text(data=(world.14days %>% filter(ctry=="BANGLADESH")), aes(x=20, y=0.15, label=percent2),
            color="red") +
  # ggtitle(if (flag.world.plot$flag.onset == 1) { ggtitle(" *Flagged for timeliness")
  # } else {ggtitle=("Not flagged for timeliness")}) +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle=0, size=10),
    # plot.title = (if (flag.world.plot$flag.onset == 1) { plot.title = element_text(color = "red")
    # } else {plot.title = element_text(color = "blue")}), 
    aspect.ratio = 0.5
  ) +
  labs(caption="* > 14 days onset to second stool") 

cat("\n## Days between onset and second stool\n")
print(p01)

##TIMELINESS Part B ##

p02  <- ggplot(data=(world.surind.02 %>%
                       filter(ontonot > 14) %>% filter(ctry=="CAMEROON")), aes(x=ontonot)) +
  geom_bar(aes(y=..prop..)) +
  geom_vline(xintercept = 14) +
  geom_vline(xintercept = 60) +
  facet_wrap_paginate(~ctry, ncol = 2, nrow = 4, page=1) +
  scale_x_continuous('days from onset to notification',limits = c(9,180)) +
  scale_y_continuous(limits = c(0.0,0.5)) +
  geom_text(data=(world.60days %>% filter(ctry=="CAMEROON")), aes(x=75, y=0.25, label=percent60.2),
            color="black") +
  geom_text(data=(world.14days.not %>% filter(ctry=="CAMEROON")), aes(x=30, y=0.40, label=percent14.1),
            color="red") +
  geom_text(data=(flag.world.plot %>% filter(ctry=="CAMEROON")), aes(x=120, y=0.10, label=prop.2),
            color="blue") +
  # ggtitle(if (flag.world.plot$flag.onset == 1) { ggtitle(" *Flagged for timeliness",)
  # } else {ggtitle=("Not flagged for timeliness")}) +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle=0, size=10),
    # plot.title = (if (flag.world.plot$flag.onset == 1) { plot.title = element_text(color = "red")
    # } else {plot.title = element_text(color = "blue")}), 
    aspect.ratio = 0.5 )
# ) +
# labs(caption="* > 14 days onset to notification, ** > 60 days onset to notification,\n *** cases >14 days / cases >60 days onset to notification\n if 0 cases >60 days proportion calculated by adding 1 case to both numerator and denominator")
# 
cat("\n## Cases with > 14 days and > 60 days between onset and notification\n")
print(p02)

##MISSING STOOL##

stool.missing.TZ <- stool.missing %>% filter(ctry=="UNITED REPUBLIC OF TANZANIA")

p03 <- stool.missing.TZ %>%
  ggplot(.,
         aes(x=as.character(ctry), y=nperarm, fill=stoolmissing, label=percent2)) +
  geom_col() +
  labs(x="Country", y="Count") +
  scale_y_continuous(limits = c(0, sum(stool.missing.TZ$nperarm + 50)), labels = comma) +
  scale_fill_manual(name="Percent of cases missing stool", values=color.stool.missing) +
  facet_wrap(~ctry) +
  # ggtitle(if (flag.world.plot$flag.missing == 1) { ggtitle(" *Flagged for missing stool",)
  # } else {ggtitle=("Not flagged for missing stool")}) +
  theme(
    panel.grid.minor=element_blank(),
    panel.background=element_rect(fill="white"),
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle=0, size=10),
    # plot.title = (if (flag.world.plot$flag.missing == 1) { plot.title = element_text(color = "red")
    # } else {plot.title = element_text(color = "blue")}), 
    aspect.ratio = 1.0
  ) +
  geom_text(size = 4, position = position_stack(vjust = 0.5))

cat("\\newpage")
cat("\n## Percent missing any stool\n")
print(p03)

## AGE FLAG ##
age.cat <- world.surind.02 %>% filter(ctry=="SOMALIA") %>%
  mutate(age_cat = cut(age.months,c(-1,11,23,35, 47, 59, 71, 83, 95, 107, 119, 131, 143, 155, 167, 179, Inf),
                       labels = c(0, 1,2,3,4, 5, 6, 7, 8, 9, 10,
                                  11, 12, 13, 14, 15))) %>%
  filter(age_cat!=15) %>%
  mutate(age_cat.2 = as.numeric(age_cat)) ## note - changed to numeric for plotting purposes. This adds + 1 to each value. Works for plotting, but for interpretation - 1 = < 1 year, 2 = < 2 year, etc. 

p04 <- age.cat %>% 
  ggplot(., aes(x=age_cat.2)) +
  geom_bar(aes(y=..prop..)) +
  geom_vline(xintercept = 5.5 ) +
  facet_wrap_paginate(~ctry, ncol = 2, nrow = 4, page=1) +
  scale_x_continuous('Years of age') +
  scale_y_continuous(limits = c(0.0,0.35)) +
  geom_text(data=(compare.age.5.15 %>% filter(ctry=="SOMALIA")), aes(x=8, y=0.15, label=ratio.1),
            color="red") +
  # ggtitle(if (flag.world.plot$flag.age == 1) { ggtitle(" *Flagged for age distribution",)
  # } else {ggtitle=("Not flagged for age distribution")}) +
  theme(
    axis.ticks.x = element_blank(),
    axis.text.x = element_text(angle=90, size=10),
    # plot.title = (if (flag.world.plot$flag.age == 1) { plot.title = element_text(color = "red")
    # } else {plot.title = element_text(color = "blue")}), 
    aspect.ratio = 0.5
  ) +
  labs(caption="* ratio < 5 years / >=5 years & < 15 yrs") 

cat("\\newpage")
cat("\n## Ratio of cases < 5 year vs. > 5 years and < 15 years\n")
print(p04)

####


sheet1=read_excel("Tables_Surv_Manu.xlsx",
                  skip=2)



# merge the flags and indicators together
flags.and.surv=sheet1 %>%  full_join(flag.world3,
                                     by=c("iso_3_code"="Admin0Iso3Code")) %>%
  filter(is.na(ctry)==F) %>% 
  mutate(flag.total = as.character(ifelse(is.na(flag.total),0,flag.total))) %>% 
  mutate(sn_group = paste0(flag.total,(targetSurv(`sub-national grouping`))),
         prov_group=paste0(flag.total,(targetSurv(`province-level`))),
         dist_group=paste0(flag.total,(targetSurv(`district-level`) )) ) %>% 
  mutate(sn_flag = targetSurv(`sub-national grouping`),
         prov_flag = targetSurv(`province-level`),
         dist_flag = targetSurv(`district-level`))
# 
# ## CHECK if there's any mismatch b/w arie's # of afp cases and ours
# flags.and.surv %>% mutate(npAFPmatch=ifelse(npAFPcases!=`No. of AFP cases (2017)`, "Mismatch", "Match")) %>%
#   mutate(difference=npAFPcases-`No. of AFP cases (2017)`) %>% filter(difference>0) %>% 
#   ggplot(aes(difference, fill=as.character(ctry))) +geom_histogram()


write.csv(flags.and.surv, "flag.sur.csv")

# merge the flags and indicators together

##########################################
#  PLOTS =================================
##########################################
## Plot of just province level grouping indicators


color.group <- c("<80" =  "#d7191c",
                 "=>80" = "#1a9641")

##Plot 1
## Note - previous code had fill="sn_flag" - changed to fill"prov_flag"
## Check with Sara ###
ggplot() +
  geom_map(data=flags.and.surv,
           aes_string(map_id = "adm0guid", fill="prov_flag"), 
           map=country.who.plot.ctry ,
           colour = color.dist.01, size = size.dist.01) +
  # geom_path(data=country.who.plot.state, aes(x=long, y=lat, group=group),
  #    colour = color.prov.01, size = size.prov.01) +
  geom_path(data=country.who.plot.ctry, aes(x=long, y=lat, group=group),
            colour = color.ctry.01, size = .1) + 
  theme(legend.position="none")+
  #scale_colour_brewer(palette = "RdBu") +
  scale_fill_manual(name="Flagged", values=color.group) +
  plotlooks01 +
  ggsave("prov_flag.png", units="in", width=5, height=8)



## Plot 2 - Over 80


color.surv.flag <- c("0" =  "#1a9641",
                     "1" = "#a6d96a",
                     "2"= "#ffffbf",
                     "3"="#fdae61",
                     "4" = "#d7191c") #,
# "0<80" = "#ffffb2" ,
# "1<80"  = "#fecc5c" ,
# "2<80" ="#fd8d3c" ,
# "3<80" = "#f03b20"  ,
# "4<80" = "#bd0026")


ggplot() +
  geom_map(data=flags.and.surv %>% filter(sn_flag!="<80"),
           aes_string(map_id = "adm0guid", fill="flag.total"), 
           map=country.who.plot.ctry ,
           colour = color.dist.01, size = size.dist.01) +
  # geom_path(data=country.who.plot.state, aes(x=long, y=lat, group=group),
  #    colour = color.prov.01, size = size.prov.01) +
  geom_path(data=country.who.plot.ctry, aes(x=long, y=lat, group=group),
            colour = color.ctry.01, size = .1) + 
  theme(legend.position="none")+
  #scale_colour_brewer(palette = "RdBu") +
  scale_fill_manual(name="Flagged", values=color.surv.flag) +
  plotlooks01 +
  ggsave("above_80_sn.png", units="in", width=5, height=8)

# Plot 3 - Under 80


color.surv.flag <- c("0" =  "#d7191c",
                     "1" = "#ac1416",
                     "2"= "#810f10",
                     "3"="#560a0b",
                     "4" = "#2b0505") 
ggplot() +
  geom_map(data=flags.and.surv %>% filter(sn_flag=="<80"),
           aes_string(map_id = "adm0guid", fill="flag.total"), 
           map=country.who.plot.ctry ,
           colour = color.dist.01, size = size.dist.01) +
  # geom_path(data=country.who.plot.state, aes(x=long, y=lat, group=group),
  #    colour = color.prov.01, size = size.prov.01) +
  geom_path(data=country.who.plot.ctry, aes(x=long, y=lat, group=group),
            colour = color.ctry.01, size = .1) + 
  theme(legend.position="none")+
  #scale_colour_brewer(palette = "RdBu") +
  scale_fill_manual(name="Flagged", values=color.surv.flag) +
  plotlooks01+
  ggsave("below_80_sn.png", units="in", width=5, height=8)


flag.world4 %>% ungroup() %>%  left_join(ctry.id.01 %>% ungroup())

ggplot() +
  geom_map(data=flags.and.surv,
           aes_string(map_id = "adm0guid", fill="prov_flag"), 
           map=country.who.plot.ctry ,
           colour = color.dist.01, size = size.dist.01) +
  # geom_path(data=country.who.plot.state, aes(x=long, y=lat, group=group),
  #    colour = color.prov.01, size = size.prov.01) +
  geom_path(data=country.who.plot.ctry, aes(x=long, y=lat, group=group),
            colour = color.ctry.01, size = .1) + 
  theme(legend.position="none")+
  #scale_colour_brewer(palette = "RdBu") +
  scale_fill_manual(name="Flagged", values=color.group) +
  plotlooks01 +
  ggsave("prov_flag.png", units="in", width=5, height=8)

