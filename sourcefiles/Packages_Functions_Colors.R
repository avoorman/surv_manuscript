
#Defining factor levels for Non Polio AFP

#Function classification of AFP
f.npclass01 <- function(y,z) {
  x = NA
  x = ifelse(str_detect(y,"wild"),
             'WPV', x)
  x = ifelse(str_detect(y,'Compatible'),
             'Compatible',x)
  x = ifelse(str_detect(y,'Not an AFP'), 
             'Not an AFP',x)
  x = ifelse((str_detect(y,'VDPV')) & (str_detect(z,'Circulating')), 
             'cVDPV', x)
  x = ifelse((str_detect(y,'VDPV')) & (str_detect(z,'Immune Deficient')), 
             'iVDPV', x)
  x = ifelse((str_detect(y,'VDPV')) & (str_detect(z,'Ambiguous')), 
             'aVDPV', x)
  x = ifelse((str_detect(y,'Discarded')) | (str_detect(y,'Pending')) | (is.na(y)), 
             'NPAFP', x)
  
  return(x)
}

#Adm03 is ggplot fortify dataframe at level of province
f.adm03 <- function(k,p,y="GUID") {
  eq.02 <- "="
  reg <- "region"
  
  m <- tidy(k, match.fun(eq.02) (k[[reg]],y))
  n <- m[order(m$order), ]
  
  p.02 <-  p %>%
    left_join(n,.,by=c("id"="adm0guid")) 
  
}


## missing dose history functions ##
## changed cut-off to be 100 (not 250 like flags) ###


f.total.doses.na <- function(y) {
  x <- NA
  x[y==0] <- 1
  x[y>=1 & y<3] <- 2
  x[y>=3] <- 3
  x[is.na(y)] <- 4
  
  x <-  factor(x, 
               levels=c(1,2,3,4),
               labels=c("0 doses", "1-2 doses", ">=3 doses","missing"))
  
  return(x)
}

f.total.doses <- function(y) {
  x <- NA
  x[y==0] <- 1
  x[y>=1 & y<3] <- 2
  x[y>=3] <- 3
  
  x <-  factor(x, 
               levels=c(1,2,3),
               labels=c("0 doses", "1-2 doses", ">=3 doses"))
  
  return(x)
}


f.miss.dose <- function(df01, ...) { 
  group_by <- quos(...)
  
  df02 <- df01 %>%
    group_by(!!!group_by) %>%
    summarise_(n = ~n(), missing_dose = ~sum(doses.missing == 0) ) %>%
    mutate_(missing_dose=~replace(missing_dose, is.na(missing_dose), 0)) %>% 
    filter_(~n >100)  %>%
    rowwise %>%
    mutate_(percent =~((100*missing_dose)/((n)))) %>%
    #select_(~-missing_dose, ~-n) %>%   # ORIGINAL: select(-missed_collection, -n) %>%
    mutate_(percent.miss.dose=~percent) %>%
    select_(~-percent)
  
}

f.no.dose <- function(df01, ...) { 
  group_by <- quos(...)
  
  df02 <- df01 %>%
    group_by(!!!group_by) %>%
    summarise_(n = ~n(), no_dose = ~sum(no.doses == 1)) %>%
    mutate_(no_dose=~replace(no_dose, is.na(no_dose), 0)) %>%
    filter_(~n >100)  %>%
    rowwise %>%
    mutate_(percent =~((100*no_dose)/((n)))) %>%
    #select_(~-no_dose, ~-n) %>%   # ORIGINAL: select(-missed_collection, -n) %>%
    mutate_(percent_nodose=~percent) %>%
    select_(~-percent)
  
}

f.full.dose <- function(df01, ...) { 
  group_by <- quos(...)
  
  df02 <- df01 %>%
    group_by(!!!group_by) %>%
    summarise_(n = ~n(), full_dose = ~sum(full.doses == 1) ) %>%
    mutate_(full_dose=~replace(full_dose, is.na(full_dose), 0)) %>% ### if no full doses
    filter_(~n >100)  %>%
    rowwise %>%
    mutate_(percent =~((100*full_dose)/((n)))) %>%
    #select_(~-no_dose, ~-n) %>%   # ORIGINAL: select(-missed_collection, -n) %>%
    mutate_(percent_fulldose=~percent) %>%
    select_(~-percent)
  
}


f.missing.dose.percent <- function(y) {
  x <- NA
  x[y==0] <- 1
  x[y > 0 & y<10] <- 2
  x[y>=10 & y<20] <- 3
  x[y>=20 & y<30] <- 4
  x[y>=30] <- 5
  x[y=="NaN"] <- 6
  
  x <-  factor(x, 
               levels=c(1,2,3,4,5,6),
               labels=c("0%", "<10%", "10 - 19%", "20 - 29%", ">=30%", "NA"))
  
  return(x)
}

f.total.dose.1 <- function(df01, ... ) { 
  group_by <- quos(...)
  
  df03 <- df01 %>%
    group_by(!!!group_by) %>%
    summarise_(nperarm=~n()) %>%
    mutate_(percent1=~((100*nperarm)/(sum(nperarm)))) 
  
}

## merging data from all flags into one dataframe, creating flag cut-offs. 

f.merge.all <- function(df01, df02, df03, df04) { 
  
  df03 <- df01 %>%
    full_join(., df02) %>%
    mutate_(percent.missing=~round(percent_missing, digits=2), percent.14=~round(percent_14days, digits=2),
            percent.60=~round(percent_60days, digits=2))%>%
    full_join(., df03) %>%
    mutate_(percent.late.onset =~round(percent_onset, digits=2)) %>%
    select(-percent_missing, -percent_14days, -percent_60days, -percent_onset, -n) %>%
    full_join(., df04) %>%
    select(-age.5, -age.15, -total.eligible.n) %>%
    mutate_(flag.missing =~NA,
            flag.missing=~if_else((percent.missing<=0.30), 1, 0, 0), 
            flag.timeliness=~NA,
            flag.timeliness=~if_else((percent.late.onset<=3.0), 1, 0, 0),
            flag.late.notification=~NA,
            flag.late.notification=~if_else(prop.60.14 <=0.03, 1, 0, 0),  
            flag.age=~NA,
            flag.age=~if_else((ratio>=4.0), 1, 0, 0)) %>%
    #mutate_(flag.late.notification=~replace(flag.late.notification, late_not_14==0, 1) ) %>% 
    mutate_(flag.total = ~sum(flag.missing + flag.timeliness + flag.late.notification + flag.age)) 
}


### Function for incorporating all flags, including new flags (df05)

f.merge.all.new <- function(df01, df02, df03, df04, df05) { 
  
  df03 <- df01 %>%
    full_join(., df02) %>%
    mutate_(percent.missing=~round(percent_missing, digits=2), percent.14=~round(percent_14days, digits=2),
            percent.60=~round(percent_60days, digits=2))%>%
    full_join(., df03) %>%
    mutate_(percent.14.st2 =~round(percent_onset, digits=2)) %>%
    select(-percent_missing, -percent_14days, -percent_60days, -percent_onset, -n) %>%
    full_join(., df04) %>%
    select(-age.5, -age.15, -total.eligible.n) %>%
    full_join(., df05) %>%
    select(-missing_dose, -n) %>%
    mutate_(flag.missing =~NA,
            flag.missing=~if_else((percent.missing<=0.30), 1, 0, 0), 
            flag.onset=~NA,
            flag.onset=~if_else((percent.14.st2<=3.0), 1, 0, 0),
            flag.onset=~if_else(prop.14.60 >=30.0 & prop.14.60 != Inf, 1, flag.onset, flag.onset),
            flag.age=~NA,
            flag.age=~if_else((ratio>=4.0), 1, 0, 0),
            flag.dose=~NA,
            flag.dose=~if_else((percent.miss.dose >=10.0), 1, 0, 0)) %>%
    mutate_(flag.total = ~sum(flag.missing + flag.onset + flag.age + flag.dose)) 
}


### Function for creating dataframe for onset to stool 2 > 14 days for including in geom_text

f.stool.14.1 <- function(df01, ... ) { 
  group_by <- quos(...)
  
  df02 <- df01 %>%
    filter(!is.na(ontostool2.03)) %>%  
    group_by(!!!group_by) %>%
    summarise_(nperarm=~n()) %>%
    mutate_(percent1=~((100*nperarm)/(sum(nperarm)))) 
} 


### Function for creating dataframe for onset to notification > 60 days for including in geom_text

f.stool.60.1 <- function(df01, ... ) { 
  group_by <- quos(...)
  
  df03 <- df01 %>%
    filter(!is.na(ontonot.60)) %>%
    group_by(!!!group_by) %>%
    summarise_(nperarm=~n()) %>%
    mutate_(percent60=~((100*nperarm)/(sum(nperarm)))) %>%
    filter(ontonot.60=="<=60")
} 

### Function for creating dataframe for onset to notification > 14 days for including in geom_text


f.stool.14.2 <- function(df01, ... ) { 
  group_by <- quos(...)
  
  df03 <- df01 %>%
    filter(!is.na(ontonot.14)) %>%
    group_by(!!!group_by) %>%
    summarise_(nperarm=~n()) %>%
    mutate_(percent14=~((100*nperarm)/(sum(nperarm)))) %>%
    filter(ontonot.14=="<=14") 
  
} 

## missing stool for geomtext

f.missing.1 <- function(df01, ... ) { 
  group_by <- quos(...)
  
  df03 <- df01 %>%
    filter(ontonot <=60) %>%
    group_by(!!!group_by) %>%
    summarise_(nperarm=~n()) %>%
    mutate_(percent1=~((100*nperarm)/(sum(nperarm)))) 
  
}


f.miss.stool <- function(df01, ...) { 
  group_by <- quos(...)
  
  df02 <- df01 %>%
    group_by(!!!group_by) %>%
    filter(ontonot <= 60) %>%
    summarise_(n = ~n(), missed_collection = ~sum(stool2missing == 0 | stool1missing == 0) ) %>%
    filter_(~n >=250)  %>%
    rowwise %>%
    mutate_(percent =~((100*missed_collection)/((n)))) %>%
    select_(~-missed_collection, ~-n) %>%   # ORIGINAL: select(-missed_collection, -n) %>%
    mutate_(percent_missing=~percent) %>%
    select_(~-percent)
  
}



### Onset to stool2 > 14 days, areas with >250 cases 

f.stool.14days <- function(df01, ...) { 
  group_by <- quos(...)
  
  df02 <- df01 %>%
    group_by(!!!group_by) %>%
    filter(ontostool2 >=0) %>%
    mutate_(ontostool2.03=~NA,
            ontostool2.03=~ifelse(ontostool2>14, ">14", ontostool2.03),
            ontostool2.03=~ifelse(ontostool2<=14, "<=14", ontostool2.03)) %>%
    filter_(~!is.na(ontostool2.03)) %>%
    summarise_(n = ~n(), late_onset = ~sum(ontostool2.03==">14")) %>%
    filter_(~n >= 250) %>%
    rowwise %>%
    mutate_(percent =~((100*late_onset)/((n)))) %>%
    select_(~-late_onset, ~-n) %>%
    mutate_(percent_onset=~percent) %>%
    select_(~-percent)
  
}


### dataframe creation onset to notification > 14 days, areas with > 250 cases 

f.14days.notify <- function(df01, ...) { 
  group_by <- quos(...)
  
  df02 <- df01 %>%
    group_by(!!!group_by) %>%
    filter(ontonot>=0) %>%
    mutate_(ontonot.14=~NA,
            ontonot.14=~ifelse(ontonot>14, ">14", ontonot.14),
            ontonot.14=~ifelse(ontonot<=14, "<=14", ontonot.14))  %>%
    filter_(~!is.na(ontonot.14)) %>%
    summarise_(n = ~n(), late_not_14 = ~sum(ontonot.14==">14") ) %>%
    filter_(~n >= 250) %>%
    rowwise %>%
    mutate_(percent =~((100*late_not_14)/((n)))) %>%
    mutate_(percent_14days=~percent) 
  
}

#### dataframe creation onset to notification > 60 days, areas with > 250 cases

f.60days.notify <- function(df01, ...) { 
  group_by <- quos(...)
  
  df02 <- df01 %>%
    group_by(!!!group_by) %>%
    filter(ontonot>=0) %>%
    mutate_(ontonot.60=~NA,
            ontonot.60=~ifelse(ontonot>60, ">60", ontonot.60),
            ontonot.60=~ifelse(ontonot<=60, "<=60", ontonot.60))  %>%
    filter_(~!is.na(ontonot.60)) %>%
    summarise_(n = ~n(), late_not_60 = ~sum(ontonot.60==">60") ) %>%
    filter_(~n >= 250) %>%
    rowwise %>%
    mutate_(percent =~((100*late_not_60)/((n)))) %>%
    rename_(percent_60days=~percent) 
  
}

#### data frame to merge notification variables

f.merge.notify <- function(df01, df02) { 
  
  df03 <- df01 %>%
    full_join(., df02) %>%
    mutate_(prop.60.14 =~(late_not_60/late_not_14)) 
}


### functions for age flag###
f.age.5 <- function(df01, ...) { 
  group_by <- quos(...)
  
  
  df02 <- df01 %>%
    filter(age.months < 60) %>%
    group_by(!!!group_by) %>%
    summarize_(n =~n()) %>% 
    rename_(age.5=~n) %>%
    select(!!!group_by, age.5) 
}

#world.surind.02, 
# ctry
# adm0guid

f.age.15 <- function(df01, ...) { 
  group_by <- quos(...)
  
  df02 <- df01 %>%
    mutate(age.months=as.numeric(age.months)) %>% 
    filter(age.months >= 60 & age.months <180) %>%
    group_by(!!!group_by) %>%
    summarize_(n =~n()) %>% 
    rename_(age.15=~n) %>%
    select(!!!group_by, age.15) 
  
}

f.merge.age <- function(df01, df02) { 
  
  df03 <- df01 %>%
    full_join(., df02) %>%
    mutate_(ratio =~(age.5/age.15)) %>%
    mutate_(total.eligible.n =~ sum(age.5,age.15,na.rm=T)) %>%
    filter_(~total.eligible.n > 250) 
  
}

targetSurv = function(x){
  ifelse(x<.8, "<80","=>80")
}

## colors ====


# color.shape.01 <- c("ENV"=17, "AFP"=16)
# color.country.01 <- c("black")
color.ctry.01 <- c("black")
color.prov.01 <-c("black")
color.dist.01 <-c("black")
# color.teh.01 <- c("light grey")
# color.small.01 <-c("orange")
color.ES <-c("ENV" ="springgreen2")
# 
# ### Size for lines and shapes
# size.country.01 <-c(1.5)
size.ctry.01 <-c(1.0) 
size.prov.01 <-c(0.3)
size.dist.01 <-c(0.1)

# #Includes plot looks with 
plotlooks01 <-  list(
  coord_map(),
  theme(panel.grid.minor=element_blank(),
        panel.background=element_rect(fill="white"),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        axis.ticks.x=element_blank(),
        axis.ticks.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        legend.background = element_rect(colour = 'black', fill = 'white', linetype='solid') # add a border to the legend
  )
)


