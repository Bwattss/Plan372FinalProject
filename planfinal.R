setwd("C:/Users/bwatt/OneDrive/Desktop/plan final data")
setwd("C:/Users/bwatt/OneDrive/Desktop/plan final data/SLD_Trans45_DBF")
library(foreign)
library(tidyverse)
#readig in EPA tool data
data = read.dbf("SLD_Trans45.dbf")

#Filtering EPA tool data to get the 10 cities I need 
area10 = data %>% filter(
  CBSA == 47260|CBSA == 44060|CBSA == 17140 |CBSA == 24340|CBSA == 31540
  |CBSA == 17460|CBSA == 38300|CBSA == 35620|CBSA == 25540|CBSA == 41620 )
  
unique(area10$CBSA_Name)

#read in my city data
setwd("C:/Users/bwatt/OneDrive/Desktop/plan final data/builtenvironment10")
spokane = read.csv("spokane.csv")
saltlakecity = read.csv("Salt Lake City.csv")
pittsburg = read.csv("Pittsburg.csv")
norfolk = read.csv("Norfolk.csv")
madison = read.csv("Madison.csv")
hartford = read.csv("Hartford.csv")
cleaveland = read.csv("cleavland.csv")
cincinnati = read.csv("Cincinnati.csv")


#I want to look at all the use types so i can count how much is residential 
spokaneuses = spokane %>% group_by(Landuse) %>% count()
cininnatiUses = cincinnati %>% group_by(ZONE_TYPE) %>% count()
saltlakeUses = saltlakecity %>% group_by(ZONE_DESCRIPTION) %>% count()
pittsburgUses = pittsburg %>% group_by(full_zoning_type) %>% count()
madisonUses = madison %>% group_by(ZONING_CODE) %>% count()
hartfordUses = hartford %>% group_by(LABEL) %>% count()
cleavelandUses = cleaveland %>% group_by(ZONE_USE) %>% count()
norfolkuses = norfolk %>% group_by(TYPE) %>% count()

#ok so now im gonna add up the total amount of uses in each city 

sum(cleavelandUses$n)
sum(saltlakeUses$n)
sum(pittsburgUses$n)
sum(spokaneuses$n)
sum(madisonUses$n)
sum(hartfordUses$n)
sum(cininnatiUses$n)
sum(norfolkuses$n)


#i want to add a column with use percentages 
area10 <- area10 %>%
  mutate(pct_resi = ( with(area10, ifelse(CBSA == 25540, .64,
              ifelse(CBSA == 44060, .397, 
              ifelse(CBSA == 17140, .418,
              ifelse(CBSA == 31540, .413,
              ifelse(CBSA == 17460, .321,
              ifelse(CBSA == 38300, .122,
              ifelse(CBSA == 41620, .366,
              ifelse(CBSA == 47260, .654,
                                                                                                       
                         1)))))))))))



colnames(area10)
area10$pct_resi

#ok consolidation 
social = read.csv("socialll.csv")

area10 = rename(area10, Geo_FIPS = CBG_ID)
area10 = left_join(area10, social, by = "Geo_FIPS")

area10 = rename(area10, testy = SE_T003_000)

#ok now im filtering out the variables I don't need
area10 = area10 %>% drop_na(testy)

#density
area10 = rename(area10, Density = SE_T003_001)

#average commute time 
area10 <- area10 %>%
mutate(traffic = ( with(area10, ifelse(CBSA == 25540, 22,
                               ifelse(CBSA == 44060, 21, 
                               ifelse(CBSA == 17140, 23,
                               ifelse(CBSA == 31540, 20,
                               ifelse(CBSA == 17460, 23,
                               ifelse(CBSA == 38300, 24,
                               ifelse(CBSA == 41620, 19,
                               ifelse(CBSA == 47260, 22,
                                      
                                      1)))))))))))
#renaming the variable I need
area10 = rename(area10, jobs = Pct_Jobs_b)

#time to consolidate 
regressdata = area10 %>% select(CBSA_Name, CBSA, jobs, traffic, Density, pct_resi)

#ok so hartford connecicut is getting kicked out because it is empty 
regressdata = regressdata %>% filter(pct_resi != .64 )

#ok I think I'm ready for my regression finally!

regression = lm(jobs ~ traffic + Density + pct_resi,data = regressdata)

summary(regression)

#doinf all the seps above but for durham

durha = read.csv("durhamzonesss.csv")
durhamuses = durha %>% group_by(ZONE_GEN) %>% count()
sum(durhamuses$n)

durhamm = read.csv("pls.csv")

durhamm = rename(durhamm, Density = SE_A00002_002)

durhamm = durhamm %>% mutate(
  pct_resi = (with(durhamm, ifelse(Geo_STATE == 37, .52, 1))))

durhamm = durhamm %>% mutate(
  traffic = (with(durhamm, ifelse(Geo_STATE == 37, 23, 1))))

durham = durhamm %>% select(Geo_QName, Density, traffic, pct_resi)

#So I have my table and i believe that I can predict now 
durham$predict = predict(regression, durham)
 
mean(durham$predict)
mean(regressdata$jobs)
mean(data$Pct_Jobs_b)
mean(durham$Density)
mean(regressdata$pct_resi)


#renaming things really quick 
regressdata <- regressdata %>%
  mutate(City = ( with(regressdata, ifelse(CBSA == 25540, "Hartford",
                                    ifelse(CBSA == 44060, "Spokane", 
                                    ifelse(CBSA == 17140, "Cincinnati",
                                    ifelse(CBSA == 31540, "Madison",
                                    ifelse(CBSA == 17460, "Cleaveland",
                                    ifelse(CBSA == 38300, "Pittsburg",
                                    ifelse(CBSA == 41620, "Salt Lake City",                                             ifelse(CBSA == 47260, "Norfolk",
                                                                                        
                                                               1)))))))))))

#making my plot
plotb = regressdata %>% group_by(City) %>% summarize(jobs = mean(jobs))
jobs = as.numeric(plotb$jobs)



ggplot(plotb, aes(x = City, y = jobs)) + geom_bar(stat = "identity")



