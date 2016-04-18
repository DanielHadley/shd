# Created By Daniel Hadley Mon Apr 18 12:39:18 EDT 2016 #
setwd("/Users/dphnrome/Documents/Git/shd")

library(dplyr)

# Load Data
raw_d <- read.csv("././Somerville_Happiness_Survey_responses_-_2011__2013__2015.csv")



#### Clean data ####
# I want to focus on 2011, but this does not seem to have all of the data from then
d <- raw_d %>%
  filter(Year != 2011)

# Convert to numeric
for(i in c(3:26)) {
  d[,i] <- as.numeric(as.character(d[,i]))
}

# round everything
d[,3:26] <-round(d[,3:26],0)


# get rid of the strange outliers (only in 2011)
# d[d > 10,3:26] <- NA



#### Model the data ####

d_for_model <- d %>%
  
  # First combine two columns into one 
  mutate(Safety = paste(How.safe.do.you.feel.walking.in.your.neighborhood.at.night_2013, 
                        How.safe.do.you.feel.walking.in.your.community.at.night._2015)) %>%
  mutate(Safety = gsub(" NA", "", Safety)) %>%
  mutate(Safety = gsub("NA ", "", Safety)) %>%
  mutate(Safety = as.numeric(as.character(Safety))) %>%
  rename(How.safe.do.you.feel.walking.in.your.neighborhood.at.night = Safety) %>%
  
  # First combine two columns into one 
  mutate(Parks = paste(How.satisfied.are.you.with.the.appearance.of.parks.in.your.neighborhood._2013, 
                       How.satisfied.are.you.with.the.appearance.of.parks.and.squares.in.your.neighborhood.  )) %>%
  mutate(Parks = gsub(" NA", "", Parks)) %>%
  mutate(Parks = gsub("NA ", "", Parks)) %>%
  mutate(Parks = as.numeric(as.character(Parks))) %>%
  rename(How.satisfied.are.you.with.the.appearance.of.parks.in.your.neighborhood = Parks) %>%
  
  ## Drop variables that aren't helpful for model
  select(-Combined_ID, -Year, -How.satisfied.are.you.with.your.life.in.general.,
         -In.general..how.similar.are.you.to.other.people.you.know._2011, 
         -When.making.decisions..are.you.more.likely.to.seek.advice.or.decide.for.yourself._2011.,
         -How.proud.are.you.to.be.a.Somerville.resident._2015 : 
           -The.availability.of.affordable.housing_2011,
         # Seriously reduces degrees of freedom b/c NAs
         # -How.would.you.rate.the.following..The.overall.quality.of.public.schools.,
         -How.would.you.rate.the.following..The.overall.quality.of.public.schools.in.your.community._2011,
         -How.would.you.rate.the.following..The.beauty.or.physical.setting_2011 :
           -How.would.you.rate.the.following..The.maintenance.of.streets.and.sidewalks_2015,
         -How.safe.do.you.feel.walking.in.your.neighborhood.at.night_2013,
         -How.safe.do.you.feel.walking.in.your.community.at.night._2015,
         -How.satisfied.are.you.with.the.appearance.of.parks.in.your.neighborhood._2013,
         -How.satisfied.are.you.with.the.appearance.of.parks.and.squares.in.your.neighborhood.,
         -What.is.your.gender._2011,
         -Marital.status._2011,
         -What.language..other.than.English..do.you.speak.at.home._2015,
         -What.is.your.race_2011_2013,
         -Are.you.of.Hispanic..Latino..or.Spanish.origin._2013,
         -Describe.your.housing.status.in.Somerville.,
         -What.neighborhood.do.you.live.in.,
         -Ward,
         -Precinct,
         -What.is.your.race.or.ethnicity._2015
         ) %>%
  
  ## Clean up
  mutate(Do.you.have.children.age.18.or.younger.who.live.with.you. = 
           tolower(Do.you.have.children.age.18.or.younger.who.live.with.you.),
         Do.you.plan.to.move.away.from.Somerville.in.the.next.two.years. = 
           tolower(Do.you.plan.to.move.away.from.Somerville.in.the.next.two.years.),
         Are.you.a.student. = 
           tolower(Are.you.a.student.))




reg <- lm(How.happy.do.you.feel.right.now. ~ ., data = d_for_model)
summary(reg)

reg_tidy <- tidy(reg)
write.csv(reg_tidy, "./happiness_reg.csv")


reg <- lm(How.satisfied.are.you.with.your.neighborhood. ~ ., data = d_for_model)
summary(reg)

reg_tidy <- tidy(reg)
write.csv(reg_tidy, "./neighborhood_reg.csv")
