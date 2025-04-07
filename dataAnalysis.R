# load libraries
library(readxl)
library(tidyverse)
library(ggplot2)
library(car)

##### Data Cleaning #####
# load all datasets
nces_schools <- read_xlsx("CollegeNavigator-MAColleges.xlsx")
car_schools <- read_xlsx("MA LGBT College Organizations.xlsx", sheet = "MA Schools")
lgbt_orgs <- read_xlsx("MA LGBT College Organizations.xlsx", sheet = "LGBT Orgs")

# get rid of last rows in college navigator data
nces_schools <- filter(nces_schools, !is.na(ID))

# merge school datasets
ma_schools <- left_join(car_schools, nces_schools, join_by(`School ID`==ID))

# keep and rename certain columns
ma_schools <- select(ma_schools, `School ID`, `School Name`, `City`, `County`, `Level`, `Public`,
                     `Enrollment Profile`, `Size & Setting`, `PctMen`, `Religion`, `Pct White`,
                     `Total Clubs`, `Type.y`, `Campus setting`, `Campus housing`,
                     `Student population`, `Undergraduate students`, `Graduation Rate`,
                     `Transfer-Out Rate`, `Net Price **`)
ma_schools <- rename(ma_schools, ID = `School ID`, Name = `School Name`, PctWhite = `Pct White`,
                     TotalClubs = `Total Clubs`, Setting = `Campus setting`, Housing = `Campus housing`,
                     NumStudents = `Student population`, NumUndergrad = `Undergraduate students`,
                     GradRate = `Graduation Rate`, TransOutRate = `Transfer-Out Rate`,
                     NetPrice = `Net Price **`)

# create new versions of certain columns
ma_schools <- mutate(ma_schools,
                     Community = ifelse(Level == "At least 2 but less than 4 years", 1, 0),
                     Public = ifelse(Public == "Public", 1, 0),
                     Graduate = case_when(
                       `Enrollment Profile` == "Exclusively undergraduate four-year" |
                         `Enrollment Profile` == "Exclusively undergraduate two-year" ~
                         "Exclusively undergraduate",
                       `Enrollment Profile` == "High undergraduate" |
                         `Enrollment Profile` == "Majority undergraduate" |
                         `Enrollment Profile` == "Majority graduate" |
                         `Enrollment Profile` == "Very high undergraduate" ~
                         "Undergraduate and graduate",
                       TRUE ~ as.character(`Enrollment Profile`)
                     ),
                     Housing = ifelse(Housing == "Yes", 1, 0)
                     )
ma_schools <- mutate(ma_schools, Graduate = factor(Graduate, levels = c("Exclusively undergraduate",
                                                                        "Undergraduate and graduate",
                                                                        "Exclusively graduate"),
                                                   ordered = TRUE))

# further column adjustments
ma_schools <- select(ma_schools, ID, Name, City, County, Public, PctMen, Religion, PctWhite, TotalClubs,
                     Setting, Housing, NumStudents, GradRate, NetPrice, Community, Graduate)
ma_schools <- rename(ma_schools, Christian = Religion)
ma_schools <- mutate(ma_schools, 
                     Christian = ifelse(Christian == "No religion", 0, 1),
                     Setting = case_when(
                       Setting == "Rural: Fringe" ~ "Rural",
                       Setting == "Suburb: Large" | Setting == "Suburb: Midsize" ~ "Suburb",
                       Setting == "Town: Distant" | Setting == "Town: Fringe" ~ "Town",
                       TRUE ~ as.character(Setting)),
                     GradRate = na_if(GradRate, "NA"),
                     NetPrice = na_if(NetPrice, "NA"))
ma_schools <- mutate(ma_schools, 
                     GradRate = as.numeric(str_sub(GradRate, 1, 2)),
                     NetPrice = str_sub(NetPrice, 2),
                     NetPrice = as.numeric(str_remove(NetPrice, ",")))

# count number of LGBT organizations at each school
lgbt_orgs <- filter(lgbt_orgs, `Club Name` != "NA")
numlgbtorgs <- lgbt_orgs |> group_by(`School ID`) |> summarize(count = n())

# add LGBT organization data to school data
ma_colleges <- left_join(ma_schools, numlgbtorgs, join_by(ID==`School ID`))
ma_colleges <- rename(ma_colleges, LGBTClubs = count)

# make final column adjustments
ma_colleges <- mutate(ma_colleges,
                      LGBTClubs = ifelse(is.na(LGBTClubs), 0, LGBTClubs),
                      PctWhite = as.integer(PctWhite),
                      MARegion = case_when(
                        County == "Berkshire" | County == "Franklin" |
                          County == "Hampshire" | County == "Hampden" ~ "West",
                        County == "Worcester" ~ "Central",
                        County == "Essex" | County == "Middlesex" | 
                          County == "Suffolk" ~ "Northeast",
                        County == "Norfolk" | County == "Bristol" |
                          County == "Plymouth" | County == "Barnstable" ~ "Southeast"
                      ))
ma_colleges <- mutate(ma_colleges,
                      MARegion = factor(MARegion, levels = c("West", "Central",
                                                             "Northeast", "Southeast"), 
                                        ordered = TRUE),
                      Setting = factor(Setting, levels = c("Rural", "Town", "Suburb",
                                                           "City: Small", "City: Midsize",
                                                           "City: Large"), ordered = TRUE))

# remove unnecessary data frames
rm(car_schools)
rm(ma_schools)
rm(nces_schools)
rm(numlgbtorgs)

##### Data Exploration #####
summary(ma_colleges)
# ID              Name               City               County             Public      
# Min.   : 1.00   Length:92          Length:92          Length:92          Min.   :0.0000  
# 1st Qu.:23.75   Class :character   Class :character   Class :character   1st Qu.:0.0000  
# Median :46.50   Mode  :character   Mode  :character   Mode  :character   Median :0.0000  
# Mean   :46.50                                                            Mean   :0.3152  
# 3rd Qu.:69.25                                                            3rd Qu.:1.0000  
# Max.   :92.00                                                            Max.   :1.0000                                                                       
# PctMen          Christian       PctWhite        TotalClubs      Setting            Housing      
# Min.   : 0.00   Min.   :0.000   Min.   : 4.00   Min.   :  1.0   Rural        : 5   Min.   :0.0000  
# 1st Qu.:34.00   1st Qu.:0.000   1st Qu.:39.75   1st Qu.: 19.0   Town         : 3   1st Qu.:0.0000  
# Median :41.50   Median :0.000   Median :52.00   Median : 48.0   Suburb       :35   Median :1.0000  
# Mean   :41.09   Mean   :0.163   Mean   :50.66   Mean   :101.7   City: Small  :10   Mean   :0.7065  
# 3rd Qu.:48.00   3rd Qu.:0.000   3rd Qu.:64.00   3rd Qu.:147.0   City: Midsize:16   3rd Qu.:1.0000  
# Max.   :86.00   Max.   :1.000   Max.   :83.00   Max.   :642.0   City: Large  :23   Max.   :1.0000                                                                                                 
# NumStudents     GradRate        NetPrice        Community       Graduate 
# Min.   :   33   Min.   :10.00   Min.   : 5573   Min.   :0.000   Exclusively undergraduate :27  
# 1st Qu.: 1386   1st Qu.:43.50   1st Qu.:17271   1st Qu.:0.000   Undergraduate and graduate:60  
# Median : 2624   Median :62.00   Median :26469   Median :0.000   Exclusively graduate      : 5  
# Mean   : 4911   Mean   :58.89   Mean   :25365   Mean   :0.163                                  
# 3rd Qu.: 5379   3rd Qu.:78.50   3rd Qu.:33100   3rd Qu.:0.000                                  
# Max.   :36624   Max.   :97.00   Max.   :51986   Max.   :1.000                                  
#                 NA's   :9       NA's   :9                                                      
# LGBTClubs       MARegion 
# Min.   :0.000   West     :17  
# 1st Qu.:1.000   Central  :11  
# Median :1.000   Northeast:46  
# Mean   :1.435   Southeast:18  
# 3rd Qu.:1.250

# univariate visualizations
hist(ma_colleges$Public)
hist(ma_colleges$PctMen)
hist(ma_colleges$Christian) # mostly 0s
hist(ma_colleges$PctWhite)
hist(ma_colleges$TotalClubs) # highly skewed right
hist(ma_colleges$Housing)
hist(ma_colleges$NumStudents) # highly skewed right
hist(ma_colleges$GradRate)
hist(ma_colleges$NetPrice)
hist(ma_colleges$Community) # mostly 0s
hist(ma_colleges$LGBTClubs) # highly skewed right

# log transform skewed variables
ma_colleges <- mutate(ma_colleges,
                      TotalClubsLog = log(TotalClubs),
                      NumStudentsLog = log(NumStudents),
                      LGBTClubsLog = log(LGBTClubs+1))
hist(ma_colleges$TotalClubsLog)
hist(ma_colleges$NumStudentsLog)
hist(ma_colleges$LGBTClubsLog)

##### Regression Modelling #####
mod <- lm(LGBTClubsLog ~ MARegion + Public + PctMen + Christian + PctWhite + Setting +
            Housing + GradRate + NetPrice + Community + Graduate + NumStudentsLog + TotalClubsLog,
          data = ma_colleges)
summary(mod)

# make scatterplots with numeric variables
# pct men
ggplot(ma_colleges, aes(x = PctMen, y = LGBTClubs)) + geom_point()
# pct white
ggplot(ma_colleges, aes(x = PctWhite, y = LGBTClubs)) + geom_point()
# grad rate
ggplot(ma_colleges, aes(x = GradRate, y = LGBTClubs)) + geom_point()
# net price
ggplot(ma_colleges, aes(x = NetPrice, y = LGBTClubs)) + geom_point()
# num students
ggplot(ma_colleges, aes(x = NumStudentsLog, y = LGBTClubs)) + geom_point()
# total clubs
ggplot(ma_colleges, aes(x = TotalClubsLog, y = LGBTClubs)) + geom_point()

# make boxplots with non-binary categorical variables
# MA region
ggplot(ma_colleges, aes(x = MARegion, y = LGBTClubs)) + geom_boxplot()
# setting
ggplot(ma_colleges, aes(x = Setting, y = LGBTClubs)) + geom_boxplot()
# graduate
ggplot(ma_colleges, aes(x = Graduate, y = LGBTClubs)) + geom_boxplot()

# make boxplots with binary categorical variables
# public
ggplot(ma_colleges, aes(x = as.factor(Public), y = LGBTClubs)) + geom_boxplot()
# christian
ggplot(ma_colleges, aes(x = as.factor(Christian), y = LGBTClubs)) + geom_boxplot()
# housing
ggplot(ma_colleges, aes(x = as.factor(Housing), y = LGBTClubs)) + geom_boxplot()
# community
ggplot(ma_colleges, aes(x = as.factor(Community), y = LGBTClubs)) + geom_boxplot()

# check for multicollinearity in model
vif(mod)

# examine correlations between all "numeric" variables
cor(select(ma_colleges, LGBTClubs, Christian, Community, GradRate, Housing, NetPrice, 
           NumStudents, PctMen, PctWhite, Public, TotalClubs))

##### Analyzing LGBT Student Orgs #####
# rename and keep certain columns
lgbt_orgs <- rename(lgbt_orgs, SchoolID = `School ID`, SchoolName = `School Name`,
                    ClubName = `Club Name`, ClubPopn = `Club Population`,
                    ClubMission = `Club Mission`)
lgbt_orgs <- select(lgbt_orgs, SchoolID:ClubName, ClubPopn:ClubMission)

# look at which kinds of clubs there are
table(lgbt_orgs$ClubPopn)
lgbt_orgs <- mutate(lgbt_orgs, ClubGenPopn = case_when(
  ClubPopn == "All LGBT" | ClubPopn == "LGBT and Allies" ~ "All LGBT and Allies",
  ClubPopn == "BIPOC LGBT" | ClubPopn == "Black LGBT" | ClubPopn == "Latinx LGBT" |
    ClubPopn == "South Asian LGBT" ~ "LGBT Students of Color",
  ClubPopn == "Asexual/Aromantic" | ClubPopn == "Bisexual/Pansexual" | 
    ClubPopn == "Trans/GNC" ~ "Specific LGBT Identity",
  ClubPopn == "LGBT in Art" | ClubPopn == "LGBT in Business" |
    ClubPopn == "LGBT in Law" | ClubPopn == "LGBT in Social Work" |
    ClubPopn == "LGBT in STEM" ~ "Specific Academic Discipline",
  ClubPopn == "LGBT Jews" | ClubPopn == "LGBT Religion" ~ "Religious Association",
  ClubPopn == "LGBT Grad Students" | ClubPopn == "LGBT in Sports" |
    ClubPopn == "LGBT Women & NB" ~ "Specific Non-LGBT Identity"
))
table(lgbt_orgs$ClubGenPopn)

# make a word cloud using club missions
library(quanteda)
lgbt_orgs_text <- filter(lgbt_orgs, ClubMission != "NA")
lgbt_orgs_text$ClubID <- seq(1:nrow(lgbt_orgs_text))
mission_corpus <- corpus(lgbt_orgs_text, docid_field = "ClubID",
                       text_field = "ClubMission")
summary(mission_corpus, n=5)
mission_tokens <- tokens(mission_corpus, 
                         remove_punct = T, remove_numbers = T)
mission_dfm <- dfm(mission_tokens) |>
  dfm_remove(stopwords('english')) |>
  dfm_trim(min_termfreq = 15, verbose = FALSE)
library(quanteda.textplots)
set.seed(14)
textplot_wordcloud(mission_dfm, color="black")
