# -- SET UP -- #
# load necessary libraries
library(shiny)              # for overall app
library(bslib)              # for certain app functions
library(tidyverse)          # for data manipulation
library(sf)                 # for map
library(ggplot2)            # for scatterplots and boxplots
library(plotly)             # for interactive graphs
library(quanteda)           # for text processing
library(quanteda.textplots) # for wordcloud
library(DT)                 # for interactive data tables

# read in data
schools <- read.csv("MA_Colleges.csv")
lgbtorgs <- read.csv("MA_LGBT_Student_Orgs.csv")

# make table for homepage
orgTypeTable <- lgbtorgs |> group_by(ClubGenPopn) |> summarize(Organizations = n()) |> 
  arrange(desc(Organizations)) |> rename(`Club Population` = ClubGenPopn) |> 
  mutate(`Sub-Populations` = 
           case_when(`Club Population` == "All LGBT and Allies" ~
                       "All LGBT, LGBT or ally",
                     `Club Population` == "Specific Academic Discipline" ~
                       "STEM, Business, Law, Art",
                     `Club Population` == "Specific LGBT Identity" ~
                       "Transgender, asexual, bisexual",
                     `Club Population` == "LGBT Students of Color" ~
                       "All POC, Black, Latinx",
                     `Club Population` == "Specific Non-LGBT Identity" ~
                       "Athletes, graduate, women",
                     `Club Population` == "Religious Association" ~
                       "All religion, Jewish"))

# prepare to make map
ma_sf <- read_sf("countyShapes/COUNTIES_POLY.shp")
county_groups <- schools |> group_by(County) |> mutate(County = toupper(County))

# modify data for scatterplots
schools <- mutate(schools,
                  NumStudents = as.numeric(NumStudents),
                  NetPrice = as.numeric(NetPrice),
                  PctMen = as.numeric(PctMen),
                  PctWhite = as.numeric(PctWhite),
                  GradRate = as.numeric(GradRate),
                  TotalClubs = as.numeric(TotalClubs),
                  LGBTClubs = as.numeric(LGBTClubs))

# modify data for boxplots
schools <- mutate(schools,
                  MARegionFac = factor(MARegion, 
                                       levels = c("West", "Central", "Northeast", "Southeast"),
                                       ordered = FALSE),
                  
                  SettingFac = factor(Setting, levels = c("Rural", "Suburb", "Town",
                                                          "City: Small", "City: Midsize", 
                                                          "City: Large"), 
                                      labels = c("Rural", "Suburb", "Town", "Small City",
                                                 "Medium City", "Large City"),
                                      ordered = TRUE),
                  GraduateFac = factor(Graduate, levels = c("Exclusively undergraduate",
                                                            "Undergraduate and graduate",
                                                            "Exclusively graduate"),
                                       labels = c("All Undergrad", "Undergrad-Grad Mix",
                                                  "All Grad"), ordered = TRUE),
                  PublicFac = factor(Public, levels = c(1, 0), 
                                     labels = c("Public", "Private"), ordered = FALSE),
                  CommunityFac = factor(Community, levels = c(1, 0),
                                        labels = c("Community College", "Non-Community College"),
                                        ordered = FALSE),
                  ChristianFac = factor(Christian, levels = c(1,0),
                                        labels = c("Christian", "Not Christian"),
                                        ordered = FALSE),
                  HousingFac = factor(Housing, levels = c(1, 0),
                                      labels = c("Campus Housing", "No Campus Housing"),
                                      ordered = FALSE))

# process data for text analysis
lgbtorgs_txt <- filter(lgbtorgs, ClubMission != "NA")
lgbtorgs_txt$ClubID <- seq(1:nrow(lgbtorgs_txt))
mission_corpus <- corpus(lgbtorgs_txt, docid_field = "ClubID", text_field = "ClubMission")
mission_tokens <- tokens(mission_corpus, remove_punct = T, remove_numbers = T)
mission_dfm <- dfm(mission_tokens) |> dfm_remove(c(stopwords('english'), "+"))


# -- BUILD APP -- #
# define UI
ui <- fluidPage(
  theme = bs_theme(bootswatch = "pulse"),
  titlePanel("LGBT Student Organizations at Colleges and Universities in Massachusetts"),
  tabsetPanel(
    # Overview
    tabPanel("Overview",
             card(p("Have you ever wondered what kinds of LGBT student organizations
                  exist at colleges and universities across Massachusetts? What about
                  whether a particular college is more or less likely to have at least
                  one LGBT student organization depending on certain school characteristics?
                  If so, you're in the right place!"),
                  p("This dashboard contains information about 92 higher education 
                  institutions in Massachusetts which had information about at least one
                  student organization on their website as of March 2025. Data about the
                  schools was collected via the American Council on Education's (ACE) Carnegie 
                  Classification of Institutions of Higher Education, Niche, College Board, 
                  and the U.S. National Center for Education Statistics (NCES) College Navigator. 
                  Data about the student organizations were collected by visiting each school's 
                  individual website, and thus only contains publicly accessible information."),
                  p("For the main takeaways from this data, check out the information below!
                  If you're curious about how the number of LGBT student organizations per
                  school is related to the Massachusetts county the school is in, check out
                  the 'Map' tab for a visual representation of different metrics across the.
                  state. If you'd like to evaluate the relationship between a numeric 
                  attribute of a school and the number of LGBT student organizations schools 
                  have, head over to the 'College Scatterplots' tab. If you're interested in
                  the relationship between a categorical attribute of a school and the
                  number of LGBT student organizations schools have, the 'College Boxplots'
                  will be a good destination! If you're wondering what the LGBT student
                  organizations have in common in terms of their missions, regardless of
                  which school they're hosted at, check out the 'Organization Wordcloud'
                  tab! Lastly, if you want to access the raw data about the schools and/or
                  the LGBT student organizations to learn more about a specific school or
                  organization, feel free to peruse the 'College Data' and 'Organization
                  Data' tabs for the ability to search and filter as you please!"),
                  p("Without further ado, here's what you should know about the state of LGBT
                  student organizations at higher education institutes in Massachusetts!")),
             layout_columns(
               value_box(title = "Total LGBT Student Organizations",
                         "132", 
                         "Across 92 schools in MA",
                         theme = "bg-gradient-indigo-purple"),
               value_box(title = "Schools with 0 LGBT Student Organizations",
                         "14", "15% of the 92 schools in MA have none",
                         theme = "bg-gradient-indigo-purple"),
               value_box(title = "Average LGBT Student Organizations",
                         "1.43",
                         "Across 92 schools in MA",
                         theme = "bg-gradient-indigo-purple")
             ),
             layout_columns(
               card(card_header("Traits of Schools with More LGBT Student Organizations"),
                    p("1. More Total Clubs: Schools with more student clubs in general tend to have 
                      more LGBT student clubs."),
                    p("2. More Students: Schools with more students tend to have more LGBT student 
                      clubs."),
                    p("3. Higher Graduation Rates: Schools where more full-time undergraduate 
                      students graduate tend to have more LGBT student clubs."),
                    p("Note: According to multiple linear regression models, when also controlling for: 
                       school being public or private, 
                       school being community college or not,
                       school location in MA, 
                       campus setting, 
                       presence of graduate students, 
                       average net price,
                       school being Christian or not,
                       percent of male students,
                       percent of white students, and
                       school having on-campus housing or not")),
               card(card_header("Types of LGBT Student Organizations"), 
                    tableOutput("orgTypeTable"))
             ),
             card(p("Dashboard created by Maura Anish in May 2025. Contact at: mbanish@umass.edu."))
    ),
    
    # Map of MA
    tabPanel("Massachusetts Map",
             selectInput(inputId = "mapvar",
                         label = "Select a measure to display:",
                         choices = list("Minimum", "Median", "Mean", "Maximum", "Sum")),
             plotlyOutput("map"),
             textOutput("mapdesc")
    ),
    
    # College Scatterplots
    tabPanel("College Scatterplots",
             selectInput(inputId = "scatplotVar",
                         label = "Select a numeric variable:",
                         choices = list("Number of Students",
                                        "Price of Attendance",
                                        "Percent Male Students",
                                        "Percent White Students",
                                        "Graduation Rate",
                                        "Total Clubs")),
             plotlyOutput("scatterplot"),
             textOutput("splotdesc")
    ),
    
    # College Boxplots
    tabPanel("College Boxplots",
             selectInput(inputId = "boxplotVar",
                         label = "Select a categorical variable:",
                         choices = list("Region of MA",
                                        "Campus Setting",
                                        "Student Body Makeup",
                                        "Public or Private",
                                        "Community or Not",
                                        "Christian or Not",
                                        "Campus Housing or Not")),
             plotOutput("boxplot"),
             textOutput("bplotdesc")
    ),
    
    # Organization Wordcloud
    tabPanel("Organization Wordcloud",
             sliderInput(inputId = "minfreq",
                         label = "Choose a minimum word frequency:",
                         min = 10, max = 50, value = 30),
             plotOutput("wordcloud"),
             card(p("These words were present in multiple LGBT student organizations' mission
                    statements. 121 out of the 132 LGBT student organizations had their mission
                    statements available online; the statements were retrieved from the 
                    individual schools' websites and are accurate as of 2025."))
    ),
    
    # College Data
    tabPanel("College Data",
             dataTableOutput("school_table")
    ),
    
    # Organization Data
    tabPanel("Organization Data",
             dataTableOutput("org_table")
    )
  )
)

# define server logic
server <- function(input, output) {
  # Overview
  output$orgTypeTable <- renderTable(orgTypeTable)
  
  # Map
  county_summary <- reactive({switch(
    input$mapvar,
    "Minimum" = county_groups |> summarize(NumSchools = n(), MinClubs = min(LGBTClubs)),
    "Median" = county_groups |> summarize(NumSchools = n(), MedClubs = median(LGBTClubs)),
    "Mean" = county_groups |> summarize(NumSchools = n(), AvgClubs = round(mean(LGBTClubs), 2)),
    "Maximum" = county_groups |> summarize(NumSchools = n(), MaxClubs = max(LGBTClubs)),
    "Sum" = county_groups |> summarize(NumSchools = n(), TotClubs = sum(LGBTClubs))
  )})
  basemap <- reactive({switch(
    input$mapvar,
    "Minimum" = ggplot(left_join(ma_sf, county_summary(), by = c("COUNTY" = "County")),
                       aes(text = paste("County: ", COUNTY, "\nSchools: ", NumSchools))) + 
                geom_sf(aes(fill = MinClubs)),
    "Median" = ggplot(left_join(ma_sf, county_summary(), by = c("COUNTY" = "County")),
                      aes(text = paste("County: ", COUNTY, "\nSchools: ", NumSchools))) + 
               geom_sf(aes(fill = MedClubs)),
    "Mean" = ggplot(left_join(ma_sf, county_summary(), by = c("COUNTY" = "County")),
                    aes(text = paste("County: ", COUNTY, "\nSchools: ", NumSchools))) + 
             geom_sf(aes(fill = AvgClubs)),
    "Maximum" = ggplot(left_join(ma_sf, county_summary(), by = c("COUNTY" = "County")),
                       aes(text = paste("County: ", COUNTY, "\nSchools: ", NumSchools))) + 
                geom_sf(aes(fill = MaxClubs)),
    "Sum" = ggplot(left_join(ma_sf, county_summary(), by = c("COUNTY" = "County")),
                   aes(text = paste("County: ", COUNTY, "\nSchools: ", NumSchools))) + 
            geom_sf(aes(fill = TotClubs)),
  )})
  output$map <- renderPlotly(ggplotly(basemap() + 
                                      scale_fill_distiller(palette = "Purples", direction = 1) + 
                                      theme_void()))
  maptxt <- reactive({switch(
    input$mapvar,
    "Minimum" = "Every school located in Hampshire County has at least 2 LGBT student
    organizations. All of the schools located in Western or Central Massachusetts have
    at least 1 LGBT student organization. There are some schools in Northeastern MA and
    Southeastern MA which have 0 LGBT student organizations.",
    "Median" = "The schools in Hampshire County and Plymouth County have the highest median
    number of LGBT student organizations. Most other counties in Massachusetts have a median 
    of 1 LGBT student organization.",
    "Mean" = "The average number of LGBT student organizations varies widely across each
    county. Hampshire County and Plymouth County have the highest average numbers of 
    LGBT student organizations in the state, while Essex County and Barnstable County
    have the lowest averages.",
    "Maximum" = "Suffolk County and Middlesex County are the homes of the schools which
    have the highest numbers of LGBT student organizations. All of the schools in Barnstable, 
    Essex, Franklin, and Hampden Counties have at most 1 LGBT student organization.",
    "Sum" = "In total, Suffolk County has the most LGBT student organizations. Barnstable
    County and Franklin County only have a total of 1 LGBT student organization each."
  )})
  output$mapdesc <- renderText({maptxt()})
  
  # College Scatterplots
  splotvars <- reactive({switch(
    input$scatplotVar,
    "Number of Students" = ggplot(data = schools, aes(x = NumStudents, y = LGBTClubs)) +
      labs(x = "Number of Students", y = "Number of LGBT Organizations"),
    "Price of Attendance" = ggplot(data = schools, aes(x = NetPrice, y = LGBTClubs)) +
      labs(x = "Price of Attendance (USD)", y = "Number of LGBT Organizations"),
    "Percent Male Students" = ggplot(data = schools, aes(x = PctMen, y = LGBTClubs)) +
      labs(x = "Percentage of Male Students", y = "Number of LGBT Organizations"),
    "Percent White Students" = ggplot(data = schools, aes(x = PctWhite, y = LGBTClubs)) +
      labs(x = "Percentage of White Students", y = "Number of LGBT Organizations"),
    "Graduation Rate" = ggplot(data = schools, aes(x = GradRate, y = LGBTClubs)) +
      labs(x = "Graduation Rate (%)", y = "Number of LGBT Organizations"),
    "Total Clubs" = ggplot(data = schools, aes(x = TotalClubs, y = LGBTClubs)) +
      labs(x = "Number of Total Clubs", y = "Number of LGBT Organizations"))
  })
  output$scatterplot <- renderPlotly({
    ggplotly(splotvars() + geom_smooth(method = "lm", color = "purple") + 
               geom_point(aes(text = Name)) + theme_minimal())
  })
  splottxt <- reactive({switch(
    input$scatplotVar,
    "Number of Students" = "There is a moderately strong positive linear relationship between 
    the number of students who attend a school and the number of LGBT student organizations
    there (r = 0.59). Most schools in the dataset have fewer than 20,000 students, with the
    exception of a few high outliers. The data regarding the number of students at each school
    was retrieved from the U.S. NCES and is accurate as of 2025.",
    "Price of Attendance" = "There is no linear relationship between the net price of attendance
    of a school and the number of LGBT student organizations there (r = 0.04). Knowing how much
    a school generally costs to attend is not useful in predicting the number of LGBT student
    organizations that are likely to be there. The data regarding the net price at each school
    was retrieved from the U.S. NCES and represents the average net price for full-time, entering 
    undergraduate students who received some form of financial aid in the 2022-2023 school year.",
    "Percent Male Students" = "There is a very weak negative linear relationship between the
    percent of male students who attend a school and the number of LGBT student organizations
    there (r = -0.16). The percentage of students who are reported as male at these schools
    ranges from 0% (at women's colleges) to 86%, but most schools in the data have a near-even
    split in gender distribution. The data regarding the percentage of male students at each
    school was retrieved from NICHE and does not specify the year of collection.",
    "Percent White Students" = "There is no linear relationship between the percent of white
    students who attend a school and the number of LGBT student organizations there (r =
    -0.06). The percentage of students who are reported as white at these schools ranges from
    4% to 83%, but the middle half of the schools have percentages ranging from 40% to 64%.
    The data regarding the percentage of white students at each school was retrieved from 
    College Board and does not specify the year of collection.",
    "Graduation Rate" = "There is a moderately strong positive linear relationship between
    the graduation rate of a school and the number of LGBT student organizations there (r = 
    0.50). All but one of the schools which have at least 2 LGBT student organizations have
    graduation rates exceeding 50%. The data regarding the graduation rate of each school was
    retrieved from the U.S. NCES and represents the outcomes for full-time undergraduate 
    students who began their studies in the fall of 2017 (for most schools) or the fall of 
    2020 (for some schools).",
    "Total Clubs" = "There is a strong positive linear relationship between the total number
    of student organizations at a school and the number of LGBT student organizations there
    (r = 0.76). This is to be expected, as the more student organizations a school has in 
    general increases the likelihood of a school having more LGBT student organizations. The
    data regarding the total number of student organizations at a school was manually
    collected by visiting each school's website. This information is assumed to be accurate
    as of 2025, but organizations which are currently inactive may be included as well."
  )})
  output$splotdesc <- renderText({splottxt()})
  
  # College Boxplots
  bplotvars <- reactive({switch(
    input$boxplotVar,
    "Region of MA" = ggplot(data = schools, aes(x = MARegionFac, y = LGBTClubs)) +
      labs(x = "Region of Massachusetts", y = "Number of LGBT Organizations"),
    "Campus Setting" = ggplot(data = schools, aes(x = SettingFac, y = LGBTClubs)) +
      labs(x = "Campus Setting", y = "Number of LGBT Organizations"),
    "Student Body Makeup" = ggplot(data = schools, aes(x = GraduateFac, y = LGBTClubs)) +
      labs(x = "Student Body Makeup", y = "Number of LGBT Organizations"),
    "Public or Private" = ggplot(data = schools, aes(x = PublicFac, y = LGBTClubs)) +
      labs(x = "Publicity Status", y = "Number of LGBT Organizations"),
    "Community or Not" = ggplot(data = schools, aes(x = CommunityFac, y = LGBTClubs)) +
      labs(x = "Community College Status", y = "Number of LGBT Organizations"),
    "Christian or Not" = ggplot(data = schools, aes(x = ChristianFac, y = LGBTClubs)) +
      labs(x = "Religion Status", y = "Number of LGBT Organizations"),
    "Campus Housing or Not" = ggplot(data = schools, aes(x = HousingFac, y = LGBTClubs)) +
      labs(x = "Campus Housing Status", y = "Number of LGBT Organizations"))
  })
  output$boxplot <- renderPlot({
    bplotvars() + geom_boxplot() + theme_minimal()
  })
  bplottxt <- reactive({switch(
    input$boxplotVar,
    "Region of MA" = "There are 17 schools in Western MA, 11 schools in Central MA, 46 schools
    in Northeastern MA, and 18 schools in Southeastern MA. While all regions share the same
    median number of LGBT student organizations (1 organization), the West and Central MA
    regions tend to have more schools with more than 1 LGBT student organization.",
    "Campus Setting" = "There are 5 schools in a rural setting, 35 schools in a suburban
    setting, 3 schools in a town, 10 schools in a small city, 16 schools in a medium-sized
    city, and 23 schools in a large city. Schools in every setting except for small cities
    have a median of 1 LGBT student organization. Schools located in a small city are more
    likely to have more than 1 LGBT student organization. The data regarding the setting of
    each school was retrieved from the U.S. NCES and is accurate as of 2025.",
    "Student Body Makeup" = "There are 27 schools with exclusively undergraduate students,
    60 schools with both undergraduate and graduate students, and only 5 schools with 
    exclusively graduate students. Regardless of the student body makeup, the median number
    of LGBT student organizations is 1. However, schools with some undergraduate students
    and some graduate students are more likely to have more than 1 LGBT student organization.
    The data regarding the student body makeup of each school was retrieved from the ACE and 
    is accurate as of 2025.",
    "Public or Private" = "There are 29 public schools and 63 private schools. Both types of
    school have a median of 1 LGBT student organization, but private schools are more likely
    to have more than 1 LGBT student organization.",
    "Community or Not" = "There are 15 community colleges and 77 non-community colleges.
    Both types of school have a median of 1 LGBT student organization, but non-community
    colleges are more likely to have more than 1 LGBT student organization.",
    "Christian or Not" = "There are 15 Christian colleges and 77 non-Christian colleges.
    Both types of school have a median of 1 LGBT student organization, but non-Christian
    colleges are much more likely to have more than 1 LGBT student organization. The data
    regarding whether a school is Christian or not was retrieved from NICHE and does not
    specify the year of collection.",
    "Campus Housing or Not" = "There are 65 schools which offer on-campus housing and 27
    schools which do not offer on-campus housing. Both types of school have a median of 1
    LGBT student organization, but schools which offer campus housing are more likely to
    have more than 1 LGBT student organization. The data regarding whether a school has
    on-campus housing or not was retrieved from the U.S. NCES and is accurate as of 2025."
    )})
  output$bplotdesc <- renderText({bplottxt()})
  
  
  # Organization Wordcloud
  cloud_dfm <- reactive({dfm_trim(mission_dfm, min_termfreq = input$minfreq, verbose = FALSE)})
  output$wordcloud <- renderPlot({
    textplot_wordcloud(cloud_dfm(), color="black", min_size = .5, max_size = 8)
  })
  
  # College Data
  output$school_table <- renderDataTable({datatable(schools, filter = "top")}) 
  
  # Organization Data
  output$org_table <- renderDataTable({datatable(lgbtorgs, filter = "top")}) 
}

# run the app
shinyApp(ui = ui, server = server)
