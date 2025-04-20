# load necessary libraries
library(shiny)
library(bslib)
library(tidyverse)
library(ggplot2)
library(quanteda)
library(quanteda.textplots)
library(DT)

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

# process data for text analysis
lgbtorgs_txt <- filter(lgbtorgs, ClubMission != "NA")
lgbtorgs_txt$ClubID <- seq(1:nrow(lgbtorgs_txt))
mission_corpus <- corpus(lgbtorgs_txt, docid_field = "ClubID", text_field = "ClubMission")
mission_tokens <- tokens(mission_corpus, remove_punct = T, remove_numbers = T)
mission_dfm <- dfm(mission_tokens) |> dfm_remove(stopwords('english'))

# define UI
ui <- fluidPage(
  titlePanel("LGBT Student Organizations at Colleges and Universities in Massachusetts"),
  tabsetPanel(
    tabPanel("Overview",
             card("Have you ever wondered what kinds of LGBT student organizations
                  exist at colleges and universities across Massachusetts? What about
                  whether a particular college is more or less likely to have at least
                  one LGBT student organization depending on certain school characteristics?
                  If so, you're in the right place!\n
                  This dashboard contains information about 92 higher education 
                  institutions in Massachusetts which had information about at least one
                  student organization on their website as of March 2025. Data about the
                  schools was collected via the Carnegie Classification of Institutions
                  of Higher Education, Niche, College Board, and the U.S. National Center 
                  for Education Statistics' College Navigator. Data about the student 
                  organizations were collected by visiting each school's individual website, 
                  and thus only contains publicly accessible information.\n
                  For the main takeaways from this data, check out the information below!
                  If you'd like to evaluate the relationship between a numeric attribute
                  of a school and the number of LGBT student organizations schools have,
                  head over to the 'College Scatterplots' tab. If you're interested in
                  the relationship between a categorical attribute of a school and the
                  number of LGBT student organizations schools have, the 'College Boxplots'
                  will be a good destination! If you're wondering what the LGBT student
                  organizations have in common in terms of their missions, regardless of
                  which school they're hosted at, check out the 'Organization Wordcloud'
                  tab! Lastly, if you want to access the raw data about the schools and/or
                  the LGBT student organizations to learn more about a specific school or
                  organization, feel free to peruse the 'College Data' and 'Organization
                  Data' tabs for the ability to search and filter as you please!\n
                  Without further ado, here's what you should know about the state of LGBT
                  student organizations at higher education institutes in Massachusetts!"),
             tableOutput("orgTypeTable")
             ),
    
    tabPanel("College Scatterplots",
             selectInput(inputId = "scatplotVar",
                         label = "Select a numeric variable to plot against the number of 
                                  LGBT organizations for each school in Massachusetts:",
                         choices = list("Number of Students",
                                        "Price of Attendance",
                                        "Percent Male Students",
                                        "Percent White Students",
                                        "Graduation Rate",
                                        "Total Clubs")),
             plotOutput("scatterplot")
    ),
    
    tabPanel("College Boxplots",
             selectInput(inputId = "boxplotVar",
                         label = "Select a categorical variable to plot against the number
                                  of LGBT organizations for each school in Massachusetts:",
                         choices = list("Region of MA",
                                        "Campus Setting",
                                        "Student Body Makeup",
                                        "Public or Private",
                                        "Community or Not",
                                        "Christian or Not",
                                        "Campus Housing or Not")),
             plotOutput("boxplot")
    ),
    
    tabPanel("Organization Wordcloud",
             sliderInput(inputId = "minfreq",
                         label = "Choose the minimum number of times for a word to be 
                                 included in organization mission statements in order
                                 for it to show up in the word cloud:",
                         min = 5, max = 50, value = 50),
             plotOutput("wordcloud")
    ),
    
    tabPanel("College Data",
             dataTableOutput("school_table")
    ),
    
    tabPanel("Organization Data",
             dataTableOutput("org_table")
    )
  )
)

# define server logic
server <- function(input, output) {
  # Overview
  output$orgTypeTable <- renderTable(orgTypeTable)
  
  # College Scatterplots
  splotvars <- reactive({switch(
    input$scatplotVar,
    "Number of Students" = ggplot(data = schools, aes(x = NumStudents, y = LGBTClubs)),
    "Price of Attendance" = ggplot(data = schools, aes(x = NetPrice, y = LGBTClubs)),
    "Percent Male Students" = ggplot(data = schools, aes(x = PctMale, y = LGBTClubs)),
    "Percent White Students" = ggplot(data = schools, aes(x = PctWhite, y = LGBTClubs)),
    "Graduation Rate" = ggplot(data = schools, aes(x = GradRate, y = LGBTClubs)),
    "Total Clubs" = ggplot(data = schools, aes(x = TotalClubs, y = LGBTClubs)))
  })
  output$scatterplot <- renderPlot({
    splotvars() + geom_point()
  })
  
  # College Boxplots
  bplotvars <- reactive({switch(
    input$boxplotVar,
    "Region of MA" = ggplot(data = schools, aes(x = MARegion, y = LGBTClubs)),
    "Campus Setting" = ggplot(data = schools, aes(x = Setting, y = LGBTClubs)),
    "Student Body Makeup" = ggplot(data = schools, aes(x = Graduate, y = LGBTClubs)),
    "Public or Private" = ggplot(data = schools, aes(x = as.factor(Public), y = LGBTClubs)),
    "Community or Not" = ggplot(data = schools, aes(x = as.factor(Community), y = LGBTClubs)),
    "Christian or Not" = ggplot(data = schools, aes(x = as.factor(Christian), y = LGBTClubs)),
    "Campus Housing or Not" = ggplot(data = schools, aes(x = as.factor(Housing), y = LGBTClubs)))
  })
  output$boxplot <- renderPlot({
    bplotvars() + geom_boxplot()
  })
  
  # Organization Wordcloud
  cloud_dfm <- reactive({dfm_trim(mission_dfm, min_termfreq = input$minfreq, verbose = FALSE)})
  output$wordcloud <- renderPlot({
    textplot_wordcloud(cloud_dfm(), color="black")
  })
  
  # College Data
  output$school_table <- renderDataTable({datatable(schools, filter = "top")}) 
  
  # Organization Data
  output$org_table <- renderDataTable({datatable(lgbtorgs, filter = "top")}) 
}

# run the app
shinyApp(ui = ui, server = server)
