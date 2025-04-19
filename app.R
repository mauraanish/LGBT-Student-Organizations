# load necessary libraries
library(shiny)
library(tidyverse)
library(ggplot2)
library(quanteda)
library(quanteda.textplots)
library(DT)

# read in data
schools <- read.csv("MA_Colleges.csv")
lgbtorgs <- read.csv("MA_LGBT_Student_Orgs.csv")

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
    tabPanel("Overview"),
    
    
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
