
# Define UI for application that draws a histogram
shinyUI(fluidPage(
  
  # Application title
  titlePanel("Jaro-Winkler vs. Binary CMD"),
  
  # Sidebar with a slider input for the number of bins
  sidebarLayout(
    sidebarPanel(
      sliderInput("jaro",
                  "Jaro-Winkler Threshold",
                  min=0.7,
                  max=0.99,
                  value=0.9,
                  step=0.01),
      p("Adjust the maximum prefix length in the Winkler booster"),
      sliderInput("prefix",
                  "Max prefix",
                  min=-1,
                  max=7,
                  value=4,
                  step=1),
      p("Number of matching characters minus number of different"),
      sliderInput("cmd",
                  "CMD Threshold",
                  min=-3,
                  max=3,
                  value=0,
                  step=1),
      p("Set number of repeated matching letters"),
      sliderInput("prefrep",
                  "Letter repetition",
                  min=0,
                  max=3,
                  value=0,
                  step=1)
      
    ),

#mainPanel("Main Panel")
    
    # Show a plot of the generated distribution
    mainPanel(htmlOutput("maintext")
            , plotOutput("JaroCMDPlot",height=400)
            , plotOutput("JaroCMDPlot_2",height=400)
    )
)
))

