#
# This is the user-interface definition of a Shiny web application. You can
# run the application by clicking 'Run App' above.
#
# Find out more about building applications with Shiny here:
#
#    http://shiny.rstudio.com/
#

library(shiny)
library(DT)
library(shinyjs)

# Define UI for application that draws a histogram
fluidPage(
    useShinyjs(),  # Set up shinyjs
    # Application title
    titlePanel("OUTIL D'ÉTALONNAGE EN LONGUEUR D'ONDE - SPECTRO OCEANOPTICS"),

    # Sidebar with a slider input for number of bins
    sidebarLayout(
        sidebarPanel(width=4,
            selectInput("lespectro","Choisir un spectro",choices=NULL),
            selectInput("calibPics","Choisir un vecteur de pics",
                        choices=names(pics_calibration)),
            sliderInput("tint",
                        "Temps d'intégration (msec):",
                        min = 5,
                        max = 1000,
                        value = 100,
                        step = 5),
            sliderInput("boxcar",
                        "Boxcar",
                        1,20,1,1),
            sliderInput("nscans",
                        "Nombre d'acquisition",
                        1,20,1,1),
            h1(""),
            h3("Paramètres d'étalonnage"),
            h3("__________________________"),
            sliderInput("fenetreEtal",
                        "Fenêtre autour des pics",
                        min=3,max=11,value=5,step=2),
            sliderInput("tol",
                        "Tolérance d'écart position de picx (pixels)",
                        min=1,max=5,value=2,step=1),
           
            actionButton("unique","Acquistion"),
            actionButton("calib","Calcul d'étalonnage"),
            actionButton("loadNewCoeff","Sauve les nouveaux coefficients"),
            h2(""),
            h3("COEFFICIENTS - LONGUEUR D'ONDE"),
            DTOutput('table')
        ),

        # Show a plot of the generated distribution
        mainPanel(width=8,
            fluidRow(
              column(6,
                plotOutput("spPlot"),
                plotOutput("picPlot", height = 600)
              ),
              column(6,
                plotOutput("calib"),
                plotOutput("regPlot"))
            )
        )
    )
)

