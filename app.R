
## Author: Emma Serrano Pérez

### Packages
library(shiny)
library(shinydashboard)
library(dashboardthemes)



### UI
ui <- dashboardPage(
                 ## Header
                dashboardHeader( #disable = TRUE
                  title = "NERITES",
                  tags$li(a(href = 'https://www.ibvf.us-csic.es/', tags$img(src = 'logoibvf.png',
                                                                            title = "IBVF", height = "20px"), style = "margin-top: 0px;"),
                          class = "dropdown"),
                  tags$li(a(href = 'https://www.us.es/', tags$img(src = 'Logo_US.png',
                                                                  title = "US", height = "20px"), style = "margin-top: 0px;"),
                          class = "dropdown"),
                  tags$li(a(href = 'https://www.csic.es/es', tags$img(src = '2560px-Logotipo_del_CSIC.svg.png',
                                                                      title = "CSIC", height = "20px"), style = "margin-top: 0px;"),
                          class = "dropdown"),
                  tags$li(a(href = 'https://next-generation-eu.europa.eu/', tags$img(src = 'nextgeneu_en.jpg',
                                                                      title = "CSIC", height = "20px"), style = "margin-top: 0px;"),
                          class = "dropdown")
                ),
                
                dashboardSidebar(
                  ## Sidebar content
                  dashboardSidebar( 
                    sidebarMenu(
                      menuItem("Home", tabName = "home", icon = icon("house")),
                      menuItem("Explore rhythmicity", icon = icon("wave-square",class = "fa-solid fa-wave-square",
                                                                  lib ="font-awesome" ), tabName = "rhythm"),
                      menuItem("Gene Coexpression Network", icon = icon("circle-nodes",class = "fa-solid fa-circle-nodes",
                                                                  lib ="font-awesome" ), tabName = "coexpression"),
                      menuItem("Predictive model", icon = icon("buromobelexperte",class="fa-brands fa-buromobelexperte",
                                                                  lib ="font-awesome"), tabName = "predictive"),
                      menuItem("Source code", icon = icon("code"), 
                               href = "https://github.com/sandungxs/NERITES_app"),
                      menuItem("Contact and Info", icon = icon("envelope"), tabName = "contact_tutorial")
                    )
                  )
                ),
                
                ## Body content
                dashboardBody(shinyDashboardThemes(theme = "blue_gradient"),
                                
                ## Home
                tabItems(
                    tabItem(tabName = "home",
                            tags$br(),
                            fluidRow(column(9,
                                            tags$div(span(tags$b("NERITES"),": bla bla bla",
                                                          style = 'color: #1f618d; font-weight: 540; font-size: 42px; font-family: "Alatsi"", Verdana, sans-serif;')),
                                            style = "margin-top: 45px; margin-left: 35px;"
                            ,
                            # column(2,
                            #        img(
                            #          src = "pharaohlogo-removebg-preview.png",
                            #          alt = "logo",
                            #          width = 220,
                            #          height = 220, style="display: block; margin-left: 0px``; margin-right: auto;"
                            #        )
                            # ),
                            column(1)
                            ),
                            fluidRow(
                              column(11,
                                     tags$br(),
                                     tags$br(),
                                     tags$div(align="justify", "Welcome to",tags$b("NERITES"), "the web application
                                              that allows you to easily and autonomously explore the response of",
                                              tags$em("Raphidocelis subcapitata"),"to nitrate stress in the medium over time,
                                              both short and long day. A tutorial can be found at the following link.",
                                              style = 'font-size: 16px; margin-left: 20px;'

                                     ),
                                     fluidRow(tags$br()),
                                     fluidRow(tags$br()),
                                     fluidRow(
                                       column(1),
                                       column(12,
                                              valueBox(a("Rhythmicity", href="#shiny-tab-rhythm", "data-toggle" = "tab", "style" = "color:white"),
                                                       "Explore how rhythmicity changes under stress",
                                                       icon = icon("wave-square",class = "fa-solid fa-wave-square",
                                                                   lib ="font-awesome" ), width = 4),
                                              
                                              valueBox(a("Gene Network", href="#shiny-tab-coexpression", "data-toggle" = "tab", "style" = "color:white"),
                                                       "Build your own gene coexpression network",
                                                       icon = icon("circle-nodes",class = "fa-solid fa-circle-nodes",
                                                                   lib ="font-awesome" ), width = 4, color = "lime"),
                                              
                                              valueBox(a("Predictive model", href="#shiny-tab-predictive", "data-toggle" = "tab", "style" = "color:white"),
                                                       "Tune your own predictive model",
                                                       icon = icon("buromobelexperte",class="fa-brands fa-buromobelexperte",
                                                                   lib ="font-awesome"), width = 4, color = "purple")
                                       )),
                                     fluidRow(tags$br()),
                                     tags$div(align="justify", style = 'font-size: 16px; margin-left: 30px;',
                                              tags$ol(
                                                tags$li(tags$b("Explore rhythmicity"), ". To explore how rhythmicity changes along the day if",tags$em("R. supcapitata"),
                                                        "is under nitrate deficiency."),
                                                tags$br(),
                                                tags$li(tags$b("Gene Coexpression Network"),". To explore how the gene coexpression network of",tags$em("R. supcapitata"),
                                                        "is build."),
                                                tags$br(),
                                                tags$li(tags$b("Predictive model"),". We have our own predictive model that you can tune to optimize the data according
                                                        to your objetives."),
                                              )
                                     ),
                                     
                                     fluidRow(br()),
                                     fluidRow(br()),
                                     fluidRow(
                                       column(7, 
                                              tags$div(align="justify", style = 'font-size: 16px; margin-left: 20px;',  br(), 
                                                       "The exploration of orthology is based on the construction of orthogroups, sets of genes that descend from a 
                         single gene in the common ancestor of the species under study. In this way, it is possible to trace the evolutionary 
                         history of these genes and to analyze the changes that the orthogroup has undergone from its appearance to 
                         its current situation in the extant species.", br(), br(), "Regarding the species supported by the tool, two different models are offered. 
                         The default model focuses on 36 species of the ", tags$b("Viridiplantae"), " clade, with representatives of the 
                         Chlorophyta and Streptophyta clades chosen to span the evolutionary set of these groups.",
                                                       br(), br(),"Furthermore, an additional ", tags$b("Global"), " model includes species from the whole Archaeplastida clade, 
                         as well as examples of Stramenopiles and Cryptophytes. Note that although both models contain the green lineage,
                         their results may vary because the common ancestors of the groups are very distant in time,
                         so for a more accurate analysis it is recommended to use the Viridiplantae model, while for a more 
                         generalist one the Global model should yield more information.", br(), br(), "After the construction of 
                         the tree, PharaohFUN allows the exploration of the proteins encoded by the orthogroup genes, implementing 
                         modules for interactive tree viewing, PFAM module determination, multiple sequence alignment, Gene Ontology 
                         terms annotation, KEGG pathways annotation, exploration of physical interactions between proteins 
                         and scientific literature annotation.")
                                       ),
                                       column(4,
                                              tags$div(style = "margin-left: 20px;",
                                                       img(
                                                         src = "species_gg46-removebg-preview.png",
                                                         alt = "species",
                                                         width = 400,
                                                         height = 600, style="display: block; margin-left: 20px; margin-right: auto;"
                                                       )
                                              )
                                       ),
                                       column(1)
                                       
                                     )

                            )
                            ))))))


                





# Define server logic required to draw a histogram
server <- function(input, output) {

    output$distPlot <- renderPlot({
        # generate bins based on input$bins from ui.R
        x    <- faithful[, 2]
        bins <- seq(min(x), max(x), length.out = input$bins + 1)

        # draw the histogram with the specified number of bins
        hist(x, breaks = bins, col = 'darkgray', border = 'white',
             xlab = 'Waiting time to next eruption (in mins)',
             main = 'Histogram of waiting times')
    })
}

# Run the application 
shinyApp(ui = ui, server = server)
