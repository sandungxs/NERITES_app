
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
                                            style = "margin-top: 45px; margin-left: 35px;",
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
                            # Introduction
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
                                       # Home Boxes
                                       column(12,style= 'margin-left: 40px;',
                                              valueBox(a("Rhythmicity", href="#shiny-tab-rhythm", "data-toggle" = "tab", "style" = "color:white"),
                                                       "Explore how rhythmicity changes under stress",
                                                       icon = icon("wave-square",class = "fa-solid fa-wave-square",
                                                                   lib ="font-awesome" ), width = 4),
                                              
                                              valueBox(a("Gene Network", href="#shiny-tab-coexpression", "data-toggle" = "tab", "style" = "color:white"),
                                                       "Build your own gene coexpression network",
                                                       icon = icon("circle-nodes",class = "fa-solid fa-circle-nodes",
                                                                   lib ="font-awesome" ), width = 4, color = "lime"),
                                              
                                              valueBox(a("Predictive model", href="#shiny-tab-predictive", "data-toggle" = "tab", "style" = "color:white"),
                                                       "Tune our predictive model",
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
                                     fluidRow(hr(style = 'margin-left: 80px;'),br()),
                                     fluidRow(
                                       column(4,
                                              tags$div(style = "margin-left: 20px;",
                                                       img(
                                                         src = "rafi.jpg",
                                                         alt = "rafi",
                                                         width = 450,
                                                         height = 300, style="display: block; margin-left: 20px; margin-right: auto;"
                                                       )
                                              )
                                       ),
                                       column(8, 
                                              tags$div(align="justify", style = 'font-size: 16px; margin-left: 140px;',  br(), br(),
                                                       "The exploration of orthology is based on the construction of orthogroups, sets of genes that descend from a 
                         single gene in the common ancestor of the species under study. In this way, it is possible to trace the evolutionary 
                         history of these genes and to analyze the changes that the orthogroup has undergone from its appearance to 
                         its current situation in the extant species.", br(), br(), "Regarding the species supported by the tool, two different models are offered. 
                         The default model focuses on 36 species of the ", tags$b("Viridiplantae"), " clade, with representatives of the 
                         Chlorophyta and Streptophyta clades chosen to span the evolutionary set of these groups.",
                                                       br(), br(),br(),
                                              column(1))),
                                     fluidRow(br()),
                                     fluidRow(column(12,
                                              tags$div(align= "justify", style = 'font-size: 16px; margin-left: 35px;', br(),
                                                       "Furthermore, an additional ", tags$b("Global"), " model includes species from the whole Archaeplastida clade, 
                         as well as examples of Stramenopiles and Cryptophytes. Note that although both models contain the green lineage,
                         their results may vary because the common ancestors of the groups are very distant in time,
                         so for a more accurate analysis it is recommended to use the Viridiplantae model, while for a more 
                         generalist one the Global model should yield more information.", br(), br(), "After the construction of 
                         the tree, PharaohFUN allows the exploration of the proteins encoded by the orthogroup genes, implementing 
                         modules for interactive tree viewing, PFAM module determination, multiple sequence alignment, Gene Ontology 
                         terms annotation, KEGG pathways annotation, exploration of physical interactions between proteins 
                         and scientific literature annotation.")),
                                                       
                                                       ),
                                       
                                       column(1),br()
                                       
                                     ),
                                     fluidRow(hr(style = 'margin-left: 80px;'))

                            )
                            ))),
                    
                    # Rhythm exploration
                    tabItem(tabName = "rhythm",
                            h2(""),
                            fluidRow(valueBox("Rhythmicity",
                                              subtitle = "Explore how rhythmicity changes under stress",
                                              icon = icon("wave-square",class = "fa-solid fa-wave-square",
                                                          lib ="font-awesome" ), width = 6)),
                            br(),
                            fluidRow(column(11,tags$div(align="justify", style = 'font-size: 16px;',
                            tags$b("NERITES"), "allows researchers to explore the expression profiles of 
                                      individual genes in",tags$b(tags$i("Raphidocelis subcapitata")), " and analyse their rhythmicity. 
                                      This data has been generated in our lab over three complete diurnal cycles under", 
                                     tags$b("long day (summer day, 16h light / 8h dark) and short day (winter day, 8h light / 16h dark)"), 
                                     "conditions. In this app, users can visualize gene expression profiles of their
                             interest, compare their patterns under short day and long day conditions. Users can also
                             perform" , tags$b("statistical analysis"),  "over the rhythmicity of gene expression profiles.", " See our", tags$b("video tutorial"),
                                     "for details or follow the next steps to perform your analysis"),
                            br(),
                            box(title = span(tags$b("Photoperiod selection"), style = "color:#34c5d1; font-size: 16px; "), status = "info", width = "500",
                                "Please select the desired photoperiod from the following list for performing the analysis:", br(),
                                
                                shinyWidgets::awesomeRadio(
                                  inputId = "photoperiod_1",
                                  label = "", 
                                  choices = c("Long Day (LD)", "Short Day (SD)"),
                                  selected = "Long Day (LD)",
                                  inline = TRUE, 
                                  status = "info"
                                ),
                                br(),
                                span(tags$b("Gene of interest"), style = "color:#34c5d1; font-size: 16px; "),
                                div(br(),"Below you can write the ID associated to the gene whose rhythmicity you
                             wish to analyze."),
                                fluidRow(
                                  column(5, textInput("geneInt1", label = "", 
                                                      width = "100%", placeholder = "R.sub gene")),
                                  column(2, div( style = "margin-top: 21px;", 
                                                 shinyWidgets::actionBttn("run_button2", "Let's go", size = "sm", icon = icon("magnifying-glass"),
                                                                          style = "float", color = "primary")))),
                                
                            ),
                            br(),
                            box(status = "info",  width = "500",
                                title = span(tags$b("Results"), style = "color:#34c5d1; font-size: 16px; "),
                                "Results for the query gene are displayed below, arranged in 
                      different tabs. The execution of each analysis is initiated from the", tags$b("Start 
                      button"),"within each of the tabs, with specific instructions for each analysis.",
                                div(br()),
                                tabsetPanel(type = "tabs",
                                            tabPanel("Graphical Representation", 
                                                     fluidRow(br()),
                                                     div("This tab shows four different results. First of all, a table with up to 5 best 
                                         matches for the query sequence in the chosen proteome, with decreasing confidence. First row
                                         corresponds to best match, which is used to perform subsequent analysis. If you are interested
                                         in another one, please copy de ID and paste it in Gene ID-based search tab. Secondly, a complete list of the genes
                                         that are assigned to the same orthogroup as the query. Next, the proportion of  genes of each species. 
                                         Finally, a gene tree show the evolutionary relationships between those genes. All results can be downloaded 
                                         using the buttons at the bottom of the page."),
                                                     fluidRow(br()),
                                                     
                                                     # shinyjs::useShinyjs(),
                                                     # shinyjs::hidden(div(id='loading.tree2',h3('Please be patient, building graph ...'))),
                                                     # uiOutput(outputId = "error_tree2"),
                                                     # fluidRow(tags$div(id = "box_tree_seq_table2")),
                                                     # fluidRow(tags$br()),
                                                     # splitLayout(cellWidths = c("50%", "50%"),
                                                     #             tags$div(id = "box_tree_text2"), tags$div(id = "box_tree_pie2")),
                                                     # fluidRow(tags$br()),
                                                     # fluidRow(tags$div(id = "box_tree_plot2")),
                                                     # splitLayout(cellWidths = c("33%", "33%", "33%"), 
                                                     #             tags$div(id = "download_tree2"),
                                                     #             tags$div(id = "download_newick2"),
                                                     #             tags$div(id = "download_tree_seqs2"))
                                            ),
                                            tabPanel("Stadistical Analysis",
                                                     fluidRow(tags$br()),
                                                     div("This tab allows for an interactive visualization of the previous tree. 
                                               In particular, in situations where gene duplications have given rise 
                                               to several clades and you want to reduce the tree not in relation to 
                                               the species that appear, but to these clades, this visualization allows 
                                               the collapse of subtrees and the simple exploration of the areas of interest. 
                                               Press Show Collapsable Tree to show the tree."),
                                                     # fluidRow(tags$br()),
                                                     # shinyWidgets::actionBttn("phylo_start2", "Show Collapsable Tree",
                                                     #                          size = "sm", icon = icon("magnifying-glass"),
                                                     #                          style = "float", color = "success"),
                                                     # fluidRow(tags$br()),
                                                     # tags$div(id = "box_phylo2"),
                                                     # fluidRow(tags$br())
                                            )
                                )
                              
                            ))) 
                            
                            
                            ),
                    
                    ## Coexpression Network tab
                    tabItem(tabName = "coexpression",
                            h2(""),
                            fluidRow(valueBox("Gene Network",
                                              subtitle = "Build your own gene coexpression network",
                                              icon = icon("circle-nodes",class = "fa-solid fa-circle-nodes",
                                                          lib ="font-awesome" ), width = 6, color = "lime")),
                            br()
                            ),
                    
                    ## Predictive model tab
                    tabItem(tabName = "predictive",
                            h2(""),
                            fluidRow(valueBox("Predictive model",
                                              subtitle = "Tune your own predictive model",
                                              icon = icon("buromobelexperte",class="fa-brands fa-buromobelexperte",
                                                          lib ="font-awesome"), width = 6, color = "purple")),
                            br()),
                    
                    ##Contact and info
                    tabItem(tabName = "contact_tutorial", 
                            
                            h2(""),
                            fluidRow(valueBox("Contact and Info", 
                                              subtitle = "Acknowledgments and other information",
                                              icon = icon("envelope"), width = 6, color = "teal")),
                            br(),
                            tags$div(align="justify",style = 'font-size: 16px; margin-left: 20px; margin-right:20px; ',tags$b("Authors:"), "Emma Serrano Pérez,
                                     Mercedes García González and Francisco José Romero Campero."),
                            tags$br(),
                            
                            tags$div(align="justify",style = 'font-size: 16px; margin-left: 20px; margin-right:20px;',"We are strongly committed to open
                            access software and open science. NERITES's source code is available
                       at GitHub following the lateral panel link and is released under a GNU General Public License v3.0. If you 
                       experience any problem using NERITES, please create an issue in GitHub and we will address it. For other
                       inquiries, send an email to eserrano3@us.es."))
                    
                    
                    )))


                




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
