
## Author: Emma Serrano Pérez

### Packages
library(shiny)
library(shinydashboard)
library(dashboardthemes)
library(DT)


## Auxiliar functions

plot.rhythm <- function(gene.id, gene.name, genes.expression, photoperiod_sel, nitrate_sel)
{
  # Initiate
  current.gene.expression.hnld <- 0
  current.gene.expression.hnsd <- 0
  current.gene.expression.lnld <- 0
  current.gene.expression.lnsd <- 0
  
  # Condition expression
  hnld.zt <- paste("HNLD",paste0("zt",sprintf(fmt = "%02d",seq(from=0,to=20,by=4))),sep="_")
  current.gene.expression.hnld <- genes.expression[gene.id,c(paste(hnld.zt,1,sep="_"),
                                                             paste(hnld.zt,2,sep="_"), paste(hnld.zt,3,sep="_"))]
  
  hnsd.zt <- paste("HNSD",paste0("zt",sprintf(fmt = "%02d",seq(from=0,to=20,by=4))),sep="_")
  current.gene.expression.hnsd <- genes.expression[gene.id,c(paste(hnsd.zt,1,sep="_"),
                                                             paste(hnsd.zt,2,sep="_"), paste(hnsd.zt,3,sep="_"))]
  
  lnld.zt <- paste("LNLD",paste0("zt",sprintf(fmt = "%02d",seq(from=0,to=20,by=4))),sep="_")
  current.gene.expression.lnld <- genes.expression[gene.id,c(paste(lnld.zt,1,sep="_"),
                                                             paste(lnld.zt,2,sep="_"), paste(lnld.zt,3,sep="_"))]
  
  lnsd.zt <- paste("LNSD",paste0("zt",sprintf(fmt = "%02d",seq(from=0,to=20,by=4))),sep="_")
  current.gene.expression.lnsd <- genes.expression[gene.id,c(paste(lnsd.zt,1,sep="_"),
                                                             paste(lnsd.zt,2,sep="_"), paste(lnsd.zt,3,sep="_"))]
  
  # Max o Min
  max.expr <- max(as.numeric(c(current.gene.expression.hnld, current.gene.expression.hnsd,
                               current.gene.expression.lnld, current.gene.expression.lnsd)))
  min.expr <- min(as.numeric(c(current.gene.expression.hnld, current.gene.expression.hnsd,
                               current.gene.expression.lnld, current.gene.expression.lnsd)))
  
  # Plot basic
  plot(x = -10,y= -10,axes=F,xlab="",ylab="FPKM",
       ylim=c((min.expr-(max.expr- min.expr)/5), max.expr),xlim=c(0,72),
       main=paste(gene.id, gene.name,sep=" - "),cex.main=2)
  axis(side=2,lwd=3)
  zt.points<-paste0("zt",sprintf(fmt = "%02d",seq(from=0,to=20,by=4)),sep="_")
  zt.points.rep<-c(paste(zt.points,1,sep=""),paste(zt.points,2,sep=""), paste(zt.points,3,sep=""))
  axis(side = 1,lwd = 3, at=seq(from=0,to=68,by=4),labels = zt.points.rep,las=2)
  
  # Conditions lines
  if( photoperiod_sel == "Long Day (LD)" && nitrate_sel == "Control (HN)" )
  {
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.hnld,type="o",lwd=3,col="blue")
  }
  else if( photoperiod_sel == "Short Day (SD)" && nitrate_sel == "Control (HN)" )
  {
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.hnsd,type="o",lwd=3,col="red")
  }
  else if( photoperiod_sel == "Long Day (LD)" && nitrate_sel == "Low Nitrate (LN)" )
  {
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.lnld,type="o",lwd=3,col="green")
  }
  else if( photoperiod_sel == "Short Day (LD)" && nitrate_sel == "Low Nitrate (LN)" )
  {
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.lnsd,type="o",lwd=3,col="yellow")
  }
  else if( photoperiod_sel == "Both" && nitrate_sel == "Control (HN)" )
  {
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.hnld,type="o",lwd=3,col="blue")
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.hnsd,type="o",lwd=3,col="red")
  }
  else if( photoperiod_sel == "Both" && nitrate_sel == "Low Nitrate (LN)" )
  {
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.lnld,type="o",lwd=3,col="green")
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.lnsd,type="o",lwd=3,col="yellow")
  }
  else if( photoperiod_sel == "Long Day (LD)" && nitrate_sel == "Both" )
  {
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.hnld,type="o",lwd=3,col="blue")
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.lnld,type="o",lwd=3,col="green")
  }
  else if( photoperiod_sel == "Short Day (SD)" && nitrate_sel == "Both" )
  {
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.hnsd,type="o",lwd=3,col="red")
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.lnsd,type="o",lwd=3,col="yellow")
  }
  else if( photoperiod_sel == "Both" && nitrate_sel == "Both" )
  {
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.hnld,type="o",lwd=3,col="blue")
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.hnsd,type="o",lwd=3,col="red")
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.lnld,type="o",lwd=3,col="green")
    lines(x = seq(from=0,by=4,to=68),current.gene.expression.lnsd,type="o",lwd=3,col="yellow")
  }
  
  
  
  # Adding lines
  for(i in 0:2)
  {
    current.line <- (max.expr- min.expr)/20
    
    if(photoperiod_sel == "Long Day (LD)")
    {
      polygon(x = c(24*i, 24*i+16, 24*i+16, 24*i),
              y=c(min.expr-current.line, min.expr-current.line, min.expr-(3/2*current.line), min.expr-(3/2*current.line)),
              lwd=2,border="blue")
      polygon(x = c(24*i+16,24*(i+1),24*(i+1),24*i+16),
              y=c(min.expr-current.line, min.expr-current.line, min.expr-(3/2*current.line), min.expr-(3/2*current.line)),
              lwd=2,border="blue",col="blue")
      current.line <- 2*current.line
    }
    else if(photoperiod_sel == "Short Day (SD)")
    {
      polygon(x = c(24*i, 24*i+8, 24*i+8, 24*i),
              y=c(min.expr-current.line, min.expr-current.line, min.expr-(current.line+(max.expr- min.expr)/40),
                  min.expr-(current.line+(max.expr- min.expr)/40)), lwd=2,border="red")
      polygon(x = c(24*i+8,24*(i+1),24*(i+1),24*i+8),
              y=c(min.expr-current.line, min.expr-current.line, min.expr-(current.line+(max.expr- min.expr)/40),
                  min.expr-(current.line+(max.expr- min.expr)/40)), lwd=2,border="red",col="red")
    }
    else
    {
      polygon(x = c(24*i, 24*i+16, 24*i+16, 24*i),
              y=c(min.expr-current.line, min.expr-current.line, min.expr-(3/2*current.line), min.expr-(3/2*current.line)),
              lwd=2,border="blue")
      polygon(x = c(24*i+16,24*(i+1),24*(i+1),24*i+16),
              y=c(min.expr-current.line, min.expr-current.line, min.expr-(3/2*current.line), min.expr-(3/2*current.line)),
              lwd=2,border="blue",col="blue")
      current.line <- 2*current.line
      
      polygon(x = c(24*i, 24*i+8, 24*i+8, 24*i),
              y=c(min.expr-current.line, min.expr-current.line, min.expr-(current.line+(max.expr- min.expr)/40),
                  min.expr-(current.line+(max.expr- min.expr)/40)), lwd=2,border="red")
      polygon(x = c(24*i+8,24*(i+1),24*(i+1),24*i+8),
              y=c(min.expr-current.line, min.expr-current.line, min.expr-(current.line+(max.expr- min.expr)/40),
                  min.expr-(current.line+(max.expr- min.expr)/40)), lwd=2,border="red",col="red")
    }
    
  }
  
  return(0)  
}



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
                                                          style = 'color: #1f618d; font-weight: 540; font-size: 42px; font-family: "Alatsi"",
                                                          Verdana, sans-serif;')),
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
                                                tags$li(tags$b("Explore rhythmicity"), ". Enables the user to explore how rhythmic genes are affected by
                                                        the photoperiod and abiotic stress (nitrate deficiency in the medium) to which",tags$em("R. supcapitata"),
                                                        "is subjected."),
                                                tags$br(),
                                                tags$li(tags$b("Gene Coexpression Network"),". If desired, the user can browse the",tags$em("R. supcapitata"),
                                                        "co-expression network generated from multiple conditions (nitrate availability, photoperiod and daytime)
                                                        to determine potentially important connections between genes."),
                                                tags$br(),
                                                tags$li(tags$b("Predictive model"),". To determine whether there is indeed a direct relationship between genes,
                                                        a predictive model has been made available that shows the most notable connections between particular
                                                        genes. Several parameters of the model can be tuned by the user on demand."),
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
                                  choices = c("Long Day (LD)", "Short Day (SD)","Both"),
                                  selected = "Long Day (LD)",
                                  inline = TRUE, 
                                  status = "info"
                                ),
                                br(),
                                span(tags$b("Nitrate availability"), style = "color:#34c5d1; font-size: 16px; "),
                                div(br(),"Please select the desired nitrate condition from the following list for performing the analysis:",br()),
                                shinyWidgets::awesomeRadio(
                                  inputId = "nitrate_1",
                                  label = "", 
                                  choices = c("Control (HN)", "Low Nitrate (LN)","Both"),
                                  selected = "Control (HN)",
                                  inline = TRUE, 
                                  status = "info"
                                ),
                                br(),
                                span(tags$b("Gene of interest"), style = "color:#34c5d1; font-size: 16px; "),
                                div(br(),"Below you can write the ID associated to the gene whose rhythmicity you
                             wish to analyze."),
                                fluidRow(
                                  column(5, textInput("geneInt1", label = "", 
                                                      width = "100%", placeholder = "Rsub_00001")),
                                  column(2, div( style = "margin-top: 21px;", 
                                                 shinyWidgets::actionBttn("run_button2", "Let's go", size = "sm", icon = icon("magnifying-glass"),
                                                                          style = "float", color = "primary")))),
                                shinyjs::useShinyjs(),
                                shinyjs::hidden(div(id='loading.graph',h3('Please be patient, building graph ...')))
                                
                            ),
                            br(),
                            box(status = "info",  width = "500",
                                title = span(tags$b("Results"), style = "color:#34c5d1; font-size: 16px; "),
                                "Results for the query gene are displayed below, arranged in 
                      different tabs. The execution of each analysis is initiated from the", tags$b("Start 
                      button"),"within each of the tabs, with specific instructions for each analysis.",
                                div(br()),
                                tabsetPanel(type = "tabs",
                                            tabPanel("Gene Representation", 
                                                     fluidRow(br()),
                                                     div("This tab shows two results. First of all you can see how your gene adapt
                                                         along the day in every condition by a graphical representation. In other instance,
                                                         you can see if your gene is rhythmic or not according to a non-parametric test (RAIN)."),
                                                     fluidRow(br()),
                                                     fluidRow(column(6,align="center",
                                                                     tags$div(id = "box_rhythm")),
                                                              column(5,br(),
                                                               span(tags$div(id = "box_rhythm_table"),style = "color:black;
                                                                            font-size: 16px;",align = "center"))),
                 
                                                     
                                            ),
                                            tabPanel("Statistical Analysis",
                                                     fluidRow(tags$br()),
                                                     div("Finally, this tab shows several measurements (mesor, phase and amplitude)
                                                         for your gene in each condition"),
                                                     fluidRow(tags$br()),
                                                     fluidRow(column(12,align="center",
                                                                     span(tags$div(id = "box_circa_table")))),
                                                     
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
                            br(),
                            fluidRow(column(11,tags$div(align="justify", style = 'font-size: 16px;',
                                                        tags$b("NERITES"), "allows researchers to explore the expression profiles of 
                                      individual genes in",tags$b(tags$i("Raphidocelis subcapitata")), " and analyse their rhythmicity. 
                                      This data has been generated in our lab over three complete diurnal cycles under", 
                                                        tags$b("long day (summer day, 16h light / 8h dark) and short day (winter day, 8h light / 16h dark)"), 
                                                        "conditions. In this app, users can visualize gene expression profiles of their
                             interest, compare their patterns under short day and long day conditions. Users can also
                             perform" , tags$b("statistical analysis"),  "over the rhythmicity of gene expression profiles.", " See our", tags$b("video tutorial"),
                                                        "for details or follow the next steps to perform your analysis")))
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
  
  # Initiation UI
  box_rhythm_exits <<- F
  box_rhythm_table_exits <<-F
  box_circa_table_exits <<- F

  ## Rhythm exploration
  
  genes.expression <- read.table("data/gene_expression_matrix_fixed")
  gene_names <- read.table("data/complete_fun_anno_raphi.csv")
  rhythm_genes_hnld <- read.table("data/hnld.rhytmic.table.tsv")[,1]
  rhythm_genes_hnsd <- read.table("data/hnsd.rhytmic.table.tsv")[,1]
  rhythm_genes_lnld <- read.table("data/lnld.rhytmic.table.tsv")[,1]
  rhythm_genes_lnsd <- read.table("data/lnsd.rhytmic.table.tsv")[,1]
  circa_params <- read.table("data/circa_parameters.tsv")

  
  
  observeEvent(input$run_button2,{
    
    shinyjs::showElement(id = 'loading.graph')
    
    # Plot rhythm
    gene.id <- input$geneInt1
    gene.name <- subset(gene_names[1,], gene.id %in% gene_names[,1])[,2]
    photoperiod_sel <- input$photoperiod_1
    nitrate_sel <- input$nitrate_1
    
    if(box_rhythm_exits)
    {
      removeUI(
        selector = "div:has(>>> #plot_rhythm)",
        multiple = TRUE,
        immediate = TRUE
      )
      box_rhythm_exits <<- F
    }
    
    insertUI("#box_rhythm", "afterEnd", ui = {
      box(width = 12,
          title = "Graphical Representation", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          plotOutput("plot_rhythm"))
    })
    
    box_rhythm_exits <<- T
    
    output$plot_rhythm <- renderPlot(plot.rhythm(gene.id = gene.id, gene.name = gene.name,
                                                 genes.expression = genes.expression, photoperiod_sel = photoperiod_sel,
                                                 nitrate_sel = nitrate_sel))
    
    # Rhythm table
    Conditions <- c("HNLD","HNSD","LNLD","LNSD")
    Rhythmicity <- c("...","...","...","...")
    rhythm_table <- data.frame(Conditions,Rhythmicity)
    
    if(gene.id %in% rhythm_genes_hnld)
    {
      rhythm_table[1,2] <- "Rhythmic gene"
    }
    else{
      rhythm_table[1,2] <- "Non-hythmic gene"
    }
    
    if(gene.id %in% rhythm_genes_hnsd)
    {
      rhythm_table[2,2] <- "Rhythmic gene"
    }
    else{
      rhythm_table[2,2] <- "Non-hythmic gene"
    }
    
    if(gene.id %in% rhythm_genes_lnld)
    {
      rhythm_table[3,2] <- "Rhythmic gene"
    }
    else{
      rhythm_table[3,2] <- "Non-hythmic gene"
    }
    
    if(gene.id %in% rhythm_genes_lnsd)
    {
      rhythm_table[4,2] <- "Rhythmic gene"
    }
    else{
      rhythm_table[4,2] <- "Non-hythmic gene"
    }
    
    if(box_rhythm_table_exits)
    {
      removeUI(
        selector = "div:has(>>> #rhythm_gene)",
        multiple = TRUE,
        immediate = TRUE
      )
      box_rhythm_table_exits <<- F
    }
    
    insertUI("#box_rhythm_table", "afterEnd", ui = {
      box(width = 12,
          title = "Rhythmicity", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          DTOutput("rhythm_gene"))
    })
    
    box_rhythm_table_exits <<- T
    
    output$rhythm_gene <- DT::renderDT(rhythm_table)
    
    shinyjs::hideElement(id='loading.graph')
    
    
    # Statistical Analysis 
    if(box_circa_table_exits)
    {
      removeUI(
        selector = "div:has(>>> #circa_table)",
        multiple = TRUE,
        immediate = TRUE
      )
      box_circa_table_exits <<- F
    }
    
    insertUI("#box_circa_table", "afterEnd", ui = {
      box(width = 12,
          title = "Parametric Results", status = "primary", solidHeader = TRUE,
          collapsible = TRUE,
          tags$div(DTOutput("circa_table")))
          
    })
    
    box_circa_table_exits <<- T
    
    output$circa_table <- DT::renderDT(DT::datatable(circa_params[gene.id, ],
                                                     options = list(scrollX = T)))
    
  })
}

# Run the application 
shinyApp(ui = ui, server = server)
