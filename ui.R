fluidPage(
  # tags$header(
  #   tags$style(type="text/css", ".jslider { max-width: 200px; }")
  # ),
  # Application title
  titlePanel("Analyse UX de dataviz.drees"),
  sidebarLayout(position="left",
                sidebarPanel(
                  selectizeInput(inputId="choix_app",label="Application à analyser",choices=apps,multiple=T,
                                 options = list(placeholder = "Nom de l'app",plugins= list('remove_button'),maxItems=1)),
                  # conditionalPanel("input.graphes === 'nb_visits' && input.choix_app !== null",
                  #                  actionButton("refresh_pop","Tirer 10 autres sessions",icon=icon("dice"))),
                  conditionalPanel("input.graphes === 'analysis_sessions'",
                                   uiOutput("choix_variables"))
                ),
                conditionalPanel("input.choix_app !== null",
                                 mainPanel(
                                   navbarPage(title="Graphiques",id="graphes",selected = "nb_visits",
                                                tabPanel(title="Nombre de visites",value="nb_visits",
                                                         dygraphOutput("nb_visiteurs_quotidiens"),
                                                         plotlyOutput("sankey_input_valeur")),
                                                tabPanel(title="Analyse des visiteurs",value="analysis_sessions",
                                                         div(class="col-lg-6 col-md-6 col-sm-12 col-xs-12",
                                                             plotlyOutput("plot_stats_sessions")),
                                                         div(class="col-lg-6 col-md-6 col-sm-12 col-xs-12",
                                                             dygraphOutput("activity_dygraph")),
                                                         div(class="col-lg-12 col-md-12 col-sm-12 col-xs-12",
                                                             uiOutput("choix_buttonsUI"),uiOutput("interaction_numberUI")),                                                         
                                                         div(class="col-lg-12 col-md-12 col-sm-12 col-xs-12",
                                                             plotlyOutput("bouttons_interaction"))                                                         
                                                         )
                                   )
                                 )
                )
  )
)