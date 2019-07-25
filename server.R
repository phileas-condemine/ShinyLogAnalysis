function(input, output,session) {
  
  shinylogs = reactive({
    req(input$choix_app)
    print(input$choix_app)
    app=input$choix_app
    source("credentials_to_mongodb.R",local = T)
    if (app == apps[1]){
      options(mongodb = cred)
      databaseName <- "indicateurs_sante"
      collectionName <- "test"}
    
    if (app == apps[2]){
      options(mongodb = cred)
      databaseName <- "UX"
      collectionName <- "DistriPens"
    }
    db <- mongo(collection = collectionName,
                url = sprintf(
                  "mongodb+srv://%s:%s@%s/%s",
                  options()$mongodb$username,
                  options()$mongodb$password,
                  options()$mongodb$host,
                  databaseName))
    data=db$find("{}")
    print("get data from mongodb")
    data=data.table(data)
    data[,"nb_click":=.(.N-1),by="id"]
    data=data[nb_click>0]
    data$nb_click=NULL
    data
  })
  
  
  stats_sessions=reactive({
    print("compute stat")
    stats=shinylogs()[,list(nb_click=.N-1,
                     duration=max(time)-min(time),
                     pause_longue=max(difftime(time,lag(time)),na.rm=T),
                     last_input=input[.N],
                     last_value_picked=valeur[.N],
                     buttons_div=uniqueN(input),
                     choices_div=uniqueN(valeur)),by="id"]
    stats
  })
  
  
  output$nb_visiteurs_quotidiens=renderDygraph({
    print("render dygraph")
    dg <- shinylogs()[,list(nb_visiteurs=uniqueN(id)),by=as.Date(time)] %>%
      dygraphs::dygraph(
        # main="Nombre de visites sur le site DistriPensions"
        )
    dg
  })
  
  output$choix_variables = renderUI({
    print("render choix des vars")
    nms=c("Nombre de clics"="nb_click",
          "Durée de la session"="duration",
          "Plus longue interruption pendant la session"="pause_longue",
          "Nombre de boutons manipulés pendant la session"="buttons_div",
          "Nombre de valeurs de paramètres testées"="choices_div")
    list(selectizeInput("var1","Abscisses",choices=nms,selected=nms[2]),
         selectizeInput("var2","Ordonnées",choices=nms,selected=nms[5]))
  })
  
  output$plot_stats_sessions=renderPlotly({
    print("render plotly stats sessions")
    stats_sessions()%>%plot_ly(x=~get(input$var1),y=~get(input$var2),
                               name=~last_input,key=~id,type = "scatter")%>%
      hide_legend()%>%layout(xaxis = list(title = input$var1), yaxis = list(title = input$var2))
  })
    
  
  output$activity_dygraph=renderDygraph({
    one_point=event_data("plotly_hover")
    req(one_point)
    one_id=one_point$key
    print(one_id)
    one_session=shinylogs()[id==one_id,]
    one_session$nb_interactions=1:nrow(one_session)
    one_session[,c("time","nb_interactions")]%>%
      dygraph(main = paste0("Nombre d'actions de la session ",one_id))%>%
      dyRangeSelector()%>%
      dyUnzoom()%>%
      dyCrosshair()
  })
  

    
}
  
  
