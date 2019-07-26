function(input, output,session) {
  
  current_id=reactiveVal("")
  
  
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
    print(nrow(data))
    data=data[,.SD[.N],by=c("time","input","id")]
    print(nrow(data))
    data[,"nb_click":=.(.N-1),by="id"]
    data=data[nb_click>0]
    print(nrow(data))
    data$nb_click=NULL
    data
  })
  
  colors_reac=reactive({
    data=shinylogs()
    inputs=unique(data$input)
    colors_df=lapply(1:length(inputs),function(i){
      my_pals=sample(palettes,length(inputs),replace=T)
      my_input=inputs[i]
      my_valeurs=unique(data[input==my_input]$valeur)
      my_colors=RColorBrewer::brewer.pal(length(my_valeurs),my_pals[i])
      if(length(my_valeurs>=10)){
        my_colors=colorRampPalette(my_colors)(length(my_valeurs))
      } 
      
      val_cols=data.table(input=my_input,valeur=my_valeurs,color=my_colors[1:length(my_valeurs)])
    })
    colors_df=rbindlist(colors_df)
    colors_df=colors_df[,.SD[1],by=c("input","valeur")]
    colors_df$color=factor(colors_df$color)
    colors_df
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
                               name=~last_input,key=~id,type = "scatter",source = "plot_stats")%>%
      hide_legend()%>%layout(xaxis = list(title = input$var1), yaxis = list(title = input$var2))
  })
  
  
  output$activity_dygraph=renderDygraph({
    one_point=event_data("plotly_hover",source = "plot_stats")
    one_click=event_data("plotly_click",source = "plot_stats")
    req(isTruthy(one_point)|isTruthy(one_click))
    # print("hover")
    # print(one_point)    
    one_id=one_point$key
    if(is.null(one_id)){
      one_id=one_click$key
    }
    # print("click")
    # print(one_click)
    one_session=shinylogs()[id==one_id,]
    one_session$nb_interactions=1:nrow(one_session)
    one_session[,c("time","nb_interactions")]%>%
      dygraph(main = paste0("Nombre d'actions de la session ",one_id))%>%
      dyRangeSelector()%>%
      dyUnzoom()%>%
      dyCrosshair()
  })
  
  sub_data_buttons = reactive({
    print("render buttons interactions")
    one_click=event_data("plotly_click",source = "plot_stats")
    req(one_click)
    one_id=one_click$key
    sub_data = data[id==one_id]
    
    sub_data = shinylogs()[id==one_id]
    
    ##### rleid trick : remove fake updates #####
    sub_data[,dup:=rleid(valeur),by="input"]
    sub_data=sub_data[,.SD[1],by=.(input,dup)]
    sub_data$dup=NULL
    
    #### add colors and start-end logic #### 
    colors_ <- colors_reac()
    sub_data = merge(sub_data,colors_,by=c("input","valeur"))
    sub_data = sub_data[,.("next_time"=lead(time),time=time,valeur=valeur,id=id,color=color),by="input"]
    last_click=max(sub_data$time)+lubridate::seconds(30)
    sub_data[is.na(next_time), "next_time":=last_click]
    sub_data$y=factor(sub_data$input,levels=unique(sub_data$input))
    setorder(sub_data,time)
    sub_data$event=1:nrow(sub_data)
    names(sub_data)
    sub_data=melt.data.table(sub_data,id.vars = c("input","valeur","id","color","y","event"))
    setnames(sub_data,"value","time")
    # sub_data[,variable:=NULL]
    setorder(sub_data,time)
    sub_data$y=forcats::fct_inorder(sub_data$y)
    sub_data$size=15
    sub_data
  })
  
  
  output$choix_buttonsUI=renderUI({
    sub_data = sub_data_buttons()
    selectizeInput("choix_buttons","Bouttons à observer",choices=unique(sub_data$input),selected=unique(sub_data$input),multiple=T,
                   options=list(plugins= list('remove_button')),width="100%")
  })  
  
  
  output$interaction_numberUI=renderUI({
    sub_data = sub_data_buttons()
    req(input$choix_buttons)
    choix=input$choix_buttons
    sub_data = sub_data[input %in% choix]
    nb_events=uniqueN(sub_data$event)
    sliderInput("interaction_number",sprintf("Commencer au n-ème événement (fenêtre de %s événements)",window_width),
                min = 1,max=max(1,nb_events-window_width),value=2,step=1,width = "100%", 
                animate = animationOptions(interval = 1000, loop = TRUE))
  })
  

  
    output$bouttons_interaction=renderPlotly({
    req(input$interaction_number)
    sub_data = sub_data_buttons()
    min_date=min(sub_data$time)
    max_date=max(sub_data$time)
    choix=input$choix_buttons
    sub_data = sub_data[input%in%choix]
    my_events = unique(sub_data$event)
    my_events = sort(my_events)
    my_events = my_events[seq(input$interaction_number,input$interaction_number+window_width,1)]
    
    tiny_data=sub_data[event %in% my_events]
    tiny_data[event %in% my_events[1] & variable %in% "time"]$size <- 24
    tiny_data[event %in% my_events[2] & variable %in% "time"]$size <- 22
    tiny_data[event %in% my_events[3] & variable %in% "time"]$size <- 20
    tiny_data[event %in% my_events[4] & variable %in% "time"]$size <- 18
    tiny_data[event %in% my_events[5] & variable %in% "time"]$size <- 16
    
    annotation_data=sub_data[event %in% my_events[1:2] & variable%in%"time"]
    annotation_data$ax=c(80,80)
    superposed=ifelse(annotation_data$input[1] == annotation_data$input[2],-1,1)
    annotation_data$ay=c(-30*superposed,-30)
    time_diff=difftime(annotation_data$time[2]-annotation_data$time[1],units = "secs")
    time_diff=round(as.numeric(time_diff),2)
    annotation_data[1,text:=paste0("Valeur:\n", valeur,"\nEvent n°: ",event)]
    annotation_data[2,text:=paste0("Valeur:\n", valeur,"\nEvent n°: ",event,"\nTimeDiff:",time_diff,"sec")]
    
    # tiny_data$size[length(tiny_data$size)] <- 20
    setorder(tiny_data,time)
    plot_ly(data=tiny_data,x=~time,y=~y,color=~color,split = ~event,size = ~size,text=~paste("Bouton: ", input, "<br>",
                                                                                        "Valeur: ", valeur, "<br>",
                                                                                        "Temps: ", time),
            type="scatter",mode="lines+markers",line = list(width=10),marker=list(line=list(width=15),symbol=8))%>%
      hide_legend()%>%layout(hovermode= "closest",xaxis = list(title = "Temps", showspikes= T, spikemode= "toaxis", spikesnap= "cursor+data",
                                                               range = c(min_date,max_date)),
                             yaxis = list(title = "Bouton"))%>%
      add_annotations(data = annotation_data,
                      x = ~time,
                      y = ~y,
                      text = ~text,
                      xref = "x",
                      yref = "y",
                      showarrow = TRUE,
                      arrowhead = 4,
                      arrowsize = .5,
                      ax = ~ax,
                      ay = ~ay)
    
  })
  
  
  
}


