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
    
    if (app == apps[3]){
      options(mongodb = cred)
      databaseName <- "UX"
      collectionName <- "Care-M"
    }
    
    if (app == apps[4]){
      options(mongodb = cred)
      databaseName <- "UX"
      collectionName <- "condition_de_vie_des_enfants"
    }
    
    if (app == apps[5]){
      options(mongodb = cred)
      databaseName <- "UX"
      collectionName <- "DicoSNDS"
    }
    
    if (app == apps[6]){
      options(mongodb = cred)
      databaseName <- "UX"
      collectionName <- "LandingPage"
    }
    
    if (app == apps[7]){
      options(mongodb = cred)
      databaseName <- "UX"
      collectionName <- "demographie_ps"
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
    data=data.table(data,fill=T)
    data = data[,c("time","id","input","valeur")]
    data = na.omit(data)
    print(nrow(data))
    data=data[,.SD[.N],by=c("time","input","id")]
    
    print(nrow(data))
    data[,"nb_click":=.(.N-1),by="id"]
    data=data[nb_click>0]
    print(nrow(data))
    data$nb_click=NULL
    data
  })
  
  output$dist_valeurs_input=renderPlotly({
    data=shinylogs()
    stats=data[,.(count=.N),by=c("input","valeur")]
    setorder(stats,-count)
    stats%>%head(50)%>%
    plot_ly(x=~input,y=~count,color=~valeur,text=~paste0("Valeur: ",valeur))%>%
      hide_legend()%>%layout(hovermode = 'compare',title="Nombre de clics sur les 50 modalités préférées",
                             xaxis=list(title = "Bouton"),yaxis=list(title = "Nombre de clics"))
    
  })
  
  
  
  # output$sankey_input_valeur=renderPlotly({
  #   
  #   data=shinylogs()
  #   # run=input$refresh_pop
  #   # ids=sample(unique(data$id),10)   
  #   # sub_data=data[id %in% ids]
  #   link_input_val = data[,.(value=.N),by=c("input","valeur")]
  #   setorder(link_input_val,-value)
  #   link_input_val = head(link_input_val,30)
  #   
  #   
  #   nodes_input=data.table(label=unique(link_input_val$input))
  #   nodes_input$color="blue"
  #   nodes_input$id=0:(nrow(nodes_input)-1)
  #   nodes_valeur=data.table(label=unique(link_input_val$valeur))
  #   nodes_valeur$color="red"
  #   nodes_valeur$id=0:(nrow(nodes_valeur)-1)+nrow(nodes_input)
  #   nodes=rbind(nodes_input,nodes_valeur)
  #   
  #   nrow(link_input_val)
  #   link_input_val = merge(link_input_val,nodes[,c("label","id")],by.x="input",by.y="label")
  #   nrow(link_input_val)
  #   setnames(link_input_val,"id","source")
  #   link_input_val = merge(link_input_val,nodes[,c("label","id")],by.x="valeur",by.y="label")
  #   nrow(link_input_val)
  #   setnames(link_input_val,"id","target")
  #   
  #   p <- plot_ly(
  #     type = "sankey",
  #     orientation = "h",
  #     
  #     node = list(
  #       label = nodes$label,
  #       color = nodes$color,
  #       pad = 15,
  #       thickness = 20,
  #       line = list(
  #         color = "black",
  #         width = 0.5
  #       )
  #     ),
  #     
  #     link = list(
  #       source = link_input_val$source,
  #       target = link_input_val$target,
  #       value =  link_input_val$value
  #     )
  #   ) %>% 
  #     layout(
  #       title = "30 modalités les plus fréquentes",
  #       font = list(
  #         size = 10
  #       )
  #     )
  #   
  #   p
  #   
  # })
  # 
  
  
  
  
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
    dg <- shinylogs()
    dg[,date_d := as.Date(time)]
    dg <- dg[,list(nb_visiteurs=uniqueN(id)),by=date_d]
    alldates <- data.table(date_d=seq.Date(min(dg$date_d), max(dg$date_d), by="day"))
    # merge
    dg <- merge(dg, alldates, by="date_d", all=TRUE)
    dg[is.na(nb_visiteurs),nb_visiteurs:=0]
    dg <- dg[,c("date_d","nb_visiteurs")] %>%
      dygraphs::dygraph(
        # main="Nombre de visites sur le site DistriPensions"
      )
    dg
  })
  
  output$choix_variables = renderUI({
    print("render choix des vars")
    
    list(selectizeInput("var1","Abscisses",choices=nms,selected=nms[2]),
         selectizeInput("var2","Ordonnées",choices=nms,selected=nms[5]))
  })
  
  output$plot_stats_sessions=renderPlotly({
    print("render plotly stats sessions")
    nm1=names(which(nms == input$var1))
    nm2=names(which(nms == input$var2))
    stats_sessions()%>%plot_ly(x=~get(input$var1),y=~get(input$var2),
                               name=~last_input,key=~id,type = "scatter",source = "plot_stats")%>%
      hide_legend()%>%layout(xaxis = list(title = nm1), yaxis = list(title = nm2))
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
    # sub_data = data[id==one_id]
    
    sub_data = shinylogs()[id==one_id]
    
    ##### Focus sur qqBoutons #####
    req(input$choix_buttons)
    choix=input$choix_buttons
    sub_data = sub_data[input%in%choix]
    
    ##### rleid trick : remove fake updates #####
    sub_data[,dup:=rleid(valeur),by="input"]#plusieurs fois la même modalité consécutivement => doublon
    sub_data=sub_data[,.SD[1],by=.(input,dup)]
    sub_data$dup=NULL
    
    #### add colors and start-end logic #### 
    colors_ <- colors_reac()
    sub_data = merge(sub_data,colors_,by=c("input","valeur"))
    # sub_data = sub_data[,.("next_time"=lead(time),time=time,valeur=valeur,id=id,color=color),by="input"]
    setorder(sub_data,time)
    sub_data[,"next_time":=.(lead(time)),by="input"]
    last_click=max(sub_data$time,na.rm=T)+lubridate::seconds(30)
    sub_data[is.na(next_time), "next_time":=last_click]
    setorder(sub_data,time,next_time)
    sub_data$event=1:nrow(sub_data)
    names(sub_data)
    sub_data=melt.data.table(sub_data,id.vars = c("input","valeur","id","color","event"))
    setnames(sub_data,"value","time")
    # sub_data[,variable:=NULL]

    sub_data$size=15
    setorder(sub_data,time)
    sub_data$y=factor(sub_data$input,levels=choix)
    sub_data$y=forcats::fct_inorder(sub_data$y)    
    sub_data
  })
  
  
  output$choix_buttonsUI=renderUI({
    one_click=event_data("plotly_click",source = "plot_stats")
    req(one_click)
    one_id=one_click$key
    sub_data = shinylogs()[id==one_id]
    selectizeInput("choix_buttons","Bouttons à observer",
                   choices=unique(sub_data$input),
                   selected=unique(sub_data$input),multiple=T,
                   options=list(plugins= list('remove_button')),width="100%")
  })  
  
  
  output$interaction_numberUI=renderUI({
    sub_data = sub_data_buttons()
    req(input$choix_buttons)
    choix=input$choix_buttons
    sub_data = sub_data[input %in% choix]
    nb_events=uniqueN(sub_data$event)
    sliderInput("interaction_number",sprintf("Commencer au n-ème événement (fenêtre de %s événements)",window_width),
                min = 1,max=max(1,nb_events-2),value=2,step=1,width = "100%", 
                animate = animationOptions(interval = 2000, loop = F))%>%shinyInput_label_embed(
                  icon("question-circle") %>%
                    bs_embed_tooltip(title = "Utilisez le clavier (flèches droite-gauche) pour passer à l'événement suivant-précédent.\nL'icône de lecture en bas à droite permet d'enchaîner les événements.\nSi vous ne parvenez plus à arrêter l'animation, changez de session en cliquant dans le graphique ci-dessus.")
                )
  })
  
  
  
  output$bouttons_interaction=renderPlotly({
    req(input$interaction_number)
    sub_data = sub_data_buttons()
    min_date = min(sub_data$time)
    max_date = max(sub_data$time)
    my_events = unique(sub_data$event)
    my_events = sort(my_events)
    my_events = my_events[seq(input$interaction_number,
                              min(input$interaction_number+window_width,
                                  length(my_events)),1)]
    
    tiny_data=sub_data[event %in% my_events]
    
    time_event1=tiny_data[event %in% my_events[1] & variable %in% "time"]$time
    annotation_right_left_event1=ifelse(time_event1>mean(c(max_date,min_date)),-1,1)
    time_event2=tiny_data[event %in% my_events[2] & variable %in% "time"]$time
    annotation_right_left_event2=ifelse(time_event2>mean(c(max_date,min_date)),-1,1)
    
    print("check times")
    print(min_date)
    print(min(tiny_data$time))
    print(min(tiny_data[variable=="time"]$time))
    print(time_event1)
    
    tiny_data[event %in% my_events[1] & variable %in% "time"]$size <- 24
    tiny_data[event %in% my_events[2] & variable %in% "time"]$size <- 22
    tiny_data[event %in% my_events[3] & variable %in% "time"]$size <- 20
    tiny_data[event %in% my_events[4] & variable %in% "time"]$size <- 18
    tiny_data[event %in% my_events[5] & variable %in% "time"]$size <- 16
    annotation_data=sub_data[event %in% my_events[1:2] & variable%in%"time"]
    annotation_data$ax=c(50*annotation_right_left_event1,200*annotation_right_left_event2)
    superposed=ifelse(annotation_data$input[1] == annotation_data$input[2],100,-100)
    annotation_data$ay=c(superposed,-100)
    time_diff=difftime(annotation_data$time[2],annotation_data$time[1],units = "secs")
    time_diff=round(as.numeric(time_diff),2)
    annotation_data[1,text:=paste0("Valeur:\n", valeur,"\nEvent n°: ",event)]
    annotation_data[2,text:=paste0("Valeur:\n", valeur,"\nEvent n°: ",event,"\nTimeDiff:",time_diff,"sec")]
    
    # tiny_data$size[length(tiny_data$size)] <- 20
    setorder(tiny_data,time)
    plot_ly(data=tiny_data,x=~time,y=~y,color=~color,split = ~event,size = ~size,text=~paste("Bouton: ", input, 
                                                                                             "\nValeur: ", valeur, 
                                                                                             "\nTemps: ", time, 
                                                                                             "\nEvent n°: ",event,
                                                                                             "\nVariable: ",variable),
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


