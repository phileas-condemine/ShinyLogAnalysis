link_input_val = data[,.(value=.N),by=c("input","valeur")]
link_input_val = link_input_val[value>10]



nodes_input=data.table(label=unique(link_input_val$input))
nodes_input$color="blue"
nodes_valeur=data.table(label=unique(link_input_val$valeur))
nodes_valeur$color="red"
nodes=rbind(nodes_input,nodes_valeur)
nodes$id = as.numeric(factor(nodes$label))-1



link_input_val = merge(link_input_val,nodes[,c("label","id")],by.x="input",by.y="label")
setnames(link_input_val,"id","source")
link_input_val = merge(link_input_val,nodes[,c("label","id")],by.x="valeur",by.y="label")
setnames(link_input_val,"id","target")    


p <- plot_ly(
  type = "sankey",
  orientation = "h",
  
  node = list(
    label = nodes$label,
    color = nodes$color,
    pad = 15,
    thickness = 20,
    line = list(
      color = "black",
      width = 0.5
    )
  ),
  
  link = list(
    source = link_input_val$source,
    target = link_input_val$target,
    value =  link_input_val$value
  )
) %>% 
  layout(
    title = "Basic Sankey Diagram",
    font = list(
      size = 10
    )
  )

p