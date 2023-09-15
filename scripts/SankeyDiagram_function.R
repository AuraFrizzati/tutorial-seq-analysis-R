sankey_diagram_outputs<- function(
    
  DSS_freqTable, # this object (DSS = Discrete State Sequence frequency table) has been created as output of "create_seq_outputs" function
  colour_palette_file,
  plot_title = ""
)
{
  
  ## [0] load required libraries
  require(dplyr)       ## data wrangling
  require(stringr)     ## regex
  require(networkD3)   ## to create a Sankey diagram
  require(htmlwidgets) ## to add title to Sankey diagram
  require(htmltools)   ## to add title to Sankey diagram
  
  ## [1] reformat DSS
  df_split<-stringr::str_split(DSS_freqTable$Sequence,"-",simplify = TRUE) %>% 
    as.data.frame()
  
  # rename columns
  names(df_split)<-sprintf("Step_%d", 1:ncol(df_split))
  
  # make the rank of the state explicit ("step1", "step2", etc)
  #n_steps<-stringr::str_sub(names(df_split), start = -1)
  n_steps <-  1:ncol(df_split)
  for (i in 1: length(n_steps)) {
    df_split[,paste0("Step_",n_steps[i])] <- 
      gsub('/1',paste0('/ST',n_steps[i],'||'),
           df_split[,paste0("Step_",n_steps[i])])
  }
  
  # add the DSS_freqTable
  df_split <- cbind(df_split,DSS_freqTable)
  
  # add count of those that do not move from Step_1
  df_split<-
    df_split %>%
    mutate(
      Step_2 = 
        if_else(
          ## Step_2 blank identifies those that do not progress
          Step_1 %in% unique(df_split$Step_1) & Step_2 == "", 
          ## changing the step id from S1 to S2 to avoid loops in the plot
          gsub('/ST1','/ST2',df_split[,"Step_1"]),
          Step_2))
  
  # reshape the table from wide to long
  list_all_steps <- list() #create an empty list
  
  for (i in 1:(length(n_steps)-1)) {
    list_all_steps[[i]]<-df_split %>%
      select(colnames(df_split)[i],colnames(df_split)[i+1],Freq) %>%
      `colnames<-`(c("source","target","value"))
  }
  
  df_all_steps <- do.call("rbind",list_all_steps)
  
  
  ## [2] Create LINKS and NODE dataframes
  
  # compress the "df_all_steps" data
  links <- 
    df_all_steps %>%
    group_by(source,target) %>%
    summarise(value = sum(value)) %>% 
    ungroup() %>%
    mutate_if(is.character, ~na_if(., '')) %>% ## replace blanks in the characters cols with NAs
    na.omit() %>% 
    mutate(
      group = gsub( "/.*$", "", source),
      group = gsub(" ", "", group, fixed = TRUE)
    ) %>% ## remove dead-end nodes
    as.data.frame()
  
  nodes <- data.frame(
    name=c(as.character(links$source),
           as.character(links$target)) %>% unique()
  )
  
  links$IDsource <- match(links$source, nodes$name)-1 # remove 1 to have ids starting at 0
  links$IDtarget <- match(links$target, nodes$name)-1 # remove 1 to have ids starting at 0
  
  ## [3] Create Nodes Groups
  nodes <-
    nodes %>%
    mutate(
      group = gsub( "/.*$", "", name)
    ) %>%
    left_join(colour_palette_file, by = c("group" = "Labels")) %>%
    mutate(
      colour = if_else(
        group == "*", "grey", Exadecimal.Codes),
      group = gsub(" ", "", group, fixed = TRUE)
    ) 
  
  nodes_groups<-
    nodes %>%
    distinct(group,colour)
  
  nodes_groups$group<-paste0('"',nodes_groups$group,'"')
  nodes_groups$colour<-paste0('"',nodes_groups$colour,'"')
  
  nodes_groups_colours_lookup <- 
    paste0("d3.scaleOrdinal() .range([",
           paste(nodes_groups$colour,collapse=','),
           "])"
    )
  
  ## [4] Plot the Sankey diagram
  Sankey_plot <- 
    networkD3::sankeyNetwork(
      Links = links, 
      Nodes = nodes,
      Source = "IDsource", 
      Target = "IDtarget",
      Value = "value", 
      NodeID = "name",
      sinksRight=FALSE,
      fontSize = 12,
      height = 600,
      width = 800, 
      NodeGroup="group",
      LinkGroup="group",
      colourScale=nodes_groups_colours_lookup
    )
  
  # add title to the diagram
  Sankey_plot<-htmlwidgets::prependContent(
    Sankey_plot, 
    htmltools::tags$p(plot_title))
  
  ## [5] create list of output objects
  output_list<-list(
    "Sankey_plot" = Sankey_plot
    ,"links" = links
    ,"nodes" = nodes
    ,"nodes_groups" = nodes_groups
  )
}