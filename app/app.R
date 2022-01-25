## app.R ##
#version 0.18
#https://stackoverflow.com/questions/50380348/shiny-how-to-reference-a-value-from-a-temporary-table
#https://stackoverflow.com/questions/37977499/how-do-you-dynamically-add-sliderinput-to-your-shiny-application
#if(!require(shinyWidgets)) install.packages("shinyWidgets", repos = "http://cran.us.r-project.org")
#if(!require(shinydashboard)) install.packages("shinydashboard", repos = "http://cran.us.r-project.org")
#if(!require(wbstats)) install.packages("wbstats", repos = "http://cran.us.r-project.org")


library(shinydashboard)
library(tidyverse)
library(wbstats) # Search and download data from the World Bank Data API.
library(shinyWidgets)
require(purrr)
library(plotly) 
library(tmap) 
library(config)
library(shinyBS) 
library(eurostat)  
library(jsTreeR)  
library(pool) 
pdf(NULL) # This needs otherwise gives "plot.pdf" error on kooplex
#library(jsonlite)
#install_github("stla/jsTreeR")

load("data/country_map.RData")
load("data/nuts1_map.RData")
load("data/nuts2_map.RData")
load("data/nuts3_map.RData")






config <- config::get()
con <- dbPool(
  drv = RPostgreSQL::PostgreSQL(),
  dbname = config$dbname,
  host = config$host,
  port = config$port,
  user = config$user,
  password = config$password
)


onStop(function() {
  poolClose(con)
})




master_table <- tbl(con, "master_table") %>%
  collect()
master_schema <- tbl(con, "master_schema") %>%
  collect()


#########
# Tree choose part
#

makeNodes <- function(leaves){
  dfs <- lapply(strsplit(leaves, "/"), function(s){
    item <-
      Reduce(function(a,b) paste0(a,"/",b), s[-1], s[1], accumulate = TRUE)
    data.frame(
      item = item,
      parent = c("root", item[-length(item)]),
      stringsAsFactors = FALSE
    )
  })
  dat <- dfs[[1]]
  for(i in 2:length(dfs)){
    dat <- base::merge(dat, dfs[[i]], all=TRUE)
  }
  f <- function(parent){
    i <- match(parent, dat$item)
    item <- dat$item[i]
    children <- dat$item[dat$parent==item]
    label <- tail(strsplit(item, "/")[[1]], 1)
    if(length(children)){
      list(
        text = label,
        data = list(value = item),
        children = lapply(children, f)
      )
    }else{
      list(text = label, data = list(value = item))
    }
  }
  lapply(dat$item[dat$parent == "root"], f)
}

inds <- vector()
for (i in 1:nrow(master_table)){
  inds[i] <- paste(master_table[i,"tbl_source"], master_table[i,"tbl_displ_name"], sep = '/')
}

nodes <- makeNodes(inds)


############################################################################################
# User interface of the app
############################################################################################

ui <- dashboardPage(
  dashboardHeader(title = "VEO Data Browser"),
  dashboardSidebar(
    sidebarMenu(id="sidebar",
                menuItem("Tables and schemas", tabName = "menu_tables", icon = icon("table")),
                menuItem("Bulk download", tabName = "menu_tree", icon = icon("angle-double-down")),
                menuItem("Visualization", icon = icon("chart-bar"),
                  menuSubItem("Collect data", tabName = "menu_merge", icon = icon("random")),
                  menuSubItem("Draw graph", tabName = "menu_viz", icon = icon("chart-bar"))
                ),
                menuItem("Info", tabName = "menu_info", icon = icon("info"))
                
    )
  ),
  
  dashboardBody(
    tabItems(
      
      tabItem(tabName = "menu_tables",
              fluidRow(
                
                box( title = span("Available tables to choose", bsButton("q2", label = "", icon = icon("info-circle"), style = "primary", size = "medium")),
                     status = "primary",
                     height = "450", width = "12",solidHeader = T,
                     column(width = 12,
                            DT::dataTableOutput("tbl_master"),
                            style = "height:380px; overflow-y: scroll;overflow-x: scroll;"
                     )
                ),
                
                bsPopover(id = "q2", title = "Available tables",
                          content = paste0("List of tables in the SQL server. <br> Choose a table and it\\'s structure appears the box below."),
                          trigger = "focus", 
                          options = list(container = "body")
                ),
                
                
                
                
                
                box( title = span("Structure of the choosen table", bsButton("q3", label = "", icon = icon("info-circle"), style = "primary", size = "medium")),
                     status = "primary",
                     width = "12",solidHeader = T,
                     column(width = 12,
                            DT::dataTableOutput("tbl_schema"),
                            style = "overflow-y: scroll;overflow-x: scroll;"
                     )
                ),
                
                bsPopover(id = "q3", title = "Structure of the choosen table",
                          content = paste0("This box shows information about the columns in the earlier chosen table"),
                          trigger = "focus", 
                          options = list(container = "body")
                ),
                
                
                
                
                
              )
      ),
      
      
      
      tabItem(tabName = "menu_merge",
              fluidRow(
                
                box( title = span("Choose from tables", bsButton("q5", label = "", icon = icon("info-circle"), style = "primary", size = "medium")), 
                     status = "primary",
                     width = "12",solidHeader = T,
                     radioButtons("resolution", label = "Geographycal resolution",
                                        choices = unique(master_table$tbl_resolution),
                                        inline = TRUE, selected ="country"),
                     radioButtons("periodicity", label = "Time resolution",
                                        choices = unique(master_table$tbl_periodicity)[unique(master_table$tbl_periodicity)!="permanent"],
                                        inline = TRUE, selected = "yearly"),
                     column(width = 12,
                            DT::dataTableOutput("tbl_master_multi"),
                            style = "height:450px; overflow-y: scroll;overflow-x: scroll;"
                     )
                ),
                
                bsPopover(id = "q5", title = "Choose from tables",
                          content = paste0("Select those tables that need to be merged in the next step. ",
                                           "Filter the available tables based on the <b>geographycal</b> and <b>time resolutions</b> and the tables those fulfill these filters automatically appear below. ",
                                           "Select more than one table. Also the search box can be used to find more easily a given table. Then push <b>Collect table</b> button and the merged table appears in \"Collected table\" box. "
                                           
                          ),
                          placement = "right", 
                          trigger = "focus", 
                          options = list(container = "body")
                ),
                
                
                box( actionButton("collectbutton", "Collect selected tables", icon("refresh"), class = "btn btn-warning"),
                     textInput("sql_query_io", label = h3("SQL query"), value = "Insert SQL command"),
                     actionButton("run_sql", "Refresh SQL query", icon("refresh"), class = "btn btn-warning" )
                ),
                
                
                box( title = span( "Code for direct download in R", bsButton("q1", label = "", icon = icon("info-circle"), style = "primary", size = "medium")), 
                     status = "primary",
                     width = "6",solidHeader = T,
                     collapsible = TRUE, collapsed = TRUE,
                     div(style = 'overflow-x: scroll', 
                         verbatimTextOutput("downloader_R_code"),
                         
                         
                     )
                ),
                
                
                
                
                bsPopover(id = "q1", title = "Communmication between R and postgreSQL server",
                          content = paste0("Clicking on the + mark appears a code in the box that downloads the table above in R. It is also possible to reach directly any tables in SQL with R codes, for example if you want to get the table with \"WHS4_100\" id : <br> <code>library(tidyverse) <br> data \\<- tbl(con, \"WHS4_100\") %>% <br> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; collect() </code><br>", 
                                           a("more info", 
                                             href = "https://db.rstudio.com/dplyr/",
                                             target="_blank")
                          ),
                          placement = "right", 
                          trigger = "focus", 
                          options = list(container = "body")
                ),
                
                box( title = span("Code for direct download in Python" , bsButton("q9", label = "", icon = icon("info-circle"), style = "primary", size = "medium")), 
                     status = "primary",
                     width = "6",solidHeader = T,
                     collapsible = TRUE, collapsed = TRUE,
                     div(style = 'overflow-x: scroll', 
                         verbatimTextOutput("downloader_python_code")
                     )
                ),
                
                bsPopover(id = "q9", title = "Communmication between python and postgreSQL server",
                          content = paste0("Clicking on the + mark appears a code in the box that downloads the table above in python." 
                          ),
                          placement = "right", 
                          trigger = "focus", 
                          options = list(container = "body")
                ),
                
                
                
                
                box( title = span("Collected table" , bsButton("q8", label = "", icon = icon("info-circle"), style = "primary", size = "medium")), 
                     status = "primary",
                     height = "535", width = "12",solidHeader = T,
                     column(width = 12,
                            DT::dataTableOutput("tbl_collected"),
                            style = "height:430px; overflow-y: scroll;overflow-x: scroll;"
                     ),
                     downloadButton('table_download', "Download cleaned table"),
                     
                ),
                
      
                
                
                bsPopover(id = "q8", title = "Collected table",
                          content = paste0("This table contains the merged tables. It is empty at the beginning, first need to select the tables above and push the <b>Collect table</b> button",
                                           "The content of this table will be visualized in the <b>Visualization</b> menu.",
                                           "Before visualization it is possible to filer the table by any columns.",
                                           "<ul>Depending on the type of a column, the filter control can be different. Initially, you see search boxes for all columns. When you click the search boxes, you may see different controls:",

"<li>For numeric/date/time columns, range sliders are used to filter rows within ranges;</li>",
"<li>For factor columns, selectize inputs are used to display all possible categories, and you can select multiple categories there (note you can also type in the box to search in all categories);</li>",
"<li>For character columns, ordinary search boxes are used to match the values you typed in the boxes;</li>",
"When you leave the initial search boxes, the controls will be hidden and the filtering values (if there are any) are stored in the boxes:",

"<li>For numeric/date/time columns, the values displayed in the boxes are of the form low ... high;</li>",
"<li>For factor columns, the values are serialized as a JSON array of the form [\"value1\", \"value2\", \"value3\"];</li>",
"When a column is filtered, there will be a clear button  in its search box, and you can click the button to clear the filter. If you do not want to use the controls, you can actually type in the search boxes directly, e.g. you may type 2 ... 5 to filter a numeric column, and the range of its slider will automatically adjusted to [2, 5]. </ul>",

                                           "To reach the full table unfold the <b>Code for direct download in R</b> or <b>Code for direct download in python</b> boxes"
                                           
                                           
                          ),
                          placement = "right", 
                          trigger = "focus", 
                          options = list(container = "body")
                ),
                
                
 






                
                box( title = "Result of SQL query", status = "primary",
                     height = "535", width = "12",solidHeader = T,
                     column(width = 12,
                            DT::dataTableOutput("tbl_sql_query"),
                            style = "height:450px; overflow-y: scroll;overflow-x: scroll;"
                     )
                ),
              )
      ),
      
      
      tabItem(tabName = "menu_viz",
              uiOutput ("choose_y_axis"),
              
              actionButton("drawgraph", "Drawgraph", icon("refresh"), class = "btn btn-warning"),
              tabsetPanel(type = "tabs",
                          tabPanel("Plot",
                                   {fluidRow(textOutput("selected_y_name"),
                                             plotlyOutput("time_plot_out"))},
                                   uiOutput("plot_add_country"),
                          ),
                          tabPanel("Map",
                                   {fluidRow(textOutput("selected_y_name1"),
                                             tmapOutput("map_out"))},
                                   uiOutput("map_sliders")
                          )
                          
                          
                          
              )
      ),
      
      tabItem(tabName = "menu_tree",
              fluidRow(

                box( title = span("Choose from available tables", bsButton("q6", label = "", icon = icon("info-circle"), style = "primary", size = "medium")),
                status = "primary",
                     width = "8",solidHeader = T,
                     column(width = 12,
                            jstreeOutput("jstree"),
                            style = "height:380px; overflow-y: scroll;overflow-x: scroll;"
                     ),
                     
                   ),
                
                bsPopover(id = "q6", title = "Choose from available tables",
                          content = paste0("The tree structure contains all the tables in the database. The \"+\" sign unfold the given branch.",
                                           "Select the tables of interest and on the right panel filter the tables based on <b>Geographical</b> and <b>Time resoltion</b>.",
                                           "If at least two tables are after filtering then an SQL command appears in the text box.",
                                           "By pushing the <b>View head of table</b> buttons shows the first 5 rows of the merged tables"
                                           
                          ),
                          placement = "right", 
                          trigger = "focus", 
                          options = list(container = "body")
                ),
                
                
                
                
                radioButtons("resolution_tree", label = "Geographycal resolution",
                                   choices = unique(master_table$tbl_resolution),
                                   inline = TRUE, selected ="country"),
                radioButtons("periodicity_tree", label = "Time resolution",
                                   choices = unique(master_table$tbl_periodicity)[unique(master_table$tbl_periodicity)!="permanent"],
                                   inline = TRUE, selected = "yearly"),
                
                
                box(width = "4", 
                    tags$h4("SQL command to the selected tables:"),
                    verbatimTextOutput("sql_out_tree"),
                    fluidRow(
                    column(6, actionButton("collectbutton_tree", "View head of table", icon("refresh"), class = "btn btn-warning")),
                    column(6, actionButton("generate_id_button_tree", "Generate SQL ID", icon("refresh"), class = "btn btn-warning")),
                    ),
                    verbatimTextOutput("sql_id_tree")
                ),
                
                box( title = span( "Code for direct download in R", bsButton("q4", label = "", icon = icon("info-circle"), style = "primary", size = "medium")), 
                     status = "primary",
                     width = "6",solidHeader = T,
                     collapsible = TRUE, collapsed = TRUE,
                     div(style = 'overflow-x: scroll', 
                         verbatimTextOutput("downloader_R_code_tree"),
                         
                         
                     )
                ),
                
                
                bsPopover(id = "q4", title = "Communmication between R and postgreSQL server",
                          content = paste0("Clicking on the + mark appears a code in the box that downloads the table above in R. It is also possible to reach directly any tables in SQL with R codes, for example if you want to get the table with \"WHS4_100\" id : <br> <code>library(tidyverse) <br> data \\<- tbl(con, \"WHS4_100\") %>% <br> &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; &nbsp; collect() </code><br>", 
                                           a("more info", 
                                             href = "https://db.rstudio.com/dplyr/",
                                             target="_blank")
                          ),
                          placement = "right", 
                          trigger = "focus", 
                          options = list(container = "body")
                ),
                
                
                box( title = span("Code for direct download in Python" , bsButton("q10", label = "", icon = icon("info-circle"), style = "primary", size = "medium")), 
                     status = "primary",
                     width = "6",solidHeader = T,
                     collapsible = TRUE, collapsed = TRUE,
                     div(style = 'overflow-x: scroll', 
                         verbatimTextOutput("downloader_python_code_tree")
                     )
                ),
                
                bsPopover(id = "q10", title = "Communmication between python and postgreSQL server",
                          content = paste0("Clicking on the + mark appears a code in the box that downloads the table above in python." 
                          ),
                          placement = "right", 
                          trigger = "focus", 
                          options = list(container = "body")
                ),
                
                
                
                
                
                
                box( title = span("Head of collected table", bsButton("q7", label = "", icon = icon("info-circle"), style = "primary", size = "medium")),
                     status = "primary",
                     height = "310", width = "12",solidHeader = T,
                     column(width = 12,
                            DT::dataTableOutput("tbl_collected_tree"),
                            style = "height:250px; overflow-x: scroll;"
                     ),

                     
                ),
                
                
                bsPopover(id = "q7", title = "Head of collected table",
                          content = paste0("This table contains the first 5 rows of the merged tables. This is empty until you select as many table to give valid SQL command and the push the <b>View head of table</b> button",
                                           "To reach the full table unfold the <b>Code for direct download in R</b> or <b>Code for direct download in python</b> boxes"
                                           
                                           
                          ),
                          placement = "right", 
                          trigger = "focus", 
                          options = list(container = "body")
                ),
               
                #box( title = "Selected table ID's", status = "primary",
                #     height = "350", width = "12",solidHeader = T,
                #     column(width = 12,
                #            verbatimTextOutput("treeSelected_R"),
                #            style = "height:250px; overflow-y: scroll;overflow-x: scroll;"
                #     )
                #),
                
               # box( title = "Result of SQL query", status = "primary",
              #       height = "535", width = "12",solidHeader = T,
               #      column(width = 12,
              #              DT::dataTableOutput("aaa"),
               #             style = "height:450px; overflow-y: scroll;overflow-x: scroll;"
              #       )
               # ),
              )
      ),
      


tabItem(tabName = "menu_info",
        includeHTML("www/info.html")
)
      
      
      
      
      
    )
  )
)


############################################################################################
# SERVER SIDE
############################################################################################




server <- function(input, output, session) {
  
  ###############################
  # Content for `menu_tables` page
  ###############################
  
  # Table of datatables
  output$tbl_master = DT::renderDataTable(
    DT::datatable(
      {master_table %>%
          select(tbl_name, tbl_displ_name, tbl_source, tbl_url, tbl_note, tbl_resolution, tbl_periodicity) %>%
          mutate(tbl_url=str_c('<a href="', tbl_url, '" target="_blank">link</a>'))},
      escape = 4,
      colnames = c('ID', 'Table name', 'Source', 'Link', 'Description', 'Area', 'Time'),
      selection = 'single',
      extensions = 'Buttons',
      options = list(lengthChange = FALSE, 
                     paging = FALSE
      )
    ),
  )
  
  # Information for each column in each datatables
  output$tbl_schema = DT::renderDataTable(
    DT::datatable(
      { s <- input$tbl_master_rows_selected
      if (!length(s)) s= 1                  
      ss <- as.character(master_table[s, 'tbl_name'])
      tbl(con, "master_schema") %>%
        filter(tbl_name==ss) %>%
        select(id, disp_name, is_key, unit, comment, data_type) %>%
        collect()
      },
      colnames = c('Column ID', 'Name', 'Key', 'Unit', 'Column description', 'Data type'),
      selection = 'single',
      extensions = 'Buttons',
      options = list(lengthChange = FALSE, 
                     paging = FALSE)
    ),
  )
  
  
  ##############################
  #Content for `menu_merge` page
  ##############################
  
  # This variable below will contain a selected version of master_table
  filtered_master_table <- reactive({
    master_table %>%
      filter(tbl_periodicity %in% c(input$periodicity, "permanent")) %>%
      filter(tbl_resolution %in% input$resolution)
  })
  
  # You can pick datatables from this tables to be merged later
  output$tbl_master_multi = DT::renderDataTable(
    DT::datatable({
      filtered_master_table() %>%
        select(tbl_name, tbl_displ_name, tbl_source, tbl_url, tbl_note, tbl_resolution, tbl_periodicity) %>%
        mutate(tbl_url=str_c('<a href="', tbl_url, '" target="_blank">link</a>'))},
      escape = 4,
      colnames = c('ID', 'Table name', 'Source', 'Link', 'Description', 'Area', 'Time'),
      selection = 'multiple',
      extensions = 'Buttons',
      options = list(lengthChange = FALSE, 
                     paging = FALSE)
    ),
  )
  
  # Button start to the datata table procedure by selecting the interesting datatables
  selected_tables <- eventReactive(input$collectbutton, {
    input$tbl_master_multi_rows_selected
  })
  
  # This 'table_plot' variable contains the merged datatable that will be later displayed and plotted
  table_plot <- eventReactive(input$collectbutton, {
    s <-  selected_tables()
    if (!length(s)) s= 1                  
    ss <- filtered_master_table()[s, 'tbl_name']
    if (length(s)==1) {
      tbl(con,as.character(ss[1,1])) %>%
        collect()
    }
    else {
      chosen_sources <- list(tbl(con,as.character(ss[1,1])))
      for (i in 2:nrow(ss)) {
        chosen_sources[[i]] <- tbl(con,as.character(ss[i,1]))
      }
      
      x <- chosen_sources %>%
        reduce(inner_join) %>%
        collect()
      x <- as.data.frame(x)
      y <- master_schema %>%
        filter(id %in% names(x))
      x
    }
  })
  
  
  
  
  
  
  # This table contains the merged datatable output
  output$tbl_collected = DT::renderDataTable(
    server = TRUE,
    DT::datatable(
      { x <-  table_plot()
      if (!length(x)) x= 1 
      y <- master_schema %>%
        filter(id %in% names(x))
      for (i in rownames(y[y$data_type=="fct",]) ){
        x[,as.character(y[i, 'id'])] <- as.factor(x[,as.character(y[i, 'id'])])
      }
      x
      },
      selection = 'single',
      extensions = 'Buttons',
      filter = 'top',
      
      options = list(lengthChange = FALSE, 
                     paging = TRUE)
    ),
  )
  
  # Print out the SQL code for datatable mergeing
  sql_code_out <- reactive ({
    s <-  selected_tables()
    if (!length(s)) s= 1                  
    ss <- filtered_master_table()[s, 'tbl_name']
    if (length(s)==1) {
      tbl(con,as.character(ss[1,1])) %>%
        collect()
    }
    else {
      chosen_sources <- list(tbl(con,as.character(ss[1,1])))
      for (i in 2:nrow(ss)) {
        chosen_sources[[i]] <- tbl(con,as.character(ss[i,1]))
      }
      
      x <- chosen_sources%>%
        reduce(inner_join)
    }
    x <- capture.output(show_query(x))
    x <- x[-1]
    
    
  })
  
  
  sql_code_out_r <- reactive({
    y <- sql_code_out()[1]
    for (i in 2:length(sql_code_out())){
      y <- paste (y, sql_code_out()[i], sep=" ")
      }
    y
  })


  
  observe({
    updateTextInput(session, "sql_query_io",
                    value = sql_code_out_r())
  })

  
  output$sql_out = renderText({ 
    sql_code_out()
  })
  
  # Button initiate to download the merged datatable in csv form
  output$table_download <- downloadHandler(
    filename = function() {
      paste("data-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      s <-  table_plot()
      if (!length(s)) s= 1                  
      write_csv(s[input[["tbl_collected_rows_all"]],], file) # This downloads only that part of the table that is selected by DT inbuilt filtering
    }
  )
  
  # Print out the code for direct R download
  output$downloader_R_code <- renderText({
    y <- sql_code_out()[1]
    for (i in 2:length(sql_code_out())){
      y <- paste (y, sql_code_out()[i], sep=" ")
    }
    x <- paste0('# The code below download the merged tables in Rstudio using SQL commands
library(tidyverse)\nlibrary(RPostgreSQL)
con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), 
                      dbname=xxx,
                      host = xxx,
                      port = xxx,
                      user = xxx,
                      password = xxx
)
data <- tbl(con, sql(\'',y, '\'))%>%
        collect()
dbDisconnect(con)
# \'data\' variable will contain the collected table')    
  x
  })

# Print out the code for direct python download 
output$downloader_python_code <- renderText({
  y <- sql_code_out()[1]
  for (i in 2:length(sql_code_out())){
    y <- paste (y, sql_code_out()[i], sep=" ")
  }
  x <- paste0('# The code below download the merged tables in python using SQL commands
import psycopg2
import pandas as pd
conn = XXX 
cur = conn.cursor()
query = \'',y, '\'
cur.execute(query)
data = pd.DataFrame(cur.fetchall())
data.columns = [desc[0] for desc in cur.description]
cur.close()
conn.close()
# \'data\' variable will contain the collected table')
  x
})

# Manual SQL code input action button
selected_sql_command <- eventReactive(input$run_sql, {
  input$sql_query_io
})

# Manual SQL code result table
output$tbl_sql_query = DT::renderDataTable(
  DT::datatable(
    { sq <- selected_sql_command()
    if (!length(sq)) sq= 'SELECT' 
    tbl(con, sql(sq)) %>%
      collect()
    },
    selection = 'single',
    extensions = 'Buttons',
    options = list(lengthChange = FALSE, 
                   paging = FALSE)
  )
)


##############################
#Content for `time_plot` page
##############################

# Chose variable for y axis
output$choose_y_axis <- renderUI({
  d <- table_plot()
  d_chose <-tbl(con, "master_schema") %>%
    #collect()
    filter(id%in%local(names(d))) %>%
    filter(is_key=="N") %>%
    filter(data_type=="dbl") %>%
    select(id)%>%
    collect()
  d_chose <- as.character(d_chose$id)
  pickerInput ("selected_y", "Select Variable",
               choices = d_chose,
               multiple = FALSE,
               selected = d_chose[1])
}) 





# Create plot button
table_plot1 <- eventReactive(input$drawgraph, {
  d <- table_plot()
  if (!length(d)) d= 1 
  d <- d[input[["tbl_collected_rows_all"]],]
  d <- as.data.frame(d)
  
  #d$date <- as.numeric(d$date)
})


output$selected_y_name <- renderText({
  x <-tbl(con, "master_schema") %>%
    #collect()
    #filter(id%in%local(names(d))) %>%
    filter(is_key=="N") %>%
    filter(data_type=="dbl") %>%
    collect()
  y <- input$selected_y
  if (!length(y)) y= 1 # This looks complicated, but otherwise gives error
  as.character(x[x$id==y, 'comment'])
})


# Draw plot output
output$time_plot_out <- renderPlotly({
  draw <- table_plot1() 
  if ("date_year" %in% names(draw) & "date_month" %in% names(draw) & "date_day" %in% names(draw)) {
    draw <- mutate(draw, date_ymd=as.Date(paste(date_year, date_month, date_day, sep = "-")))
  } else {
    if ("date_year" %in% names(draw) & "date_month" %in% names(draw)) {
      draw <- mutate(draw, date_ymd=as.Date(paste(date_year, date_month, "01", sep = "-")))
    } else{
      if ("date_year" %in% names(draw)) {
        draw <- mutate(draw, date_ymd=as.Date(paste(date_year,"01", "01", sep = "-")))
      }
    }
  }
  
  
  if ( "iso_a3" %in% names(draw) & !("nuts1_id" %in% names(draw))& !("nuts2_id" %in% names(draw)) & !("nuts2_id" %in% names(draw))    ) {
    draw <- rename(draw, location = iso_a3)
  } else {
    if ("nuts1_id" %in%  names(draw)  ) {
      draw <- rename(draw, location = nuts1_id)
    }
    if ("nuts2_id" %in%  names(draw)  ) {
      draw <- rename(draw, location = nuts2_id)
    }
    if ("nuts3_id" %in%  names(draw)  ) {
      draw <- rename(draw, location = nuts3_id)
    }
    if ("lau" %in% names(draw)) {
      draw <- rename(draw, location = lau_id)
    }
    
  }
  
  k <- input$selected_y
  draw <- draw %>%
    dplyr::select(location, date_ymd, k) %>%
    arrange(date_ymd)
  sell <- input$plot_add_country_checkbox
  if (!length(sell)) sell = unique(draw$location)[1:15]  
  draw_sel <- draw %>%
    filter( location %in% sell)
  p <- plot_ly(data = draw, x = ~date_ymd, y = ~get(names(draw)[3]), 
               split= ~ location, type = 'scatter', mode = 'lines', 
               line = list(color = 'lightgray', size = 2), showlegend = FALSE) %>%
    layout(xaxis = list(title="Time"), yaxis = list(title = input$selected_y)) %>%
    add_lines(data = draw_sel, x = ~date_ymd,  y = ~get(names(draw_sel)[3]),
              line = list(color = ~location, size = 24), showlegend = TRUE)
  #   p <- p %>% layout(title= 'comment', xaxis = list(title="Time"), yaxis = list(title = input$selected_y))
  
  p
})

output$plot_add_country <- renderUI({
  draw <- table_plot1()  
  
  if ( "iso_a3" %in% names(draw) & !("nuts1_id" %in% names(draw))& !("nuts2_id" %in% names(draw)) & !("nuts2_id" %in% names(draw))    ) {
    draw <- rename(draw, location = iso_a3)
  }
  if ("nuts1_id" %in%  names(draw)  ) {
    draw <- rename(draw, location = nuts1_id)
  }
  if ("nuts2_id" %in%  names(draw)  ) {
    draw <- rename(draw, location = nuts2_id)
  }
  if ("nuts3_id" %in%  names(draw)  ) {
    draw <- rename(draw, location = nuts3_id)
  }
  if ("lau" %in% names(draw)) {
    draw <- rename(draw, location = lau_id)
  }
  
  box( title = "Add country/region to the plot ->", status = "primary",
       width = "6",solidHeader = T, collapsible = TRUE, collapsed = TRUE,
       column(width = 12,
              checkboxGroupInput (inputId = "plot_add_country_checkbox", label = "",
                                  choices = unique(draw$location), selected = unique(draw$location)[1:15], inline = TRUE),
       )
  )
  
  
  
  
})



##############################
#Content for `map_plot` page
##############################    

# Slider input to choose the date for the map

output$map_sliders <- renderUI({
  draw <- table_plot1() 
  if ("date_year" %in% names(draw) & "date_month" %in% names(draw) & "date_day" %in% names(draw)) {
    draw <- mutate(draw, date_ymd=as.Date(paste(date_year, date_month, date_day, sep = "-")))
    draw <- draw %>%
      dplyr::select(date_ymd, input$selected_y) %>%
      arrange(date_ymd)
    draw <- as.data.frame(draw)
    sliderTextInput(inputId = "choose_date_map", label = "Choose date", 
                    choices = unique(draw$date_ymd),
                    selected = unique(draw$date_ymd)[1])
    
  } else {
    if ("date_year" %in% names(draw) & "date_month" %in% names(draw)) {
      draw <- mutate(draw, date_ymd=paste(date_year, date_month, sep = "-"))
      draw <- draw %>%
        dplyr::select(date_ymd, input$selected_y) %>%
        arrange(date_ymd)
      draw <- as.data.frame(draw)
      sliderTextInput(inputId = "choose_date_map", label = "Choose date", 
                      choices = unique(draw$date_ymd),
                      value = unique(draw$date_ymd)[1])
      
      
      
      
    } else{
      if ("date_year" %in% names(draw)) {
        draw <- mutate(draw, date_ymd=date_year)
        draw <- draw %>%
          dplyr::select(date_ymd, input$selected_y) %>%
          arrange(date_ymd)
        draw <- as.data.frame(draw)
        draw$date_ymd <- as.numeric(draw$date_ymd)
        sliderInput(inputId = "choose_date_map", label = "Choose date", 
                    min = min(draw$date_ymd) , max = max(draw$date_ymd),
                    value = max(draw$date_ymd), animate = TRUE,
                    sep = "")
      }
    }
  }
  
  
})


# Draw map output



observe({
  output$map_out <- renderTmap({
    draw <- table_plot1() 
    if ("date_year" %in% names(draw) & "date_month" %in% names(draw) & "date_day" %in% names(draw)) {
      draw <- mutate(draw, date_ymd=as.Date(paste(date_year, date_month, date_day, sep = "-")))
    } else {
      if ("date_year" %in% names(draw) & "date_month" %in% names(draw)) {
        draw <- mutate(draw, date_ymd=paste(date_year, date_month, sep = "-"))
      } else{
        if ("date_year" %in% names(draw)) {
          draw <- mutate(draw, date_ymd=date_year)
        }
      }
    }
    
    
    
    
    k <- input$selected_y
    
    s <-  input$choose_date_map
    if (!length(s)) s = draw$date_ymd[1]   
    
    
    if ( "iso_a3" %in% names(draw) & !("nuts1_id" %in% names(draw))& !("nuts2_id" %in% names(draw)) & !("nuts2_id" %in% names(draw))    ) {
      draw <- draw %>%
        dplyr::select(iso_a3, date_ymd, k) %>%
        filter(date_ymd== s) %>%
        arrange(date_ymd)
      draw <- as.data.frame(draw)
      map_data <- left_join(country_map@data, draw) %>%
        rename(location = iso_a3 )
      
      country_map@data$map_variable <- map_data[,k]
      map <- country_map
    } else {
      if ("nuts1_id" %in% names(draw)) {
        draw <- draw %>%
          filter(date_ymd== s) %>%
          dplyr::select(nuts1_id, date_ymd, k) %>%
          arrange(date_ymd)
        draw <- as.data.frame(draw)
        map_data <- left_join(nuts1_map@data, draw)  %>%
          rename(location = nuts1_id )
        
        nuts1_map@data$map_variable <- map_data[,k]
        map <- nuts1_map
      } else {
        if ("nuts2_id" %in% names(draw)) {
          draw <- draw %>%
            filter(date_ymd== s) %>%
            dplyr::select(nuts2_id, date_ymd, k) %>%
            arrange(date_ymd)
          draw <- as.data.frame(draw)
          map_data <- left_join(nuts2_map@data, draw)  %>%
            rename(location = nuts2_id )
          
          nuts2_map@data$map_variable <- map_data[,k]
          map <- nuts2_map
        }else{
          if ("nuts3_id" %in% names(draw)) {
            draw <- draw %>%
              filter(date_ymd== s) %>%
              dplyr::select(nuts3_id, date_ymd, k) %>%
              arrange(date_ymd)
            draw <- as.data.frame(draw)
            map_data <- left_join(nuts3_map@data, draw)  %>%
              rename(location = nuts3_id )
            
            nuts3_map@data$map_variable <- map_data[,k]
            map <- nuts3_map}
          
          
          
          
        }
        
        
      } 
    }
    
    tm_shape(map) + 
      tmap::tm_polygons(col = "map_variable", palette = "seq", style ="cont",
                        #popup.vars=c(
                        # "Country: "="location",
                        #  "Value: " = "map_variable") )+ 
      )+
      tm_view(set.view=1, colorNA = NULL) 
    
    
    
  })
  
  
})


### TREE   
output$jstree <-
  renderJstree(
    jstree(nodes, search = TRUE, theme= "proton", multiple = TRUE, checkboxes = TRUE)
  )


filtered_master_table_tree <- reactive({
  if (!length(input$jstree_selected)) {
    sel_inds <- "Nothing selected"
  }  else {
    sel_inds <- vector()
    for (i in 1:length(input$jstree_selected))
      sel_inds[i] <- input$jstree_selected[[i]]$text
    sel_inds <- sel_inds[!(sel_inds%in%master_table$tbl_source)]
    sel_inds <- master_table[master_table$tbl_displ_name%in%sel_inds, 'tbl_name']
  }
  sel_inds
})


filtered_master_table_tree_sel <- reactive({
  if (!length(input$jstree_selected)) {
    x <- "Nothing selected"
  } else {
  x <- master_table %>%
    filter(tbl_name %in% filtered_master_table_tree()$tbl_name) %>%
    filter(tbl_periodicity %in% c(input$periodicity_tree, "permanent")) %>%
    filter(tbl_resolution %in% input$resolution_tree)
  }
  x
})







# Print out the SQL code for datatable mergeing
sql_code_out_tree <- reactive ({
  ss <- filtered_master_table_tree_sel()[, 'tbl_name']
  if (!length(input$jstree_selected)) {
    x <- "Nothing selected"
  }  else {
    chosen_sources <- list(tbl(con,as.character(ss[1,1])))
    for (i in 2:nrow(ss)) {
      chosen_sources[[i]] <- tbl(con,as.character(ss[i,1]))
    }
    
    x <- chosen_sources%>%
      reduce(inner_join)
    
    x <- capture.output(show_query(x))
    x <- x[-1]
  }
  x
  
})



output$sql_out_tree = renderText({ 
  sql_code_out_tree()
})








table_head_tree <- eventReactive(input$collectbutton_tree, {
  ss <- filtered_master_table_tree_sel()[, 'tbl_name']
  if (!length(input$jstree_selected)) {
    x <- "Nothing selected"
  }  else {
    chosen_sources <- list(tbl(con,as.character(ss[1,1])))
    for (i in 2:nrow(ss)) {
      chosen_sources[[i]] <- tbl(con,as.character(ss[i,1]))
    }
    x <- chosen_sources %>%
      reduce(inner_join) %>%
      head(5)%>%
      collect()
    x <- as.data.frame(x)
    x
  }
})




sql_id_out <- eventReactive(input$generate_id_button_tree, {
  master_sql <- tbl(con, "master_sql")  %>%
    collect()
  y <- sql_code_out_tree()[1]
  for (i in 2:length(sql_code_out_tree())){
    y <- paste (y, sql_code_out_tree()[i], sep=" ")
  }
  if (!length(master_sql)) {
    x <- tibble(
      sql_num = 1,
      sql_id = "id1",
      sql_query = y,
      sql_date = Sys.time(),
    )
    dbWriteTable(con, "master_sql",x , append = TRUE, row.names = FALSE)
  } else {
    x <- tibble(
      sql_num = max(master_sql$sql_num)+1,
      sql_id = paste("id", (max(master_sql$sql_num)+1), sep = ""),
      sql_query = y,
      sql_date = Sys.time(),
    )
    dbWriteTable(con, "master_sql",x , append = TRUE, row.names = FALSE)
  }
  master_sql <- tbl(con, "master_sql")  %>%
    collect()
  paste('curl "http://kooplex-veo.elte.hu:8701/veo_sql?id=id', max(master_sql$sql_num), '"  >data.json', sep = "")
})



output$sql_id_tree = renderText({ 
  sql_id_out ()
})

output$tbl_collected_tree = DT::renderDataTable(
  DT::datatable(
    { table_head_tree()
    },
    #selection = 'single',
    #extensions = 'Buttons',
    options = list(lengthChange = FALSE, 
                   paging = FALSE,
                   dom = 't')
  )
)



output$treeSelected_R <- renderPrint({
  #input$jstree_selected[[1]]$text
  if (!length(input$jstree_selected)) {
    sel_inds <- "Nothing selected"
  }  else {
    sel_inds <- vector()
    for (i in 1:length(input$jstree_selected))
      sel_inds[i] <- input$jstree_selected[[i]]$text
    sel_inds <- sel_inds[!(sel_inds%in%master_table$tbl_source)]
    sel_inds <- master_table[master_table$tbl_displ_name%in%sel_inds, 'tbl_name']
  }
  sel_inds
})

output$downloader_R_code_tree <- renderText({
  y <- sql_code_out_tree()[1]
  for (i in 2:length(sql_code_out_tree())){
    y <- paste (y, sql_code_out_tree()[i], sep=" ")
  }
  x <- paste0('# The code below download the merged tables in Rstudio using SQL commands
library(tidyverse)\nlibrary(RPostgreSQL)
con <- DBI::dbConnect(RPostgreSQL::PostgreSQL(), 
                      dbname=xx,
                      host = xx,
                      port = xx,
                      user = xx,
                      password = xx
)
data <- tbl(con, sql(\'',y, '\'))%>%
        collect()
dbDisconnect(con)
# \'data\' variable will contain the collected table')    
x
})


# Print out the code for direct python download 
output$downloader_python_code_tree <- renderText({
  y <- sql_code_out_tree()[1]
  for (i in 2:length(sql_code_out_tree())){
    y <- paste (y, sql_code_out_tree()[i], sep=" ")
  }
  x <- paste0('# The code below download the merged tables in python using SQL commands
import psycopg2
import pandas as pd
conn = xx
cur = conn.cursor()
query = \'',y, '\'
cur.execute(query)
data = pd.DataFrame(cur.fetchall())
data.columns = [desc[0] for desc in cur.description]
cur.close()
conn.close()
# \'data\' variable will contain the collected table')
  x
})


}

shinyApp(ui, server)


