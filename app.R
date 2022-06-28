#shiny app for parking lot simulation
library(tidyverse)
library(magrittr)
library(shiny)
library(DT)
library(shinydashboard)
library(shinybusy)
library(simmer)


#clear workspace and run Garbage Collector
rm(list=ls())
gc(T,T,T)


ui <- dashboardPage(title = "Parking Lot Simulation",
  
  dashboardHeader(
  title = tags$a(href="http://www.datadrivensupplychain.com",target="_blank",tags$img(src='ddsc_logo.png',height="85%"))),

  dashboardSidebar(
  width = 300,
      sidebarMenu(
	  

	  
		HTML("<br><font size=3>Parking Lot Simulation</br></font>"),
		menuItem("Simulation Parameters", tabName = "simulation_parameters"),
		menuItem("Output: Utilization Boxplot", tabName = "output_utilization_boxplot"),
		menuItem("Output: Parking Lot Usage Time Series", tabName = "output_total_parkinglot_count"),
		menuItem("Link to App Code on Github",tabName= "code")
  
  ) ),
  
  dashboardBody(
  #busy indicator
  shinybusy::add_busy_bar(timeout = 1000, color = "#F00", centered = FALSE, height = "15px"),
  
	tabItems(
	
	   
	    tabItem(tabName = "simulation_parameters",
        fluidRow( 
		column(width=12,
		
		numericInput("parkingspot_count",
                       "Parking Spot Count",
                       min=0,
                       step=1,
                       max=Inf,
                       value=25),
					   
					   #arrivals per hour!!
		numericInput("arrivals_per_hour",
                       "Arrivals Per Hour",
                       min=0,
                       step=1,
                       max=Inf,
                       value=20),
					   
					   HTML("<br>"),
		numericInput("mean_time_in_spot",
                       "Time In Parking Spot - Mean",
                       min=0,
                       step=1,
                       max=Inf,
                       value=30),
					   
			HTML("<br>"),		   
numericInput("sd_time_in_spot",
                       "Time In Parking Spot - Standard Deviation",
                       min=0,
                       step=1,
                       max=Inf,
                       value=5),

					   
		HTML("<br>"),
		
		selectInput("spot_preference_method",
		label = "How to Choose a Parking Space",
		choices = c("Prefer Closer","Any Open Space"),
		selected = "Prefer Closer"),
	
		HTML("<br>"),
		numericInput("simrep_count",
                       "Simulation Replications",
                       min=0,
                       step=1,
                       max=Inf,
                       value=10),
					   
		HTML("<br>"),
		numericInput("hours_to_simulate",
                       "Hours to Simulate",
                       min=1,
                       step=0.5,
                       max=Inf,
                       value=24),

    HTML("<br>"),




		actionButton("begin_simulation", "Begin Simulation")
		   
		))),	  
		  tabItem(tabName = "output_utilization_boxplot",
        fluidRow( 
		column(width=12,		plotOutput("utilization_boxplot")))),

tabItem(tabName = "output_total_parkinglot_count",
        fluidRow( 
          column(width=12,		plotOutput("total_parkinglot_count")))),

tabItem(tabName = "code",
        fluidRow( 
          column(width=12,		
          tags$a(href="https://github.com/datadrivensupplychain/des_parkinglot_app",
                 "Github Repository For This App",
                 target="_blank"))))
		  #next three right parentheses close out UI
		  )))
	  


server <- function(input, output) {



#observeEvent for using uploaded individual files ----
observeEvent(input$begin_simulation, {

showModal(modalDialog(title = "Simulation In Progress", "Please Wait"))

#run simulation
sim_out <- lapply(1:input$simrep_count, function(i){
parkinglot_env <- simmer("parkinglot")


#define trajectory for cars ---
car_trajectory <- trajectory("car_in_parkinglot") %>%
seize("parking_lot",amount = 1) %>%  
set_attribute(c("selected_spot"), function() {
server_count <- get_server_count(parkinglot_env, paste0("space", 1:input$parkingspot_count))

#find indices of available spaces

df <- cbind.data.frame(space = 1:input$parkingspot_count,
                       server = server_count) %>%
  dplyr::filter(server==0) %>%
  dplyr::arrange(space) %>%
  dplyr::ungroup() %>%
dplyr::mutate(prob= 0.5^dplyr::row_number())


#select a space.  If just a single space is available, select that.
if(input$spot_preference_method == "Prefer Closer"){
space_index <-  ifelse(nrow(df)==1, df$space[1], sample(x=df$space,size=1,replace=FALSE,prob=df$prob)) }

if(input$spot_preference_method == "Any Open Space"){
space_index <-  ifelse(nrow(df)==1, df$space[1], sample(x=df$space,size=1,replace=FALSE)) }

#return space_index, that's the space to occupy
return(space_index) } ) %>%

simmer::select(function() paste0("space", get_attribute(parkinglot_env, "selected_spot"))) %>%
seize_selected() %>%
timeout(function() {max(rnorm(n=1,mean=input$mean_time_in_spot,sd=input$sd_time_in_spot), 1)}) %>% #normally distributed time, min 1 minute
release_selected() %>%
release("parking_lot",1)
#end trajectory

#run environment
parkinglot_env %>%
simmer::add_resource(name= paste0("space",1:input$parkingspot_count), capacity=1,queue_size=0,queue_size_strict=TRUE) %>%
simmer::add_resource(name = "parking_lot", capacity= input$parkingspot_count,queue_size=Inf)%>%

simmer::add_generator(name_prefix = "Car", trajectory= car_trajectory, function() {rexp(n=1,rate=(input$arrivals_per_hour/60))}) %>% 
  #rate is average arrivals per unit of time.
#timeout is in minutes, so arrivals must be in minutes.  want 30 arrivals per hour --> (30/60) arrivals per minute
simmer::run(until = (60*input$hours_to_simulate)) %>% simmer::wrap() })
  

showModal(modalDialog(title = "Simulation Complete", "Computing Output Charts and Tables"))



mon_arrivals_df <- get_mon_arrivals(sim_out,per_resource=TRUE) %>%
  dplyr::mutate(name = paste0("Car", as.numeric(stringr::str_replace(name,"Car",""))+1))


mon_resources_df <- simmer.plot::get_mon_resources(sim_out) %>%
  dplyr::mutate(spacenumber = as.numeric(stringr::str_replace(resource,'space','') ))

#make resource a factor, ordered by parking space location (1, 2, 3, 4... 25)
mon_resources_df$resource <- factor(mon_resources_df$resource,levels=c('parking_lot',paste0("space",1:input$parkingspot_count)))


#calculate time utilization across different simulations
utilization_df <- mon_resources_df %>%
  
  dplyr::ungroup() %>%
  dplyr::arrange(replication,resource,time) %>%
  dplyr::group_by(replication,resource) %>%
  dplyr::mutate(next_time = dplyr::lead(time)) %>%
  tidyr::replace_na(list( next_time = input$hours_to_simulate*60)) %>% #run until end
  dplyr::mutate(time_x_server = (next_time-time)*server,
                time_x_queue = (next_time - time)*queue) %>%
  dplyr::group_by(replication,resource) %>%
  dplyr::summarise(
    mean_server_quantity = sum(time_x_server)/(input$hours_to_simulate*60),
    mean_queue_quantity = sum(time_x_queue)/(input$hours_to_simulate*60),
    mean_server_plus_queue_quantity = sum(time_x_server + time_x_queue)/(input$hours_to_simulate*60),
    server_utilization = sum(time_x_server)/(mean(capacity)*input$hours_to_simulate*60))

#for non-utilized resources, add rows
df1 <- tidyr::crossing(resource=c('parking_lot',paste0('space',1:input$parkingspot_count)),replication=1:input$simrep_count) %>%
  dplyr::left_join(utilization_df) %>%
  tidyr::replace_na(list(mean_server_quantity=0, mean_queue_quantity=0, mean_server_plus_queue_quantity=0, server_utilization=0))

utilization_df <- df1
rm(df1)

utilization_df$resource <- factor(utilization_df$resource,levels=c('parking_lot',paste0("space",1:input$parkingspot_count)))

minval_utilization <- min(utilization_df$server_utilization)
maxval_utilization <- max(utilization_df$server_utilization)


utilization_seq <- seq( minval_utilization , maxval_utilization, 
                        plyr::round_any( (maxval_utilization - minval_utilization )/10,accuracy=0.0000000000001,f=floor))


output$utilization_df_output <- DT::renderDataTable({utilization_df})


output$utilization_boxplot <- renderPlot({
  ggplot2::ggplot(data = utilization_df,aes(x=resource,y=server_utilization,group=resource))+geom_boxplot() +
    stat_summary(fun=mean, geom="point", shape=20, size=5, color="red", fill="red") +
    theme_bw() +
    labs(title = "Parking Lot Utilization, Across Simulation Replications",
         subtitle = paste0(
           "Mean Parking Spaces Occupied: ",
           plyr::round_any(mean(utilization_df$mean_server_quantity[utilization_df$resource=='parking_lot']),0.01),
           ". Mean Cars Waiting in Lot: ",
           plyr::round_any(mean(utilization_df$mean_queue_quantity[utilization_df$resource=='parking_lot']),0.01),
           ". Mean Cars in Lot (Parked + Waiting): ",
           plyr::round_any(mean(utilization_df$mean_server_plus_queue_quantity[utilization_df$resource=='parking_lot']),0.01))) +
    scale_y_continuous(breaks= scales::breaks_pretty(10),labels=scales::percent) +
    theme(text = element_text(size=20),axis.text.x = element_text(angle=90, hjust=1))+
    xlab("Resource: Whole Parking Lot or Individual Space")+
    ylab("Utilization Across Simulation Replications") +
    theme(text = element_text(size=20)) }, height=1000)


  #plot total parking lot quantity over time
  df1 <- mon_resources_df %>% dplyr::filter(resource=='parking_lot')  %>%
    dplyr::mutate(replication_text = paste0("Replication #",replication)) 
  
  maxval_total_lot <- max(c(df1$server,df1$queue, (input$arrivals_per_hour/60)*input$mean_time_in_spot , input$parkingspot_count))
  
  df1$replication_text <- factor(x=df1$replication_text,levels=paste0("Replication #",1:input$simrep_count))
  
  output$total_parkinglot_count <- renderPlot({ ggplot2::ggplot(data = df1, aes(x=time/60)) +
    geom_line(aes(y = server),color="#00FF00",size=1) + facet_wrap(~replication_text) +
    geom_line(aes(y=queue),color="#ff0000",size=1) +
    #add in little's law line
    geom_hline(yintercept = (input$arrivals_per_hour/60)*input$mean_time_in_spot,size=1,color='black',linetype='twodash')+
    geom_hline(yintercept = input$parkingspot_count,size=1,color='blue',linetype='twodash') +
    theme_bw()+
    scale_fill_identity()+
    theme(text = element_text(size=20),axis.text.x = element_text(angle=90, hjust=1))+

    scale_x_continuous(breaks=seq(0,10000,2))+
      scale_y_continuous(breaks= scales::breaks_pretty(10)) +

    xlab("Hours From Simulation Start")+
    ylab("Cars Parked And Waiting")+
    
    labs(title="Parking Lot: Total Cars Parked (Green) and Waiting (Red)",
         subtitle=paste0("Blue Horizonal Dashed Line Indicates Parking Lot Capacity.  Black Horizontal Dashed Line Indicates Little's Law: Average of ",
                         plyr::round_any((input$arrivals_per_hour/60)*input$mean_time_in_spot,0.01),
                         " Cars In Parking Lot"),
         caption="If Arrival Rate Exceeds Service Rate (Service Rate = # of Spaces * Average Time Occupying a Space), # Cars Waiting Will Grow Without End") }, height=1000) 

showModal(modalDialog(title = "Simulation Complete", "Please Switch to Output Tabs"))



})





}

# Run the application 
shinyApp(ui = ui, server = server) 