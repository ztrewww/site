#JDBC connection
s9f2715069fee836a02a707 #
connection_to_db <- function(){
library(rJava)
library(RJDBC)
jdbcDriver =JDBC("oracle.jdbc.OracleDriver",classPath="C:/R/ojdbc6.jar")
jdbcConnection =dbConnect(jdbcDriver, "jdbc:oracle:thin:@//DESKTOP-S7GKRP9:1521/xe", "re","s9f2715069fee836a02a707")
paste("table", Sys.time(), sep = "_") = dbReadTable(con,'BP_DATA')
#ggplot2_data = dbReadTable(con, 'GGPLOT2_DATA')
}

methods(call_library());

list.of.packages[-3];

methods(call_library())
list.of.packages

#Remove function 
rm(list = "add_name")

call_library <- function(){
  
  for (value in list.of.packages) {
    library(value, character.only = TRUE)
  }
  
}
#function hivas call_lib("broom")

call_lib <- function(library_name){
  
  if (!is.null(library_name)) {
  list.of.packages <- append(list.of.packages, library_name);
  }
  
  for (value in list.of.packages) {
    library(value, character.only = TRUE)
  }
  
}

restore_func <- function(data_frame_name, new_data_frame_name){
  
  list.of.packages <- c("ggalt", "ggplot2", "dplyr")
  call_library()
  
  data_frame_name <- as.data.frame(data_frame_name)
  #new_data_frame_name <- as.data.frame(new_data_frame_name)

  new_data_frame_name <- data_frame_name %>% filter(!grepl("Total", Country))
  #new_data_frame_name <- data_frame_name %>% filter(!grepl("Rest of", Country))
  new_data_frame_name <- as.data.frame(new_data_frame_name)
  
  unique()
  
}


subset_high_hp <- function(full_table, df_name) {
  sub_df <- full_table %>% 
    filter(Year > 2000)
  
  assign(x = df_name, value = sub_df, envir = globalenv())
}


mtcars_Func <- function(full_table, df_name) {
  sub_df <- full_table %>% 
    filter(hp > 200)
  
  assign(x = df_name, value = sub_df, envir = globalenv())
}



#bar_plot
ggplot(bp, aes(x=Region, y=pop)) + geom_bar(stat = "identity")

#MOre rows on a plot
grid.arrange(ggplot(ab, aes(ab$pop)) + stat_ash(),
             ggplot(ab, aes(ab$biomass_mtoe)) + stat_bkde(),
             ggplot(ab,aes(ab$co2_mtco2)) + stat_density(),
             nrow=3)

#Barplot with region's population
ggplot(bp2, aes(bp2$Region, bp2$pop)) + geom_bar(stat="identity", width = 0.5, fill="tomato2") + labs(title="Bar Chart")


ggplot(bp2, aes(x=Year, y=coalcons_mtoe)) + 
  geom_point(aes(col=Region, size=oilcons_mtoe)) + 
  geom_smooth(method="loess", se=F) + theme_bw()



ggplot(sub_bp, aes(x=Year, y=pop)) + 
  geom_point()



ggplot(sub_bp, aes(x=Year, y=sub_bp$pop)) + 
  geom_bar(stat="identity", width=.5, fill="green") + 
  labs(title="Ordered Bar Chart", 
       subtitle="Make Vs Avg. Mileage", 
       caption="source: mpg") + 
  theme(axis.text.x = element_text(angle=1, vjust=0.9))


ggplot(sub_bp, aes(Year))
 + geom_bar(aes(fill=pop), width = 0.5) + 
  theme(axis.text.x = element_text(angle=65, vjust=0.6)) + 
  labs(title="Histogram on Categorical Variable", 
       subtitle="Manufacturer across Vehicle Classes") 



ggplot(sub_bp, aes(x = "", y=pop, fill = factor(Region))) + 
  geom_bar(width = 1, stat = "identity") +
  theme(axis.line = element_blank()) 
        + 
  labs(fill="class", 
       caption="Source: mpg") + coord_polar(theta = "y", start=0)


ggplot(sub_bp, aes(Region)) + geom_bar(aes(fill=pop), width = 0.5) + 
  theme(axis.text.x = element_text(angle=1, vjust=0.6)) +  labs(title="Categorywise Bar Chart", 
       subtitle="Manufacturer of vehicles", 
       caption="Source: Manufacturers from 'mpg' dataset")
   

ggplot(sub_bp, aes(x=Year, y=pop)) +    geom_line(color="#69b3a2", size=2)
             