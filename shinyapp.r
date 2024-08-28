
library(ggplot2)
library(tidyverse)
library(cluster)
library(sf)
library(leaflet)
library(shiny)
library(vctrs)
library(dbplyr)
library(mapview)
library(shinythemes)
library(bslib)

set.seed(123)

# read in data
df <- read.csv("C:\\Warehouse\\Advanced R Programming for Data Science\\DataCoSupplyChainDataset.csv", header=TRUE)

#removing unneeded columns
remove_cols <- c("Type","Days.for.shipping..real.","Days.for.shipment..scheduled.",
                 "Sales.per.customer","Delivery.Status","Late_delivery_risk",
                 "Category.Id","Category.Name", 
                 "Order.City", "Order.Country", "Order.Customer.Id", "order.date..DateOrders.", "Order.Id",
                 "Customer.Country","Customer.Email", "Customer.Fname",
                 "Customer.Id","Customer.Lname", "Customer.Password",
                 "Customer.Segment", "Customer.State", "Customer.Street", 
                 "Customer.Zipcode", "Department.Id", "Department.Name", 
                 "Order.Item.Cardprod.Id", "Order.Item.Discount", 
                 "Order.Item.Discount.Rate", "Order.Item.Id",
                 "Order.Item.Product.Price",
                 "Order.Item.Profit.Ratio",	"Order.Item.Quantity", 
                 "Sales", "Order.Item.Total", "Order.Profit.Per.Order",
                 "Order.Region", "Order.State", "Order.Status",
                 "Order.Zipcode", "Product.Card.Id", "Product.Category.Id",
                 "Product.Description", "Product.Price", 
                 "Product.Status",	"shipping.date..DateOrders.",	"Shipping.Mode")


data <- df %>% 
  select(-remove_cols)
data

data <- Filter(function(x)!all(is.na(x)), data) 

data <- na.omit(data)

#size
str(data)

#renamedata 
data <- data %>%
  rename("Benefit_per_order" = "Benefit.per.order",
         "Product_Image" = "Product.Image",
         "Product_Name" = "Product.Name",
         "Customer_City" = "Customer.City")

colnames(data)


unique(data$Product_Name)
n_distinct(data$Product_Name)
x <- table(data$Product_Name)

typeof(x)
head(x)

x <- data.frame(x)
colnames(x)

data_cluster <- select(data, c("Product_Name", "Longitude","Latitude"))
colnames(data_cluster)

#assign an id to each product for clustering
library(dplyr)
data_cluster <- data_cluster %>% group_by(Product_Name) %>% mutate(id=cur_group_id())
nrow(data_cluster)
tail(data_cluster)
##n_distinct(data_cluster$id)

#legend table to refer
legend <- unique(data_cluster[ , c("Product_Name", "id")])
legend <- legend %>% arrange(id)
nrow(legend)
#K-means
library(factoextra)
set.seed(123)

#get the id of the desired product
id = 27 #textbox

#retrieve information for that id
for_the_cluster <- data_cluster[data_cluster$id == id,c(2,3)]

#get the elbow
fviz_nbclust(for_the_cluster, kmeans, method = "wss")
elbow <- 3 #textbox

#pass the elbow to get the cluster
ke <- kmeans(for_the_cluster, centers = elbow, nstart = 25)
fviz_cluster(ke, data = for_the_cluster) #visualize the clusters

#get latitude and longitude for the clusters
ke$centers

z <- data.frame(ke$centers)
colnames(z)
str(z)

library(maps)

country <- map.where(database="world", 
                     z$Longitude, z$Latitude)
state <- map.where(database="state", 
                   z$Longitude, z$Latitude)
county <- map.where(database="county", 
                    z$Longitude, z$Latitude)

locations <- data.frame(country,state,county)
finaldf <- data.frame(c(z,locations))


# create UI
ui <- fluidPage(
  
  titlePanel("Warehouse Location Recommender System"),
  theme = bs_theme(bootswatch = "quartz"),
  tabsetPanel(
    
    # Title tab panel
    tabPanel("Introduction",
             img(src = "worldMap.png",height='500px',width='800px'),
             h2("Warehouse Location Recommender System"),
             p(strong("By Prashnim Seth, Harshita Singh")),
             p(a(href = "https://github.com/Prashnim/Warehouse-Location-Recommendation-System", "Github Repo(1)"), style = "color:deepskyblue"),
             p(a(href = "https://github.com/HarshitaSinghWMU/Warehouse-Location-Recommendation-System-", "Github Repo(2)"), style = "color:deepskyblue"),
             
             h6("Abstract:", style = "font-size: 18px;font-weight: bold;"),
             p("This project aims to build a recommender system that analyses the demand and groups it into regions and suggests a suitable location for a business or a warehouse as per the product. 
  The project will follow a content-based filtering approach by applying an unsupervised learning algorithm – KMeans Clustering to compare products and similar order locations to provide a recommendation for storing the product in a closer warehouse.", style = "text-align: justify;"),
             
             h4("Introduction", style = "font-size: 19px;font-weight: bold;"),
             p("A Recommendation System is a tool that provides suggestions for a service or a product. 
  These systems help with retaining customers, increasing sales, boosting cart value, speeding up the work pace and much more.
  A lot of big companies use recommendation systems with their products.
  For example, Amazon uses recommendation system to suggest similar or more products based on the customer’s search pattern, 
  Netflix uses it to suggest similar movies or series to keep their customers hooked, Spotify uses it to recommend songs of the same genre or a similar artist.
  Usually, recommendation systems are used for online services, but they can be used in multiple traditional offline services too. 
  Recommendation Systems can help suggest restaurants with specific cuisines, identify locations with more customers or drive business value by suggesting in-store merchandizing strategies. 
  The goal of this project is to build a recommender system that will suggest a suitable location for the product warehouses to reduce the shipping time, delays and increase sales of the product. 
  This will be done by identifying locations where most customers buy a specific product and its quantities and to suggest the best location for a warehouse.")
  ),
    
    
    # System Architecture tab panel
    tabPanel("System Architecture",
             h3("System Architecture:", style = "font-size: 18px;font-weight: bold;"),
             img(src = "system.png",height='500px',width='900px'),
             h4("Design Overview:",style = "font-size: 18px;font-weight: bold;"),
             p("As per system architecture, the location and product information of all orders was extracted to form the given dataset for further processing and analysis. The extracted information was pre-processed, and a new cleaner dataset was created. Data Analysis was on the features to explore and make sure if the dataset is ready or not. A map was created to visualize the neighborhood as per the product location groups and further understanding.  

After this basic analysis, the features were further filtered and analyzed for clustering. The filtered dataset was now ready to be clustered. 

A model was built based on content-based filtering approach by applying KMeans clustering here. The desired product for which the recommendation is to be generated is selected here and clusters are formed based on Euclidean distance between the several latitudes and longitudes from which this product orders were made. 

After selecting the optimal number clusters using the elbow method, the model is processed, and we are presented with the cluster centers in the form of latitudes and longitudes that represent the recommended locations for this product’s warehouse. 

By reverse geocoding, using the cluster center latitudes and longitudes the exact city and country were extracted for recommending the best locations for the products warehouse. ")
    ),
    
    # Data Processing tab panel
    tabPanel("Data Pre-Processing and Visualization",
             h2("A:Dataset Description and Exploratory Data Analysis  :", style = "font-size: 18px;font-weight: bold;"),
             p(a(href = "https://data.mendeley.com/datasets/8gx2fvg2k6/5", "Dataset Link"), style = "color:deepskyblue"),
             p("The selected dataset is about the supply chain information by the company DataCo Global. The dataset consists of several columns with information on the products, shipping, customer details and sales. 

The dataset is a mix of structured and unstructured data. It consists of 180519 rows and 53 columns. There are 118 unique products in this dataset."), 
             
             p("Exploring different columns:"),
             div(
             p(strong("1.Most expensive Products")),
             img(src = "MEP.png",height='300px',width='400px')
             ),
             br(),
             div(
               p(strong("2.Least Expensive Products")),
             img(src = "LEP.png",height='300px',width='400px')
             ),
             br(),
             div(
               p(strong("3.Days for shipping real vs scheduled ")),
             img(src = "DSS.png",height='300px',width='400px')
             ),
             br(),
             div(
               p(strong("4.Displaying distinct products and markets ")),
             img(src = "Table.png",height='300px',width='400px')
             ),
             br(),
             div(
               p(strong("5.Count of each Delivery Status ")),
             img(src = "count.png",height='300px',width='400px')
             ),
             br(),
             div(
               p(strong("6.Scatter plot of Latitude and Longitude ")),
               img(src = "scatter.png",height='300px',width='400px'),
               p("It was created to visualize the variance in the latitudes and longitudes provided")
             ),
             br(),
             
             p(strong("The columns that would be used in this project are divided into two categories:")) ,
             p("1.	Location Details – The columns with information about the location and other details about where location details about the order. 
The columns in this category are Latitude, Longitude, Customer City and Market."),
             p("2.	Product Details – The columns which give information about the product and the order made.
The columns in this category are Benefit per Order, Product Name, Product Image.
These columns were further filtered upon more exploration and analysis of the dataset."),
             
             h4("B.  Data Pre-processing:", style = "font-size: 18px;font-weight: bold;") ,
             p("After exploring the dataset, in this step all the unnecessary columns were removed. The column names were changed to make them easier to understand and work on.
The remaining columns or the selected features were - Benefit_per_order, Customer_City, Latitude, Longitude, Market, Product_Image, Product_Name.
Then the rows with all null values were removed and the rows that had any empty cells were omitted from the final dataset.
The pre-processed dataset to be used for analysis now had 180519 rows and 7 columns remaining.
"),
             dataTableOutput("cleantable")
    ),
    
    # Analysis Map tab panel
    tabPanel("Analysis",
             sidebarLayout(
               sidebarPanel(
                 # Analysis type selector
                 selectInput(inputId = "analysis", label = "Select analysis type:",
                             choices = c("10 most ordered products", "10 least ordered products","Highest benefit", "Least benefit", "map"),
                             selected = "10 most ordered products")
               ),
               mainPanel(
                 h2("Analysis"),
                 tabsetPanel(
                   tabPanel("Plot", plotOutput("plot", width = "100%", height = "600px")),
                   tabPanel("Map", leafletOutput("map", width = "100%", height = "600px"))

                 )
               )
             )
    ),
    
    # Clustering tab panel
    tabPanel("Clustering",
             p("The following map displays the locations of the selected product based on its similarity to other products:"),
             sidebarLayout(
               sidebarPanel(
                 textInput(inputId = "productid", label= "Please enter a Product ID"),
                 textInput(inputId = "clusterid", label= "Select the number of clusters:")
               ),
               mainPanel(
                 h2("Clustering"),
                 dataTableOutput("Legend"),
                 #tabPanel("elbow", plotOutput("plot", width = "100%", height = "600px"))
                 #textOutput(outputId = "message")
                 plotOutput("plot1"),
                 plotOutput("plot2"),
                 dataTableOutput("finaltable")
               )
             )
             
    ),
 
    # Results tab panel
    tabPanel("Results",
             h2("Results"),
             p("The model produced different cluster plots for different products that give us the location recommendations for every product. 

The tables and figures below show the steps used to achieve the recommendations: "),
             div(
               p(strong("Legend Table that maps products to an ID")),
               img(src = "Legend.png",height='300px',width='400px')
             ),
             br(),
             p(strong("Recommendation for the product Adult Dog Diapers: ")),
             div(
               p(strong("Elbow Curve for Product – Adult Dog Diapers ")),
               img(src = "elbow.png",height='300px',width='400px')
             ),
             br(),
             div(
               p(strong("Clusters for Adult Dog Diapers ")),
               img(src = "cluster.png",height='300px',width='400px')
             ),
             br(),
             div(
               p(strong("Generated recommendations for warehouse locations for Adult Dog Diapers")),
               img(src = "location.png",height='300px',width='400px')
             ),
             br(),
             p(strong("Recommendation for the product First Aid Kit: ")),
             div(
               p(strong("Elbow Curve for Product –First Aid Kit ")),
               img(src = "elbow.FA.png",height='300px',width='400px')
             ),
             br(),
             div(
               p(strong("Clusters for First Aid Kit")),
               img(src = "cluster.FA.png",height='300px',width='400px')
             ),
             br(),
             div(
               p(strong("Generated recommendations for warehouse locations for First Aid Kit")),
                 img(src = "location.FA.png",height='300px',width='400px')
               ),
               
              p("Some rows might have NA in country, state, and county due to the latitude and longitude pointing to a location without a name or some place in the ocean. In this case, the best recommended location would be the nearest shipping port from the pointed location."),
             
              div(
                p(strong("Map for the order locations grouped by products ")),
                img(src = "map.p.png",height='300px',width='400px')
              ),
              div(
                p(strong("Map with Legend for the order locations grouped by products. ")),
                img(src = "mapLP.png",height='300px',width='400px')
              )
             
    ),
  # Conclusion
  tabPanel("Conclusion",
           h3("Conclusion:", style = "font-size: 18px;font-weight: bold;"),
           p("In conclusion, this recommendation system generates recommendations based on the location of orders by calculating the Euclidean distance of the all the product orders per product."),           p(strong("Answered Questions:")),
           p("1. What place do most orders come from?"),

p("Most orders come from the country USA."),

p("2.What product is the most in demand?"),

p("Perfect Fitness Perfect Rip Deck” is the most in demand product."),

p("3 Where is it most beneficial to have storage space for every product?"),

p(strong("Examples:")),  
p("1.The product “Adult Dog Diapers” is recommended to be stored in Mohave County, Arizona, USA."),
p("2.The product “First Aid Kit” is recommended to be stored in Kern County, California, USA and Fayette County, West Virginia, USA."),
p(strong("Future Works:")),
p("In the next version of this system, the system could use benefits per order to assign weight to each point and then find the cluster centers. The elbow method could be automated so that the step of picking the optimal number clusters manually is eliminated. The NA values from latitudes and longitudes in the results could automatically be set to point to the closest shipping port from the pointed location")
 )
)
)

  
     
  

# create server
server <- function(input, output) {
  #Tables display
  output$Legend <-  renderDataTable(legend)
  output$cleantable <- renderDataTable(data, options = list(pageLength = 5))
  output$finaltable <- renderDataTable(getFinal())
  
  getFinal <- function(){
    id <- input$productid
    elbow <- input$clusterid
    for_the_cluster <- data_cluster[data_cluster$id == id,c(2,3)]
    ke <- kmeans(for_the_cluster, centers = elbow, nstart = 25)
    fviz_cluster(ke, data = for_the_cluster)
    ke$centers
    z <- data.frame(ke$centers)
    country <- map.where(database="world", 
                         z$Longitude, z$Latitude)
    state <- map.where(database="state", 
                       z$Longitude, z$Latitude)
    county <- map.where(database="county", 
                        z$Longitude, z$Latitude)
    
    locations <- data.frame(country,state,county)
    finaldf <- data.frame(c(z,locations))
    return(finaldf)
  }
  
  output$plot <- renderPlot({
    # get data for plot
    get_data() %>%
      # create plot
      ggplot(aes(z = Longitude, y = Latitude)) +
      geom_point() +
      theme_minimal() +
      labs(title = input$title, z = "Longitude", y = "Latitude", color = "Product")
  })
  #######  
  #display elbow curve
  output$plot1 <- renderPlot({
    id <- input$productid
    for_the_cluster <- data_cluster[data_cluster$id == id,c(2,3)]
    fviz_nbclust(for_the_cluster, kmeans, method = "wss")
  })
  #display clusters
  output$plot2 <- renderPlot({
    id <- input$productid
    elbow <- input$clusterid
    for_the_cluster <- data_cluster[data_cluster$id == id,c(2,3)]
    ke <- kmeans(for_the_cluster, centers = elbow, nstart = 25)
    fviz_cluster(ke, data = for_the_cluster)
  })
  ################

  
  output$plot <- renderPlot({
    # get data for plot
    get_data() %>%
      # create plot
      ggplot(aes(x = Longitude, y = Latitude)) +
      geom_point() +
      theme_minimal() +
      labs(title = input$title, x = "Longitude", y = "Latitude", color = "Product")
  })
  
  # create function to get data for plot
  get_data <- function() {
    switch(input$analysis,
           "10 most ordered products" = {
             data %>%
               count(Product_Name) %>%
               top_n(10, wt = -n) %>%
               arrange(desc(n))
           },
           
           "10 most ordered products" = {
             data %>%
               count(Product_Name) %>%
               top_n(10, wt = n) %>%
               arrange(desc(n))
           },
           "Highest_benefit" = {
             data %>%
               group_by(Product_Name) %>%
               summarize(mean_benefit = mean(Benefit_per_order)) %>%
               top_n(10, wt = mean_benefit) %>%
               arrange(desc(mean_benefit))
           },
           "Least benefit" = {
             data %>%
               group_by(Product_Name) %>%
               summarize(mean_benefit = mean(Benefit_per_order)) %>%
               top_n(-10, wt = mean_benefit) %>%
               arrange(mean_benefit)
           },
           "map" = {
             data %>%
               filter(!is.na(Product_Image)) %>%
               group_by(Product_Name, Product_Image) %>%
               slice(1) %>%
               st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
           },
           cluster <- reactive({
             data_cluster %>%
               filter(Product_Name == input$data_cluster) %>%
               select(Longitude, Latitude) %>%
               na.omit() %>%
               kmeans(centers = 3) %>%
               .$centers %>%
               as_tibble() %>%
               set_names(c("Longitude", "Latitude"))
           })
    )
  }
  
  # create function to generate map
  output$map <- renderLeaflet({
    # get data for map
    get_data() %>%
      # create map
      leaflet() %>%
      addTiles() %>%
      addProviderTiles(provider = "Stamen.Toner") %>% # change map type to Stamen.Toner
      addMarkers(popup = ~as.character(Product_Name))
  })
  output$plot <- renderPlot({
    switch(input$analysis,
           "10 most ordered products" = {
             x %>% 
               arrange(desc(Freq)) %>%
               slice(1:10) %>%
               ggplot(., aes(y=Var1, x=Freq))+
               geom_bar(stat='identity',fill = "#00bc6c")+
               labs(title = "10 most ordered products", x ="order count", y = "product name")
             
           },
           
           "10 least ordered products" = {
             x %>% 
               arrange(Freq) %>%
               slice(1:10) %>%
               ggplot(., aes(y=Var1, x=Freq))+
               geom_bar(stat='identity',fill = "#00bc6c")+
               labs(title = "10 least ordered products", x ="order count", y = "product name")
           },
           "Highest benefit" = {
             data %>% group_by(Product_Name) %>% 
               summarise(Benefit_per_order=sum(Benefit_per_order),
                         .groups = 'drop')  %>%
               arrange(desc(Benefit_per_order)) %>%
               slice(1:10) %>%
               ggplot(., aes(y=Product_Name, x=Benefit_per_order))+
               geom_bar(stat='identity',fill = "pink")
           },
           "Least benefit" = {
             data %>% group_by(Product_Name) %>% 
               summarise(Benefit_per_order=sum(Benefit_per_order),
                         .groups = 'drop')  %>%
               arrange(Benefit_per_order) %>%
               slice(1:10) %>%
               ggplot(., aes(y=Product_Name, x=Benefit_per_order))+
               geom_bar(stat='identity',fill = "navy")
           },
           "Map" = {
             map = data %>%
               filter(!is.na(Product_Image)) %>%
               group_by(Product_Name, Product_Image) %>%
               slice(1) %>%
               st_as_sf(coords = c("Longitude", "Latitude"), crs = 4326)
             mapview(map, zcol = "Product_Name", legend = T, legend.control = list(position = "bottomleft"))
             
           },
    
           cluster <- {
             data_cluster %>%
               filter(Product_Name == input$product) %>%
               select(Longitude, Latitude) %>%
               na.omit() %>%
               kmeans(centers = 3) %>%
               .$centers %>%
               as_tibble() %>%
               set_names(c("Longitude", "Latitude")) %>%
               leaflet() %>%
               addTiles() %>%
               addMarkers(clusterOptions = markerClusterOptions())
           }
    )
  })
  
  
}

# run app
shinyApp(ui=ui, server=server)

