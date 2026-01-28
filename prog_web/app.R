# Rshiny 
install.packages('shiny')
library(shiny)

# Contenu de l’interface
ui <- fluidPage("Hello World")

# Commandes à executer
server <- function(input, output){}

# Association interface & commandes
shinyApp(ui = ui, server = server)
