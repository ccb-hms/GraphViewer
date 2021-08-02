

library(shiny)
library(BioPlex)
library(BioNet)
library(ggnetwork)

gr <- readRDS("ribo_graph.rds")
gr <- BioNet::largestComp(gr)
ig <- igraph::graph_from_graphnel(gr)

# quick fix:
igraph::vertex_attr(ig, "ENTREZID") <- as.character(igraph::vertex_attr(ig, "ENTREZID"))

.getNumericVertexAttributes <- function(ig)
{
    vas <- igraph::vertex_attr(ig)
    ind <- vapply(vas, is.numeric, logical(1))
    return(names(vas)[ind])
}

.getNumericEdgeAttributes <- function(ig)
{
    vas <- igraph::edge_attr(ig)
    ind <- vapply(vas, is.numeric, logical(1))
    return(names(vas)[ind])
}

buildUI <- function(ig)
{
    nchoices <- .getNumericVertexAttributes(ig)
    echoices <- .getNumericEdgeAttributes(ig)
    
    ui <- fluidPage(
        
        # Application title
        titlePanel("BioPlex graph data viewer"),
        
        # Sidebar with a slider input for number of bins 
        sidebarLayout(
            sidebarPanel(
                selectInput("ndata",
                            label = "Node data:",
                            choices = nchoices,
                            selected = nchoices[1]),
                selectInput("edata",
                            label = "Edge data:",
                            choices = echoices,
                            selected = echoices[1])
            ),
            
            # TODO: support hover 
            mainPanel(
                plotOutput("ggnplot", hover = hoverOpts(id = "plot_hover" ))
            )
        )
    )
    return(ui)
}

# Define server logic required to draw a histogram
server <- function(input, output) {

    output$ggnplot <- renderPlot({
        # generate bins based on input$bins from ui.R
        ndata <- input$ndata
        edata <- input$edata

        # draw the histogram with the specified number of bins
        ggplot(ggnetwork(ig, ), 
               aes(x = x, y = y, xend = xend, yend = yend)) +
            geom_nodes(aes_string(size = ndata)) +
            geom_edges(aes_string(color = edata), 
                       arrow = arrow(length = unit(6, "pt"), type = "closed")) +
            theme_blank()
    })
    
    output$plot_hoverinfo <- renderPrint({
        cat("Hover (throttled):\n")
        str(input$plot_hover)
    })
}


GraphViewer <- function(ig)
{
    shinyApp(ui = buildUI(ig), server = server)
}

# run the app
GraphViewer(ig)
