library(shiny)
library(shinythemes)
library(ggplot2)
library(dplyr)
library(plotly)

#Currently using example data prior to upload of PCGP log2CPM data
data <- read.csv("data/Example_Expression_Data.csv", stringsAsFactors = FALSE, header=TRUE)

# Extract just DIPG samples
DIPG <- data %>% filter(Subtype=="DIPG") #29 tumors
NBSHGG <- data %>% filter(Subtype=="NBS-HGG" & histone!="H3.3 K27M") #46 tumors


# Define ui
ui <- fluidPage(theme = shinytheme("united"),   #fluidPage create display to automatically adjust to the dimensions of the user's window
  titlePanel(strong("Gene Expression in Pediatric High Grade Gliomas (HGGs)", style = "color:blue")),
  
  sidebarLayout(position="left",  #always takes 2 arguments: sidebarPanel and mainPanel
                sidebarPanel(
                  textInput("gene", h3("Type in human gene symbol"), value = "CCND2"),
                  selectInput("select", h3("Select a comparison"), 
                              choices = list("All HGGs", 
                                             "DIPGs by H3 Status",
                                             "DIPGs: ACVR1 Mut vs ACVR1 WT",
                                             "NBS-HGGs H3 G34R vs. H3 WT",
                                             "All HGGs: Trk Fusion vs Trk WT"),
                                              selected = "All HGGs")),
                
                
                mainPanel(
                  p("Expression Data from 83 pediatric HGGs including DIPGs and NBS-HGGs.  Ages range from <1 year to 22 years old.  Tumors can be queried by tumor type (DIPG vs Non-brainstem HGG), H3 status, ACVR1 mutation status and the presence of Trk fusions."),
                  p(strong("App currently using EXAMPLE DATA.  Data from Pediatric Cancer Genome Project (PCGP) cohort will be uploaded soon")),
                  #p("More information regarding the sample cohort can be found in Wu et al", 
                  #span("Nat Genet.", style="font-style:italic"),
                  # "2014."),
                  textOutput("description"),
                  plotlyOutput("boxplot"),
                )
  ))


#Define server logic
server <- function(input, output) {
  
  output$gene<-renderText({
    toupper(input$gene)})
  
  output$boxplot<-renderPlotly({
    
    #Switch function transforms output of a select widget 
    
    
    df<-switch(input$select, 
               "All HGGs"=data,
               "DIPGs by H3 Status"=DIPG,
               "NBS-HGGs H3 G34R vs. H3 WT"=NBSHGG,
               "DIPGs: ACVR1 Mut vs ACVR1 WT"=DIPG,
               "All HGGs: Trk Fusion vs Trk WT"=data)
    
    group<-switch(input$select,
                  "All HGGs"=df$Subtype,
                  "DIPGs by H3 Status"=df$histone,
                  "NBS-HGGs H3 G34R vs. H3 WT"=df$histone,
                  "DIPGs: ACVR1 Mut vs ACVR1 WT"=df$ACVR1_Mut,
                  "All HGGs: Trk Fusion vs Trk WT"=df$Trk)
    
    colors<-switch(input$select,
                   "All HGGs"=c("goldenrod", "red3"),
                   "DIPGs by H3 Status"=c("#00AFBB", "#FC4E07", "#E7B800"),
                   "NBS-HGGs H3 G34R vs. H3 WT"=c("#00AFBB", "#E7B800"),
                   "DIPGs: ACVR1 Mut vs ACVR1 WT"=c("purple", "#E7B800"),
                   "All HGGs: Trk Fusion vs Trk WT"=c("seagreen", "#E7B800"))
    
    # So hover label will not be gibberish.
    cpm <- df[,toupper(input$gene)]

    p <- ggplot(df, aes(x=group, y=cpm, fill=group)) + 
      geom_boxplot(width=0.4, aes(alpha=0.9), outlier.shape = NA) +
      scale_fill_manual(values=colors) +
      geom_point(aes(fill=group), size = 2, position = position_jitter(width=0.1)) +
      ggtitle(toupper(input$gene)) +
      xlab("Subtype") + ylab("Log2CPM") + theme_classic() +
      theme(axis.text=element_text(size=16),axis.text.x = element_text(angle = 45),
            axis.title=element_text(size=16),
            plot.title = element_text(hjust = 0.5, size=20, face="bold"),
            legend.text=element_text(size=14)) +
      guides(color=guide_legend(group)) +
      scale_alpha(guide = 'none')
    
    # Necessary non-sense to hide ggplotly boxplot outliers since it doesn't honor outlier.shape = NA.
    # See https://github.com/ropensci/plotly/issues/1114 for details.
    p <- plotly_build(p)

    p$x$data <- lapply(p$x$data, FUN = function(x){
  		if (x$type == "box") {
    		x$marker = list(opacity = 0)
  		}
  		return(x)
	})

	p
  })
  
}

# Run app ----
shinyApp(ui, server)
