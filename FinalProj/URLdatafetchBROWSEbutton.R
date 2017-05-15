library(shiny)


ui <- fluidPage(
  fileInput("file", "Select a file"),
  verbatimTextOutput("text")
)

server <- function(input, output, session) {
  output$text <- renderText({
    filePath <- input$file$datapath
    fileText <- paste(readLines(filePath), collapse = "\n")
    fileText
  })
}

shinyApp(ui = ui, server = server)

# Time Series Plotting
library(ggplot2)
library(xts)
library(dygraphs)

# Get IBM and Linkedin stock data from Yahoo Finance
jetF <- "https://fred.stlouisfed.org/graph/fredgraph.csv?chart_type=line&recession_bars=on&log_scales=&bgcolor=%23e1e9f0&graph_bgcolor=%23ffffff&fo=Open+Sans&ts=12&tts=12&txtcolor=%23444444&show_legend=yes&show_axis_titles=yes&drp=0&cosd=1990-04-01&coed=2017-04-01&height=450&stacking=&range=Max&mode=fred&id=MJFUELUSGULF&transformation=lin&nd=1990-04-01&ost=-99999&oet=99999&lsv=&lev=&mma=0&fml=a&fgst=lin&fgsnd=2009-06-01&fq=Monthly&fam=avg&vintage_date=&revision_date=&line_color=%234572a7&line_style=solid&lw=2&scale=left&mark_type=none&mw=2&width=1168"

del_url <- "http://real-chart.finance.yahoo.com/table.csv?s=DAL&a=05&b=11&c=2007&d=05&e=11&f=2017&g=d&ignore=.csv"

lnkd_url <- "http://real-chart.finance.yahoo.com/table.csv?s=LNKD&a=07&b=24&c=2010&d=07&e=24&f=2015&g=d&ignore=.csv"

yahoo.read <- function(url){
  dat <- read.table(url,header=TRUE,sep=",")
  df <- dat[,c(1,5)]
  df$Date <- as.Date(as.character(df$Date))
  return(df)}

ibm  <- yahoo.read(ibm_url)
lnkd2 <- yahoo.read(lnkd_url)
JetTest <- yahoo.read(jetF)


ggplot(ibm,aes(Date,Close)) + 
  geom_line(aes(color="ibm")) +
  geom_line(data=lnkd2,aes(color="lnkd")) +
  labs(color="Legend") +
  scale_colour_manual("", breaks = c("ibm", "lnkd"),
                      values = c("blue", "brown")) +
  ggtitle("Closing Stock Prices: IBM & Linkedin") + 
  theme(plot.title = element_text(lineheight=.7, face="bold"))

MJFUELUSGULF

pdfetch_FRED(c("GDPC1", "PCECC96"))
jetTest <- pdfetch_FRED('MJFUELUSGULF',cosd='1990-04-01', coed='2017-04-01')


install.packages('httr'), zoo, xts, XML, lubridate, jsonlite, reshape2, xml2,
stringr, readr')











