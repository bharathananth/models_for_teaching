library("deSolve")
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(gridExtra)

bistable <- function(t, y, p) {
  with(as.list(c(y, p)), {
    dX <- X^h/(K^h + X^h) - d*X
    list(X=dX)
  })
}

## prepare data structures to create UI programmatically
y0    <- c(X=1)
parms <- c(K = 1, h=2, d=0.25)
aspect <- 0.5

makelist <- function(i, obj, min=NA, max=NA, step=NA, width=NULL) {
  list(inputId=names(obj[i]), label=names(obj[i]),
       value=unname(obj[i]), min=min, max=max, step=step,
       width=width)
}

## two lists of lists
L_parms <- lapply(1:length(parms), makelist, obj=parms, min=0, max=2, step=0.05, width=75)
L_y0 <- lapply(1:length(y0), makelist, obj=y0, min=0, max=5, step=0.1, width=75)

server <- function(input, output, session) {
  output$bistable <- renderPlot({
    L_input <- reactiveValuesToList(input) # to enable width
    y0    <- with(L_input, c(X=X))
    parms <- with(L_input, c(K=K, h=h, d=d))
    times <- seq(0, 150, .1)
    out <- ode(y0, times, bistable, parms)
    df1 <- data.frame(out) %>% gather(var, value,-time)
    par(mfrow=c(2, 1))
    f <- ggplot(df1, aes(x=time, y=value, color=var)) + geom_line(size=1) + theme_bw() + xlab("Time") + 
      ylab("Species") + theme(aspect.ratio = 1/4, legend.position = "top", legend.title = element_blank()) +
      ggtitle("time course") + ylim(c(0,5))
    print(f)
    
  }, height = function() {
    aspect * session$clientData$output_bistable_width
  })
}

ui <- fluidPage(
  headerPanel("A simple bistable system"),
  withMathJax("$$\\frac{dX}{dt} = \\frac{X^h}{X^h + K^h} - dX$$"),
  sidebarLayout(
    sidebarPanel(
      ## generic creation of UI elements
      h4("Initial values"),
      lapply(L_y0, function(x) do.call("numericInput", x)),   # <--------
      
      h4("Parameters"),
      lapply(L_parms, function(x) do.call("numericInput", x)), # <--------
      
      width = 4
    ),
    mainPanel(
      h4("Simulation results"),
      plotOutput("bistable")
    )
  )
)

shinyApp(ui = ui, server = server)