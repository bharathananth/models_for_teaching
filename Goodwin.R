library("deSolve")
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(gridExtra)

goodwin <- function(t, y, p) {
  with(as.list(c(y, p)), {
    dX <- p_1*K^h/(K^h + Z^h) - d_1*X
    dY <- p_2*X - d_2*Y
    dZ <- p_3*Y - d_3*Z
    list(c(X=dX, Y=dY, Z= dZ))
  })
}

## prepare data structures to create UI programmatically
y0    <- c(X=1, Y=0, Z=0)
parms <- c(p_1=1, p_2=1, p_3=1, d_1=0.25, d_2=0.25, d_3=0.25,K=2, h=4)
aspect <- 0.8

makelist <- function(i, obj, min=NA, max=NA, step=NA, width=NULL) {
  list(inputId=names(obj[i]), label=names(obj[i]),
       value=unname(obj[i]), min=min, max=max, step=step,
       width=width)
}

## two lists of lists
L_parms <- lapply(1:length(parms), makelist, obj=parms, min=0, max=10, step=0.2, width=200)
L_y0 <- lapply(1:length(y0), makelist, obj=y0, min=0, max=5, step=0.2, width=200)

server <- function(input, output, session) {
  output$goodwin <- renderPlot({
    L_input <- reactiveValuesToList(input) # to enable width
    y0    <- with(L_input, c(X=X, Y=Y, Z=Z))
    parms <- with(L_input, c(p_1=p_1, p_2=p_2, p_3=p_3, d_1=d_1, d_2=d_2, d_3=d_3, K= K, h =h))
    times <- seq(0, 150, .1)
    out <- ode(y0, times, goodwin, parms)
    df1 <- data.frame(out) %>% gather(var, value,-time)
    par(mfrow=c(2, 1))
    f1 <- ggplot(df1, aes(x=time, y=value, color=var)) + geom_line(size=1) + theme_bw() + xlab("Time") + 
      ylab("Species") + theme(aspect.ratio = 1/4, legend.position = "top", legend.title = element_blank()) +
      ggtitle("time course")
    
    df2 <- data.frame(out, stringsAsFactors = FALSE) %>% arrange(time)
    f2 <- ggplot(df2, aes(x=X,y=Y)) + geom_path() + theme_bw() + xlab("X") + ylab("Y") + theme(aspect.ratio = 1/4) +
      ggtitle("phase portrait")
    grid.arrange(f1, f2, ncol=1)
  }, height = function() {
    aspect * session$clientData$output_goodwin_width
  })
}

ui <- fluidPage(
  headerPanel("Goodwin oscillator"),
  withMathJax(),
  helpText("\\begin{eqnarray} \\frac{dX}{dt} =& p_1 \\frac{K^h}{K^h + Z^h} - d_1  X \\\\ \\frac{dY}{dt} =& p_2 X - d_2 Y \\\\ \\frac{dZ}{dt} =& p_3 Y - d_3 Z \\end{eqnarray}"),
  sidebarLayout(
    sidebarPanel(
      ## generic creation of UI elements
      h4("Initial values"),
      withMathJax(),
      lapply(L_y0, function(x) do.call("sliderInput", x)),   # <--------
      
      h4("Parameters"),
      lapply(L_parms, function(x) do.call("sliderInput", x)), # <--------
      
      width = 3
    ),
    mainPanel(
      h4("Simulation results"),
      plotOutput("goodwin")
    )
  )
)

shinyApp(ui = ui, server = server)