library("deSolve")
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(gridExtra)

selkov <- function(t, y, p) {
  with(as.list(c(y, p)), {
    dX <- alpha - beta*X*Y^2
    dY <- beta*X*Y^2-gamma*Y
    list(c(X=dX, Y=dY))
  })
}

## prepare data structures to create UI programmatically
y0    <- c(X=1.01, Y=0.99)
parms <- c(alpha=1,beta=0.9,gamma=0.9)
aspect <- 0.8

makelist <- function(i, obj, min=NA, max=NA, step=NA, width=NULL) {
  list(inputId=names(obj[i]), label=names(obj[i]),
       value=unname(obj[i]), min=min, max=max, step=step,
       width=width)
}

## two lists of lists
L_parms <- lapply(1:length(parms), makelist, obj=parms, min=0, max=2, step=0.05, width=200)
L_y0 <- lapply(1:length(y0), makelist, obj=y0, min=0, max=4, step=0.05, width=200)

server <- function(input, output, session) {
  output$selkov <- renderPlot({
    L_input <- reactiveValuesToList(input) # to enable width
    y0    <- with(L_input, c(X=X, Y=Y))
    parms <- with(L_input, c(alpha=alpha,beta=beta,gamma=gamma))
    times <- seq(0, 150, .1)
    out <- ode(y0, times, selkov, parms)
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
    aspect * session$clientData$output_selkov_width
  })
}

ui <- fluidPage(
  headerPanel("Selkov model of glycolysis"),
  withMathJax("\\begin{eqnarray} \\frac{dX}{dt }&= \\alpha - \\beta  X  Y^2 \\\\ \\frac{dY}{dt} &= \\beta X Y^2 - \\gamma Y \\end{eqnarray}"),
  sidebarLayout(
    sidebarPanel(
      ## generic creation of UI elements
      h4("Initial values"),
      lapply(L_y0, function(x) do.call("sliderInput", x)),   # <--------
      
      h4("Parameters"),
      lapply(L_parms, function(x) do.call("sliderInput", x)), # <--------
      
      width = 4
    ),
    mainPanel(
      h4("Simulation results"),
      plotOutput("selkov")
    )
  )
)

shinyApp(ui = ui, server = server)