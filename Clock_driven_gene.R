library("deSolve")
library(tidyr)
library(dplyr)
library(magrittr)
library(ggplot2)
library(gridExtra)

ccg <- function(t, y, p) {
  with(as.list(c(y, p)), {
    dX <- a + A*cos(2*pi*t/24) - d*X
    list(X=dX)
  })
}

## prepare data structures to create UI programmatically
y0    <- c(X=1)
parms <- c(a=2, A=0, d=0.5)
aspect <- 0.8

makelist <- function(i, obj, min=NA, max=NA, step=NA, width=NULL) {
  list(inputId=names(obj[i]), label=names(obj[i]),
       value=unname(obj[i]), min=min, max=max, step=step,
       width=width)
}

## two lists of lists
L_parms <- lapply(1:length(parms), makelist, obj=parms, min=0, max=2, step=0.05, width=75)
L_y0 <- lapply(1:length(y0), makelist, obj=y0, min=0, max=3, step=0.05, width=75)

server <- function(input, output, session) {
  output$ccg <- renderPlot({
    L_input <- reactiveValuesToList(input) # to enable width
    y0    <- with(L_input, c(X=X))
    parms <- with(L_input, c(a=a, A=A, d=d))
    times <- seq(0, 150, .1)
    out <- ode(y0, times, ccg, parms)
    df1 <- data.frame(out) %>% 
            mutate(input = with(L_input, a + A*cos(2*pi*time/24))) %>%
            gather(var, value,-time) %>%
            group_by(var) %>%
            mutate(norm_value = value/mean(value))
            
    f1 <- ggplot(df1, aes(x=time, y=value, color=var)) + geom_line(size=1) + theme_bw() + xlab("Time") + 
      ylab("Species") + theme(aspect.ratio = 1/4, legend.position = "top", legend.title = element_blank()) +
      ggtitle("time course")
    
    f2 <- ggplot(df1, aes(x=time, y=norm_value, color=var)) + geom_line(size=1) + theme_bw() + xlab("Time") + 
      ylab("Species") + theme(aspect.ratio = 1/4, legend.position = "top", legend.title = element_blank()) +
      ggtitle("normalized time course") + ylim(c(0,2))
    
    grid.arrange(f1, f2, ncol=1)

      }, height = function() {
    aspect * session$clientData$output_ccg_width
  })
}

ui <- fluidPage(
  headerPanel("Output of a clock-driven gene"),
  withMathJax("$$\\frac{dX}{dt} = a + A \\cos \\left(\\frac{2\\pi t}{24}\\right) - dX$$"),
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
      plotOutput("ccg")
    )
  )
)

shinyApp(ui = ui, server = server)