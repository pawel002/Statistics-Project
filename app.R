library(shiny)
library(ggplot2)
library(comprehenr)
library(pracma)


ui <- fluidPage(
  
  titlePanel("Central Limit Theorem"),
  
  sidebarLayout(
    
    sidebarPanel(
      
      # wybor rozkładu
      selectInput(inputId = "data",
                  label = "Choose distribution:",
                  choices = c("Uniform", "Exponential", "Chi2", "Beta", "MyFunction")),
      
      # zadanie ilosci probek
      numericInput(inputId = "sample",
                   label = "Sample Size:",
                   value = 10000),
      
      # zadanie wielkości średniej
      numericInput(inputId = "average",
                   label = "Average Over (n):",
                   value = 2),
      
      # opis po angielsku
      strong("The red line on the upper plot describes the theoretical density function
             and, on the lower one, distribution N(0,1), to which the lower histogram
             should converge for sufficiently large n, according to CLT."),
      
      strong("To simplify the interface, and avoid calculating mean and
             covariance, the following distributions have been used: "),
      p("Uniform - U(0,1)"),
      p("Exponential - Exp(1)"),
      p("Chi2 - Chi(5)"),
      p("Beta - Beta(a=2, b=5)"),
      p("MyFunction - density function given by 0<=x<1: e^x, 1<=x<2:  -x+2.71+1,
        2<=x<=3: 5/x. This density function is then multiplied by const c=0.16, so it
        is normalized. Mean = 1.55 and variance = 0.64. Upper graph is accurate for n<=10. Generating samples for MyFunction is slow!!"),
      strong("Both histograms are normalized, so they represent actual 
             density functions with area under equal to 1. Second graph shows 
             the distribution of variables after applying the central limit theorem."),
    ),
    
    mainPanel(
      
      # obiekty przechowujące oba histogramy
      plotOutput("plot1"),
      plotOutput("plot2")
      
    )
  )
)

server <- function(input, output) {
  
  # funkcja N(0,1)
  normal01 <- function(x) {
    return(exp(-0.5*x*x)/(sqrt(2*pi)))
  }
  
  # znormalizowana funkcja
  myFunction<- function(x) {
    if(0<=x && x<1) return(0.1679089 * exp(x))
    if(1<=x && x<2) return(0.1679089 * (-x+2.71+1))
    if(2<=x && x<=3) return(0.1679089 * 5/x)
  }
  
  # zwraca nowy wektor z wartosciami myfunc(i)
  myFunctionArray<- function(x) {
    return(to_vec(for (i in x) myFunction(i)))
  }
  
  # tworzy sample o wielkosci n dla myfunc
  myFunctionSample<- function(n) {
    return(sample(x = linspace(0, 3, 100), size = n, prob = myFunctionArray(linspace(0, 3, 100)), replace = TRUE))
  }
  
  # rozkłady
  distributionPlot <- eventReactive(c(input$data, input$sample, input$average), {
    
    # jeżeli wybrany został rozkład jednostajny
    if (input$data == "Uniform") {
      
      # generujemy wektor danych do 1. histogramu - analogicznie dla kazdego rozkladu
      x = runif(input$sample * input$average, 0, 1)
      
      # generujemy wykres gestosci - analogicznie dla kazdego rozkladu
      x1 = seq(0, 1, by=1)
      y1 = to_vec(for(i in 1:2) 1)
      
      plot = data.frame(x, x1, y1)
      
      # wykładniczy
    } else if(input$data == "Exponential") {
      
      x = rexp(input$sample * input$average, 1)
      
      x1 = linspace(0, 5, 100)
      y1 = dexp(x1, 1)
      
      plot = data.frame(x, x1, y1)
      
      # chi^2
    } else if(input$data == "Chi2") {
      
      x = rchisq(input$sample * input$average, df = 5)
      
      x1<- linspace(0, 20, 100)
      y1 = dchisq(x1, df = 5)
      
      plot = data.frame(x, x1, y1)
      
      # beta
    } else if(input$data == "Beta") {
      
      x = rbeta(input$sample * input$average, 2, 5)
      
      x1<- linspace(0, 1, 100)
      y1 = dbeta(x1, 2, 5)
      
      plot = data.frame(x, x1, y1)
      
      # moja funkcja zdefiniowana w MyFunction
    } else if (input$data == "MyFunction") {
      
      x <- myFunctionSample(input$sample * input$average)
      
      x1 <- linspace(0, 3, 100)
      y1 <- myFunctionArray(x1)
      
      plot = data.frame(x, x1, y1)
    }
    return(plot)
  })
  
  # rozkłady średnich
  averageDistributionPlot <- eventReactive(c(input$data, input$sample, input$average), {
    
    # jednostajny
    if (input$data == "Uniform") {
      
      # wygenerowanie danych zgodnie z CLT - analogicznie dla kadego rozkładu
      x = to_vec(for(i in 1:input$sample) (mean(runif(input$average, 0, 1))- 0.5)*sqrt(input$average)/(1/3))
      
      # wygenerowanie wykresu N(0,1)
      x1 = linspace(-3, 3, 100)
      y1 = to_vec(for (i in x1) normal01(i));
      
      plot = data.frame(x, x1, y1)
      
      # wykladnicza
    } else if(input$data == "Exponential") {
      
      x = to_vec(for(i in 1:input$sample) (mean(rexp(input$average, 1)) -1)*sqrt(input$average))
      
      x1 = linspace(-3, 3, 100)
      y1 = to_vec(for (i in x1) normal01(i));
      
      plot = data.frame(x, x1, y1)
      
      # chi^2
    } else if(input$data == "Chi2") {
      
      x = to_vec(for(i in 1:input$sample) (mean(rchisq(input$average, df=5)) - 5)*sqrt(input$average)/sqrt(10))
      
      x1 = linspace(-3, 3, 100)
      y1 = to_vec(for (i in x1) normal01(i));
      
      plot = data.frame(x, x1, y1)
      
      # beta
    } else if(input$data == "Beta") {
      
      x = to_vec(for(i in 1:input$sample) (mean(rbeta(input$average, 2, 5)) - 2/7)*sqrt(input$average)/0.15)
      
      x1 = linspace(-3, 3, 100)
      y1 = to_vec(for (i in x1) normal01(i));
      
      plot = data.frame(x, x1, y1)
      
      # moja funkcja
    } else if (input$data == "MyFunction") {
    
    x = to_vec(for(i in 1:input$sample) (mean(myFunctionSample(input$average)) - 1.5500789)*sqrt(input$average)/0.8003721028)
    
    x1 = linspace(-3, 3, 100)
    y1 = to_vec(for (i in x1) normal01(i));
    
    plot = data.frame(x, x1, y1)
  }
    return(plot)
  })
  
  output$plot1 <- renderPlot({
    
    # tworznie histogramu + wykresu
    ggplot(data = distributionPlot(), aes(x)) + 
      geom_histogram(aes(y=..density..), color="black", fill="grey40", bins=10*input$average, boundary = 0, closed = "left") + 
      geom_line(aes(x1, y1), color="red", size=2) +
      ggtitle(paste("Probability density function of", input$data))

  })
  
  output$plot2 <- renderPlot({
    
    # tworznie histogramu + wykresu
    ggplot(data = averageDistributionPlot(), aes(x)) + 
      geom_histogram(aes(y=..density..), color="black", fill="grey40", bins=10*input$average, boundary = 0, closed = "left") + 
      geom_line(aes(x1, y1), color="red", size=2)+
      ggtitle(paste("Distribution of average values of", input$data, "over", input$average, "samples."))
  })
  
}

shinyApp(ui, server)
