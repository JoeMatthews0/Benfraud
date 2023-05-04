library(shiny); library(shinythemes); library(ggplot2); library(DT); library(bslib); library(gridExtra)

ui <- fluidPage(theme = bs_theme(
  bg = "#0b3d91", fg = "white", primary = "#FCC780",
  base_font = font_google("Space Mono"),
  code_font = font_google("Space Mono")
              ),
    titlePanel("Ben: Fraud Detector"),
    img(src='NCLLogo.jpeg', 
                     width = 200,
                     align = "right"),

    h3("Enter your sample of data (values between £1000 and £10000) separated by commas"),
    textInput(inputId = 'sample1',
              label = NULL,
              value = "1000, 2000, 3000"),
    h3("Our sample so far:"),
    verbatimTextOutput("sampleVector"),
    plotOutput("hist"),
    htmlOutput("summaryText"),
    h3("Ready to find out if you got away with it?"),
    selectInput("reveal1", 
                label = NULL,
                choices = c("Yes",
                            "No"),
                selected = "No"),
    conditionalPanel(condition = "input.reveal1 == 'Yes'",
                    htmlOutput("benfordP1"),
                    h3("See Ben's working?"),
                    selectInput("reveal2", 
                                label = NULL,
                                choices = c("Yes", "No"),
                                selected = "No"),
                    conditionalPanel(condition = "input.reveal2 == 'Yes'",
                                    h3("Ben looked at the number of times each first digit in the sample occurred
                                        and compared it with what we'd expect to see from Benford's Law."),
                                    h3("It then used a chi-squared hypothesis test to see how consistent the first digit frequency
                                        was with Benford's Law."),
                                    dataTableOutput("resultsTable"),
                                    htmlOutput("benfordChi"),
                                    plotOutput("propPlot1"),
                                    h3("Play on easy mode?"),
                                    selectInput("reveal3", 
                                                label = NULL,
                                                choices = c("Yes", "No"),
                                                selected = "No"),
                                    conditionalPanel(condition = "input.reveal3 == 'Yes'",
                                                    h3("Enter your sample of data (numbers >= 1) separated by commas"),
                                                    textInput(inputId = 'sample2', 
                                                              label = NULL, 
                                                              value = "1, 10, 100"),
                                                    plotOutput("propPlot2"),
                                                    h3("See the results?"),
                                                    selectInput("reveal4",
                                                                label = NULL,
                                                                choices = c("Yes", "No"),
                                                                selected = "No"),
                                                    conditionalPanel(condition = "input.reveal4 == 'Yes'",
                                                                    htmlOutput("benfordP2")
                                                                    )
                                                    )
                                    )
                    )
)

server <- function(input, output) {
    
    benf.prop <- log10(1 + (1 / (1 : 9)))
    
    sample1 <- reactive({
      as.numeric(unlist(strsplit(input$sample1,",")))
    })
    
    leadFigs1 <- reactive({
      apply(sapply(1 : 9, function(y){
        floor(sample1() / (10^floor(log10(sample1())))) == y}
        ), 2, sum)
    })
    
    output$sampleVector = renderText({
      sample1()
    })
    
    output$hist = renderPlot({
      df = data.frame(obs = sample1(),
                      claim = 1 : length(sample1()))
      g1 <- ggplot(data = df, aes(x = obs)) + geom_histogram(fill = "blue") + 
        xlab("Sample") +
        ylab("Frequency") +
        scale_x_log10()
      g2 <- ggplot(data = df, aes(y = obs)) +
        geom_boxplot(fill = "blue") +
        ylab("Claim") +
        theme(axis.title.x=element_blank(),
              axis.text.x=element_blank(),
              axis.ticks.x=element_blank()) +
              scale_y_log10()
      g3 <- ggplot(data = df, aes(x = claim, y = obs)) +
        geom_line(col = "blue") +
        geom_point(col = "blue") +
        scale_x_discrete(limits = factor(1 : length(sample1())), labels = 1 : length(sample1())) +
        xlab("Claim") +
        ylab("Sample") +
        scale_y_log10()
      grid.arrange(g1, g2, g3, nrow = 1)
    })
    
    output$summaryText <- renderText({
      paste("<span style=\"font-size:24px\">
            Number of claims: ", 
            length(sample1()), "<br>Total amount claimed: £", 
            round(sum(sample1()), 2), "<br>Smallest claim: £", 
            round(min(sample1()), 2), "<br>Largest claim: £", 
            round(max(sample1()), 2), "<br>Average claim: £", 
            round(mean(sample1()), 2), "</span>"
            )
    })
    
    output$resultsTable = renderDataTable({
      expected <- length(sample1()) * benf.prop
      difference <- leadFigs1() - expected
      data.frame(Digit = 1 : 9,
                 Observed = leadFigs1(),
                 Expected = round(expected, 2),
                 Difference = round(difference, 2),
                 Diff.Sq = round(difference ^ 2, 2),
                 Diff.Sq.Div.E = round((difference ^ 2) / expected, 2)
      )
    })
    
    output$benfordP1 = renderText({
      p = chisq.test(leadFigs1(), p = benf.prop)$p.value
      
      if(p < 0.05){
        paste("<span style=\"color:#F62217;font-size:36px\">FRAUD DETECTED! There is about a ", 
              round(p * 100, 2), 
              "% chance (or roughly 1 in ",
              round(1 / p, 0),
              ") these entered numbers were genuine. Please go to jail.</span>")
      } else{
        paste("<span style=\"color:#52D017;font-size:36px\">No fraud detected! There is about a ", 
              round(p * 100, 2), 
              "% chance (or roughly 1 in ",
              round(1 / p, 0),
              ") these entered numbers were genuine.</span>")
      }
    })
    
    output$benfordChi = renderText({
      chi = chisq.test(leadFigs1(), p = benf.prop)$statistic
      
      paste("<span style=\"font-size:30px\">The chi-squared statistic here was approximately ",
            round(chi, 2),
            "</span>")
    })
    
    output$propPlot1 = renderPlot({
      df = data.frame(Digit = 1 : 9,
                      Proportion = c(benf.prop, leadFigs1() / sum(leadFigs1())),
                      Group = rep(c("Benford", "Sample"), each = 9)
                      )
      ggplot(data = df, aes(x = Digit, y = Proportion, col = Group)) +
        geom_point() +
        geom_line() +
        scale_x_discrete(limits = factor(1 : 9), labels = 1 : 9)
    })
    
    leadFigs2 <- reactive({
      x <- as.numeric(unlist(strsplit(input$sample2,",")))
      apply(sapply(1 : 9, function(y){floor(x / (10^floor(log10(x)))) == y}), 2, sum)
    })
    
    output$benfordP2 = renderText({
      p = chisq.test(leadFigs2(), p = benf.prop)$p.value
      if(p < 0.05){
        paste("<span style=\"color:#F62217;font-size:36px\">FRAUD DETECTED! There is about a ", 
              round(p * 100, 2), 
              "% chance (or roughly 1 in ",
              round(1 / p, 0),
              ") these entered numbers were genuine. Please go to jail.</span>")
      } else{
        paste("<span style=\"color:#52D017;font-size:36px\">No fraud detected! There is about a ", 
              round(p * 100, 2), 
              "% chance (or roughly 1 in ",
              round(1 / p, 0),
              ") these entered numbers were genuine.</span>")
      }
    })
    
    output$propPlot2 = renderPlot({
      df = data.frame(Digit = 1 : 9,
                      Proportion = c(benf.prop, leadFigs2() / sum(leadFigs2())),
                      Group = rep(c("Benford", "Sample"), each = 9)
      )
      ggplot(data = df, aes(x = Digit, y = Proportion, col = Group)) +
        geom_point() +
        geom_line() +
        scale_x_discrete(limits = factor(1 : 9), labels = 1 : 9)
    })
}

shinyApp(ui = ui, server = server)