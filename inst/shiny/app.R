# Load libraries needed
library(shiny)
#library(ggplot2)
library(purrr)
library(rootSolve)
library(scatterplot3d)
library(dplyr)
library(readxl)
library(plotly)
library(shinyFiles)
library(DT)

quasar = read_excel("QUASAR.xls")
model = lm(RFEWIDTH~AB1450+LINEFLUX,data=quasar)
coeff = coef(model)

# Define UI for application that draws a histogram
ui <- fluidPage(

    # Application title
    titlePanel("Quasar Dataset: Multile Linear Regression"),
    # Sidebar with a slider input for number of bins



    sidebarLayout(
        sidebarPanel(
            sliderInput("beta0",
                        "Choose beta0 to fit regression plane to data",
                        min = round(-2*abs(coeff[1]),2),
                        max = round(2*abs(coeff[1]),2),
                        value = 1156.6,
                        step=0.1),

            sliderInput("beta1",
                        "Choose beta1 to fit regression plane to data",
                        min = round(-2*abs(coeff[2]),2),
                        max = round(2*abs(coeff[2]),2),
                        value = -25.5,
                        step=0.1),

            sliderInput("beta2",
                        "Choose beta2 to fit regression plane to data",
                        min = round(-2*abs(coeff[3]),2),
                        max = round(2*abs(coeff[3]),2),
                        value = 32,
                        step=0.1),
            fileInput(inputId = "filedata",
                      label = "Upload data. Choose xls file",
                      accept = c(".xls")),

        ),

        # Show a plot of the generated distribution
        mainPanel(

            plotlyOutput("regressPlot"),
            plotlyOutput("MLR"),
            plotOutput("qqplot"),
            plotOutput("resvsfit"),
            tableOutput("tab"),
            DTOutput(outputId = "table"),
            tableOutput("betas")



        )
    )
)

# Define server logic required to draw a histogram
server <- function(input, output,session) {
    data <- reactive({
        req(input$filedata)
        read_excel(input$filedata$datapath)
    })

    output$table <- renderDT(data())

    output$regressPlot <- renderPlotly({
        x1 <- as.vector(quasar$AB1450)
        x2 <- as.vector(quasar$LINEFLUX)
        x3 <- rnorm(50)>0.5

        ### Calculate z on a grid of x-y values
        x1.seq <- seq(min(x1),max(x1),length.out=25)
        x2.seq <- seq(min(x2),max(x2),length.out=25)
        z <- t(outer(x1.seq, x2.seq, function(x,y) input$beta0+input$beta1*x+input$beta2*y))

        #### Draw the plane with "plot_ly" and add points with "add_trace"
        cols <- c("#f5cb11", "#b31d83")
        cols <- cols[x3+1]
        library(plotly)
        plot_ly(x=~x1.seq, y=~x2.seq, z=~z,
                colors = c("#f5cb11", "#b31d83"),type="surface") %>%
            add_trace(data=df, x=as.vector(quasar$AB1450), y=as.vector(quasar$LINEFLUX), z=as.vector(quasar$RFEWIDTH), mode="markers", type="scatter3d",
                      marker = list(color=cols, opacity=0.7, symbol=105)) %>%
            layout(scene = list(aspectmode = "manual", aspectratio = list(x=1, y=1, z=1),
                xaxis = list(title = "AB1450", range = c(min(x1),max(x1))),
                yaxis = list(title = "LINEFLUX", range = c(min(x2),max(x2))),
                zaxis = list(title = "RFEWIDTH", range = pretty(z)[c(1,8)])))

    })

    observe({
        val <- input$beta0
        updateSliderInput(session, "beta0", value = val, step = .1)

    })
    observe({
        val <- input$beta1
        updateSliderInput(session, "beta1", value = val, step = .1)

    })
    observe({
        val <- input$beta2
        updateSliderInput(session, "beta2", value = val, step = .1)

    })

    output$MLR <- renderPlotly({
        x1 <- as.vector(quasar$AB1450)
        x2 <- as.vector(quasar$LINEFLUX)
        x3 <- rnorm(50)>0.5

        ### Calculate z on a grid of x-y values
        x1.seq <- seq(min(x1),max(x1),length.out=25)
        x2.seq <- seq(min(x2),max(x2),length.out=25)
        z <- t(outer(x1.seq, x2.seq, function(x,y) coeff[1]+coeff[2]*x+coeff[3]*y))

        #### Draw the plane with "plot_ly" and add points with "add_trace"
        cols <- c("#f5cb11", "#b31d83")
        cols <- cols[x3+1]
        library(plotly)
        plot_ly(x=~x1.seq, y=~x2.seq, z=~z,
                colors = c("#f5cb11", "#b31d83"),type="surface") %>%
            add_trace(data=df, x=as.vector(quasar$AB1450), y=as.vector(quasar$LINEFLUX), z=as.vector(quasar$RFEWIDTH), mode="markers", type="scatter3d",
                      marker = list(color=cols, opacity=0.7, symbol=105)) %>%
            layout(scene = list(aspectmode = "manual", aspectratio = list(x=1, y=1, z=1),
                xaxis = list(title = "AB1450", range = c(min(x1),max(x1))),
                yaxis = list(title = "LINEFLUX", range = c(min(x2),max(x2))),
                zaxis = list(title = "RFEWIDTH", range = pretty(z)[c(1,8)])))

    })

    # output$coeff <- renderDT(
    #     data <- reactive(data())
    # )

    output$betas <- renderTable({
        data <- data()
        Y = as.matrix(data$Y)
        X = as.matrix(data[,2:ncol(data)])
        betas = solve(t(X)%*%X)%*%t(X)%*%Y

    })

    output$qqplot <- renderPlot({
        data <- data()
        Y = as.matrix(data$Y)
        X = as.matrix(data[,2:ncol(data)])
        betas = solve(t(X)%*%X)%*%t(X)%*%Y
        errors = Y-X%*%betas
        qqnorm(errors, pch = 1, frame = FALSE)
        qqline(errors, col = "steelblue", lwd = 2)
    })

    output$resvsfit <- renderPlot({
        data <- data()
        Y = as.matrix(data[,1])
        X = as.matrix(data[,2:ncol(data)])
        betas = solve(t(X)%*%X)%*%t(X)%*%Y
        fitted = X%*%betas
        residuals = Y-fitted
        plot(fitted,residuals, main="Residuals vs Fitted",
             xlab="Fitted ", ylab="Residuals ", ylim = c(-abs(min(residuals))-10,abs(max(residuals)+10)), pch=19)
        abline(h=c(0), col=c("red"), lty=c(1,2), lwd=c(1, 3))
    })


}

# Run the application
shinyApp(ui = ui, server = server)
