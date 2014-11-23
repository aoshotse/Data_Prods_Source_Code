library('lattice')
library('ggplot2')
data(mtcars)
data(longley)
data(swiss)
data(diamonds)
dn <- as.data.frame(data.matrix(diamonds))
data(barley)
bl <- as.data.frame(data.matrix(barley))
DF <- mtcars
DF1 <- longley
DF2 <- swiss
DF3 <- diamonds
DF3a <- dn
DF4 <- barley
DF4a <- bl

library(shiny)
shinyServer(
  function(input, output) {
    v <- reactive({c('am',  input$id1)})
    b <- reactive({c('mpg', 'am',  input$id1)})
    mtcars <- reactive({subset(DF, 1==1, select=b())})
    fit1 <- function(id1) summary(lm(mpg ~., data = mtcars()))
    vif <- function(id1) diag( solve( cor( mtcars()[ , v() ] ) ) )   #####
    fit2 <- function(id1) lm(mpg ~., data = mtcars())
    output$oid3 <- renderPrint({c('am', input$id1)}) #
    output$oid1 <- renderPrint({fit1(input$id1)})
    output$newbar <- renderPlot (
{
  Rsquared_value <- fit1()$r.squared*100
  pv <- as.factor(' ')
  barchart(Rsquared_value~pv, ylim=c(0,100),xlab='Rsquared_value',ylab='Percent of Variation Accounted for by Model')
}
    )
output$oid2 <- renderPrint({vif()})
output$newfrp <- renderPlot (
{
  par(mfrow=c(2,2))
  plot(fit2())
}
)     ## output mpg   



v1 <- reactive({c('Population',  input$id11)})
b1 <- reactive({c('Unemployed', 'Population',  input$id11)})
longley <- reactive({subset(DF1, 1==1, select=b1())})
fit11 <- function(id11) summary(lm( Unemployed~., data = longley()))
vif1 <- function(id11) diag( solve( cor( longley()[ , v1() ] ) ) )   #####
fit12 <- function(id11) lm(Unemployed ~., data = longley())
output$oid13 <- renderPrint({c('Population', input$id11)}) #
output$oid11 <- renderPrint({fit11(input$id11)})
output$newbar1 <- renderPlot (
{
  Rsquared_value1 <- fit11()$r.squared*100
  pv1 <- as.factor(' ')
  barchart(Rsquared_value1~pv1, ylim=c(0,100),xlab='Rsquared_value',ylab='Percent of Variation Accounted for by Model')
}
)
output$oid12 <- renderPrint({vif1()})
output$newfrp1 <- renderPlot (
{
  par(mfrow=c(2,2))
  plot(fit12())
}
)       ## output unemp


v2 <- reactive({c('Infant.Mortality',  input$id21)})
b2 <- reactive({c('Fertility', 'Infant.Mortality',  input$id21)})
swiss <- reactive({subset(DF2, 1==1, select=b2())})
fit21 <- function(id21) summary(lm( Fertility~., data = swiss()))
vif2 <- function(id21) diag( solve( cor( swiss()[ , v2() ] ) ) )   #####
fit22 <- function(id21) lm(Fertility ~., data = swiss())
output$oid23 <- renderPrint({c('Infant.Mortality', input$id21)}) #
output$oid21 <- renderPrint({fit21(input$id21)})
output$newbar2 <- renderPlot (
{
  Rsquared_value2 <- fit21()$r.squared*100
  pv2 <- as.factor(' ')
  barchart(Rsquared_value2~pv2, ylim=c(0,100),xlab='Rsquared_value',ylab='Percent of Variation Accounted for by Model')
}
)
output$oid22 <- renderPrint({vif2()})
output$newfrp2 <- renderPlot (
{
  par(mfrow=c(2,2))
  plot(fit22())
}
)       ## output fertility


v3 <- reactive({c('cut',  input$id31)})
b3 <- reactive({c('price', 'cut',  input$id31)})
diamonds <- reactive({subset(DF3, 1==1, select=b3())})
dn <- reactive({subset(DF3a, 1==1, select=b3())})
fit31 <- function(id31) summary(lm( price~., data = diamonds()))
vif3 <- function(id31) diag( solve( cor( dn()[ , v3() ] ) ) )   #####
fit32 <- function(id31) lm(price ~., data = diamonds())
output$oid33 <- renderPrint({c('cut', input$id31)}) #
output$oid31 <- renderPrint({fit31(input$id21)})
output$newbar3 <- renderPlot (
{
  Rsquared_value3 <- fit31()$r.squared*100
  pv3 <- as.factor(' ')
  barchart(Rsquared_value3~pv3, ylim=c(0,100),xlab='Rsquared_value',ylab='Percent of Variation Accounted for by Model')
}
)
output$oid32 <- renderPrint({vif3()})
output$newfrp3 <- renderPlot (
{
  par(mfrow=c(2,2))
  plot(fit32())
}
)       ## output price


v4 <- reactive({c('year',  input$id41)})
b4 <- reactive({c('yield', 'year',  input$id41)})
barley <- reactive({subset(DF4, 1==1, select=b4())})
bl <- reactive({subset(DF4a, 1==1, select=b4())})
fit41 <- function(id41) summary(lm( yield~., data = barley()))
vif4 <- function(id41) diag( solve( cor( bl()[ , v4() ] ) ) )   #####
fit42 <- function(id41) lm(yield ~., data = barley())
output$oid43 <- renderPrint({c('year', input$id41)}) #
output$oid41 <- renderPrint({fit41(input$id41)})
output$newbar4 <- renderPlot (
{
  Rsquared_value4 <- fit41()$r.squared*100
  pv4 <- as.factor(' ')
  barchart(Rsquared_value4~pv4, ylim=c(0,100),xlab='Rsquared_value',ylab='Percent of Variation Accounted for by Model')
}
)
output$oid42 <- renderPrint({vif4()})
output$newfrp4 <- renderPlot (
{
  par(mfrow=c(2,2))
  plot(fit42())
}
)       ## output barley


   
}
)