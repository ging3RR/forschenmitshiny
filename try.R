#----------------
select.list(names(mpg),multiple=TRUE,
             title='select your variable names',
             graphics=TRUE)

select.list(names(mpg))

names(mpg)


plot(lm(model ~ cty + hwy, mpg))

plot(hwy ~ cyl, data = mpg)


y <- eventReactive(input$button,{
  mpg %>% select(input$y_var)
})
x1 <- eventReactive(input$button,{
  mpg %>% select(input$x_var1)
})


mpg_shiny <- eventReactive(input$button, {
  mpg %>% select(input$y_var, input$x_var1)
})

output$table <- renderDataTable({
  mpg_shiny()
})





data <- mpg %>% select(c(manufacturer, model, hwy, cty,year)) %>% 
  filter(manufacturer == "audi") %>% 
  filter(model == "a4") 


ggplot(data, aes(model,hwy)) +
  geom_point() +
  facet_grid(cols = vars(year))

ggplot(data, aes(model,cty)) +
  geom_point() +
  facet_grid(cols = vars(year))




