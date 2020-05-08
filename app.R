library(shiny)
library(shinydashboard)
library(ggplot2)

ui <- dashboardPage(
  dashboardHeader(title = "Hi-C visualization"),
  dashboardSidebar(sidebarMenuOutput("menu") ),
  dashboardBody( 
    tabItems(
      tabItem(tabName = "Login", helpText(h2("Login")), textInput("user","User Name:", value=""), passwordInput("passwd","Password", ""), actionButton("submit_login","Login")),
      tabItem(tabName = "Regester", helpText(h2("Registier a new user")), textInput("newuser","User Name:", value=""),textInput("email","Email:", value=""), passwordInput("newpasswd","Passward", ""),actionButton("submit_register","Submit")),
      tabItem(tabName = "Data", verbatimTextOutput("Data")),
      tabItem(tabName = "Summary",
              selectInput("variable", "Variable:",
                          list("Cylinders" = "cyl", 
                               "Transmission" = "am", 
                               "Gears" = "gear")),
              verbatimTextOutput("Summary"), 
              verbatimTextOutput("Summary2")),
      tabItem(tabName = "Plot", plotOutput("Plot")),
      tabItem(tabName = "Plot1",plotOutput("Plot1")),
      tabItem(tabName = "Logout",  actionButton("submit_logout","Logout"))
    )
  )
)


server <- function(input, output) {
  set.seed(123)
  data = diamonds[sample(1:nrow(diamonds), 10000, replace = F), ]
  USER<- reactiveValues(logged=FALSE)
  observeEvent(input$submit_login, {
    if(isolate( input$user) == "" | isolate( input$passwd) == ""){
      USER$logged=FALSE
      showModal(modalDialog(
        title = "Warning",
        "Empty user name or password!",
        easyClose = TRUE
      ))
      return(FALSE)
    }
    user.list=read.table("users.txt", header=TRUE, sep=",")
    users=as.vector(user.list$users)
    pass =as.vector(user.list$passwd)
    email=as.vector(user.list$email)
    temp=isolate( input$user)
    wh=which(users == isolate( input$user))
    if(length(wh) == 0){
      USER$logged=FALSE
      showModal(modalDialog(
        title = "Warning",
        "User is not registered!",
        easyClose = TRUE
      ))
      return(FALSE)
    }
    if(pass[wh[1]] == isolate( input$passwd) ){
      USER$logged=TRUE
      write(c(isolate( input$user), date()),"usage_log.txt", append=T )
      return(TRUE)
    }else{
      USER$logged=FALSE
      showModal(modalDialog(
        title = "Warning",
        "Wrong user name or password!",
        easyClose = TRUE
      ))
      return(FALSE)
    }  
  })
  observeEvent(input$submit_register,{
    if(isolate( input$newuser) == "" | isolate( input$newpasswd) == ""| isolate( input$email) == ""){
      USER$logged=FALSE
      showModal(modalDialog(
        title = "Warning",
        "Please fill all the fields!",
        easyClose = TRUE
      ))
      return(FALSE)
    }
    temp=data.frame(users=isolate( input$newuser), passwd =isolate( input$newpasswd), email=isolate( input$email) )
    user.list=read.table("users.txt", header=TRUE,sep=",")
    user.list=rbind(user.list, temp)
    write.table(user.list,"users.txt",row.names=F,sep=",")
    USER$logged=TRUE
    return(TRUE)
  })
  observeEvent( input$submit_logout,{
    USER$logged=FALSE
    return(FALSE)
  })
    
    
  output$menu <- renderMenu({
    if(USER$logged){
      sidebarMenu(
        menuItem("Data", tabName = "Data", icon = icon("dashboard")),
        menuItem("Summary", icon = icon("th"), tabName = "Summary",badgeColor = "green"),
        menuItem("Plot", tabName = "Plot", icon = icon("bar-chart-o")),
        menuItem("Plot1", tabName = "Plot1", icon = icon("bar-chart-o")),
        menuItem("Logout", tabName = "Logout", icon = icon("bar-chart-o"))
      )
    }else{
      sidebarMenu(
        menuItem("Login", tabName = "Login", icon = icon("dashboard")),
        menuItem("Register", tabName = "Regester", icon = icon("dashboard"), badgeLabel="new")
      )
    }
  })
  output$Data <- renderPrint({str(data)})
  output$Summary <- renderPrint({str(data)})
  output$Summary2 <- renderPrint({str(data)})
  output$Plot <- renderPlot({
    ggplot(data, aes(x = price, fill = cut)) + 
      geom_histogram(position = "fill", bins = 30) +
      ggtitle("histogram plot") +
      theme(plot.title = element_text(hjust = 0.5)) + xlab("")
  })
  output$Plot1 <- renderPlot({
    ggplot(data = data, aes(x = carat, y = price, colour = color)) +
      geom_point() +  ggtitle("scatter diagram") +
      theme(plot.title = element_text(hjust = 0.5))
  })
}
shinyApp(ui, server)