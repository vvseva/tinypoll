library(shiny)
library(bslib)
library(dplyr)
library(plotly)

# Initialize empty data frame to store votes
votes_data <- reactiveVal(data.frame(
  option = character(),
  timestamp = as.POSIXct(character())
))

# Admin password (in real applications, use proper authentication)
ADMIN_PASSWORD <- "admin123"

ui <- page_navbar(
  title = "Simple Poll System",
  
  # Voting Page
  nav_panel(
    title = "Vote",
    layout_columns(
      col_widths = c(8),
      value_box(
        title = "Make Your Choice",
        value = "",
        showcase = bsicons::bs_icon("check2-square"),
        p("Please select one option:"),
        radioButtons("vote_option", NULL,
                     choices = c("Stay at the Deans Conference room" = "A",
                                 "Move to a different location" = "B"),
                     selected = character(0)),
        actionButton("submit_vote", "Submit Vote",
                     class = "btn-primary")
      )
    )
  ),
  
  # Admin Page
  nav_panel(
    title = "Admin",
    layout_columns(
      col_widths = c(4, 8),
      card(
        card_header("Admin Authentication"),
        passwordInput("admin_password", "Enter Admin Password:"),
        actionButton("check_password", "Login",
                     class = "btn-primary")
      ),
      card(
        card_header("Poll Results"),
        uiOutput("results_ui")
      )
    ),
    # Bar chart card
    card(
      card_header("Results Visualization"),
      plotlyOutput("results_plot")
    ),
    # Add bulk votes card
    card(
      card_header("Add Bulk Votes"),
      selectInput("bulk_option", "Select Option:",
                  choices = c("Stay at the Deans Conference room" = "A",
                              "Move to a different location" = "B")),
      actionButton("add_bulk_votes", "Add 30 Votes",
                   class = "btn-warning")
    ),
    card(
      card_header("Delete Votes"),
      actionButton("delete_all", "Delete All Records",
                   class = "btn-warning")
    )
  )
)

server <- function(input, output, session) {
  # Authentication status
  is_authenticated <- reactiveVal(FALSE)
  
  # Handle vote submission
  observeEvent(input$submit_vote, {
    req(input$vote_option)
    
    # Add new vote to the data
    new_vote <- data.frame(
      option = input$vote_option,
      timestamp = Sys.time()
    )
    
    votes_data(rbind(votes_data(), new_vote))
    
    # Show success message
    showModal(modalDialog(
      title = "Success",
      "Your vote has been recorded!",
      easyClose = TRUE
    ))
    
    # Reset radio buttons
    updateRadioButtons(session, "vote_option", selected = character(0))
  })
  
  # Handle bulk votes
  observeEvent(input$add_bulk_votes, {
    req(is_authenticated())
    
    # Create 30 new votes
    bulk_votes <- data.frame(
      option = rep(input$bulk_option, 30),
      timestamp = Sys.time()
    )
    
    votes_data(rbind(votes_data(), bulk_votes))
    
    showModal(modalDialog(
      title = "Success",
      "30 votes have been added!",
      easyClose = TRUE
    ))
  })
  
  
  observeEvent(input$delete_all, {
    req(is_authenticated())
    ## drop all records
    votes_data(data.frame(option = character(), timestamp = as.POSIXct(character())))
    
    
    })
  
  # Handle admin authentication
  observeEvent(input$check_password, {
    if (input$admin_password == ADMIN_PASSWORD) {
      is_authenticated(TRUE)
    } else {
      showModal(modalDialog(
        title = "Error",
        "Incorrect password!",
        easyClose = TRUE
      ))
    }
  })
  
  # Render results UI
  output$results_ui <- renderUI({
    if (!is_authenticated()) {
      return(p("Please log in to view results"))
    }
    
    # Calculate results
    results <- votes_data() %>%
      count(option) %>%
      mutate(percentage = n / sum(n) * 100)
    
    tagList(
      layout_columns(
        value_box(
          title = "Total Votes",
          value = nrow(votes_data()),
          showcase = bsicons::bs_icon("bar-chart")
        ),
        value_box(
          title = "Stay at Deans Conference",
          value = sprintf("%d (%.1f%%)",
                          sum(results$n[results$option == "A"]),
                          results$percentage[results$option == "A"]),
          showcase = bsicons::bs_icon("trophy")
        ),
        value_box(
          title = "Move to Different Location",
          value = sprintf("%d (%.1f%%)",
                          sum(results$n[results$option == "B"]),
                          results$percentage[results$option == "B"]),
          showcase = bsicons::bs_icon("trophy")
        )
      )
    )
  })
  
  # Render interactive bar chart
  output$results_plot <- renderPlotly({
    req(is_authenticated())
    
    # Calculate results for plotting
    results <- votes_data() %>%
      count(option) %>%
      mutate(
        percentage = n / sum(n) * 100,
        option_label = ifelse(option == "A", 
                              "Stay at Deans Conference",
                              "Move to Different Location")
      )
    
    # Create interactive bar chart
    plot_ly(results, 
            x = ~option_label, 
            y = ~n,
            type = "bar",
            text = ~sprintf("%.1f%%", percentage),
            textposition = "auto",
            marker = list(color = c("rgba(71, 137, 191, 0.8)", 
                                    "rgba(219, 112, 147, 0.8)"))) %>%
      layout(
        title = "Vote Distribution",
        xaxis = list(title = "Options"),
        yaxis = list(title = "Number of Votes"),
        showlegend = FALSE
      )
  })
}

shinyApp(ui, server)
