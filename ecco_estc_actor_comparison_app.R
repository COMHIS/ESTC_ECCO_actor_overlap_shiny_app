library(shiny)
library(dplyr)
library(ggplot2)
library(DT)

library(estcr)  # Load the COMHIS ESTCR package
library(eccor)  # Load the COMHIS ECCOR package

ecco_core <- load_ecco_core()  # Load ECCO core data
estc_core <- load_estc_core()  # Load ESTC core data
estc_actor_links <- load_estc_actor_links()  # Load ESTC actor links data 

# Define UI
ui <- fluidPage(
  titlePanel("Actor Name Search"),
  
  sidebarLayout(
    sidebarPanel(
      textInput("search_word", "Enter actor to search for:", value = "Aristotle"),
      actionButton("search_btn", "Search"),
      uiOutput("actorChoices"),  # Dynamic UI for checkboxes
      downloadButton("downloadTable", "Download table as CSV")  # Download button for table
    ),
    
    mainPanel(
      plotOutput("barChart"),
      plotOutput("decadeChart"),
      DTOutput("combinedTable")  # Unified table with sorting/filtering enabled
    )
  )
)

# Define server logic
server <- function(input, output, session) {
  # Reactive values to store possible actor matches and combined data
  possible_actors <- reactiveVal(NULL)
  table_data <- reactiveVal(data.frame())
  
  # Observe search button event to start a new search
  observeEvent(input$search_btn, {
    req(input$search_word)
    
    # Reset reactive values on new search
    possible_actors(NULL)
    table_data(data.frame())
    
    word <- input$search_word  # No isolate() here, ensure reactivity on new search
    
    # Initial search: get all names containing the search term
    possible_matches <- estc_actor_links %>%
      filter(grepl(word, actor_name_primary, ignore.case = TRUE)) %>%
      distinct(actor_name_primary)
    
    # Store possible matches in a reactive variable
    possible_actors(possible_matches)
    
    # Render checkboxes for search refinement
    output$actorChoices <- renderUI({
      checkboxGroupInput(
        "selected_actors", "Select Actors to Include:",
        choices = c("All Matches" = "all", "Exact Matches Only" = "exact", possible_actors()$actor_name_primary),
        selected = "all"
      )
    })
    
    # Trigger updates for charts and table after the search button is clicked
    updateTableAndCharts(word, input$selected_actors)
  })
  
  # Observe checkbox selections and refine the results accordingly
  observeEvent(input$selected_actors, {
    req(input$search_word)
    req(possible_actors())
    
    word <- input$search_word  # No isolate() here, we need it to react to search term
    selected_actors <- input$selected_actors
    
    # Trigger updates for charts and table whenever the checkboxes are changed
    updateTableAndCharts(word, selected_actors)
  })
  
  # Helper function to update table and charts based on search term and selected actors
  updateTableAndCharts <- function(word, selected_actors) {
    # Logic for filtering based on checkbox selection
    if ("exact" %in% selected_actors) {
      # Exact matches only: Filter for exact matches where actor name exactly matches the search term
      exact_matches <- possible_actors() %>%
        filter(tolower(actor_name_primary) == tolower(word)) %>%
        distinct(actor_name_primary)
    } else if ("all" %in% selected_actors) {
      # All matches: Return all actor names containing the search term
      exact_matches <- possible_actors()
    } else {
      # If specific actors are selected, match them directly
      exact_matches <- possible_actors() %>%
        filter(actor_name_primary %in% selected_actors) %>%
        distinct(actor_name_primary)
    }
    
    # Use the count_actor_word_with_estc() function to populate 'counts'
    counts <- count_actor_word_with_estc(estc_actor_links, ecco_core, exact_matches$actor_name_primary)
    
    # Join estc_actor_links to ensure actor_name_primary is present
    combined_data <- counts$matched_ids %>%
      left_join(estc_core, by = "estc_id") %>%
      left_join(ecco_core %>% select(estc_id, ecco_id), by = "estc_id") %>%
      left_join(estc_actor_links %>%
                  filter(tolower(actor_name_primary) %in% tolower(exact_matches$actor_name_primary)) %>%
                  select(estc_id, actor_name_primary),  # Only select necessary columns
                by = "estc_id")
    
    # Filter out rows with NA actor_name_primary
    if ("actor_name_primary" %in% colnames(combined_data)) {
      combined_data <- combined_data %>%
        filter(!is.na(actor_name_primary))  # Only filter if column exists
    }
    
    combined_data <- combined_data %>%
      select(estc_id, ecco_id, short_title, actor_name_primary, publication_year) %>%
      mutate(source_type = "In ESTC and ECCO") %>%
      bind_rows(
        counts$non_matched_ids %>%
          left_join(estc_core, by = "estc_id") %>%
          left_join(ecco_core %>% select(estc_id, ecco_id), by = "estc_id") %>%
          left_join(estc_actor_links %>% 
                      filter(tolower(actor_name_primary) %in% tolower(exact_matches$actor_name_primary)) %>%
                      select(estc_id, actor_name_primary),  # Only select necessary columns
                    by = "estc_id") %>%
          filter(!is.na(actor_name_primary)) %>%
          select(estc_id, ecco_id, short_title, actor_name_primary, publication_year) %>%
          mutate(source_type = "In ESTC")
      )
    
    # Store the combined data in the reactive variable for CSV download
    table_data(combined_data)
    
    # Render the combined table with sorting and filtering
    output$combinedTable <- renderDT({
      datatable(combined_data, 
                options = list(pageLength = 50, 
                               autoWidth = TRUE, 
                               dom = 'lfrtip'),  # Enable filtering (l), search (f), sorting (r), paging (p)
                rownames = FALSE) %>%
        formatStyle(
          'source_type',
          backgroundColor = styleEqual(
            c("In ESTC", "In ESTC and ECCO"),
            c("#d95f02", "#1b9e77")  # Colors: Orange for ESTC only, Green for both ESTC and ECCO
          )
        )
    })
    
    # Prepare data for the main bar chart
    plot_data <- data.frame(
      category = c("In ESTC", "In ESTC and ECCO"),
      count = c(counts$total_count, counts$matching_count)
    )
    
    # Generate the main bar chart
    output$barChart <- renderPlot({
      ggplot(plot_data, aes(x = category, y = count, fill = category)) +
        geom_bar(stat = "identity") +
        scale_fill_manual(values = c("In ESTC" = "#d95f02", "In ESTC and ECCO" = "#1b9e77")) +
        labs(title = paste("Occurrences of distinct estc_id's with actor '", 
                           if(any(selected_actors == "all")) {
                             word
                           } else {
                             paste(selected_actors, collapse = ", ")
                           }, "'", sep = ""),
             x = "Location", y = "Count", fill = "Location") +
        theme_minimal() +
        geom_text(aes(label = count), vjust = -0.5, color = "black", size = 6)
    })
    
    # Prepare data for the decade bar chart
    decade_counts <- count_word_by_decade(estc_actor_links, estc_core, ecco_core, exact_matches$actor_name_primary)
    
    # Generate the decade bar chart
    output$decadeChart <- renderPlot({
      ggplot(decade_counts, aes(x = publication_decade, y = count, fill = count_type)) +
        geom_bar(stat = "identity", position = "dodge") +
        scale_fill_manual(values = c("In ESTC" = "#d95f02", "In ESTC and ECCO" = "#1b9e77")) +
        labs(title = paste("Occurrences of distinct estc_id's with actor '", 
                           if(any(selected_actors == "all")) {
                             word
                           } else {
                             paste(selected_actors, collapse = ", ")
                           }, "' by Decade", sep = ""),
             x = "Decade", y = "Count", fill = "Location") +
        theme_minimal() +
        scale_x_continuous(breaks = seq(1700, 1800, by = 10)) +
        geom_text(aes(label = count), vjust = -0.5, color = "black", size = 5)
    })
  }
  
  # CSV Download Handler
  output$downloadTable <- downloadHandler(
    filename = function() {
      paste("actor_search_results-", Sys.Date(), ".csv", sep="")
    },
    content = function(file) {
      write.csv(table_data(), file, row.names = FALSE)
    }
  )
}

# Refactored function to count occurrences of a word in actor_name_primary
count_actor_word_with_estc <- function(estc_data, ecco_data, selected_actors) {
  # Filter ESTC actor links data for selected actor names
  filtered_data <- estc_data %>%
    filter(tolower(actor_name_primary) %in% tolower(selected_actors)) %>%
    distinct(estc_id)
  
  # Total unique estc_ids in ESTC
  total_count <- filtered_data %>%
    distinct(estc_id) %>%
    tally() %>%
    pull(n)
  
  # estc_ids that exist in both ESTC and ECCO
  matching_count <- filtered_data %>%
    inner_join(ecco_data %>% distinct(estc_id), by = "estc_id") %>%
    distinct(estc_id) %>%
    tally() %>%
    pull(n)
  
  # Get the matched estc_ids (in both ESTC and ECCO)
  matched_ids <- filtered_data %>%
    distinct(estc_id) %>%
    inner_join(ecco_data %>% distinct(estc_id), by = "estc_id")
  
  # Get non-matched estc_ids (in ESTC but not in ECCO)
  non_matched_ids <- filtered_data %>%
    distinct(estc_id) %>%
    anti_join(ecco_data %>% distinct(estc_id), by = "estc_id")
  
  return(list(total_count = total_count,
              matching_count = matching_count,
              matched_ids = matched_ids,
              non_matched_ids = non_matched_ids))
}

count_word_by_decade <- function(actor_data, core_data, ecco_data, selected_actors) {
  # Filter estc_actor_links data for selected actor names (in ESTC)
  filtered_data <- actor_data %>%
    filter(tolower(actor_name_primary) %in% tolower(selected_actors)) %>%
    distinct(estc_id)
  
  # Join with ESTC core data to get the publication decades
  matched_decades <- core_data %>%
    filter(estc_id %in% filtered_data$estc_id) %>%
    select(estc_id, publication_decade)
  
  # Total counts in ESTC by decade
  total_decade_count <- matched_decades %>%
    filter(publication_decade >= 1700) %>%
    group_by(publication_decade) %>%
    summarise(count = n_distinct(estc_id), count_type = "In ESTC", .groups = "drop")
  
  # Count occurrences in both ESTC and ECCO by decade
  matching_decade_count <- matched_decades %>%
    filter(estc_id %in% ecco_data$estc_id, publication_decade >= 1700) %>%
    group_by(publication_decade) %>%
    summarise(count = n_distinct(estc_id), count_type = "In ESTC and ECCO", .groups = "drop")
  
  # Combine the counts for ESTC and ECCO
  combined_counts <- bind_rows(total_decade_count, matching_decade_count)
  
  return(combined_counts)
}

# Run the application 
shinyApp(ui = ui, server = server)
