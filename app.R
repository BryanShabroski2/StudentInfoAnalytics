#==============================================================================
# Author: Bryan Luke Shabroski
# Data: .XLSX files about student information and related survey answers
# Purpose: Generate meaningful analytics showing student survey responses
# Graphs: Bar charts and diverging bar charts for group comparisons
#==============================================================================

# Load Required Libraries =====================================================
library(shiny)
library(bslib)
library(fontawesome)
library(ggplot2)
library(shinyWidgets)
library(shinyjs)
library(dplyr)
library(tidyr)
library(gridExtra)
#=====================================================

ui <- page_sidebar(
  
  sidebar = sidebar(
    useShinyjs(),
    width = "500px",
    
    #Custom CSS --------------------------------------------------
    tags$style(HTML("
      /* Slider Styling */
      .irs {
        height: 30px !important;
        font-size: 11px;
      }
      .irs-bar, .irs-line {
        height: 6px !important;
      }
      .irs-handle {
        width: 12px !important;
        height: 12px !important;
      }
      .irs-grid-text {
        font-size: 10px;
        white-space: normal;
        word-wrap: break-word;
      }
      .irs-min, .irs-max {
        display: none !important;
      }
      
      /* Input Container Spacing */
      .shiny-input-container {
        margin-bottom: 30px !important;
      }
      
      /* Binghamton University Green - Checkboxes */
      input[type='checkbox']:checked {
        background-color: #005A43 !important;
        border-color: #005A43 !important;
      }
      input[type='checkbox']:focus {
        border-color: #6cc24a !important;
        box-shadow: 0 0 0 0.2rem rgba(0, 90, 67, 0.25) !important;
      }
      
      /* Binghamton University Green - Radio Buttons */
      input[type='radio']:checked {
        background-color: #005A43 !important;
        border-color: #005A43 !important;
      }
      input[type='radio']:focus {
        border-color: #6cc24a !important;
        box-shadow: 0 0 0 0.2rem rgba(0, 90, 67, 0.25) !important;
      }
      
      /* Binghamton University Green - Sliders */
      .irs-bar {
        background: linear-gradient(to right, #005A43, #6cc24a) !important;
        border: none !important;
      }
      .irs-handle {
        background: #005A43 !important;
        border: 2px solid #004333 !important;
      }
      .irs-handle:hover {
        background: #6cc24a !important;
      }
      .irs-from, .irs-to, .irs-single {
        background: #005A43 !important;
        color: white !important;
      }
      
      /* Binghamton University Green - Accordion Headers */
      .accordion-button {
        color: #005A43 !important;
      }
      .accordion-button:not(.collapsed) {
        background-color: #edf3f1 !important;
        color: #005A43 !important;
      }
      .accordion-button:focus {
        border-color: #6cc24a !important;
        box-shadow: 0 0 0 0.2rem rgba(0, 90, 67, 0.25) !important;
      }
      
      /* Binghamton University Green - Labels and Icons */
      .form-check-label, .control-label {
        color: #004333 !important;
      }
      .accordion-button::after {
        filter: hue-rotate(180deg) saturate(2) !important;
      }
    ")),
    
    # Accordion Panels =====================================================
    accordion(
      
      # User Guide Panel =====================================================
      accordion_panel(
        title = "User Guide", 
        icon = fontawesome::fa("book"),
        HTML("
          <div style='padding: 15px; line-height: 1.6;'>
            <h4 style='color: #005A43; margin-bottom: 15px; text-align: center; border-bottom: 2px solid #6cc24a; padding-bottom: 10px;'>
              Binghamton University Student Survey Analytics Guide
            </h4>
            
            <div style='margin-bottom: 20px;'>
              <h5 style='color: #005A43;'>Getting Started</h5>
              <ol style='margin-left: 15px;'>
                <li><strong>Choose Graph Type:</strong> Pick either Bar Graph (single population analysis), Diverging Bar Chart (two population comparison), or Demographic Answer Analysis (Find outliers) </li>
                <li><strong>Select Questions:</strong> Choose question(s) from a category</li>
                <li><strong>Filter Students:</strong> Use sliders to select specific student populations (Does not apply to Demographic Answer Analysis)</li>
                <li><strong>Analyze Results:</strong> View how different student groups responded (Does not apply to Demographic Answer Analysis)</li>
              </ol>
            </div>
            
            <div style='margin-bottom: 20px;'>
              <h5 style='color: #005A43;'>Question Selection</h5>
              <ul style='margin-left: 15px;'>
                <li><strong>Academic Advising+:</strong> Questions about belonging and academic readiness</li>
                <li><strong>Health & Wellness:</strong> University support for student health</li>
                <li><strong>Preparedness & Belonging:</strong> Financial readiness, community connection, and friendship</li>
                <li><strong>Grit:</strong> Persistence, focus, and work ethic measures</li>
              </ul>
              <p style='color: #BF0D3E; font-style: italic;'>❗You can only select from one category at a time.❗</p>
            </div>
            
            <div style='margin-bottom: 20px;'>
              <h5 style='color: #005A43;'>Graph Types</h5>
              <ul style='margin-left: 15px;'>
                <li><strong>Bar Graph:</strong> Shows response distribution for selected student population</li>
                <li><strong>Diverging Bar Chart:</strong> Horizontal comparison showing two student populations</li>
                <li><strong>Demographic Answer Analysis:</strong> Displays 5 category breakdowns ordered from lowest to highest percentage of your selected response. Choose which response level to analyze (e.g., &quotStrongly disagree&quot to identify struggling groups, or &quotStrongly agree&quot to find high-performing groups).</li>
                
              </ul>
            </div>
            
            <div style='margin-bottom: 20px;'>
              <h5 style='color: #005A43;'>Abbreviation Guide</h5>
              <div style='background-color: #f8f9fa; padding: 10px; border-radius: 5px; border-left: 4px solid #005A43;'>
                
                <p style='margin-bottom: 10px; font-weight: bold; color: #005A43;'>Residential Communities:</p>
                <ul style='margin-left: 15px; margin-bottom: 15px;'>
                  <li><strong>CitW</strong> = College-in-the-Woods</li>
                  <li><strong>Dick</strong> = Dickinson Community</li>
                  <li><strong>Hill</strong> = Hillside Community</li>
                  <li><strong>Hin</strong> = Hinman College</li>
                  <li><strong>Mtn</strong> = Mountainview College</li>
                  <li><strong>New</strong> = Newing College</li>
                  <li><strong>Susq</strong> = Susquehanna Community</li>
                </ul>
                
                <p style='margin-bottom: 10px; font-weight: bold; color: #005A43;'>Colleges:</p>
                <ul style='margin-left: 15px; margin-bottom: 15px;'>
                  <li><strong>CCPA</strong> = College of Community and Public Affairs</li>
                  <li><strong>Harpur</strong> = Harpur College of Arts and Sciences</li>
                  <li><strong>Mgmt</strong> = School of Management</li>
                  <li><strong>Nursing</strong> = Decker College of Nursing and Health Sciences</li>
                  <li><strong>Watson</strong> = Thomas J. Watson College of Engineering and Applied Science</li>
                  <li><strong>CEO</strong> = Non-matriculated students</li>
                </ul>
              </div>
            </div>
          </div>
        ")
      ),
      #=====================================================
      
      # Graph Type Selection Panel =====================================================
      accordion_panel(
        title = "Type of Graph",
        icon = fontawesome::fa("chart-simple"),
        
        radioButtons(
          "graph_buttons",
          "Select option:",
          choices = list("Bar Graph" = "bar", 
                         "Diverging Bar Chart" = "diverging", 
                         "Demographic Answer Analysis" = "insights"),
          selected = "bar"
        ),
        conditionalPanel(
          condition = "input.graph_buttons == 'insights'",
          
          br(),
          
          radioButtons(
            "insights_response",
            "Select response to analyze:",
            choices = list(
              "Strongly disagree" = "Strongly disagree",
              "Somewhat disagree" = "Somewhat disagree", 
              "Neither agree nor disagree" = "Neither agree nor disagree",
              "Somewhat agree" = "Somewhat agree",
              "Strongly agree" = "Strongly agree"
            ),
            selected = "Strongly disagree"
          )
        )
      ),
      
      #=====================================================
      
      # Question Categories =====================================================
      
      # Academic Advising Questions =====================================================
      accordion_panel(
        title = "Academic Advising+ Questions",
        icon = fontawesome::fa("school"),
        
        checkboxGroupInput(
          "advising_questions",
          "Select Questions:",
          choices = list(
            "I feel I belong at Binghamton University." = "Q4_1",
            "I am ready to meet the academic challenges of Binghamton University." = "Q4_2"
          ),
          selected = NULL
        )
      ),
      #=====================================================
      
      # Health and Wellness Questions =====================================================
      accordion_panel(
        title = "Health and Wellness Questions",
        icon = fontawesome::fa("heart"),
        
        checkboxGroupInput(
          "health_questions",
          "Select Questions:",
          choices = list(
            "Binghamton University is concerned with my personal health/wellness." = "Q11_1", 
            "Binghamton University has a number of services available to help me be healthy at college." = "Q11_2"
          ),
          selected = NULL
        )
      ),
      #=====================================================
      
      # Preparedness and Belonging Questions =====================================================
      accordion_panel(
        title = "Preparedness and Belonging Questions",
        icon = fontawesome::fa("thumbs-up"),
        
        checkboxGroupInput(
          "belonging_questions",
          "Select Questions:",
          choices = list(
            "I feel like I have the resources to make me a financially responsible college student." = "Q15_1",
            "I feel connected to the Binghamton University community." = "Q15_2",
            "Binghamton University is a place where I am able to perform up to my full potential." = "Q15_3",
            "I have found one or more communities or groups where I feel I belong at Binghamton University." = "Q15_4",
            "I am ready to make friends at Binghamton University." = "Q15_5"
          ),
          selected = NULL
        )
      ),
      #=====================================================
      
      # Grit Questions =====================================================
      accordion_panel(
        title = "Grit Questions",
        icon = fontawesome::fa("hammer"), 
        
        checkboxGroupInput(
          "grit_questions",
          "Select Questions:",
          choices = list(
            "New ideas and projects sometimes distract me from previous ones." = "Grit_1",
            "Setbacks don't discourage me." = "Grit_2",
            "I have been obsessed with a certain idea or project for a short time but later lost interest." = "Grit_3",
            "I am a hard worker." = "Grit_4",
            "I often set a goal but later choose to pursue a different one." = "Grit_5",
            "I have difficulty maintaining my focus on projects that take more than a few months to complete." = "Grit_6",
            "I finish whatever I begin." = "Grit_7",
            "I am diligent." = "Grit_8"
          ),
          selected = NULL
        )
      ),
      #=====================================================
      
      # Student Population Selection Section =====================================================
      
      # Student 1 Header =====================================================
      div(
        style = "padding: 10px 10px; max-width: 95%; font-weight: bold; text-align: center; font-size: 18px; color: #6cc24a;",
        "Student Demographics 1:"
      ),
      
      # Student Population Slider =====================================================
      div(
        style = "padding: 10px 10px; max-width: 95%; text-align: center;",
        sliderTextInput(
          inputId = "pop",
          label = "Transfer Status:",
          choices = c("All", "New Freshman", "Transfer"),
          selected = "All",
          grid = TRUE
        )
      ),
      #=====================================================
      
      # First-Generation Slider =====================================================
      div(
        style = "padding: 10px 10px; max-width: 95%; text-align: center;",
        sliderTextInput(
          inputId = "gen",
          label = "First-Generation Status:",
          choices = c("All", "Y", "N"),
          selected = "All",
          grid = TRUE
        )
      ),
      #=====================================================
      
      # Housing Slider =====================================================
      div(
        style = "padding: 10px 10px; max-width: 95%; text-align: center;",
        sliderTextInput(
          inputId = "housing",
          label = "Housing Type:",
          choices = c("All", "On", "Off"),
          selected = "All",
          grid = TRUE
        )
      ),
      #=====================================================
      
      # Residential Slider =====================================================
      div(
        id = "community_wrapper",
        style = "padding: 10px 10px; max-width: 95%; text-align: center;",
        sliderTextInput(
          inputId = "comm",
          label = "Residential Community:",
          choices = c("All", "CitW", "Dick", "Hill", "Hin", "Mtn", "New", "Susq"),
          selected = "All",
          grid = TRUE
        )
      ),
      #=====================================================
      
      # College Slider =====================================================
      div(
        style = "padding: 10px 10px; max-width: 95%; text-align: center;",
        sliderTextInput(
          inputId = "college",
          label = "College:",
          choices = c("All", "CCPA", "Harpur", "Mgmt", "Nursing", "Watson", "CEO"),
          selected = "All",
          grid = TRUE
        )
      ),
      #=====================================================

      # Student Population Selection 2 =====================================================
      conditionalPanel(
        condition = "input.graph_buttons == 'diverging'",
        
        # Student 2 Header =====================================================
        div(
          style = "padding: 10px 10px; max-width: 95%; font-weight: bold; text-align: center; font-size: 18px; color: #6cc24a;",
          "Student Demographics 2:"
        ),
        
        # Student Population Slider 2 =====================================================
        div(
          style = "padding: 10px 10px; max-width: 95%; text-align: center;",
          sliderTextInput(
            inputId = "pop2",
            label = "Transfer Status:",
            choices = c("All", "New Freshman", "Transfer"),
            selected = "All",
            grid = TRUE
          )
        ),
        #=====================================================
        
        # First-Generation Slider 2 =====================================================
        div(
          style = "padding: 10px 10px; max-width: 95%; text-align: center;",
          sliderTextInput(
            inputId = "gen2",
            label = "First-Generation Status:",
            choices = c("All", "Y", "N"),
            selected = "All",
            grid = TRUE
          )
        ),
        #=====================================================
        
        # Housing Slider 2 =====================================================
        div(
          style = "padding: 10px 10px; max-width: 95%; text-align: center;",
          sliderTextInput(
            inputId = "housing2",
            label = "Housing Type:",
            choices = c("All", "On", "Off"),
            selected = "All",
            grid = TRUE
          )
        ),
        #=====================================================
        
        # Community Slider 2 =====================================================
        div(
          id = "community_2_wrapper",
          style = "padding: 10px 10px; max-width: 95%; text-align: center;",
          sliderTextInput(
            inputId = "comm2",
            label = "Residential Community:",
            choices = c("All", "CitW", "Dick", "Hill", "Hin", "Mtn", "New", "Susq"),
            selected = "All",
            grid = TRUE
          )
        ),
        #=====================================================
        
        # College Slider 2 =====================================================
        div(
          style = "padding: 10px 10px; max-width: 95%; text-align: center;",
          sliderTextInput(
            inputId = "college2",
            label = "College:",
            choices = c("All", "CCPA", "Harpur", "Mgmt", "Nursing", "Watson", "CEO"),
            selected = "All",
            grid = TRUE
          )
        )
      ),
      #=====================================================
      
      id = "acc",  
      open = NULL
    )
  ),
  
  # Main Content Area =====================================================
  card(
    style = "border: 2px solid #005A43; border-radius: 8px;",
    plotOutput(outputId = "displayPlot")
  )
  #=====================================================
)


# Server Logic =====================================================
server <- function(input, output, session) {
  
  # Data Loading =====================================================
  dfFinalStuType <- readRDS("studentData/dfFinalStuType.rds")
  dfSurveyAnswers <- readRDS("surveyData/dfFinalSurveyAnswers.rds")
  #=====================================================
  
  # Merge our survey and student datasets by their XID (Student ID) 
  studentData <- merge(dfFinalStuType, dfSurveyAnswers, by = "XID")
  
  # Survey Responses =====================================================
  survey_answer_string_list <- c(
    "Strongly disagree", 
    "Somewhat disagree", 
    "Neither agree nor disagree", 
    "Somewhat agree", 
    "Strongly agree"
  )
  
  grit_string_list <- c(
    "Not at all like me", 
    "Not much like me", 
    "Somewhat like me", 
    "Mostly like me", 
    "Very much like me"
  )
  #=====================================================
  
  # Question Group Observers =====================================================
  # These insure that only one group of questions is selected at a time.
  
  observeEvent(input$advising_questions, {
    if (length(input$advising_questions) > 0) {
      updateCheckboxGroupInput(session, "health_questions", selected = character(0))
      updateCheckboxGroupInput(session, "belonging_questions", selected = character(0))
      updateCheckboxGroupInput(session, "grit_questions", selected = character(0))
    }
  })
  
  observeEvent(input$health_questions, {
    if (length(input$health_questions) > 0) {
      updateCheckboxGroupInput(session, "advising_questions", selected = character(0))
      updateCheckboxGroupInput(session, "belonging_questions", selected = character(0))
      updateCheckboxGroupInput(session, "grit_questions", selected = character(0))
    }
  })
  
  observeEvent(input$belonging_questions, {
    if (length(input$belonging_questions) > 0) {
      updateCheckboxGroupInput(session, "advising_questions", selected = character(0))
      updateCheckboxGroupInput(session, "health_questions", selected = character(0))
      updateCheckboxGroupInput(session, "grit_questions", selected = character(0))
    }
  })
  
  observeEvent(input$grit_questions, {
    if (length(input$grit_questions) > 0) {
      updateCheckboxGroupInput(session, "advising_questions", selected = character(0))
      updateCheckboxGroupInput(session, "health_questions", selected = character(0))
      updateCheckboxGroupInput(session, "belonging_questions", selected = character(0))
    }
  })
  
  # Housing-Related Observers =====================================================
  # This is used to hide the community section when off campus is selected. 
  # Off campus students do not belong to any on campus community.
  
  observeEvent(input$housing, {
    if (input$housing == "Off") {
      hide("community_wrapper")
    } else {
      show("community_wrapper")
    }
  })
  
  observeEvent(input$housing2, {
    if (input$graph_buttons == "diverging" && input$housing2 == "Off") {
      hide("community_2_wrapper")
    } else {
      show("community_2_wrapper")
    }
  })
  #=====================================================
  
  # Key Insights Observer =====================================================
  observeEvent(get_selected_questions(), {
    selected <- get_selected_questions()
    
    if (!is.null(selected)) {
      is_grit <- selected$group_name == "grit"
      
      if (is_grit) {
        updateRadioButtons(
          session, 
          "insights_response",
          choices = list(
            "Not at all like me" = "Not at all like me",
            "Not much like me" = "Not much like me",
            "Somewhat like me" = "Somewhat like me", 
            "Mostly like me" = "Mostly like me",
            "Very much like me" = "Very much like me"
          ),
          selected = "Not at all like me"
        )
      } else {
        updateRadioButtons(
          session,
          "insights_response", 
          choices = list(
            "Strongly disagree" = "Strongly disagree",
            "Somewhat disagree" = "Somewhat disagree",
            "Neither agree nor disagree" = "Neither agree nor disagree", 
            "Somewhat agree" = "Somewhat agree",
            "Strongly agree" = "Strongly agree"
          ),
          selected = "Strongly disagree"
        )
      }
    }
  })
  #=====================================================
  
  # Helper Functions =====================================================
  
  # Here we determine which question(s) and question group are selected
  
  get_selected_questions <- reactive({
    groups <- list(
      advising = input$advising_questions,
      health = input$health_questions,
      belonging = input$belonging_questions,
      grit = input$grit_questions
    )
    
    active <- Filter(function(qs) length(qs) > 0, groups)
    
    if (length(active) != 1) {
      return(NULL)
    }
    
    selected_questions <- unlist(active)
    
    
    # We return both our group name (for condition from earlier) and questions selected for data analysis
    return(list(
      group_name = names(active)[1],
      questions = selected_questions
    ))
  })
  
  # Here we filter our student data based on the slider input
  # No need to filter if we want all the students in a particular subset
  filter_student_data <- function(data, pop, gen, housing, comm, college) {
    if (pop != "All") {
      data <- data[data$STUDENT_POPULATION_DESC == pop, ]
    }
    if (gen != "All") {
      data <- data[data$FIRST_GENERATION_IND == gen, ]
    }
    if (housing != "All") {
      data <- data[data$HOUSING_TYPE == housing, ]
    }
    if (comm != "All") {
      data <- data[data$COMMUNITY == comm, ]
    }
    if (college != "All") {
      data <- data[data$COLLEGE_DESC == college, ]
    }
    return(data)
  }
  
  #This helper is used for our Demographic Answer analysis, it will get us the ordered percentages
  #for each category based on the selected question.
  calculate_category_insights <- function(studentData, selected_questions, target_response) {
    
    categories <- list(
      "Transfer Status" = list(
        column = "STUDENT_POPULATION_DESC",
        values = c("New Freshman", "Transfer")
      ),
      "First-Generation Status" = list(
        column = "FIRST_GENERATION_IND", 
        values = c("Y", "N")
      ),
      "Housing Type" = list(
        column = "HOUSING_TYPE",
        values = c("On", "Off")
      ),
      "Residential Community" = list(
        column = "COMMUNITY",
        values = c("CitW", "Dick", "Hill", "Hin", "Mtn", "New", "Susq")
      ),
      "College" = list(
        column = "COLLEGE_DESC",
        values = c("CCPA", "Harpur", "Mgmt", "Nursing", "Watson", "CEO")
      )
    )
    
    category_results <- list()
    
    for (category_name in names(categories)) {
      column_name <- categories[[category_name]]$column
      possible_values <- categories[[category_name]]$values
      
      category_data <- data.frame()
      
      for (value in possible_values) {
        subgroup_data <- studentData[studentData[[column_name]] == value, ]
        
        if (nrow(subgroup_data) == 0) next
        
        responses <- unlist(subgroup_data[, selected_questions, drop = FALSE])
        responses <- responses[!is.na(responses)]
        
        if (length(responses) == 0) next
        
        target_count <- sum(responses == target_response)
        total_count <- length(responses)
        target_percentage <- (target_count / total_count) * 100
        
        category_data <- rbind(category_data, data.frame(
          Subgroup = value,
          TargetPercentage = target_percentage,
          TotalResponses = total_count,
          stringsAsFactors = FALSE
        ))
      }
      
      if (nrow(category_data) > 0) {
        category_data <- category_data[order(category_data$TargetPercentage), ]
        category_results[[category_name]] <- category_data
      }
    }
    
    return(category_results)
  }
  
  # Main Plot Rendering =====================================================
  output$displayPlot <- renderPlot({
    
    # Get our questions and make sure questions are selected
    selected <- get_selected_questions()
    if(is.null(selected)) {
      return(NULL)
    }
    
    # Get our questions and graph type from the radio buttons
    selected_questions <- selected$questions
    graph_type <- input$graph_buttons
    req(graph_type)
    
    # Make sure our questions are in dataset we are searching
    missing_cols <- selected_questions[!selected_questions %in% colnames(studentData)]
    if(length(missing_cols) > 0) {
      return(NULL)
    }
    
    # If grit questions are selected we use different responses for the chart
    is_grit <- selected$group_name == "grit"
    levels_used <- if (is_grit) grit_string_list else survey_answer_string_list
    
    # Generate Bar Chart =====================================================
    if (graph_type == "bar") {
      
      # Filiter data based on input sliders
      filtered_data <- filter_student_data(
        studentData, 
        input$pop, 
        input$gen, 
        input$housing, 
        input$comm, 
        input$college
      )
      
      # Make sure data exists
      if(nrow(filtered_data) == 0) {
        return(NULL)
      }
      
      # Extract or data
      values <- factor(
        unlist(filtered_data[, selected_questions, drop = FALSE]), 
        levels = levels_used
      )
      values <- values[!is.na(values)]
      
      if(length(values) == 0) {
        return(NULL)
      }
      
      # Calculate percentages for display
      df <- as.data.frame(prop.table(table(values)) * 100)
      colnames(df) <- c("Answer", "Percent")
      
      # Generate Bar Chart =====================================================
      colors <- rep(c("#005A43", "#6cc24a"), length.out = nrow(df))
      
      ggplot(df, aes(x = Answer, y = Percent)) +
        geom_bar(stat = "identity", fill = colors) +
        geom_text(
          aes(label = paste0(round(Percent, 1), "%")), 
          vjust = -0.5, 
          color = "#004333", 
          size = 3.5, 
          fontface = "bold"
        ) +
        labs(
          y = "Percent of Students", 
          x = "Answer Choice", 
          title = "Student Response Distribution"
        ) +
        theme_minimal(base_size = 14) +
        theme(
          axis.text.x = element_text(angle = 0, hjust = 0.5, vjust = 1, margin = margin(t = 5)),
          plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 20), color = "#005A43"),
          axis.title = element_text(color = "#005A43"),
          axis.text = element_text(color = "#004333")
        ) +
        scale_y_continuous(expand = expansion(mult = c(0, 0.1)))
      #=====================================================
      # Generate Diverging Bar Chart ===================================================== 
    } else if (graph_type == "diverging") {
      
      # Filter our data
      filtered_data1 <- filter_student_data(
        studentData, 
        input$pop, 
        input$gen, 
        input$housing, 
        input$comm, 
        input$college
      )
      
      filtered_data2 <- filter_student_data(
        studentData,
        input$pop2,
        input$gen2,
        input$housing2, 
        input$comm2,
        input$college2
      )
      
      # Make sure data exists
      if(nrow(filtered_data1) == 0 || nrow(filtered_data2) == 0) {
        return(NULL)
      }
      
      # Extract our data
      values1 <- factor(
        unlist(filtered_data1[, selected_questions, drop = FALSE]), 
        levels = levels_used
      )
      values1 <- values1[!is.na(values1)]
      
      values2 <- factor(
        unlist(filtered_data2[, selected_questions, drop = FALSE]), 
        levels = levels_used
      )
      values2 <- values2[!is.na(values2)]
      
      # Create data for chart
      if(length(values1) > 0 && length(values2) > 0) {
        
        # Calculate percentages
        percent_table1 <- prop.table(table(values1)) * 100
        percent_table2 <- prop.table(table(values2)) * 100
        
        all_levels <- levels_used
        percent1 <- setNames(rep(0, length(all_levels)), all_levels)
        percent2 <- setNames(rep(0, length(all_levels)), all_levels)
        
        percent1[names(percent_table1)] <- percent_table1
        percent2[names(percent_table2)] <- percent_table2
        
        # Create diverging data frame
        diverging_df <- data.frame(
          Answer = factor(all_levels, levels = rev(all_levels)),
          Group1 = -percent1,
          Group2 = percent2
        )
        
        # Convert to long format for ggplot
        diverging_long <- diverging_df %>%
          tidyr::pivot_longer(cols = c(Group1, Group2), names_to = "Group", values_to = "Percent") %>%
          dplyr::mutate(Group = ifelse(Group == "Group1", "Group 1", "Group 2"))
        
        # Create diverging bar chart
        ggplot(diverging_long, aes(x = Answer, y = Percent, fill = Group)) +
          geom_col(position = "identity", width = 0.8) +
          geom_text(
            aes(label = ifelse(abs(Percent) > 1, paste0(round(abs(Percent), 1), "%"), "")),
            position = position_stack(vjust = 0.5),
            color = "white", 
            size = 3.5, 
            fontface = "bold"
          ) +
          coord_flip() +
          scale_y_continuous(
            labels = function(x) abs(x),
            limits = c(
              -max(abs(diverging_long$Percent)) * 1.2, 
              max(abs(diverging_long$Percent)) * 1.2
            )
          ) +
          geom_hline(yintercept = 0, color = "black", size = 0.5) +
          labs(
            x = "Response",
            y = "Percent of Students", 
            fill = "Student Group",
            title = "Student Group Comparison"
          ) +
          theme_minimal(base_size = 14) +
          theme(
            panel.grid.major.y = element_blank(),
            panel.grid.minor = element_blank(),
            axis.text.y = element_text(size = 12),
            plot.title = element_text(hjust = 0.5, size = 16, margin = margin(b = 20), color = "#005A43"),
            axis.title = element_text(color = "#005A43"),
            axis.text = element_text(color = "#004333"),
            legend.title = element_text(color = "#005A43"),
            legend.text = element_text(color = "#004333")
          ) +
          scale_fill_manual(
            values = c("Group 1" = "#005A43", "Group 2" = "#6cc24a"),
            labels = c("Group 1" = "Student Demographic 1", "Group 2" = "Student Demographic 2")
          )
        #=====================================================
      }
    } else if (graph_type == "insights") {
      # get responses
      target_response <- input$insights_response
      req(target_response)
      
      #Calculate insights for each category
      category_insights <- calculate_category_insights(studentData, selected_questions, target_response)
      
      if (length(category_insights) == 0) {
        return(NULL)
      }
      
      plot_list <- list()
      
      for (category_name in names(category_insights)) {
        category_data <- category_insights[[category_name]]
        
        if (nrow(category_data) == 0) next
        
        category_data$Subgroup <- factor(category_data$Subgroup, levels = category_data$Subgroup)
        
        colors <- rep(c("#005A43", "#6cc24a"), length.out = nrow(category_data))
        
        p <- ggplot(category_data, aes(x = Subgroup, y = TargetPercentage)) +
          geom_col(fill = colors, width = 0.7) +
          geom_text(
            aes(label = paste0(round(TargetPercentage, 1), "%")),
            vjust = -0.5,
            size = 3.5,
            fontface = "bold",
            color = "#004333"
          ) +
          labs(
            title = category_name,
            subtitle = paste("Ordered by", paste0("'", target_response, "'"), "responses"),
            y = paste("% of", paste0("'", target_response, "'"), "Responses"),
            x = ""
          ) +
          theme_minimal(base_size = 11) +
          theme(
            plot.title = element_text(hjust = 0.5, size = 13, color = "#005A43", face = "bold"),
            plot.subtitle = element_text(hjust = 0.5, size = 10, color = "#004333"),
            axis.text.x = element_text(angle = 0, hjust = 1, size = 9),
            axis.title = element_text(color = "#005A43"),
            panel.grid.major.x = element_blank(),
            panel.grid.minor = element_blank()
          ) +
          scale_y_continuous(
            limits = c(0, max(category_data$TargetPercentage) * 1.2),
            expand = expansion(mult = c(0, 0.1))
          )
        
        plot_list[[length(plot_list) + 1]] <- p
      }
      
      if (length(plot_list) >= 3) {
        top_row <- gridExtra::grid.arrange(
          plot_list[[1]], plot_list[[2]], plot_list[[3]], 
          ncol = 3, 
          top = paste("Category Analysis:", paste0("'", target_response, "'"), "Response Rates")
        )
        
        if (length(plot_list) >= 4) {
          remaining_plots <- plot_list[4:length(plot_list)]
          bottom_row <- do.call(gridExtra::grid.arrange, 
                                c(remaining_plots, list(ncol = length(remaining_plots))))
          
          gridExtra::grid.arrange(top_row, bottom_row, nrow = 2, heights = c(1, 1))
        } else {
          top_row
        }
      } else {
        do.call(gridExtra::grid.arrange, c(plot_list, list(ncol = length(plot_list))))
      }
    }
    else {
      return(NULL)
    }
  })
}

shinyApp(ui = ui, server = server)