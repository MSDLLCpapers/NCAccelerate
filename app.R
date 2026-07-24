### NCAcelerate R Shiny App ###
### Ruilian (Roxy) Zhang ###
### Last update in July 2026 ###

library(shiny)
library(shiny.fluent)
library(shinydashboard)
library(tidyverse)
library(bslib) # theme
library(readxl) # read excel file
library(cellranger) # read all columns for excel file
library(DT) # datatable
library(ggprism) # x axis minor ticks
library(scales) # log-transformation of plot breaks and labels
library(table1) # significant digits padding
library(knitr) # download PK parameter table as PDF
library(kableExtra)
library(rmarkdown)
library(shinyBS) # bsModal

#  Remove all variables from the current environment
rm(list=ls())


#### UI ####

ui <- fluidPage(
  # Apply Minty theme
  theme = bs_theme(version = 4, bootswatch = "minty"),
  # Page title
  tags$head(
    tags$style(
      HTML(
        "
        body {
          background-color: #f0f0f0; /* Set background color */
          font-family: Arial, sans-serif; /* Change font family */
        }
        .container {
          margin-top: 20px; /* Add some top margin to the main container */
          max-width: 95%; /* Adjust for larger screens */
        }
        .page-header {
          background-color: #c1dedc; /* Header background color */
          color: white; /* Header text color */
          padding: 10px 0; /* Add padding to the header */
          text-align: center; /* Center align header text */
        }
        .well {
          background-color: white; /* Well background color */
          border: 1px solid #ddd; /* Add a border around the well */
          border-radius: 5px; /* Round the corners of the well */
          box-shadow: 0 0 5px rgba(0, 0, 0, 0.1); /* Add a subtle box shadow */
          padding: 20px; /* Add padding inside the well */
        }
        .plot-title {
          text-align: center; /* Center align plot title */
          font-size: 18px; /* Increase font size for plot title */
          margin-bottom: 20px; /* Add margin to the bottom of plot title */
        }
        .plot {
          border: 1px solid #ddd; /* Add a border around the plot area */
          border-radius: 5px; /* Round the corners of the plot area */
          padding: 10px; /* Add padding inside the plot area */
          background-color: white; /* Plot background color */
        }
        /* Tooltip styles */
        .info-icon {
          margin-left: 5px;
          color: #17a2b8;
          cursor: pointer;
        }
        .tooltip {
          position: relative;
          display: inline-block;
        }
        .tooltip .tooltiptext {
          visibility: hidden;
          width: 120px;
          background-color: black;
          color: #fff;
          text-align: center;
          border-radius: 6px;
          padding: 5px 0;
          position: absolute;
          z-index: 1;
          bottom: 150%;
          left: 50%;
          margin-left: -60px;
          opacity: 0;
          transition: opacity 0.3s;
        }
        .tooltip:hover .tooltiptext {
          visibility: visible;
          opacity: 1;
        }
        "
      )
    )
  ),
  # Page content
  div(
    class = "container",
    div(
      class = "page-header",
      h1("NCAcelerate")
    ),
    fluidRow(
      # Upload a csv file
      column(
        width = 3,
        br(),
        HTML('<h5 style="font-weight: bold;">Upload Data</h5>'),
        fileInput("file", "Upload Concentration Data", 
                  buttonLabel = "Browse...",
                  placeholder = "Select a file",
                  accept = c(".csv", ".xlsx", ".xls")),
        fileInput("file_para", "Upload Final Parameters Data",
                  buttonLabel = "Browse...",
                  placeholder = "Select a file",
                  accept = c(".csv", ".xlsx", ".xls")),
        tags$hr(),
        HTML('<h6 style="font-weight: bold; margin-bottom: 2px;">Filter Concentration Data</h6>'),
        HTML('<div style="font-size: 12px; color: #888; margin-bottom: 2px;">Use backspace to de-select</div>'),
        selectInput("select_analyte", "Analyte", choices = NULL),
        selectInput("select_matrix", "Matrix", choices = NULL, multiple = TRUE),
        selectInput("select_panel", "Panel", choices = NULL, multiple = TRUE),
        selectInput("select_period", "Period", choices = NULL, multiple = TRUE),
        selectInput("select_dose", "Dose", choices = NULL, multiple = TRUE),
        selectInput("exclude_group", "Groups to exclude", choices = NULL, multiple = TRUE),
        checkboxInput("exclude_check", "Exclude group(s) above from the plot", value = FALSE)
      ),
      # Main tabs
      column(
        width = 9,
        br(),
        tabsetPanel(
          tabPanel("Instructions",
                   div(class = "well",
                       HTML('<h6 style="font-weight: bold;">Introduction</h6>'),
                       HTML('<ul style="font-size: 14px; margin-top: 0px; ">
                           <li>This app is designed for automating below items of FIH (first in human) SAD (single ascending dose) and MAD (multiple ascending dose) analysis:</li>
                           <ul>
                                <li>Time-concentration plot</li>
                                <li>PK parameter summary table</li>
                                <li>Dose proportionality projection</li>
                           </ul>
                           </ul>'),
                       HTML('<h6 style="font-weight: bold;">Data</h6>'),
                       HTML('<ul style="font-size: 14px; margin-top: 0px; margin-bottom: 6px;">
                           <li>Designed for Ardena and Watson-styled data.</li>
                           <li>Please use the output time-concentration and PK parameter data (stacked) from the <strong>Phoenix NCA workflow</strong>. 
                           Names of the steps in the workflow:</li>
                           <ul>
                                <li>Step 1a: Prep Sample Data_EXPORT for RShiny CvT Plot</li>
                                <ul>
                                     <li>If the output file is an excel file, please make sure the data has only one tab</li>
                                </ul>
                                <li>Step 4a/c: EXPORT Results for RShiny PK Parameter Table</li>
                           </ul>
                           <li>Acceptable data formats are .csv, .xlsx, .xls.</li>
                           <li>MAD time concentration data: for the purpose of the app, if period is not populated, it will be populated with 1.</li>
                           <li>Variables needed in input time-concentration data:</li>
                           </ul>'),
                       HTML('<style>
                           .center-text {
                               text-align: center;
                           }
                           table {
                               border-collapse: collapse;
                               margin-left: 40px;
                           }
                           .no-bullets {
                               list-style-type: none;
                           }
                           </style>'),
                       HTML('<table border="1" style="font-size: 14px; margin-top: 0px; border-collapse: collapse;">
                               <tr>
                                   <th>Variable Name*</th>
                                   <th>Variable Explanation</th>
                                   <th>Needed in Watson Data</th>
                                   <th>Needed in Ardena Data</th>
                               </tr>
                               <tr>
                                   <td>PKEXTNAM</td>
                                   <td class="center-text">column value: “ARDENA”</td>
                                   <td class="center-text"></td>
                                   <td class="center-text">✓</td>
                               </tr>
                               <tr>
                                   <td>PKEXTORRES</td>
                                   <td class="center-text">concentration value</td>
                                   <td class="center-text">✓</td>
                                   <td class="center-text">✓</td>
                               </tr>
                               <tr>
                                   <td>PKEXTNMHR</td>
                                   <td class="center-text">nominal hour</td>
                                   <td class="center-text">✓</td>
                                   <td class="center-text">✓</td>
                               </tr>
                               <tr>
                                   <td >PKEXTDOSE**</td>
                                   <td class="center-text">dose value and unit</td>
                                   <td class="center-text">✓</td>
                                   <td class="center-text">✓</td>
                               </tr>
                               <tr>
                                   <td>PKEXTSTUDYID</td>
                                   <td class="center-text">study ID</td>
                                   <td class="center-text">✓</td>
                                   <td class="center-text">✓</td>
                               </tr>
                               <tr>
                                   <td>PKEXTSRCANL</td>
                                   <td class="center-text">analyte</td>
                                   <td class="center-text">✓</td>
                                   <td class="center-text">✓</td>
                               </tr>
                               <tr>
                                   <td>PKEXTSPECAB</td>
                                   <td class="center-text">matrix</td>
                                   <td class="center-text">✓</td>
                                   <td class="center-text">✓</td>
                               </tr>
                               <tr>
                                   <td>PKEXTPKSEQ</td>
                                   <td class="center-text">panel</td>
                                   <td class="center-text">✓</td>
                                   <td class="center-text">✓</td>
                               </tr>
                               <tr>
                                   <td>PKEXTPERIOD</td>
                                   <td class="center-text">period</td>
                                   <td class="center-text">✓</td>
                                   <td class="center-text">✓</td>
                               </tr>
                               <tr>
                                   <td>PKEXTSTDYDY</td>
                                   <td class="center-text">day</td>
                                   <td class="center-text">✓</td>
                                   <td class="center-text">✓</td>
                               </tr>
                           </table>'),
                       HTML('<ul class="no-bullets" style="font-size: 12px; margin-top: 10px;">
                           <li>* Column names of the input data should be the same as Variable Name (e.g., Column name “Period” won’t work, it has to be “PKEXTPERIOD”).</li>
                           <li>** Ardena data format should be value followed by unit (e.g., 50 mg), no second row in column header. Watson data format should be value only (e.g., 50), with unit (e.g., mg) in the second row of column header.</li>
                           </ul>'),
                       HTML('<h6 style="font-weight: bold;">Layout</h6>'),
                       HTML('<ul style="font-size: 14px; margin-top: 0px; ">
                           <li>The "Data Check", "Data Summary", "Time-Concentration Plot" tabs are used for time-concentration data.</li>
                           <li>The "PK Parameter Table" and "Dose Proportionality" tabs are used for PK parameter data (stacked).</li>
                           </ul>')
                   )),
          tabPanel("Data Check",
                   div(
                     class = "well",
                     HTML('<ul style="font-size: 12px; color: #888; margin-top: 0px; ">
                           <li>This tab shows the uploaded time-concentration data</li>
                           <li>Concentration unit conversion is optional:</li>
                           <ol start=1>1. Choose a "Original unit", which should be the same as what is shown in ORRESU upon uploading</ol>
                           <ol start=1>2. PKEXTORRES(concentration value) and ORRESU(concentration unit) will show converted values</ol>
                           <ol start=1>3. Choose a different "Convert to" unit if needed, <strong>do not</strong> change "Original unit"</ol>
                           </ul>'),
                     HTML('<strong>The concentration unit should have been converted in Phoenix workflow, use only when needed.</strong>'),
                     fluidRow(
                       column(
                         width = 4,
                         selectInput("from_unit", "Original unit",
                                     choices = c("pg/mL", "ng/mL", "ug/mL", "mg/mL", "pM", "nM", "uM", "mM"),
                                     selected = "nM")
                         #HTML('<div style="font-size: 12px; color: #888;">Concentration unit in original data</div>')
                       ),
                       column(
                         width = 4,
                         selectInput("to_unit", "Convert to",
                                     choices = c("pM", "nM", "uM", "mM", "pg/mL", "ng/mL", "ug/mL", "mg/mL"),
                                     selected = "ng/mL")
                       ),
                       column(
                         width = 4,
                         textInput("MW", "Molecular weight in KD", placeholder = "Enter a number here")
                       )
                     ),
                     actionButton("convert", "Convert"),
                     br(),
                     br(),
                     br(),
                     dataTableOutput("raw_data"))
          ),
          tabPanel("Mean Concentration Data", 
                   div(
                     class = "well",
                     HTML('<ul style="font-size: 12px; color: #888; margin-top: 0px; ">
                           <li>Columns are renamed for plotting purpose</li>
                           <li>Data grouped by ANALYTE, MATRIX, PANEL, PERIOD, DOSE, NMHR</li>
                           <li>Column N shows the number of subjects within each group</li>
                           </ul>'),
                     fluidRow(
                       column(3, textInput("lloq_value", "Enter LLOQ value", value = "")),
                       column(3, 
                              #br(),
                              #br(),
                              #checkboxInput("apply_blq", "Apply BLQ rules for AM", value = FALSE),
                              #checkboxInput("impute_gm", "Impute 0 values with 1/2 LLOQ for GM", value = FALSE)
                              # div(class = 'tooltip', 
                              #     tags$i(class = 'info-icon fas fa-info-circle',
                              #            tags$span(class = 'tooltiptext', 'Additional information about BLQ rules'))
                              # )
                       )
                     ),
                     fluidRow(
                       column(6, checkboxInput("apply_blq", "Apply BLQ rules", value = FALSE))
                     ),
                     fluidRow(
                       HTML('<ul style="font-size: 12px; color: #888; margin-top: 0px; ">
                             <li>Time point with >50% of subjects that have concentrations lower than LLOQ will not be reported</li>
                             <li>Time point with a mean concentration lower than LLOQ will not be reported</li>
                             <li> Exclusion only applies to the terminal phase of the profile</li>
                             </ul>')
                     ),
                     fluidRow(
                       column(6, checkboxInput("impute_gm", "Impute 0 values with 1/2 LLOQ for GM", value = FALSE))
                     ),
                     fluidRow(
                       HTML('<ul style="font-size: 12px; color: #888; margin-top: 0px; ">
                             <li>Replace 0 values with 1/2 LLOQ.</li>
                             <li>If not checked, GM shows "NC" for groups with 0 as concentration values.</li>
                             </ul>')
                     ),
                     dataTableOutput("data_summary"))
          ),
          navbarMenu("Time-Concentration Plot",
                     tabPanel("SAD",
                              tags$head(
                                tags$script(HTML("
                                  $(document).on('shiny:connected', function(event) {
                                    var sendDims = function() {
                                        setTimeout(function() {
                                          var height = $('#plot').height();
                                          var width = $('#plot').width();
                                          if (width > 0 && height > 0) {
                                            Shiny.setInputValue('plot_height', height, {priority: 'event'});
                                            Shiny.setInputValue('plot_width', width, {priority: 'event'});
                                          }
                                        }, 500); // delay by 500 milliseconds
                                    };
                                    $(window).resize(sendDims);
                                    sendDims();
                                  });
                                "))
                              ),
                              div(
                                class = "well",
                                HTML('<ul style="font-size: 12px; color: #888; margin-top: 0px; ">
                                      <li>For SAD data</li>
                                      <li>Use "Filter Data" to select data subgroups for plotting</li>
                                      <li>The plot shows filtered and unit-converted (if applicable) concentration</li>
                                      <li>Error bar: when plotting AM, SD is used; when plotting GM, geometric SD is used</li>
                                      <li>Use "Submit Changes" button to apply plot customization </li>
                                      </ul>'),
                                plotOutput("plot"),
                                br(),
                                # HTML('<h6 style="font-weight: bold; margin-bottom: 0px;">Groups to exclude</h6>'),
                                # selectInput("exclude_group", "", choices = NULL, multiple = TRUE),
                                # checkboxInput("exclude_check", "Exclude group(s) above from the plot", value = FALSE),
                                tags$hr(),
                                HTML('<h6 style="font-weight: bold;">Plot customization</h6>'),
                                fluidRow(
                                  column(2, textInput("start_time", "Start time", value = 0)),
                                  column(2, textInput("end_time", "End time", value = "")),
                                  column(2, numericInput("x_interval", "Time interval", value = 12))
                                ),
                                fluidRow(
                                  column(4, radioButtons("am_gm", "Summary method", choices = c("AM", "GM"))),
                                  column(4, radioButtons("error_bar", "Error bar", choices = c("On", "Off"), selected = "Off")),
                                  column(4, radioButtons("axis_scale", "Y-axis scale", choices = c("Linear", "Log")))
                                ),
                                fluidRow(
                                  column(6, textInput("pk_target", "Add a reference line for PK target", value = "", placeholder = "Enter a number here")),
                                  column(6, textInput("lloq", "Add a reference line for LLOQ", value = "", placeholder = "Enter a number here"))
                                ),
                                fluidRow(
                                  column(6, textInput("x_axis_title", "Change X-axis title", value = "", placeholder = "Leave blank as default")),
                                  column(6, textInput("y_axis_title", "Change Y-axis title", value = "", placeholder = "Leave blank as default"))
                                ),
                                fluidRow(
                                  column(6, textInput("plot_title", "Enter plot title", value = ""))
                                ),
                                actionButton("submit", "Submit Changes"),
                                br(),
                                br(),
                                downloadButton("download_plot", "Download Plot", class = "btn btn-primary")
                              )
                     ),
                     tabPanel("MAD",
                              tags$head(
                                tags$script(HTML("
                                  $(document).on('shiny:connected', function(event) {
                                    var sendDims = function() {
                                        setTimeout(function() {
                                          var height = $('#plot_mad').height();
                                          var width = $('#plot_mad').width();
                                          if (width > 0 && height > 0) {
                                            Shiny.setInputValue('plot_height_mad', height, {priority: 'event'});
                                            Shiny.setInputValue('plot_width_mad', width, {priority: 'event'});
                                          }
                                        }, 500); // delay by 500 milliseconds
                                    };
                                    $(window).resize(sendDims);
                                    sendDims();
                                  });
                                "))
                              ),
                              div(
                                class = "well",
                                HTML('<ul style="font-size: 12px; color: #888; margin-top: 0px;">
                                      <li>For MAD data</li>
                                      <li>Use "Filter Data" to select data subgroups for plotting</li>
                                      <li>The plot shows filtered and unit-converted (if applicable) concentration</li>
                                      <li>Error bar: when plotting AM, SD is used; when plotting GM, geometric SD is used</li>
                                      <li>Use "Submit Changes" button to apply plot customization</li>
                                      </ul>'),
                                selectInput("select_day", "Select days to plot", choices = NULL, multiple = TRUE),
                                plotOutput("plot_mad"),
                                br(),
                                #HTML('<h6 style="font-weight: bold;">Groups to exclude</h6>'),
                                #selectInput("exclude_group_mad", "", choices = NULL, multiple = TRUE),
                                #checkboxInput("exclude_check_mad", "Exclude group(s) above from the plot", value = FALSE),
                                tags$hr(),
                                HTML('<h6 style="font-weight: bold;">Plot customization</h6>'),
                                fluidRow(
                                  column(2, 
                                         conditionalPanel(
                                           condition = "input.free_x_mad == false",
                                           textInput("start_time_mad", "Start time", value = 0))
                                         ),
                                  column(2, 
                                         conditionalPanel(
                                           condition = "input.free_x_mad == false",
                                           textInput("end_time_mad", "End time", value = ""))
                                         ),
                                  column(2, 
                                         conditionalPanel(
                                           condition = "input.free_x_mad == false",
                                           numericInput("x_interval_mad", "Time interval", value = 6))
                                         )
                                ),
                                fluidRow(
                                  column(4, 
                                         checkboxInput("free_x_mad", "Free X axis scale", value = FALSE))
                                ),
                                fluidRow(
                                  column(4, radioButtons("am_gm_mad", "Summary method", choices = c("AM", "GM"))),
                                  column(4, radioButtons("error_bar_mad", "Error bar", choices = c("On", "Off"), selected = "Off")),
                                  column(4, radioButtons("axis_scale_mad", "Y-axis scale", choices = c("Linear", "Log")))
                                ),
                                fluidRow(
                                  column(6, textInput("pk_target_mad", "Add a reference line for PK target", value = "", placeholder = "Enter a number here")),
                                  column(6, textInput("lloq_mad", "Add a reference line for LLOQ", value = "", placeholder = "Enter a number here"))
                                ),
                                fluidRow(
                                  column(6, textInput("x_axis_title_mad", "Change X-axis title", value = "", placeholder = "Leave blank as default")),
                                  column(6, textInput("y_axis_title_mad", "Change Y-axis title", value = "", placeholder = "Leave blank as default"))
                                ),
                                fluidRow(
                                  column(6, textInput("plot_title_mad", "Enter plot title", value = ""))
                                ),
                                actionButton("submit_mad", "Submit Changes"),
                                br(),
                                br(),
                                downloadButton("download_plot_mad", "Download Plot", class = "btn btn-primary")
                              )
                     )
          ),
          tabPanel("PK Parameter Table",
                   div(
                     class = "well",
                     HTML('<ul style="font-size: 12px; color: #888; margin-top: 0px; ">
                           <li>Upload final PK parameters data before proceed</li>
                           <li>Column N shows the number of subjects within each group</li>
                           <li>To calculate Accumulation Ratio, stratify by "Day"</li>
                           </ul>'),
                     selectInput("select_pk_para", "Select PK parameters to summarize", choices = NULL, multiple = TRUE),
                     selectInput("select_stratify", "Select variable(s) to stratify by", choice = c("Analyte", "Matrix", "Panel", "Period", "Dose", "Day"), multiple = TRUE), 
                     # Currently manually define grouping variables here and using a mapping logic in server code, can be refined in next release
                     selectInput("select_time_var", "Select time variable(s) to be reported in a 'Median (Min-Max)' format", choice = NULL, multiple = TRUE),
                     selectInput("select_ar", "Select variable(s) to calculate Accumulation Ratio (MAD data only)", choice = NULL, multiple = TRUE),
                     #textInput("gm_impute", "Enter a value to impute the 0 values for GM calculation (e.g. 1/2*LLOQ)", value = "", placeholder = "Enter a number here"),
                     #HTML('<div style="font-size: 12px; color: #888;">Time variable(s) selected above are not imputed</div>'),
                     br(),
                     HTML('<h5 style="text-align: center; font-weight: bold;">Summary PK Parameter Table - GM (CV%)</h5>'),
                     br(),
                     HTML('<strong>Please be sure this box is <span style="color: red;">checked</span> before downloading.</strong>'),
                     checkboxInput("mask_n_pk_para", "Mask N from the table", value = TRUE),
                     DTOutput("summary_table"),
                     br(),
                     HTML('<h10>Results are geometric mean (coefficient of variation %) unless otherwise stated.</h10>
                           <br>
                           <h10>Time variables not including t1/2 are reported in median (min-max) format.</h10>
                           <br>
                           <h10>NC (Not Calculable) is reported for parameters where at least one individual had a parameter value of 0, therefore Geometric Mean cannot be calculated.</h10>
                           <br>
                           <h10>A dash "-" is reported for parameters where at least one individual had a missing parameter value.</h10>
                          '),
                     br(),
                     br(),
                     downloadButton("download_html", "Download Table", class = "btn btn-primary")
                   ) # div
          ), # tabPanel
          tabPanel("Dose Proportionality",
                   tags$head(
                     tags$script(HTML("
                                  $(document).on('shiny:connected', function(event) {
                                    var sendDims = function() {
                                        setTimeout(function() {
                                          var height = $('#plot_dp').height();
                                          var width = $('#plot_dp').width();
                                          if (width > 0 && height > 0) {
                                            Shiny.setInputValue('plot_height_dp', height, {priority: 'event'});
                                            Shiny.setInputValue('plot_width_dp', width, {priority: 'event'});
                                          }
                                        }, 500); // delay by 500 milliseconds
                                    };
                                    $(window).resize(sendDims);
                                    sendDims();
                                  });
                                "))
                   ),
                   div(
                     class = "well",
                     HTML('<ul style="font-size: 12px; color: #888; margin-top: 0px; ">
                           <li>Upload final PK parameters data before proceed</li>
                           </ul>'),
                     fluidRow(
                       column(4, selectInput("pkParams", "Select a PK parameter", choices = NULL)),
                       column(4, textInput("PKUnits", "Parameter unit", value = "")),
                       column(4, textInput("doseUnits", "Dose unit", value = ""))
                     ),
                     fluidRow(
                       column(4, selectInput("subjid_exclude", "Subject ID to exclude", choice = NULL, multiple = TRUE))
                     ),
                     tags$hr(),
                     br(),
                     HTML('<h5 style="text-align: center; font-weight: bold;">Dose Proportionality</h5>'),
                     plotOutput("dp_plot"),
                     br(),
                     htmlOutput('PKCI'),
                     tags$hr(),
                     HTML('<h6 style="font-weight: bold;">Plot customization</h6>'),
                     br(),
                     fluidRow(
                       column(8,
                              p(style = "display: inline-block; vertical-align:bottom; width: 125px;", "X-axis limits"),
                              div(style = "display: inline-block; vertical-align:middle; width: 20%;", numericInput('xLower', "Lower", NA, width = '100%')),
                              div(style = "display: inline-block; vertical-align:middle; width: 20%;", numericInput('xUpper', "Upper", NA, width = '100%')),
                              div(style = "display: inline-block; vertical-align:-15px; width: 20%;", checkboxInput('xAuto', "Auto", value = TRUE))
                       )
                     ),
                     fluidRow(
                       column(3,
                              p(style = "display: inline-block; vertical-align:bottom; width: 125px;", "Y-axis scale"),
                              div(style = "display: inline-block; vertical-align:middle; width: 20%;", radioButtons("y_axis_scale", "", choices = c("Linear", "Log")))
                       ),
                       column(3,
                              p(style = "display: inline-block; vertical-align:bottom; width: 125px;", "X-axis scale"),
                              div(style = "display: inline-block; vertical-align:middle; width: 20%;", radioButtons("x_axis_scale", "", choices = c("Linear", "Log")))
                       )
                     ),
                     fluidRow(
                       column(8,
                              p(style = "display: inline-block; vertical-align:bottom; width: 125px;", "Error bar width"),
                              div(style = "display: inline-block; vertical-align:middle; width: 20%;", numericInput('errorBarWidth', "", NA, width = '100%')),
                              div(style = "display: inline-block; width: 20%;", checkboxInput('errorBarAuto', "Auto", TRUE))
                       )
                     ),
                     fluidRow(
                       column(8,
                              p(style = "display: inline-block; width: 150px;", "Prediction interval"),
                              div(style = "display: inline-block; width: 20%;", checkboxInput('removePI', "Remove"))
                       )
                     ),
                     br(),
                     downloadButton("download_dp_plot", "Download Plot", class = "btn btn-primary"),
                     br(),
                     tags$hr(),
                     br(),
                     HTML('<h5 style="text-align: center; font-weight: bold;">Parameter Projections</h5>'),
                     fluidRow(
                       column(4, textInput("projectDoses", label = c("Doses to project"), value = "", placeholder = 'e.g. 200, 400, 800'))
                     ),
                     fluidRow(
                       column(4, textInput("PKEM", label = "PK parameter NOAELs", value = NULL, width = NULL, placeholder = 'Enter a value here'))
                     ),
                     tableOutput('PK_proj'),
                     downloadButton("download_dp_html", "Download Table", class = "btn btn-primary")
                   ) # div
          )# tabPanel
        ) # tabsetPanel
      ) # column for tabsetPanel
    ) # fluidRow
  ), # div
  br()
)



#### Server ####

server <- function(input, output, session) {
  
  #### Pre-defined functions ####
  
  # Customize time stamp
  get_timestamp <- function() {
    format(Sys.time(), "%Y-%m-%d_%H-%M")
  }
  
  # For time-concentration plot and pk parameter table
  geoMean <- function(x) exp(mean(log(x), na.rm = TRUE))
  geoCV <- function(x) 100 * sqrt(exp(sd(log(x), na.rm = TRUE) ^ 2) - 1)
  geoSD <- function(x) exp(sd(log(x), na.rm = TRUE))
  
  # For dose proportionality
  CIL <-
    function(x) {
      exp(mean(log(x[x > 0])) - qt(0.975, length(x) - 1) * sd(log(x)) / sqrt(length(x)))
    }
  CIU <-
    function(x) {
      exp(mean(log(x[x > 0])) + qt(0.975, length(x) - 1) * sd(log(x)) / sqrt(length(x)))
    }
  
  assessDP = function(model = NULL, ci = NULL){
    ci = signif(ci, 3)
    est = signif(model$coefficients[[2]], 3)
    sum = summary(model)
    r2 = signif(sum$r.squared, 2)
    
    if(ci[1]<1 & ci[2]<1) {res = paste0("Parameter slope is ", est, " with 95% CI [", ci[1], ", ", ci[2], "] and r-squared of ", r2, "; assume parameter is less than dose proportional.")
    } else if(ci[1]>1 & ci[2]>1) {res = paste0("Parameter slope is ", est, " with 95% CI [", ci[1], ", ", ci[2], "] and r-squared of ", r2, "; assume parameter is more than dose proportional.")
    } else if(ci[1]<=1 & ci[2]>=1) {res = paste0("Parameter slope is ", est, " with 95% CI [", ci[1], ", ", ci[2], "] and r-squared of ", r2, "; assume parameter is dose proportional.")
    } else {res = paste0("Parameter slope is ", est, " with 95% CI [", ci[1], ", ", ci[2], "] and r-squared of ", r2, "; Could not assess dose proportionality.")}
    
    return(res)
  }
  
  
  #### Upload files ####
  # Initialize reactiveVal for data_raw
  data_raw <- reactiveVal()
  # Store the original, unmodified data
  data_original <- reactiveVal()
  
  current_file_name <- reactiveVal()
  
  processConcentrationData = function(fileName, data) {
    current_file_name(fileName)
    # Determine the file extension
    file_ext <- tools::file_ext(fileName)
    
    if (file_ext == "csv") {
      # For csv files
      headers <- read.csv(data, header = FALSE, nrows = 1, as.is = T)
      
      orresu <- read.csv(data)$PKEXTORRES[1] # extract the concentration unit
      nmhru <- read.csv(data)$PKEXTNMHR[1] # extract NMHR unit
      
      if (!("PKEXTNAM" %in% headers)) {
        # For Watson data
        doseu <- read.csv(data)$PKEXTDOSE[1] # extract dose unit
      }
      
      df <- read.csv(data, skip = 2, header = FALSE) # Skip the first 2 rows (units in the headers of PKEXTNMHR and PKEXTORRRES)
    } else if (file_ext %in% c("xlsx", "xls")) {
      # For excel files
      headers <- readxl::read_excel(data, sheet = 1, range = anchored("A1", dim = c(1, NA)), col_names = FALSE)
      orresu <- readxl::read_excel(data, sheet = 1)$PKEXTORRES[1]
      nmhru <- readxl::read_excel(data, sheet = 1)$PKEXTNMHR[1]
      
      if (!("PKEXTNAM" %in% headers)) {
        # For Watson data
        doseu <- readxl::read_excel(data, sheet = 1)$PKEXTDOSE[1]
      }
      
      df <- readxl::read_excel(data, sheet = 1, skip = 2, col_names = FALSE)
    } else {
      stop("Unsupported file format. Please upload a CSV or Excel file.")
    }
    
    colnames(df) <- headers
    df$ORRESU <- rep(orresu, nrow(df))
    df$NMHRU <- rep(nmhru, nrow(df))
    
    if (!("PKEXTNAM" %in% headers)) {
      # For Watson data
      df$DOSEUNIT <- rep(doseu, nrow(df))
      
      df <- df %>% 
        rename(DOSE = PKEXTDOSE)
    } else {
      # For Ardena data
      df <- df %>% 
        mutate(
          DOSE = as.numeric(gsub("[^0-9]", "", PKEXTDOSE)),
          DOSEUNIT = gsub("[0-9 ]", "", PKEXTDOSE)
        ) %>% # Can accommodate both "10 mg" and "10mg" formats
        select(- PKEXTDOSE)
    }
    
    df <- df %>% 
      mutate(PKEXTSTDYDY = if_else(
        PKEXTSTDYDY != "" & !is.na(PKEXTSTDYDY),
        # Can read both "Day1" and "1" formats
        as.numeric(gsub("[^0-9]", "", PKEXTSTDYDY)),
        NA_real_
      )) %>% 
      {
        if ("PKEXTPERIOD" %in% names(.)) {
          # Populate empty PKEXTPERIOD values with 1 (Ardena MAD)
          mutate(., PKEXTPERIOD = if_else(is.na(PKEXTPERIOD) | PKEXTPERIOD == "", 1, PKEXTPERIOD))
        } else {
          # Add the PKEXTPERIOD column if it doesn't exist and populate with 1 (Watson MAD)
          mutate(., PKEXTPERIOD = 1)
        }
      } %>% 
      select(PKEXTSTUDYID, PKEXTSUBJID, PKEXTPKSEQ, PKEXTPERIOD, PKEXTSTDYDY, PKEXTNMHR, NMHRU, PKEXTORRES, ORRESU, DOSE, DOSEUNIT, everything()) %>%
      arrange(PKEXTSTUDYID, PKEXTPKSEQ, PKEXTPERIOD, PKEXTSTDYDY, PKEXTNMHR, DOSE, PKEXTSUBJID) %>% 
      mutate(PKEXTORRES = as.numeric(as.character(PKEXTORRES)))
    
    data_raw(df)  # Set the reactiveVal
    data_original(df)
  }
  
  # Read the concentration data from input file
  observe({
    req(input$file)
    
    processConcentrationData(input$file$name, input$file$datapath)
  })
  
  
  #### Data check ####
  
  # Update "From unit" selection base on the concentration unit in ORRESU
  # observe({
  #   req(data_original())
  #   # Get unique ORRESU values for the drop-down menu
  #   unit_choice <- unique(data_original()$ORRESU)
  #   # Update the choices in the selectInput
  #   updateSelectInput(session, "from_unit", choices = unit_choice)
  # })
  
  
  # Unit conversion for concentration column
  converted_column <- reactive({
    # Check if MW input exists and is not NA; otherwise, set mw to 1
    if (is.null(input$MW) || input$MW == "" || is.na(as.numeric(input$MW))) {
      # Allow for conversions that don't need mw
      # e.g. From nM to uM
      mw <- 1
    } else {
      mw <- as.numeric(input$MW)
    }
    
    conc0 <- as.numeric(data_original()$PKEXTORRES) # Always base the conversion on data_original
    
    # # Normalize the unit inputs to handle "xM" and "xmol/L" equivalently
    # normalize_unit <- function(unit) {
    #  gsub("mol/L", "M", unit)
    # }
    
    # Conversion factors
    conversion_factors <- c(
      "pM" = 1e-12,
      "nM" = 1e-9,
      "uM" = 1e-6,
      "mM" = 1e-3,
      "pg/mL" = 1e-12,
      "ng/mL" = 1e-9,
      "ug/mL" = 1e-6,
      "mg/mL" = 1e-3
    )
    
    ## Normalize units before looking up conversion factors
    #from_unit_normalized <- normalize_unit(input$from_unit)
    #to_unit_normalized <- normalize_unit(input$to_unit)
    
    from_factor <- conversion_factors[[input$from_unit]]
    to_factor <- conversion_factors[[input$to_unit]]
    
    if (grepl("g/mL$", input$from_unit) == TRUE) {
      converted_column <- ((from_factor / to_factor) * conc0) / mw # Convert from xg/mL to xM
    } else {
      converted_column <- ((from_factor / to_factor) * conc0) * mw # Convert from xM to xg/mL
    }
    
    # if (grepl("g/mL$", input$from_unit) == TRUE && grepl("M$", input$to_unit) == TRUE) {
    #   converted_column <- ((from_factor / to_factor) * conc0) / mw # Convert from xg/mL to xM
    # } else if (grepl("M$", input$from_unit) == TRUE && grepl("g/mL$", input$to_unit) == TRUE) {
    #   converted_column <- ((from_factor / to_factor) * conc0) * mw # Convert from xM to xg/mL
    # } else {
    #   converted_column <- conc0
    # }
  })
  
  # When the Convert button is clicked, update data_raw based on the conversion
  observeEvent(input$convert, {
    df <- data_original()  # Get original data
    df$PKEXTORRES <- converted_column()  # Update the converted concentration value
    df$ORRESU <- input$to_unit # Update the converted unit
    data_raw(df)
  })
  
  
  # Return a DataTable with options for data_raw
  output$raw_data <- renderDataTable({
    req(data_raw())
    
    datatable(data_raw(),
              class = 'display', # Table format
              caption = "Scroll right for more columns",
              filter = 'top', # Column filters
              options = list(
                pageLength = 10, # Number of rows per page
                scrollX = TRUE, # Enable horizontal scrolling
                searchHighlight = TRUE # Enable highlighted searching content
                #autoWidth = TRUE # Enable automatic column width adjustment
                #fixedHeader = TRUE # Enable the FixedHeader extension
              )
    )
  })
  
  
  
  #### Mean Concentration Data ####
  
  #### Filter data ####
  # Update analyte choices
  observe({
    req(data_raw())
    # Get unique analyte for the drop-down filter
    analyte_choices <- unique(data_raw()$PKEXTSRCANL)
    # Update the choices
    updateSelectInput(session, "select_analyte", choices = analyte_choices, selected = analyte_choices[1])
  })
  
  # Filter data by selected analyte
  data_filtered_analyte <- reactive({
    req(data_raw(), input$select_analyte)
    data_raw() %>%
      filter(PKEXTSRCANL %in% input$select_analyte)
  })
  
  # Update matrix choices
  observe({
    req(data_filtered_analyte())
    matrix_choices <- unique(data_filtered_analyte()$PKEXTSPECAB)
    updateSelectInput(session, "select_matrix", choices = matrix_choices, selected = matrix_choices)
  })
  
  # Filter data by selected matrices
  data_filtered_matrix <- reactive({
    req(data_filtered_analyte(), input$select_matrix)
    data_filtered_analyte() %>%
      filter(PKEXTSPECAB %in% input$select_matrix)
  })
  
  
  # Update panel choices
  observe({
    req(data_filtered_matrix())
    panel_choices <- unique(data_filtered_matrix()$PKEXTPKSEQ)
    updateSelectInput(session, "select_panel", choices = panel_choices, selected = panel_choices)
  })
  
  # Filter data by selected panels
  data_filtered_panel <- reactive({
    req(data_filtered_matrix(), input$select_panel)
    data_filtered_matrix() %>%
      filter(PKEXTPKSEQ %in% input$select_panel)
  })
  
  
  # Update period choices
  observe({
    req(data_filtered_panel())
    
    period_choices <- unique(data_filtered_panel()$PKEXTPERIOD)
    period_choices <- sort(as.numeric(period_choices))
    
    updateSelectInput(session, "select_period", choices = period_choices, selected = period_choices)
  })
  
  # Filter data by selected periods
  data_filtered_matrix_period <- reactive({
    req(data_filtered_panel(), input$select_period)
    
    data_filtered_panel() %>%
      filter(PKEXTPERIOD %in% input$select_period)
  })
  
  # Update dose choices
  observe({
    req(data_filtered_matrix_period())
    
    dose_choices <- unique(data_filtered_matrix_period()$DOSE)
    dose_choices <- sort(as.numeric(dose_choices))
    
    updateSelectInput(session, "select_dose", choices = dose_choices, selected = dose_choices)
  })
  
  # Filter data by selected doses
  data_filtered_matrix_period_dose <- reactive({
    req(data_filtered_matrix_period(), input$select_dose) # selected = dose_choices
    
    data_filtered_matrix_period() %>%
      filter(DOSE %in% input$select_dose)
  })
  
  # Extract data groups
  group_info <- reactive({
    req(data_filtered_matrix_period_dose())
    
    data_filtered_matrix_period_dose() %>%
      select(PKEXTSRCANL, PKEXTSPECAB, PKEXTPKSEQ, PKEXTPERIOD, DOSE) %>%
      distinct() %>%
      mutate(concatenated = paste(PKEXTSRCANL, PKEXTSPECAB, PKEXTPKSEQ, PKEXTPERIOD, DOSE, sep = "_"))
  })
  
  # Update drop down menu
  observe({
    updateSelectInput(session, "exclude_group", choices = group_info()$concatenated)
  })
  
  
  # Exclude selected groups
  data_filtered <- reactive({
    req(data_filtered_matrix_period_dose())
    
    data <- data_filtered_matrix_period_dose()
    
    if (input$exclude_check && !is.null(input$exclude_group) && length(input$exclude_group) > 0) {
      data <- data %>%
        mutate(concatenated = paste(PKEXTSRCANL, PKEXTSPECAB, PKEXTPKSEQ, PKEXTPERIOD, DOSE, sep = "_")) %>%
        filter(!concatenated %in% input$exclude_group)
    }
    
    data
  })
  
  
  # Summary level data
  data <- reactive({
    req(data_filtered())
    
    # Initial data preparation
    prepared_data <- data_filtered() %>%
      rename_all(~gsub("PKEXT", "", .)) %>%
      rename(
        ANALYTE = SRCANL, 
        MATRIX = SPECAB, 
        PANEL = PKSEQ, 
        CONCENTRATION = ORRES,
        CONC_UNIT = ORRESU,
        NMHR_UNIT = NMHRU,
        DAY = STDYDY
      ) %>% 
      mutate(CONCENTRATION_GM = CONCENTRATION) # Copy the column for GM 1/2 lloq replacement
    
    # Check if "Apply BLQ Rules" checkbox is checked
    if (input$apply_blq && !is.na(input$lloq_value)) {
      # Attempt to convert LLOQ value to numeric, capture potential NA if conversion fails
      lloq_value <- as.numeric(input$lloq_value)
      
      # Calculate the percentage of values below LLOQ and mean concentration for each NMHR
      blq_info <- prepared_data %>%
        group_by(ANALYTE, MATRIX, PANEL, PERIOD, DOSE, DOSEUNIT, NMHR, DAY) %>% 
        summarise(
          below_lloq_pct = mean(CONCENTRATION < lloq_value, na.rm = TRUE) * 100,
          mean_concentration = mean(CONCENTRATION, na.rm = TRUE),
          .groups = 'drop'
        )
      
      # print(blq_info)
      
      # Identify groups to exclude based on BLQ rules
      exclude_groups <- blq_info %>%
        filter(below_lloq_pct > 50 | mean_concentration < lloq_value) %>%
        filter(NMHR != 0) %>% 
        mutate(key = paste(ANALYTE, MATRIX, PANEL, PERIOD, DOSE, DOSEUNIT, NMHR, DAY, sep = "_")) %>%
        pull(key)
      
      # print("exclude groups: ")
      # print(head(exclude_groups))
      
      tmax_tbl <- prepared_data %>%
        group_by(ANALYTE, MATRIX, PANEL, PERIOD, DOSE, DOSEUNIT, DAY) %>%
        summarise(
          tmax = NMHR[which.max(CONCENTRATION)],
          .groups = "drop"
        )
      
      print(tmax_tbl)
      
    #   # Apply exclusion rule
    #   prepared_data <- prepared_data %>%
    #     mutate(key = paste(ANALYTE, MATRIX, PANEL, PERIOD, DOSE, DOSEUNIT, NMHR, DAY, sep = "_")) %>%
    #     mutate(BLQ_EXCLUDED = if_else(key %in% exclude_groups, "Y", "N")) %>% 
    #     select(-key)
    #   
    # } else {
    #   prepared_data <- prepared_data %>%
    #     mutate(BLQ_EXCLUDED = "N")  # Set all to "N" if BLQ rules are not applied
    # }
      
      # 01/23/2026 UPDATED: apply exclusion rule only AFTER tmax
      prepared_data <- prepared_data %>%
        left_join(
          tmax_tbl,
          by = c("ANALYTE", "MATRIX", "PANEL", "PERIOD", "DOSE", "DOSEUNIT", "DAY")
        ) %>%
        mutate(
          key = paste(ANALYTE, MATRIX, PANEL, PERIOD, DOSE, DOSEUNIT, NMHR, DAY, sep = "_"),
          BLQ_EXCLUDED = if_else(
            key %in% exclude_groups & NMHR > tmax,
            "Y", "N"
          )
        ) %>%
        select(-key, -tmax)
      } else {
        prepared_data <- prepared_data %>%
          mutate(BLQ_EXCLUDED = "N")  # Set all to "N" if BLQ rules are not applied
      }
    
    
    # impute 0s in CONCENTRATION with 1/2 lloq for GM calculation
    if (input$impute_gm == TRUE & !is.na(input$lloq_value)) {
      
      lloq_value <- as.numeric(input$lloq_value)
      
      prepared_data <- prepared_data %>%  
        mutate(CONCENTRATION_GM = if_else(NMHR != 0 & CONCENTRATION_GM == 0, 1/2 * lloq_value, CONCENTRATION_GM))
      
    }
    
    # Data summarization
    summarized_data <- prepared_data %>%
      group_by(ANALYTE, MATRIX, PANEL, PERIOD, DOSE, DOSEUNIT, NMHR, DAY) %>% 
      summarise(
        BLQ_EXCLUDED = first(BLQ_EXCLUDED), # first() ensures that the non-grouped columns correctly handle potential conflicts
        ANALYTE = first(ANALYTE),
        GM = geoMean(CONCENTRATION_GM),
        GCV = geoCV(CONCENTRATION_GM),
        GSD = geoSD(CONCENTRATION_GM),
        AM = mean(CONCENTRATION),
        SD = sd(CONCENTRATION),
        CONC_UNIT = first(CONC_UNIT),
        NMHR_UNIT = first(NMHR_UNIT),
        N = n()
      ) %>%
      filter(!is.na(SD)) %>% # SD is NA for groups with only 1 subject
      ungroup() %>%
      mutate(GM = ifelse(NMHR != 0 & GM == 0, "NC", GM)) %>% # Replace 0 with "NC" where NMHR is not 0
      select(ANALYTE, MATRIX, PANEL, PERIOD, DAY, NMHR, DOSE, DOSEUNIT, BLQ_EXCLUDED, N, everything()) %>%
      arrange(DOSE, PERIOD, DAY, NMHR) %>%
      unique() # To avoid otherwise duplicated columns
    
    summarized_data
  })
  
  # Return a DataTable with options for Data Summary
  output$data_summary <- renderDataTable({
    req(data())
    
    # Identify numeric columns and round them
    # According to NCA guidance, time variables: 2 decimal places; calculated variables: 3
    numeric_cols <- sapply(data(), is.numeric)
    data_rounded <- data() # Create a new reactive item for rounded data
    data_rounded[numeric_cols] <- lapply(data_rounded[numeric_cols], function(x) round(x, digits = 3))
    
    # Define options for DataTable
    dt_options <- list(
      pageLength = 10, 
      scrollX = TRUE,
      searchHighlight = TRUE
    )
    
    datatable(data_rounded,
              rownames = FALSE,
              options = dt_options)
  })
  
  
  #### Time-concentration plot ####  
  
  #### SAD ####
  
  # Plot Options
  # Define a reactive value for each text box input
  pk_target_input <- reactiveVal()
  lloq_input <- reactiveVal()
  plot_title_input <- reactiveVal()
  
  # Observe when the "Submit" button is clicked
  observeEvent(input$submit, {
    # Capture the input values from the text boxes
    pk_target_input(as.numeric(input$pk_target))
    lloq_input(as.numeric(input$lloq))
    plot_title_input(input$plot_title)
  })
  
  # Create a reactive expression for y_mean so it can be referred in other functions
  # Outside of reactive plot since it is also referred in renderPlot
  y_mean_reactive <- reactive({
    if (input$am_gm == "AM") "AM" else "GM"
  })
  
  y_sd_reactive <- reactive({
    if (input$am_gm == "AM") "SD" else "GSD"
  })
  
  
  # Create reactive plot  
  plot_output <- reactive({
    
    data_source <- data() %>%
      filter(BLQ_EXCLUDED == "N")
    
    # Filter out GM = NC
    if (input$am_gm == "GM"){
      data_source <- data_source %>% 
        filter(GM != "NC") %>% 
        mutate(GM = as.numeric(GM)) # take care of the non-numeric 0 introduced in summary data
    }
    
    # Update x axis plotting range using the min and max of NMHR
    #if (input$start_time == "" || input$end_time == "") {
    if (input$end_time == "") { # 48 is the arbitrary number set in UI
      updateTextInput(session, "start_time", value = min(data_source$NMHR, na.rm = TRUE))
      updateTextInput(session, "end_time", value = max(data_source$NMHR, na.rm = TRUE))
    }
    
    # Use y_mean_reactive to assign y_mean
    y_mean <- y_mean_reactive()
    
    y_sd <- y_sd_reactive()
    
    # Calculate the maximum y-axis limit for the use of PK target label padding
    max_y <- max(data_source[[y_mean]] + data_source$SD)
    
    data_source <- data_source %>% 
      mutate(
        # Create a sorted factor for the interaction variable
        interaction_var = interaction(PANEL, PERIOD, DOSE, sep = " - "),
        interaction_var = factor(interaction_var, 
                                 levels = unique(interaction(PANEL, PERIOD, DOSE, sep = " - "))[order(PANEL, PERIOD, DOSE)])
      )
    
    if(nrow(data_source) > 0) {
      p <- ggplot(data = data_source,
                  aes(x = NMHR, y = .data[[y_mean]], color = interaction_var)) +
                    # interaction(PERIOD, as.numeric(DOSE), sep = " - ")
        geom_line(linewidth = 0.6) +
        geom_point(size = 1.5) +
        coord_cartesian(xlim = c(as.numeric(input$start_time), as.numeric(input$end_time))) +
        scale_x_continuous(breaks = seq(as.numeric(input$start_time), as.numeric(input$end_time), by = as.numeric(input$x_interval))) +
        theme_bw() +
        scale_color_discrete(name = paste0("Panel - Period - Dose (", unique(data()$DOSEUNIT), ")")) +
        xlab(ifelse(input$x_axis_title != "",
                    input$x_axis_title,
                    paste0("\n Time (", unique(data()$NMHR_UNIT), ")"))) +
        ylab(ifelse(input$y_axis_title != "",
                    input$y_axis_title,
                    paste0(unique(data()$ANALYTE), " Concentration (", unique(data()$CONC_UNIT), ") \n"))) +
        ggtitle(plot_title_input()) +
        geom_hline(yintercept = pk_target_input(), linetype = "dashed", size = 0.6, color = "red") + # PK target
        # annotate(geom = "text",
        #          x = (as.numeric(input$end_time)/5)*4, # On the right side, depends on actual plot ratio
        #          y = pk_target_input() + max_y/20, # Use max_y/20 as padding space so the displayed space between dashed line and text will be consistent across different plots
        #          label = paste0("PK Target = ", pk_target_input(), " ", unique(data()$CONC_UNIT)),
        #          size = 4, hjust = 1, color = "red") +
        geom_hline(yintercept = lloq_input(), linetype = "dashed", size = 0.6, color = "black") + # LLOQ
        # annotate(geom = "text",
        #          #x = Inf,
        #          x = (as.numeric(input$end_time)/5)*4,
        #          y = lloq_input() + max_y/20,
        #          label = paste0("LLOQ = ", lloq_input(), " ", unique(data()$CONC_UNIT)),
        #          size = 4,
        #          hjust = 1,
        #          color = "black") +
        theme(plot.title = element_text(hjust = 0.5), # Center the title
              plot.caption = element_text(hjust = 0)) + # Left-align the footnote
        labs(
          caption = {
            # Initialize an empty vector to hold caption lines
            caption_lines <- c()
            
            # Add PK Target line if input is provided and valid
            if (!is.null(pk_target_input()) && !is.na(pk_target_input())) {
              caption_lines <- c(caption_lines, paste0("PK Target (red dashed line): ", pk_target_input(), " ", unique(data()$CONC_UNIT)))
            }
            
            # Add LLOQ line if input is provided and valid
            if (!is.null(lloq_input()) && !is.na(lloq_input())) {
              caption_lines <- c(caption_lines, paste0("LLOQ (black dashed line): ", lloq_input(), " ", unique(data()$CONC_UNIT)))
            }
            
            # Combine caption lines with line breaks, or set to NULL if empty
            if (length(caption_lines) > 0) {
              paste(caption_lines, collapse = "\n")
            } else {
              NULL  # No caption if both inputs are NULL or empty
            }
          }
        ) # Footnote
      
      
    } else {
      # Provide a message or handle the situation when there are no data points
      p <- ggplot() + labs(title = "No data points in the selected X-axis range")
    }
    
    # Switch to log scale based on radio button selection
    if (input$axis_scale == "Log") {
      
      log_max_y <- ceiling(log10(max_y))
      dynamic_breaks <- 10^(seq(0, log_max_y, by = 1)) # Create dynamic breaks
      
      p <- p + scale_y_log10(
        breaks = dynamic_breaks, # Specify the dynamic breaks
        labels = scales::label_comma() # Insert a comma every three digits
      ) +
        expand_limits(y = dynamic_breaks) + # Ensure the y-axis includes the top break value
        annotation_logticks(sides = "lr", size = 0.3) + # Add log scale tick marks, "l" is for left, etc.
        theme(panel.grid.minor = element_blank()) # Hide the minor grid lines because they don't align with the ticks
    }
    
    # Add error bars to plot based on radio button selection
    if (input$error_bar == "On") {
      # y_mean can be referred here
      if (input$am_gm == "AM") {
      p <- p + geom_errorbar(aes(x = NMHR, ymin = .data[[y_mean]] - .data[[y_sd]], ymax = .data[[y_mean]] + .data[[y_sd]]), width = 0.6) # , position = position_dodge(0.2)
      }
      if (input$am_gm == "GM") {
        p <- p + geom_errorbar(aes(x = NMHR, ymin = .data[[y_mean]] / .data[[y_sd]], ymax = .data[[y_mean]] * .data[[y_sd]]), width = 0.6)
      }
    }
    
    # Return plot object
    p
  })
  
  # Show plot
  output$plot <- renderPlot({
    req(input$select_matrix, input$select_period)
    
    plot_output()
    
  }, res = 96)

  # Download plot
  output$download_plot <- downloadHandler(
    filename = function() {
      if (!is.null(input$plot_title) && nzchar(input$plot_title)) { # nzchar checks if the string has any characters
        title <- input$plot_title
      } else {
        title <- "time_concentration"
      }
      paste0(title, "_", gsub("[^0-9]","", get_timestamp()), ".png") # Save as png for better plot quality
    },
    content = function(file) {
      # Default to 8x6 inches if dimensions are zero or unavailable
      width_in <- if (!is.null(input$plot_width) && input$plot_width > 0) input$plot_width / 96 else 8
      height_in <- if (!is.null(input$plot_height) && input$plot_height > 0) input$plot_height / 96 else 6
      
      tryCatch({
        ggsave(file, plot_output(), width = width_in, height = height_in, device = "png")
      }, error = function(e) {
        cat("Error in downloading plot:", e$message, "\n")
      })
    }
  )
  
  #### Uploading plot to S3 ####
  #### MAD ####
  
  # Populate day-related drop down menus
  observe({
    req(data_filtered())
    
    select_day_choices <- unique(data_filtered()$PKEXTSTDYDY)
    select_day_choices <- sort(as.numeric(select_day_choices))
    
    updateSelectInput(session, "select_day", choices = select_day_choices)
  })
  
  # Plot Options
  # Define a reactive value for each text box input
  pk_target_input_mad <- reactiveVal()
  lloq_input_mad <- reactiveVal()
  plot_title_input_mad <- reactiveVal()
  
  # Observe when the "Submit" button is clicked
  observeEvent(input$submit_mad, {
    # Capture the input values from the text boxes
    pk_target_input_mad(as.numeric(input$pk_target_mad))
    lloq_input_mad(as.numeric(input$lloq_mad))
    plot_title_input_mad(input$plot_title_mad)
  })
  
  y_mean_reactive_mad <- reactive({
    if (input$am_gm_mad == "AM") "AM" else "GM"
  })
  
  y_sd_reactive_mad <- reactive({
    if (input$am_gm_mad == "AM") "SD" else "GSD"
  })
  
  
  # Mapping function to convert numeric days to descriptive labels
  day_labeler <- function(value) {
    return(paste("Day", value))
  }
  
  
  # Create reactive plot
  plot_output_mad <- reactive({
    
    # Map user selection of free x scale for facet wrap
    scale_parameter <- case_when(
      isTruthy(input$free_x_mad) ~ "free_x",
      TRUE ~ "fixed" # Default
    )
    
    data_source <- data() %>%
      filter(BLQ_EXCLUDED == "N")
    
    # Filter out GM = NC
    if (input$am_gm_mad == "GM"){
      data_source <- data_source %>% 
        filter(GM != "NC") %>% 
        mutate(GM = as.numeric(GM)) # take care of the non-numeric 0 introduced in summary data
    }
    
    # Update x axis plotting range using the min and max of NMHR
    if (input$end_time_mad == "") {
      updateTextInput(session, "start_time_mad", value = min(data_source$NMHR, na.rm = TRUE))
      updateTextInput(session, "end_time_mad", value = max(data_source$NMHR, na.rm = TRUE))
    }
    
    # Use y_mean_reactive to assign y_mean
    y_mean_mad <- y_mean_reactive_mad()
    
    y_sd_mad <- y_sd_reactive_mad()
    
    # Calculate the maximum y-axis limit for the use of PK target label padding
    max_y_mad <- max(data_source[[y_mean_mad]] + data_source$SD)
    
    pk_target_value_mad <- pk_target_input_mad()
    
    # Use selected days for facet_wrap
    selected_days <- input$select_day
    
    if (length(selected_days) > 0) {
      
      if(nrow(data_source) > 0) {
        
        data_source <- data_source %>% 
          filter(DAY %in% selected_days) %>% 
          mutate(
            # Create a sorted factor for the interaction variable
            interaction_var = interaction(PANEL, PERIOD, DOSE, sep = " - "),
            interaction_var = factor(interaction_var, 
                                     levels = unique(interaction(PANEL, PERIOD, DOSE, sep = " - "))[order(PANEL, PERIOD, DOSE)])
          )
        
        p <- ggplot(data_source, aes(x = NMHR, y = .data[[y_mean_mad]], color = interaction_var)) +
          geom_line(linewidth = 0.6) +
          geom_point(size = 1.5) +
          facet_wrap(~ DAY, 
                     scales = scale_parameter,
                     nrow = 1,
                     labeller = labeller(DAY = day_labeler)) +
          labs(color = paste0("Panel - Period - Dose (", unique(data_source$DOSEUNIT), ")")) +
          theme_bw() +
          ylab(ifelse(input$y_axis_title_mad != "",
                      input$y_axis_title_mad,
                      paste0(unique(data_source$ANALYTE), " Concentration (", unique(data_source$CONC_UNIT), ") \n"))) +
          xlab(paste0("Time (", unique(data_source$NMHR_UNIT), ")")) +
          ggtitle(plot_title_input_mad()) +
          geom_hline(yintercept = pk_target_value_mad, linetype = "dashed", size = 0.6, color = "red") +
          # annotate(geom = "text",
          #          x = (as.numeric(input$end_time_mad)/5)*4,
          #          y = pk_target_value_mad + max_y_mad/20,
          #          label = paste0("PK Target = ", pk_target_value_mad, " ", unique(data_source$CONC_UNIT)),
          #          size = 4, hjust = 1, color = "red") +
          geom_hline(yintercept = lloq_input_mad(), linetype = "dashed", size = 0.6, color = "black") +
          # annotate(geom = "text",
          #          x = (as.numeric(input$end_time_mad)/5)*4,
          #          y = lloq_input_mad() + max_y_mad/20,
          #          label = paste0("LLOQ = ", lloq_input_mad(), " ", unique(data_source$CONC_UNIT)),
          #          size = 4,
          #          hjust = 1,
          #          color = "black") +
          theme(plot.title = element_text(hjust = 0.5), # Center the title
                plot.caption = element_text(hjust = 0)) + # Left-align the footnote
          labs(
            caption = {
              # Initialize an empty vector to hold caption lines
              caption_lines_mad <- c()
              
              # Add PK Target line if input is provided
              if (!is.null(pk_target_input_mad()) && !is.na(pk_target_input_mad())) {
                caption_lines_mad <- c(caption_lines_mad, paste0("PK Target (red dashed line): ", pk_target_input_mad(), " ", unique(data_source$CONC_UNIT)))
              }
              
              # Add LLOQ line if input is provided
              if (!is.null(lloq_input_mad()) && !is.na(lloq_input_mad())) {
                caption_lines_mad <- c(caption_lines_mad, paste0("LLOQ (black dashed line): ", lloq_input_mad(), " ", unique(data_source$CONC_UNIT)))
              }
              
              # Combine caption lines with line breaks, or set to NULL if empty
              if (length(caption_lines_mad) > 0) {
                paste(caption_lines_mad, collapse = "\n")
              } else {
                NULL  # No caption if both inputs are NULL or empty
              }
            }
          )  # Footnote
        
        # Use user input for start, end, interval only when x axis scale is fixed
        if (scale_parameter == "fixed") {
          p <- p +
            coord_cartesian(xlim = c(as.numeric(input$start_time_mad), as.numeric(input$end_time_mad))) +
            scale_x_continuous(breaks = seq(as.numeric(input$start_time_mad), as.numeric(input$end_time_mad), by = as.numeric(input$x_interval_mad)))
        }
        
        if (input$axis_scale_mad == "Log") {
          log_max_y_mad <- ceiling(log10(max_y_mad))
          dynamic_breaks_mad <- c(0.01, 0.1, 1, 10^(seq(0, log_max_y_mad, by = 1))) # Create dynamic breaks
          
          p <- p + scale_y_log10(
            breaks = dynamic_breaks_mad, # Specify the dynamic breaks
            labels = scales::label_comma() # Insert a comma every three digits
          ) +
            expand_limits(y = dynamic_breaks_mad) + # Ensure the y-axis includes the top break value
            annotation_logticks(sides = "lr", size = 0.3) + # Add log scale tick marks, "l" is for left, etc.
            theme(panel.grid.minor = element_blank()) # Hide the minor grid lines because they don't align with the ticks
        }
        
        if (input$error_bar_mad == "On") {
          if (input$am_gm_mad == "AM") {
            p <- p + geom_errorbar(aes(x = NMHR, ymin = .data[[y_mean_mad]] - .data[[y_sd_mad]], ymax = .data[[y_mean_mad]] + .data[[y_sd_mad]]), width = 0.6)
          }
          if (input$am_gm_mad == "GM") {
            p <- p + geom_errorbar(aes(x = NMHR, ymin = .data[[y_mean_mad]] / .data[[y_sd_mad]], ymax = .data[[y_mean_mad]] * .data[[y_sd_mad]]), width = 0.6)
          }
        }
        
      } else {
        p <- ggplot() + labs(title = "No data points in the selected X-axis range")
      }
    } 
    
    p
  })
  
  # Show plot
  output$plot_mad <- renderPlot({
    req(input$select_matrix, input$select_period)
    
    plot_output_mad()
    
  }, res = 96)
  
  # Download plot
  output$download_plot_mad <- downloadHandler(
    filename = function() {
      if (!is.null(input$plot_title_mad) && nzchar(input$plot_title_mad)) { # nzchar checks if the string has any characters
        title <- input$plot_title_mad
      } else {
        title <- "time_concentration"
      }
      paste0(title, "_", gsub("[^0-9]","", get_timestamp()), ".png") # Save as png for better plot quality
    },
    content = function(file) {
      width_in_mad <- if (!is.null(input$plot_width_mad) && input$plot_width_mad > 0) input$plot_width_mad / 96 else 8
      height_in_mad <- if (!is.null(input$plot_height_mad) && input$plot_height_mad > 0) input$plot_height_mad / 96 else 6
      
      tryCatch({
        ggsave(file, plot_output_mad(), width = width_in_mad, height = height_in_mad, device = "png")
      }, error = function(e) {
        cat("Error in downloading plot:", e$message, "\n")
      })
    }
  )
  
  # Save plot as png into S3 bucket
  
  #### PK parameter table ####
  
  # Initialize reactiveVal for pk_para_file
  pk_para_file <- reactiveVal()
  
  current_parameter_file_name <- reactiveVal()
  
  processFinalParameterData = function(fileName, data) {
    current_parameter_file_name(fileName)
    
    file_ext <- tools::file_ext(fileName)
    
    if (file_ext == "csv") {
      
      df <- read.csv(data)
      
      if (any(is.na(df$Parameter[1]) || df$Parameter[1] == "")) { # For Watson data, Parameter column
        doseu <- df$PKEXTDOSE[1]
        df <- df[-1, ]  # Delete the column header row with dose unit
        
        df <- df %>% 
          mutate(PKEXTDOSE = paste0(PKEXTDOSE, " ", doseu))
      }
      
    } else if (file_ext %in% c("xlsx", "xls")) {
      df <- readxl::read_excel(data, sheet = 1)
      
      if (any(is.na(df$Parameter[1]) || df$Parameter[1] == "")) { # For Watson data
        doseu <- df$PKEXTDOSE[1]
        df <- df[-1, ]
        
        df <- df %>% 
          mutate(PKEXTDOSE = paste0(PKEXTDOSE, " ", doseu))
      }
      
    } else {
      stop("Unsupported file format. Please upload a CSV or Excel file.")
    }
    
    pk_para_file(df)  # Set the reactiveVal
  }
  
  # Read the file parameter data from input file
  observe({
    req(input$file_para)
    
    processFinalParameterData(input$file_para$name, input$file_para$datapath)
  })
  
  # Make parameters in Parameter column expand to different columns
  pk_para_transformed <- reactive({
    #pk_para_wider <- reactive({
    req(pk_para_file())
    
    pk_para_data <- pk_para_file()  # Extract the data frame from the reactiveVal
    
    wider_data <- pk_para_data %>% # Should only pass the data frame (instead of pk_para_file()) to pivot_wider
      filter(Parameter != "Flag_AUC_%Ext_pred") %>% # Filter out the rows with "Parameter" == "Flag_AUC_%Ext_pred"
      select(-Units) %>% # Avoid duplicated rows with multiple NAs, which will generate confusing N in group_by 
      pivot_wider(names_from = Parameter, values_from = Estimate) %>% # values_fill = 0 populate NA with 0
      mutate(PKEXTDOSE = as.numeric(gsub("[^0-9.]", "", PKEXTDOSE))) %>% # Get rid of the unit in original PKEXTDOSE column so it can be arranged
      arrange(PKEXTDOSE) %>%
      {if (!"PKEXTPERIOD" %in% names(.)) mutate(., PKEXTPERIOD = 1) else .} %>% # Add the PKEXTPERIOD column if it doesn't exist and populate with 1 (Watson MAD)
      mutate(PKEXTPERIOD = if_else(is.na(PKEXTPERIOD) | PKEXTPERIOD == "", 1, PKEXTPERIOD)) %>% # Populate empty PKEXTPERIOD values with 1 (Ardena MAD)
      rename(
             # "t1/2" = HL_Lambda_z,
             # "AUC0-inf" = AUCINF_pred,
             # "AUC0-last" = AUClast,
             "Analyte" = PKEXTSRCANL,
             "Matrix" = PKEXTSPECAB,
             "Dose" = PKEXTDOSE,
             "Period" = PKEXTPERIOD,
             "Panel" = PKEXTPKSEQ,
             "Day" = PKEXTSTDYDY) %>% # First rename using initial renaming to match the renaming vector keys
      mutate(Day = as.numeric(gsub("[^0-9.]", "", Day))) # Day can be empty
    
    wider_data
  })
  
  
  # # Replace 0 values with user input
  # # Isolate user input to avoid re-evaluation on pk_para_wider()
  # if (!is.null(input$select_pk_para) && !is.null(input$gm_impute)) {
  #   
  #   # Determine columns to modify based on the presence of select_time_var
  #   cols_to_modify <- input$select_pk_para
  #   
  #   # Only impute non-time variables
  #   if(!is.null(input$select_time_var)) {
  #     cols_to_modify <- setdiff(input$select_pk_para, input$select_time_var)
  #   }
  #   
  #   for (col in cols_to_modify) {
  #     if (col %in% names(wider_data)) {
  #       wider_data[[col]] <- ifelse(wider_data[[col]] == 0, as.numeric(input$gm_impute), wider_data[[col]])
  #     }
  #   }
  # }
  
  
  pk_para_with_ar <- reactive({
    wider_data <- pk_para_transformed()
    # Calculate Accumulation Ratio (AR) for user-selected parameters
    if (!is.null(input$select_ar) && length(unique(wider_data$Day)) > 1) { # Make sure Day column has multiple values
      
      ar_params  <- input$select_ar
      
      for (param in ar_params) {
        param_ar_data <- wider_data %>%
          group_by(PKEXTSUBJID, Dose, Panel, Period) %>%
          summarize(
            max_day = max(Day),
            min_day = min(Day),
            min_day_value = .data[[param]][Day == min(Day)],
            max_day_value = .data[[param]][Day == max(Day)],
            ar_value = ifelse(min_day == max_day, NA_real_, max_day_value / min_day_value) # if only one Day in the group, show NA
          ) %>%
          ungroup() %>% 
          rename(!!paste0(param, "_AR") := ar_value) # := is used for defining new names in functions like rename or mutate
        
        wider_data <- wider_data %>% 
          left_join(param_ar_data %>% select(PKEXTSUBJID, Dose, Panel, Period, max_day, !!paste0(param, "_AR")),
                    by = c("PKEXTSUBJID", "Dose", "Panel", "Period")) %>% 
          mutate(!!paste0(param, "_AR") := if_else(Day == max_day, .data[[paste0(param, "_AR")]], NA_real_)) %>% # Ensure AR is only populated for max_day rows
          select(-max_day) %>% 
          select(everything(), ends_with("_AR"))
        
      }
      
      #print(head(wider_data[, tail(names(wider_data), 5)]))
      #wider_data
    } else {
      print("AR not calculated")
    }
    wider_data
    #print(head(wider_data[, tail(names(wider_data), 5)]))
  })
  
  # Global reactive for the renaming vector
  renaming_vector <- reactive({
    req(pk_para_file())
    
    # Extract the data frame from the reactiveVal
    pk_para_data <- pk_para_file()
    
    # Create a 1:1 map of Parameter to Units
    parameter_unit_map <- pk_para_data %>%
      select(Parameter, Units) %>%
      distinct() %>%
      filter(!is.na(Units) & Units != "") %>% # Remove rows where Units might be NA or blank
      mutate(Parameter = case_when(
        Parameter == "HL_Lambda_z" ~ "t1/2",
        Parameter == "AUCINF_pred" ~ "AUC0-inf",
        Parameter == "AUClast" ~ "AUC0-last",
        TRUE ~ Parameter  # Default case to keep existing Parameter values
      ))
    
    # Create a named vector for renaming, with Parameter as name and Parameter (Unit) as value
    setNames(
      paste0(parameter_unit_map$Parameter, " (", parameter_unit_map$Units, ")"),
      parameter_unit_map$Parameter
    )
  })
  
  # Update selections
  # observeEvent(pk_para_wider(), {
  #   req(pk_para_wider())
  observeEvent(pk_para_transformed(), {
    req(pk_para_transformed())
    
    # Columns to exclude in select_pk_para
    exclude_columns <- c("Analyte", 
                         "Matrix",
                         "Dose",
                         "Period", 
                         "Panel", 
                         "PKEXTSUBJID",
                         "Day") #"PKEXTSTDYDY"
    # Filter out the columns to exclude
    choices <- setdiff(colnames(pk_para_transformed()), exclude_columns)
    
    # Update pk parameter selection dropdown with default selections
    updateSelectInput(session, "select_pk_para", 
                      choices = sort(choices),
                      selected = c("AUC0-inf", "AUC0-last", "Cmax", "Tmax", "t1/2"))
    
    # For Dose Proportionality tool
    updateSelectInput(session, "pkParams", 
                      choices = sort(choices))
    
    updateSelectInput(session, "subjid_exclude",
                      choices = sort(unique(pk_para_transformed()$PKEXTSUBJID)))
  })
  
  # Summary table for user-selected variables
  # Initiate reactive values
  user_selected_para <- reactiveVal() # For "Select PK parameters to summarize"
  user_selected_stratum <- reactiveVal() # For "Select variable(s) to stratify by"
  user_selected_time_var <- reactiveVal() # For "Select time variable(s)"
  user_selected_ar <- reactiveVal() # For "Calculate Accumulation Ratio"
  
  # Map user selection of stratum to column names
  stratum_map <- c( 
    Analyte = "Analyte", 
    Matrix = "Matrix",
    Panel = "Panel", 
    Period = "Period",
    Dose = "Dose",
    Day = "Day")
  
  # Capture user input and store it in reactive value
  observeEvent(input$select_pk_para, {
    user_selected_para(input$select_pk_para)
    # Pipe user selection of PK parameters into the "select time variable" drop-down menu
    updateSelectInput(session, "select_time_var", choices = input$select_pk_para)
    # Update accumulation ratio selection drop-down
    updateSelectInput(session, "select_ar", choices = input$select_pk_para)
  })
  
  observeEvent(input$select_stratify, {
    # Map user selection to column names
    mapped_stratum <- stratum_map[input$select_stratify]
    # Ensure it's a character vector
    mapped_stratum <- as.character(mapped_stratum)
    # Store the internal variable names in the reactive value
    user_selected_stratum(mapped_stratum)
  })
  
  observeEvent(input$select_time_var, {
    user_selected_time_var(input$select_time_var)
  })
  
  observeEvent(input$select_ar, {
    user_selected_ar(input$select_ar)
  })
  
  
  # Summarize on user selected variables
  summary_data <- reactive({
    req(pk_para_with_ar(),
        #pk_para_wider(),
        user_selected_para(),
        user_selected_stratum(),
        renaming_vector())
    
    
    selected_parameters <- if (!is.null(user_selected_ar())) {
      c(user_selected_para(), paste0(user_selected_ar(), "_AR"))
    } else {
      user_selected_para()
    }
    
    print(paste0("User selected pk para and ar para: ", selected_parameters))
    
    # Columns in summary table: group-by variables + PK parameters
    #selected_cols <- c(user_selected_stratum(), user_selected_para())
    
    
    # Accumulation ratio columns are added base on user selection
    if (is.null(user_selected_ar())) {
      ar_cols <- NULL
      selected_cols <- c(user_selected_stratum(), user_selected_para())
    } else {
      ar_cols <- paste0(user_selected_ar(), "_AR")
      selected_cols <- c(user_selected_stratum(), user_selected_para(), ar_cols)
    }
    
    # Subset columns to display
    data <- pk_para_with_ar()[, selected_cols, drop = FALSE]
    print("individual level data for pk para table: ")
    print(head(data))
    
    # Ensure group-by columns are unquoted
    group_cols <- rlang::syms(user_selected_stratum())
    
    
    # Calculate summary statistics
    result <- data %>% 
      group_by_at(vars(!!!group_cols)) %>%
      summarize(
        across(all_of(selected_parameters), ~ {
          if (is.numeric(.x) && cur_column() %in% user_selected_time_var()) { # cur_column() gives the name of the current column (in across() only)
            paste0(format(signif(median(.x, na.rm = TRUE)), nsmall = 2), 
                   " (", format(signif(min(.x, na.rm = TRUE)), nsmall = 2),
                   "-", format(signif(max(.x, na.rm = TRUE)), nsmall = 2), ")")
          } else if (is.numeric(.x)) {
            # If numeric and column is not in user selection of time variables - use GM & GCV
            if (any(.x == 0, na.rm = TRUE)) {
              # Replace the non-calculable GMs with NC
              "NC"
            } else if (any(is.na(.x))) {
              # Replace the NAs in non-last day AR rows with "-"
              "-"
            } else {
              paste0(signif_pad(geoMean(.x), digits = 3), " (", signif_pad(geoCV(.x), digits = 3), ")") # Get 8.10 instead of 8.1
            } 
          } else {
            # Non-numeric - concatenate unique values
            paste(unique(.x), collapse = ", ")
          }
        }),
        "N" = n()
      ) %>% 
      select(user_selected_stratum(), "N", selected_parameters)
    
    print("summary level data for pk para table: ")
    print(head(result))
    
    # Apply renaming to add parameter units to the header
    rename_vec <- renaming_vector()
    valid_names <- names(result) %in% names(rename_vec) # Generates a logical vector
    
    if (any(valid_names)) { # If there is at least one TRUE
      cols_to_rename <- names(result)[valid_names]
      names(result)[valid_names] <- rename_vec[cols_to_rename]
    }
    
    result
  })
  
  
  # Render summary table
  output$summary_table <- renderDataTable({
    req(summary_data())
    
    summary_df <- summary_data()
    
    dt_options_pk_para <- list(
      paging = FALSE,
      scrollX = TRUE,
      searchHighlight = TRUE,
      fixedHeader = TRUE # Enable the FixedHeader extension
    )
    
    if(input$mask_n_pk_para) {
      n_col_index_pk_para <- match("N", names(summary_df)) - 1
      
      dt_options_pk_para$columnDefs <- list(list(visible = FALSE, targets = n_col_index_pk_para))
    }
    
    datatable(summary_df,
              elementId = 'summary_table',
              rownames = FALSE,
              options = dt_options_pk_para)
  })
  
  
  output$download_html <- downloadHandler(
    filename = function() {
      paste0("pk_parameter_table_", gsub("[^0-9]","",get_timestamp()), ".docx") #html
    },
    content = function(file) {
      data_to_download <- summary_data()
      
      if(input$mask_n_pk_para) {
        data_to_download$N <- NULL  # Remove N column if masked
      }
      
      # Path to a temporary Rmd file
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      # Create the R Markdown template content dynamically
      template_content <- paste(
        "---",
        "title: \"Summary PK Parameters - Geometric Mean (CV% Geometric Mean)\"",
        "output: word_document", 
        "params:",
        "  data: NULL",
        "---",
        "",
        "```{r, echo = FALSE}",
        "knitr::kable(params$data)", # Simple table formatting without kableExtra enhancements to adjust for word output
        "```",
        "",
        "",
        "Results are geometric mean (coefficient of variation %) unless otherwise stated.",
        "",
        "Time variables not including t1/2 are reported in median (min-max) format.",
        "",
        "NC (Not Calculable) is reported for parameters where at least one individual had a parameter value of 0, therefore Geometric Mean cannot be calculated.",
        "",
        "A dash '-' is reported for parameters where at least one individual had a missing parameter value.",
        sep = "\n"
      )
      
      # Write the Rmd content to the temporary file
      writeLines(template_content, tempReport)
      
      # Render the Rmd file to html
      rmarkdown::render(tempReport, output_file = file,
                        output_format = word_document(),
                        params = list(data = data_to_download),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
  #### Dose proportionality tool ####
  
  # Inherit some of the variable names from Brian's code to keep things consistent
  
  PKP <- reactive({
    req(pk_para_transformed(), input$pkParams)
    
    data <- pk_para_transformed() %>%
      dplyr::filter(
        !!sym(input$pkParams) != 0, 
        !is.na(!!sym(input$pkParams)),
        !PKEXTSUBJID %in% input$subjid_exclude
        ) # Leverages rlang's tidy evaluation system
    
    # Check if dose and parameter have more than 1 level for fitting
    data$LNY = log(data[[input$pkParams]])
    data$LNX = log(data[["Dose"]])
    
    data
  })
  
  gm_PKP <- reactive({
    req(PKP())
    
    data <- PKP() %>%
      dplyr::group_by(Dose) %>%
      dplyr::summarise(
        gm = geoMean(.data[[input$pkParams]]),
        CIL = CIL(.data[[input$pkParams]]),
        CIU = CIU(.data[[input$pkParams]])
      )
    
    data <- signif(as.data.frame(data), 3)
    
    data
  })
  
  modelPKP <- reactive({
    req(PKP())
    
    model <- lm(LNY ~ LNX, na.action = "na.exclude", data = PKP())
    
    model
  })
  
  predLinePKP <- reactive({
    req(PKP(), modelPKP())
    
    line <- as.data.frame(exp(
      (predict(modelPKP(), newdata = list(LNX = log(PKP()[["Dose"]])), interval = "confidence")))
    )
    
    line$Dose <- PKP()[["Dose"]]
    
    line
    
  })
  
  # Dose proportionality plot
  dose_prop_plot <- reactive({
    if(!is.null(input$pkParams)) {
      xLower = as.numeric(ifelse(!input$xAuto, input$xLower, NA))
      xUpper = as.numeric(ifelse(!input$xAuto, input$xUpper, NA))
      autoWidth = as.integer((range(gm_PKP()$Dose)[2] - range(gm_PKP()$Dose)[1])/60)
      errorBarWidth = ifelse(!input$errorBarAuto, input$errorBarWidth, autoWidth)
      
      
      # Apply conditional filters based on xLower and xUpper, to make y axis range also reactive
      # When to expect plot x axis range change: xUpper < max
      filtered_gm <- gm_PKP()
      filtered_predline <- predLinePKP()
      
      if (!is.na(xLower)) {
        filtered_gm <- filtered_gm %>%
          filter(Dose >= xLower)
        
        filtered_predline <- filtered_predline %>% 
          filter(Dose >= xLower)
      }
      if (!is.na(xUpper)) {
        filtered_gm <- filtered_gm %>%
          filter(Dose <= xUpper)
        
        filtered_predline <- filtered_predline %>% 
          filter(Dose <= xUpper)
      }
      
      # Plotting
      p <- ggplot(data = filtered_gm) + #gm_PKP()
        geom_point(aes(x = Dose, y = gm)) +
        geom_errorbar(aes(x = Dose, ymin = CIL, ymax = CIU), width = errorBarWidth) +
        geom_line(data = filtered_predline, aes(x = Dose, y = fit)) + # predLinePKP()
        labs(x = ifelse(input$doseUnits == "", "Dose", paste0("Dose (", input$doseUnits, ")")), 
             y = ifelse(input$PKUnits == "", input$pkParams, paste0(input$pkParams, " (", input$PKUnits, ")"))) +
        theme_bw() +
        theme(text = element_text(size = 18)) +
        xlim(xLower, xUpper)
      
      # If user doesn't remove PI, add this layer
      if(!input$removePI) {p = p + geom_ribbon(data = filtered_predline, aes(x = Dose, ymin = lwr, ymax = upr), alpha = 0.2)} #predLinePKP()
      
    } else {
      p <- ggplot(NULL) + 
        theme_bw() +
        labs(title = "No parameter selected")
    }
    
    
    # Log scale transformation based on radio button selection
    if (input$y_axis_scale == "Log") {
      
      log_max_y_dp <- ceiling(log10(max(filtered_gm$gm)))
      dynamic_breaks_y_dp <- c(0.01, 0.1, 1, 10^(seq(0, log_max_y_dp, by = 1)))
      
      p <- p + scale_y_log10(
        # breaks = scales::trans_breaks("log10", function(x) 10^x),
        # labels = scales::trans_format("log10", scales::math_format(10^.x))
        breaks = dynamic_breaks_y_dp, # Specify the dynamic breaks
        labels = scales::label_comma() # Insert a comma every three digits
      ) +
        annotation_logticks(sides = "l", size = 0.3) + # Adds ticks on the left side
        theme(panel.grid.minor = element_blank()) # Removes minor grid lines
    }
    
    
    if (input$x_axis_scale == "Log") {
      
      log_max_x_dp <- ceiling(log10(max(filtered_gm$Dose)))
      dynamic_breaks_x_dp <- c(0.01, 0.1, 1, 10^(seq(0, log_max_x_dp, by = 1)))
      
      p <- p + scale_x_log10(
        breaks = dynamic_breaks_x_dp,
        labels = scales::label_comma()
      ) +
        annotation_logticks(sides = "b", size = 0.3) +
        theme(panel.grid.minor = element_blank())
    }
    
    p
  })
  
  output$dp_plot <- renderPlot({
    dose_prop_plot()
  })
  
  # Plot footnote
  output$PKCI = renderUI({
    if(!is.null(modelPKP()) & !is.null(gm_PKP())){
      ci = confint(modelPKP(), 'LNX', level = 0.95)
      PKCI = assessDP(modelPKP(), ci)
      HTML(paste0("<b>Assessment:</b> ", PKCI))
    } else{
      PKCI = NULL
      HTML("")
    }
  })
  
  output$download_dp_plot <- downloadHandler(
    filename = function() {
      paste0("dose_proportionality", "_", gsub("[^0-9]","",get_timestamp()), ".png")
    },
    # content = function(file) {
    #   ggsave(file, dose_prop_plot(), device = "png")
    # }
    content = function(file) {
      width_in_dp <- if (!is.null(input$plot_width_dp) && input$plot_width_dp > 0) input$plot_width_dp / 96 else 8
      height_in_dp <- if (!is.null(input$plot_height_dp) && input$plot_height_dp > 0) input$plot_height_dp / 96 else 6
      
      tryCatch({
        ggsave(file, dose_prop_plot(), width = width_in_dp, height = height_in_dp, device = "png")
      }, error = function(e) {
        cat("Error in downloading plot:", e$message, "\n")
      })
    }
  )
  
  
  # Parameter projections table
  # Define PredPKP as a reactive expression outside of observe
  PredPKP <- reactive({
    req(input$projectDoses, modelPKP(), gm_PKP()) # Ensure required inputs and reactives are available
    
    doseVector <- as.numeric(unlist(strsplit(input$projectDoses, split = ",")))
    logDoses <- log(doseVector)
    newdata <- data.frame(LNX = logDoses) # Use newdata as a more descriptive name
    
    # Use req to ensure that modelPKP() and the doses vector are available
    pred <- signif(as.data.frame(exp(predict(modelPKP(), newdata, interval = "confidence"))), 3)
    pred$Dose <- doseVector # Assign the doses vector to a new column
    
    pred
  })
  
  PK <- reactive({
    req(gm_PKP(), PredPKP())
    
    pk <- dplyr::full_join(gm_PKP(), PredPKP())
    
    pk<- mutate(pk, 
                OBMPK = signif(as.numeric(input$PKEM)/gm, 3),
                OBMPKLB = signif(as.numeric(input$PKEM)/CIL, 3),
                OBMPKUB = signif(as.numeric(input$PKEM)/CIU, 3),
                EMPK = signif(as.numeric(input$PKEM)/fit, 3),
                EMPKLB = signif(as.numeric(input$PKEM)/lwr, 3),
                EMPKUB = signif(as.numeric(input$PKEM)/upr, 3))
    
    pk
  })
  
  PKChar <- reactive({
    req(PK())
    
    char <- mutate(PK(), 
                   Observed = paste0(gm, " (", CIL, ", ", CIU, ")"),
                   ObservedEM = paste0(OBMPK, " (", OBMPKLB, ", ", OBMPKUB, ")"),
                   Prediction = paste0(fit, " (", lwr, ", ", upr, ")"),
                   PredictedEM = paste0(EMPK, " (", EMPKLB, ", ", EMPKUB, ")"),
                   Parameter = input$pkParams) %>% 
      select(Parameter, Dose, Observed, ObservedEM, Prediction, PredictedEM)
    
    char
  })
  
  PKS <- reactive({
    req(PKChar())
    
    pks <- PKChar()
    
    pks$Units <- input$PKUnits
    
    pks <- pks %>% 
      select(Parameter, Units, Dose, Observed, ObservedEM, Prediction, PredictedEM)
    
    pks
  })
  
  
  projection_table <- reactive({
    if(!is.null(PKChar())) {
      
      dt <- PKChar()
      
      names(dt) = c("Parameter",
                    ifelse(input$doseUnits == "", "Dose", paste0("Dose (", input$doseUnits, ")")),
                    ifelse(input$PKUnits == "", "Observed", paste0("Observed (", input$PKUnits, ")")),
                    "Observed EM",
                    ifelse(input$PKUnits == "", "Prediction", paste0("Prediction (", input$PKUnits, ")")),
                    "Prediction EM")
      dt
    } else {data.frame(Parameter = "No parameter selected")}
  })
  
  output$PK_proj <- renderTable({
    projection_table()
  })
  
  
  output$download_dp_html <- downloadHandler(
    filename = function() {
      paste0("parameter_projections", "_", gsub("[^0-9]","",get_timestamp()), ".docx")
    },
    content = function(file) {
      data_to_download <- projection_table()
      
      tempReport <- file.path(tempdir(), "report.Rmd")
      
      template_content <- paste(
        "---",
        "title: \"Parameter Projections\"",
        "output: word_document",
        "params:",
        "  data: NULL",
        "---",
        "",
        "```{r, echo = FALSE}",
        "knitr::kable(params$data)",
        "```",
        "",
        sep = "\n"
      )
      
      writeLines(template_content, tempReport)
      
      rmarkdown::render(tempReport, output_file = file,
                        output_format = word_document(),
                        params = list(data = data_to_download),
                        envir = new.env(parent = globalenv())
      )
    }
  )
  
  
}


#### App ####
shinyApp(ui, server)
