

#   ------------------------------------------------------------------------



#' analysis UI Function
#'
#' @description A shiny Module.
#'
#' @param id shiny id
#'
#' @rdname mod_analysis
#'
#' @importFrom shiny NS tagList
#' @importFrom shinyjs useShinyjs
#' @importFrom shinycssloaders withSpinner
#' @importFrom bsicons bs_icon
#'
mod_analysis_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),

    fluidPage(
      useShinyjs(),

      # Custom CSS to rotate the tab titles
      tags$style(
        HTML(
          "
      /* Target the tab titles in the navlistPanel */
      .nav-pills > li > a {
        writing-mode: vertical-rl; /* Set text orientation to vertical right-to-left */
        transform: rotate(180deg); /* Rotate the text to make it readable */
        padding-left: 5px;
        padding-right: 5px;
        background-color: white; /* Change background color */
        color: black; /* Change text color */
      }

      .nav-stacked > li > a {
        margin-right: 5px; /* Reduce bottom margin */
      }

      .form-group{
      margin-bottom: 10px;
      }
      "
        )
      ),

      fluidRow(
        column(
          width = 6,

          navlistPanel(id = ns("tabSwitch"),
                       tabPanel("Automatique",
                                uiOutput(ns("auto_grouping_class_selectUI")),
                                uiOutput(ns("visualization_auto_groupingUI"))
                       ),
                       tabPanel("Manuelle",
                                uiOutput(ns("man_grouping_inputUI")),
                                uiOutput(ns("visualization_man_groupingUI"))
                       ),
                       , well = FALSE, widths = c(1,11)
          )
        ),

        # column
        column(
          width = 6,
          withSpinner(
            leafletOutput(ns(
              "analysemap"
            ), height = 700)
          )
        )

        # column
        # column(width = 2, uiOutput(ns(
        #   "groupOverviewUI"
        # ))) # column
      ), # fluidRow

      fluidRow(
        uiOutput(ns("longitudinal_seriesUI"))
      )
    ) # fluidPage)
  )# taglist
} # function mod_analysis_ui

#' analysis Server Functions
#'
#' @noRd
#'
#' @import shiny
#' @importFrom leaflet leafletProxy clearGroup leafletOutput renderLeaflet addPolylines
#' @importFrom htmltools HTML div img
#' @importFrom dplyr filter mutate if_else pull bind_cols select rowwise
#' @importFrom shinyjs onclick runjs
#' @importFrom rhandsontable rHandsontableOutput rhandsontable hot_context_menu renderRHandsontable hot_to_r
#' @importFrom sf st_drop_geometry
#' @importFrom plotly renderPlotly plotlyOutput
#'
#'
mod_analysis_server <- function(id, con){
  moduleServer( id, function(input, output, session){

    ns <- session$ns

    ### REACTIVE VALUES ####
    r_val <- reactiveValues(

      region_already_clicked = FALSE, # check if region already clicked to show grouping selection
      profile_display = FALSE, # controle if metric and axis is selected = display the profile

      # automatic grouping ui
      auto_grouping_select = NULL,

      # manual grouping ui
      man_grouping_scale_select = NULL,
      ui_metric = NULL,
      man_grouping_var_select = NULL,
      man_grouping_quantile = NULL,
      man_grouping_no_classes = NULL,
      man_grouping_table_placeholder = NULL,
      man_grouping_editable_table = NULL,
      man_grouping_apply_changes = NULL,
      stacked_barplots = NULL,
      table_overview_classes = NULL,
      violinplots = NULL,
      table_overview_var_groups = NULL,

      # longitudinal series
      longitudinal_plot = NULL,

      # map
      opacity = list(clickable = 0.01, not_clickable = 0.10), # opacity value to inform the user about available bassins and regions

      ### metric selected by user
      selected_metric = NULL, # select main metric column name
      selected_metric_name = NULL, # select main metric name to display for user
      selected_metric_type = NULL, # select main metric type name to display for user

      ### geoserver controler
      cql_filter = NULL, # WMS filter
      sld_body = NULL, # WMS SLD symbology

      # data
      bassins = NULL, # bassins data
      regions_in_bassin = NULL, # all the regions in selected bassin
      network_region_axis = NULL, # all the axis in the selected region
      network_region = NULL, # all DGOs in selected region
      selected_region_feature = NULL, # region data clicked
      region_click = NULL, # region clicked information list
      axis_click = NULL, # axis clicked information list
      dgo_axis = NULL, # all DGO in selected axis
      axis_start_end = NULL, # start / end df coordinates to add pin on map
      strahler = NULL, # min and max strahler values to set strahler filter UI
      min_max_metric = NULL, # min and max metric values to set metric filter UI
      selected_axis_df = NULL, # DGO in axis dataframe to plot longitudinal profile
      data_section = NULL, # DGO elevation data for section profile
      roe_region = NULL, # ROE data in selected region
      roe_axis = NULL, # ROE data in selected axis
      hydro_sites_region = NULL, # hydro sites data in selected region
      data_dgo_clicked = NULL, # DGO clicked by user for cross section profile

      # grouping
      grouping_table_data = NULL
    )

    ### RENDER UI ####

    #### automatic grouping ####

    output$auto_grouping_class_selectUI <- renderUI({
      if (!is.null(r_val$auto_grouping_select)) {
        r_val$auto_grouping_select
      } else {
        HTML('<label class="control-label" id="wait-metric-label">
             Cliquez sur une bassin hydrographique pour afficher la classification automatique du reseau hydrographique </label>')
      }
    })

    #### manual grouping ####

    # top description
    output$man_grouping_descriptionUI <- renderUI({
      fluidRow({
        if (is.null(r_val$ui_metric)) {
          HTML('<label class="control-label" id="wait-metric-label">
             Cliquez sur une region hydrographique pour afficher la creation de classes. </label>')
        } else {
          HTML('<label class="control-label" id="wait-metric-label">
             Sélectionnez une métrique pour créer une classification régionale des segments de rivière. Une pré-classification peut être créée soit sur la base des segments de la région, soit sur la base des segments d\'un axe fluvial sélectionné. Vous pouvez modifier directement les valeurs ou, en option, ajuster le quantile et le nombre de classes. </label>')
        }
      })
    })

    # input elements for classes-creation
    output$man_grouping_inputUI <- renderUI({
      div(
        fluidRow(
          uiOutput(ns("man_grouping_descriptionUI"))
        ),
        fluidRow(
          column(width = 6,
                 uiOutput(ns("man_grouping_metric_selectUI")), # selectinput metric
                 fluidRow(
                   column(width = 6,
                          uiOutput(ns("man_grouping_quantileUI"))),
                   column(width = 6,
                          uiOutput(ns("man_grouping_no_classesUI"))),
                   uiOutput(ns("man_grouping_scale_selectUI")))),
          column(width = 6,
                 uiOutput(ns("man_grouping_table_placeholderUI")),
                 uiOutput(ns("man_grouping_editable_tableUI")),
                 uiOutput(ns("man_grouping_apply_changesUI")))
        )
      )
    })

    # metric selection
    output$man_grouping_metric_selectUI <- renderUI({
      r_val$ui_metric
    })

    # scale (region vs axis) select
    output$man_grouping_scale_selectUI <- renderUI({
      r_val$man_grouping_scale_select
    })

    # quantile set
    output$man_grouping_quantileUI <- renderUI({
      r_val$man_grouping_quantile
    })

    # no of classes set
    output$man_grouping_no_classesUI <- renderUI({
      r_val$man_grouping_no_classes
    })

    #placeholder when axis not yet selected
    output$man_grouping_table_placeholderUI <- renderUI({
      r_val$man_grouping_table_placeholder
    })

    # editable table
    output$man_grouping_editable_tableUI <- renderUI({
      r_val$man_grouping_editable_table
    })

    # Apply classes button
    output$man_grouping_apply_changesUI <- renderUI({
      r_val$man_grouping_apply_changes
    })

    # output manual grouping (plots)
    output$visualization_man_groupingUI <- renderUI({
      div(
        fluidRow(
          column(width = 6,
                 uiOutput(ns("table_overview_var_groupsUI")),
                 plotlyOutput(ns("violinplotsUI"))
          ),
          column(width = 6,
                 uiOutput(ns("table_overview_classesUI")),
                 plotlyOutput(ns("stacked_barplotsUI"))
          )
        )
      )
    })

    output$stacked_barplotsUI <- renderPlotly({
      r_val$stacked_barplots
    })

    output$table_overview_classesUI <- renderUI({
      r_val$table_overview_classes
    })

    output$violinplotsUI <- renderPlotly({
      r_val$violinplots
    })

    output$table_overview_var_groupsUI <- renderUI({
      r_val$table_overview_var_groups
    })


    #### LONGITUDINAL PLOT ####

    output$longitudinal_seriesUI <- renderPlotly({
      r_val$longitudinal_plot
    })


    ### INIT MAP ####

    output$analysemap <- renderLeaflet({
      r_val$bassins = data_get_bassins(opacity = r_val$opacity, con = con)
      map_init_bassins(bassins_data = r_val$bassins,
                       id_logo_ign_remonterletemps = ns("logo_ign_remonterletemps"))
    })

    onclick(id = "logo_ign_remonterletemps", expr =
              runjs(sprintf("window.open('%s', '_blank')",
                            utils_url_remonterletemps(lng = input$analysemap_center$lng,
                                                      lat = input$analysemap_center$lat,
                                                      zoom = input$analysemap_zoom)))
    )


    ### INIT Grouping class ####

    # # UI create choose group
    # output$groupingUI <- renderUI({
    #   if (!is.null(r_val$ui_grouping)) {
    #     div(style = "display: flex; align-items: center; margin-bottom: 0px", r_val$ui_grouping, )
    #   } else {
    #     HTML(
    #       '<label class="control-label" id="wait-metric-label">
    #          Cliquez sur une région hydrographique pour afficher la sélection des groupages </label>'
    #     )
    #   }
    # })

    ### EVENT MAP CLICK ####

    observeEvent(input$analysemap_shape_click, {

      #### bassin clicked ####

      if (input$analysemap_shape_click$group == params_map_group()[["bassin"]]) {


        # disable the click interactivity for the bassin selected
        r_val$bassins = r_val$bassins %>%
          mutate(click = if_else(display == TRUE,
                                 TRUE,
                                 click)) %>%
          mutate(click = if_else(display == TRUE & cdbh == input$analysemap_shape_click$id,
                                 FALSE,
                                 click))
        # get the regions data in selected bassin
        r_val$regions_in_bassin = data_get_regions_in_bassin(selected_bassin_id = input$analysemap_shape_click$id,
                                                             opacity = r_val$opacity,
                                                             con = con)
        # update map : zoom in clicked bassin, clear bassin data, display region in bassin
        leafletProxy("analysemap") %>%
          map_add_regions_in_bassin(bassin_click = input$analysemap_shape_click,
                                    regions_data = r_val$regions_in_bassin,
                                    bassins_data = r_val$bassins)
      }


      ### region clicked ####

      if (input$analysemap_shape_click$group == params_map_group()$region) {


        # store the region click values
        r_val$region_click = input$analysemap_shape_click
        # disable the click interactivity for the bassin selected
        r_val$regions_in_bassin = r_val$regions_in_bassin %>%
          mutate(click = if_else(display == TRUE,
                                 TRUE,
                                 click)) %>%
          mutate(click = if_else(display == TRUE & gid == r_val$region_click$id,
                                 FALSE,
                                 click))

        # save the selected region feature for mapping
        r_val$selected_region_feature = data_get_region(region_click_id = r_val$region_click$id,
                                                        con = con)
        # set region name to download
        r_val$region_name = utils_normalize_string(r_val$selected_region_feature$lbregionhy)

        # get network of region
        r_val$network_region = data_get_network_region(selected_region_id = r_val$region_click$id,
                                                       con = con)

        # get the axis in the region
        r_val$network_region_axis = data_get_axis(selected_region_id = r_val$region_click$id,
                                                  con = con)
        # get ROE in region
        r_val$roe_region = data_get_roe_in_region(r_val$region_click$id,
                                                  con = con)
        # get hydro sites in region
        r_val$hydro_sites_region = data_get_hydro_sites(r_val$region_click$id,
                                                        con = con)
        # get strahler data
        r_val$strahler = isolate(data_get_min_max_strahler(selected_region_id = r_val$region_click$id,
                                                           con = con))


        # map region clicked with regional network and overlayers
        leafletProxy("analysemap") %>%
          map_region_clicked(region_click = input$analysemap_shape_click,
                             selected_region_feature = r_val$selected_region_feature,
                             regions_data = r_val$regions_in_bassin,
                             roe_region = r_val$roe_region,
                             hydro_sites_region = r_val$hydro_sites_region) %>%
          map_metric(wms_params = params_wms()$metric,
                     cql_filter = paste0("gid_region=",r_val$selected_region_feature[["gid"]]),
                     sld_body = NULL,
                     data_axis = r_val$network_region_axis)

        # print(paste0("Clicked on: ", input$analysemap_shape_click))
        # print(paste0("selected region feature: ", r_val$selected_region_feature))

        # map_axis(data_axis = r_val$network_region_axis)

        # create radiobutton-select for automatic classification
        r_val$auto_grouping_select <-
          radioButtons(ns("auto_grouping_select"),
                       "Selectionnez un indicateur :",
                       c("Type d'utilisation du sol dominant",
                         "Urban pressure",
                         "Agricultural pressure",
                         "Confinement",
                         "Stream Power Index"))


        # create elements of manual grouping pane
        r_val$ui_metric = selectInput(ns("metric"), NULL,
                                      choices = params_get_metric_choices(),
                                      selected  = params_get_metric_choices()[1],
                                      width = "100%")
        r_val$man_grouping_scale_select <- radioButtons(ns("man_grouping_scale_select"),
                                                        "Base de classification",
                                                        c("Region", "Axe fluvial"),
                                                        selected = "Region",
                                                        inline = TRUE)
        r_val$man_grouping_quantile <- numericInput(inputId = ns("man_grouping_quantile"), "Quantile [%]", value = 95, min = 0, max = 100)
        r_val$man_grouping_no_classes <- numericInput(inputId = ns("man_grouping_no_classes"), "Classes", value = 4, min = 2, max = 10, step = 1)

        r_val$man_grouping_apply_changes <- actionButton(inputId = ns("man_grouping_apply_changes"), "Appliquer")

      }

      ### axis clicked ####

      if (input$analysemap_shape_click$group == params_map_group()$axis) {
        # save the clicked axis values
        r_val$axis_click = input$analysemap_shape_click
        # reget the axis in the region without the selected axis
        r_val$network_region_axis = data_get_axis(selected_region_id = r_val$region_click$id,
                                                  con = con) %>%
          filter(axis != r_val$axis_click$id)
        # get the DGO axis data
        r_val$dgo_axis = data_get_network_axis(selected_axis_id = r_val$axis_click$id,
                                               con = con) %>%
          mutate(measure = measure/1000)
        # extract axis start end point
        r_val$axis_start_end = data_get_axis_start_end(dgo_axis = r_val$dgo_axis)
        # get ROE in axis clicked
        r_val$roe_axis = r_val$roe_region %>%
          filter(axis == r_val$axis_click$id)

        # map dgo axis when axis clicked and metric selected
        leafletProxy("analysemap") %>%
          map_dgo_axis(selected_axis = r_val$dgo_axis, region_axis = r_val$network_region_axis,
                       main_metric = r_val$selected_metric, second_metric = r_val$selected_profile_metric) %>%
          map_axis_start_end(axis_start_end = r_val$axis_start_end, region_axis = r_val$network_region_axis)

        # create or update profile dataset with new axis
        r_val$selected_axis_df = r_val$dgo_axis %>%
          as.data.frame()

        if (input$man_grouping_scale_select == "Axe fluvial" &
            !is.null(r_val$dgo_axis) ) {

          r_val$man_grouping_table_placeholder <- NULL # Ensure table is shown
          r_val$man_grouping_editable_table <- rHandsontableOutput(ns("man_grouping_editable_table"), width = "100%")

          # create classes-table
          r_val$grouping_table_data = create_df_input(
            axis_data = r_val$dgo_axis,
            variable_name = input$metric,
            no_classes = input$man_grouping_no_classes,
            quantile = input$man_grouping_quantile
          )
        }
      }

      ### dgo clicked ####

      # if (input$analysemap_shape_click$group == params_map_group()$dgo_axis) {
      #   # get data with dgo id
      #   r_val$data_section = data_get_elevation_profiles(selected_dgo_fid = input$analysemap_shape_click$id,
      #                                                    con = con)
      #   # get dgo clicked feature
      #   r_val$data_dgo_clicked = r_val$dgo_axis %>%
      #     filter(fid == input$analysemap_shape_click$id)
      #
      #   # Highlight clicked DGO
      #   leafletProxy("analysemap") %>%
      #     map_dgo_cross_section(selected_dgo = r_val$data_dgo_clicked)
      # }

    })

    ### EVENT AUTOMATIC GROUPING RUN ####


    ### EVENT MANUAL GROUPING RUN ####

    #### input variables changed ####

    observeEvent(list(input$metric,
                      input$man_grouping_scale_select,
                      input$man_grouping_quantile,
                      input$man_grouping_no_classes,
                      input$man_grouping_scale_select
    ), {

      # track input
      track_inputs(input = input)

      # check for valid values
      if (!is.null(input$metric) &
          !is.null(input$man_grouping_scale_select) &
          !is.null(input$man_grouping_quantile) &
          !is.null(input$man_grouping_no_classes)) {

        if ((input$man_grouping_scale_select == "Region") &
            !is.null(r_val$network_region)) {

          r_val$man_grouping_table_placeholder <- NULL # Clear placeholder text
          # create table-output
          r_val$man_grouping_editable_table <- rHandsontableOutput(ns("man_grouping_editable_table"), width = "100%")

          # create classes-table
          r_val$grouping_table_data = create_df_input(
            axis_data = r_val$network_region,
            variable_name = input$metric,
            no_classes = input$man_grouping_no_classes,
            quantile = input$man_grouping_quantile
          )
        }

        else if (input$man_grouping_scale_select == "Axe fluvial" &
                 is.null(r_val$dgo_axis) ) {
          # show text placeholder
          r_val$man_grouping_table_placeholder = HTML('<label class="control-label" id="wait-metric-label">
             Cliquez sur une axe fluvial pour afficher la creation de classes. </label>')

          # remove table-output
          r_val$man_grouping_editable_table <- NULL
        }

        else if (input$man_grouping_scale_select == "Axe fluvial" &
                 !is.null(r_val$dgo_axis) ) {

          r_val$man_grouping_table_placeholder <- NULL # Ensure table is shown
          r_val$man_grouping_editable_table <- rHandsontableOutput(ns("man_grouping_editable_table"), width = "100%")

          # create classes-table
          r_val$grouping_table_data = create_df_input(
            axis_data = r_val$dgo_axis,
            variable_name = input$metric,
            no_classes = input$man_grouping_no_classes,
            quantile = input$man_grouping_quantile
          )
        }


      }

    })

    # observeEvent(list(r_val$dgo_axis, input$tabSwitch), {
    #
    #   if (input$tabSwitch == "Manuelle" & !is.null(r_val$dgo_axis)) {
    #
    #   }
    #
    # })


    # update table when values are edited (either via editing the table or setting the variables in the UI)
    observeEvent(r_val$grouping_table_data, {

      if (!is.null(r_val$grouping_table_data)) {
        output$man_grouping_editable_table <- renderRHandsontable({
          tmp <- isolate(r_val$grouping_table_data %>% select(!variable))# Gotta isolate it or it'll cause infinite loop, see https://github.com/jrowen/rhandsontable/issues/166
          rownames(tmp) <- NULL
          rhandsontable( tmp, rowHeaders = NULL) %>%
            hot_context_menu(allowRowEdit = FALSE, allowColEdit = FALSE)
        })
      }
    })

    # Update the reactive values when user edits table in the UI
    observeEvent(input$man_grouping_editable_table, {

      r_val$grouping_table_data <- hot_to_r(input$man_grouping_editable_table) %>%
        bind_cols(
          r_val$grouping_table_data %>%
            select(variable)
        )
    })

    #### apply button clicked ####
    observeEvent(input$man_grouping_apply_changes,{

      # Create classified network by adding the classes and colors
      classified_network <- r_val$network_region %>%
        assign_classes(classes = r_val$grouping_table_data)

      # add classified network to map
      leafletProxy("analysemap") %>%
        clearGroup(params_map_group()$dgo_axis) %>%
        # clearGroup("classified_dgos") %>%
        addPolylines(data = classified_network,
                     # layerId = ~classified_dgos,
                     weight = 5,
                     color = ~color,
                     opacity = 1,
                     highlightOptions = highlightOptions(
                       color = "red",
                       sendToBack = TRUE
                     ))
      # %>%
      #   addPolylines(data = classified_network,
      #                # layerId = ~class,
      #                weight = 5,
      #                color = ~color,
      #                opacity = 0.7,
      #                label = ~classified_network[[input$man_grouping_var_select]] %>%
      #                  st_drop_geometry() %>%
      #                  round(2))

      # create plots

      if (!is.null(r_val$dgo_axis)) {
        #   # create classified axis network
        classified_axis <- r_val$dgo_axis %>%
          assign_classes(classes = r_val$grouping_table_data)
        #
        #   # merge regional and axis network in one df
        merged_network <- merge_regional_axis_dfs(classified_network,
                                                  classified_axis,
                                                  input$metric)
        #
        #
        r_val$stacked_barplots = create_plotly_barplot(merged_network)
        # r_val$table_overview_classes = NULL
        r_val$violinplots = create_plotly_violinplot(merged_network, input$metric)
        #   # r_val$table_overview_var_groups = NULL

        # create plotly longitudinal series plot
        # r_val$longitudinal_plot <-
        #   plot_class_series_plotly(classified_axis,
        #                            x = "measure",
        #                            y = input$metric,
        #                            cat = "class_name",
        #                            colors = get_colors_char_df(classified_axis))
      }




    })

    ### EVENT MOUSEOVER ####

    # observeEvent(c(input$metric, input$unit_area), ignoreInit = TRUE, {
    #
    #   # track input
    #   track_inputs(input = input)
    #
    #   # change field if unit_area in percentage
    #   if (!is.null(input$metric) && input$unit_area == "percent"
    #       && (input$metric_type %in% c("landuse", "continuity"))){
    #     r_val$selected_metric = paste0(input$metric,"_pc")
    #   } else if (!is.null(input$metric)) {
    #     r_val$selected_metric = input$metric
    #   }
    #
    #   if (!is.null(input$metric)){
    #     r_val$selected_metric_name = params_metrics_choice()[[input$metric_type]]$metric_type_values[[input$metric]]$metric_title
    #     r_val$selected_metric_type = params_metrics_choice()[[input$metric_type]]$metric_type_title
    #
    #   }
    # })
  })
}

## To be copied in the UI
# mod_analysis_ui("analysis_1")

## To be copied in the server
# mod_analysis_server("analysis_1")
