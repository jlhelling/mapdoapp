#' documentation UI Function
#'
#' @description A shiny Module.
#'
#' @param id,input,output,session Internal parameters for {shiny}.
#'
#' @noRd
#'
#' @import shiny
mod_documentation_ui <- function(id){
  ns <- NS(id)
  tagList(
    golem_add_external_resources(),
    fluidPage(
      fluidRow(
        column(
          width = 9,
          h1("Le projet"),
          HTML("<p style='text-align: justify;'>
          <b>Mapd'O</b> est un projet envisageant le développement d'une interface web d'analyse de données hydromorphologiques et de modèles pour les opérateurs. Le projet est porté par le laboratoire <i>CNRS UMR 5600 - Environnement Ville Société (EVS)</i> et est soutenu par <i>l'Office Français de la Biodiversité</i> depuis janvier 2023. <br> <br>

            L'application est basée sur les résultats de la <a href='https://tramebleue.github.io/fct-cli/' target='_blank' style='text-decoration: underline;'>Fluvial Corridor Toolbox</a>, également développée par le laboratoire EVS. Ces outils fournissent des approches géomatiques à l'analyse de la topographie et de l'occupation des sols, permettant la production de cartes de la continuité latérale et d'autres caractéristiques des cours d'eau. Les couches de fond de vallée et de corridor fluvial sont d'abord extraites d'un modèle numérique d'élévation. Diverses mesures morphologiques sont ensuite extraites du réseau à intervalles réguliers (occupation et utilisation du sol, largeur, pente, surface drainée, altitude).

L'objectif de l'application est de fournir des outils d'analyse et d'interprétation de ces métriques afin de faciliter les diagnostics hydromorphologiques des cours d'eau français à différentes échelles (bassin versant, sous-bassin/région, axe fluvial). </p>
               ")
        ),
        column(
          width = 3,
          h2("Contact"),
          HTML("
            Lise Vaudor <br>
            <a href='mailto:lise.vaudor@ens-lyon.fr?subject=Application%20MAPDO'>lise.vaudor@ens-lyon.fr</a> <br>
            CNRS UMR 5600, Environnement Ville Soci&eacute;t&eacute; <br>
            <a href='https://umr5600.cnrs.fr/' target='_blank'>https://umr5600.cnrs.fr/</a></p>
         ")
        )
      ),


      tags$a(
        href = "https://evs-gis.github.io/mapdowebsite/",
        icon("book"),
        "Cliquez ici pour ouvrir la documentation et en savoir plus sur le projet et sa méthodologie.",
        target = "_blank"
      )
    )
  )
}

#' documentation Server Functions
#'
#' @noRd
mod_documentation_server <- function(id){
  moduleServer( id, function(input, output, session){
    ns <- session$ns

  })
}

## To be copied in the UI
# mod_documentation_ui("documentation_1")

## To be copied in the server
# mod_documentation_server("documentation_1")
