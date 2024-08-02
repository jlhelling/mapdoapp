#' get IGN remonterletemps url.
#'
#' This function take longitude and latitude to build and url to go IGN remonterletemps website on the same place.
#'
#' @param lng longitude.
#' @param lat latitude.
#' @param zoom zoom level.
#'
#' @importFrom glue glue
#'
#' @return a string with url link.
#' @export
#'
#' @examples
#' utils_url_remonterletemps(lng=6.869433, lat=45.923690, zoom = 12)
utils_url_remonterletemps <- function(lng=6.869433,
                                      lat=45.923690,
                                      zoom = 12){
  url <- glue::glue("https://remonterletemps.ign.fr/comparer/basic?x={lng}&y={lat}&z={zoom}&layer1=GEOGRAPHICALGRIDSYSTEMS.PLANIGNV2&layer2=ORTHOIMAGERY.ORTHOPHOTOS&mode=vSlider")
  return(url)
}
