streetview.metadata <- function(dt, api_key, road.points.panoid.location, save.location){
  ID <- dt$ID; lat <- dt$lat; long <- dt$long
  api <- list()
  api[[1]] <- 'https://maps.googleapis.com/maps/api/streetview/metadata?size=600x300&location='
  api[[2]] <- paste0(lat, ',', long, '&')
  api[[3]] <- paste0('key=', api_key)
  api.url <- paste0(unlist(api), collapse = '')
  panorama <- try(unlist(rjson::fromJSON(file=api.url)))
  dt.panoid <- try(data.table::data.table(roads.point.id = ID, t(panorama)))
  # Save the road pano match
  save.file <- paste0(save.location, 'road.points.id:', ID, '; pano_id:', dt.panoid$pano_id, '.rds')
  saveRDS(dt.panoid, file=save.file)
  #return(dt.panoid)
}