## Check if cluster_IDs match with sample
check_waypoints_to_cluster_id <- function(sampling_data = sampling_data,
                                          rlc_data = rlc_data,
                                          column_cluster_id_sampling = "show_cluster_id",
                                          column_waypoints_id_rlc = "cluster_ID") {
  # Make the dataset dataframes
  sampling_data <- as.data.frame(sampling_data)
  rlc_data <- as.data.frame(rlc_data)
  
  # Check columns available in the datasets
  if(!column_cluster_id_sampling %in% colnames(sampling_data)) stop("Make sure that the column name of the cluster_ID is correct.")
  if(!column_waypoints_id_rlc %in% colnames(rlc_data)) stop("Make sure that the column name of the show_cluster_id is correct.")
  
  ## list of cluster_IDs from sampling and rlc
  list_cluster_id_sampling <- sampling_data[[column_cluster_id_sampling]]
  list_cluster_id_rlc <- rlc_data[[column_waypoints_id_rlc]]
  
  ## check both lists match and no cluster_ID is missing
  difference_rlc_to_sample <- setdiff(list_cluster_id_sampling,list_cluster_id_rlc)
  difference_sample_to_rlc <- setdiff(list_cluster_id_rlc,list_cluster_id_sampling)
  
  if(!identical(difference_rlc_to_sample, character(0))){
    cat(paste(paste("Attention: \n\nThe following cluster IDs are missing from the rlc list", difference_rlc_to_sample, sep = " "),
              paste("The following cluster IDs might be matching from the sampling", difference_sample_to_rlc, sep = " "), sep = '\n'))
  } else {
    cat("All your clusters from the sampling frame are identical to the ones in the rlc data!!!\n\n")
  }
  
  ## check if every cluster_ID have 3 HH
  check_3_hh <- rlc_data %>% 
    group_by(!!sym(column_waypoints_id_rlc)) %>% 
    summarise(HHs = n()) %>% 
    mutate(flag = HHs != 3)
  
  if(any(check_3_hh$flag)) {
    cat(paste("\n\nHowever, not all your clusters have a total of 3 HHs.\n\n"))
    print(knitr::kable(check_3_hh %>% filter(flag) %>% dplyr::select(-flag)))
  } else {

    cat(paste("\n\nAll your clusters have a total of 3 HHs.\n\n"))
  }
}

## Function to create the polygon of the geoshape area
create_rlc_area_geoshape <- function(area_data,
                                     geoshape_column_name = "geoshape",
                                     crs = 4326) {
  # Make the dataset dataframes
  area_data <- as.data.frame(area_data)
  # Check columns available in the datasets
  if(!geoshape_column_name %in% colnames(area_data)) stop("Make sure that the column name of the geoshape is correct.")
  
  # Retrieve Geoshape area
  geoshape_df <- as.data.frame(do.call(rbind, strsplit(matrix(unlist(strsplit(area_data[[geoshape_column_name]], ";")),
                                                              ncol = 1, byrow = T), " "))) %>% 
    dplyr::select(V1,V2) %>% 
    mutate_all(., as.numeric) %>% 
    mutate(`.id` = 1)
  
  # create Geoshape for the sampled area
  geoshape_sf <- sfheaders::sf_polygon(
    obj = geoshape_df
    , polygon_id = ".id"
    , x = "V2"
    , y = "V1"
  ) %>% st_set_crs(crs)
  return(geoshape_sf)
}

## Check if cluster_IDs are inside the sampled area
check_waypoints_in_area <- function(area_data, 
                                    rlc_data,
                                    column_waypoints_id_rlc = "cluster_ID",
                                    column_waypoints_lat = "cluster_lat",
                                    column_waypoints_long = "cluster_long",
                                    geoshape_column_name = "geoshape",
                                    create_heat_map = F,
                                    crs = 4326) {
  
  # Make the dataset dataframes
  area_data <- as.data.frame(area_data)
  rlc_data <- as.data.frame(rlc_data)
  
  # Check columns available in the datasets
  if(!column_waypoints_id_rlc %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_ID is correct.")
  if(!column_waypoints_lat %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_lat is correct.")
  if(!column_waypoints_long %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_long is correct.")
  if(!geoshape_column_name %in% colnames(area_data)) stop("Make sure that the column name of the geoshape is correct.")
  
  # create the area of the geoshape file
  area_polygon <- create_rlc_area_geoshape(area_data = area_data,
                                           geoshape_column_name = geoshape_column_name,
                                           crs = crs)
  
  # read the cluster_ID points from rlc and check if points are in polygon
  cluster_points <- rlc_data %>% 
    dplyr::select(column_waypoints_id_rlc, column_waypoints_lat,column_waypoints_long) %>% 
    unique() %>% 
    mutate_at(vars(c(column_waypoints_lat,column_waypoints_long)), as.numeric) %>% 
    st_as_sf(coords = c(column_waypoints_long,column_waypoints_lat), crs = crs) %>% 
    mutate(intersect = st_intersects(geometry, area_polygon, sparse = F),
           distance = ifelse(!intersect, as.numeric(st_distance(geometry, area_polygon)),0)) %>% 
    mutate(intersect = ifelse(distance > 50, F,T))
  
  if(nrow(cluster_points %>% filter(!intersect)) > 0) {
    cat(paste("Attention:\nThe following cluster IDs are located outside of the sampling area \nwith a distance larger than 50m.\n"))
    print(knitr::kable(cluster_points %>% filter(!intersect) %>% dplyr::select(-intersect) %>%st_drop_geometry()))
  } else{
    cat("All your cluster points are located inside the sampling area.\n\n")
  }

  
  if(create_heat_map){
    cat("Generating the map:.\n\n")
    pb <- txtProgressBar(min = 0, max = 100, style = 3)
    heat <- rlc_data %>% 
      dplyr::select(column_waypoints_id_rlc,column_waypoints_lat,column_waypoints_long) %>% 
      mutate_at(vars(c(column_waypoints_lat,column_waypoints_long)), as.numeric) %>% 
      unique 
    setTxtProgressBar(pb, 20)
    heat <- data.table::setDT(heat)
    info <- st_bbox(area_polygon)
    
    heat_kde <- KernSmooth::bkde2D(heat[ , list(heat[[column_waypoints_long]],heat[[column_waypoints_lat]])],
                                   range.x = list(c(as.numeric(info$xmin[[1]]),
                                                    as.numeric(info$xmax[[1]])),
                                                  c(as.numeric(info$ymin[[1]]),
                                                    as.numeric(info$ymax[[1]]))),
                                   bandwidth = c(250/as.numeric(st_area(area_polygon)),
                                                 250/as.numeric(st_area(area_polygon))),
                                   gridsize = c(1300,1300))
    setTxtProgressBar(pb, 40)
    kerelDensityRaster <- raster::raster(list(x=heat_kde$x1,y=heat_kde$x2,z = heat_kde$fhat))
    
    kerelDensityRaster_2 <- raster::crop(kerelDensityRaster,raster::extent(area_polygon))
    kerelDensityRaster_3 <- raster::mask(kerelDensityRaster_2,area_polygon)
    setTxtProgressBar(pb, 60)
    kerelDensityRaster_3@data@values[which(as.character(kerelDensityRaster_3@data@values) == "0")] <- NA
    
    reverse <- rev(RColorBrewer::brewer.pal(11,"Spectral"))
    palRaster <- leaflet::colorNumeric(reverse, domain = kerelDensityRaster_3@data@values, na.color = "transparent")
    setTxtProgressBar(pb, 80)
    # Create map# Create maptest
    map_2 <- leaflet::leaflet() %>%
      leaflet::addTiles() %>%
      leaflet::addPolygons(data = area_polygon) %>%
      leaflet::addRasterImage(kerelDensityRaster_3,
                              colors = palRaster,
                              opacity = .8)
    setTxtProgressBar(pb, 100)
    close(pb)
    return(map_2)
  }else{
    # create quick map to show the polygon and the points with the cluster IDs
    map_1 <- leaflet::leaflet() %>% 
      leaflet::addTiles() %>% 
      leaflet::addPolygons(data = area_polygon) %>% 
      leaflet::addCircleMarkers(
        data = cluster_points %>% filter(distance<50),
        fillColor = "#EE5858",
        fillOpacity = 0.8,
        stroke = T,
        color = "#e1e1e1",
        weight= 2,
        radius = 5,
        popup = ~ cluster_points[[column_waypoints_id_rlc]]
        # clusterOptions = leaflet::markerClusterOptions()
      ) %>% 
      leaflet::addCircleMarkers(
        data = cluster_points %>% filter(distance>50),
        fillColor = "yellow",
        fillOpacity = 0.8,
        stroke = T,
        color = "#e1e1e1",
        weight= 2,
        radius = 5,
        popup = ~ cluster_points[[column_waypoints_id_rlc]]
      )
    return(map_1)
  }
    
}

## Check distance from waypoints to cluster sample
check_waypoints_to_cluster_distance <- function(sampling_data,
                                                rlc_data,
                                                area_data,
                                                column_cluster_id_sampling = "show_cluster_id",
                                                column_clusters_lat = "show_final_lat",
                                                column_clusters_long = "show_final_long",
                                                column_waypoints_id_rlc = "cluster_ID",
                                                column_waypoints_lat = "cluster_lat",
                                                column_waypoints_long = "cluster_long",
                                                geoshape_column_name = "geoshape",
                                                crs = 4326) {
  # Make the dataset dataframes
  sampling_data <- as.data.frame(sampling_data)
  area_data <- as.data.frame(area_data)
  rlc_data <- as.data.frame(rlc_data)
  
  # Check columns available in the datasets
  if(!column_cluster_id_sampling %in% colnames(sampling_data)) stop("Make sure that the column name of the show_cluster_id is correct.")
  if(!column_clusters_lat %in% colnames(sampling_data)) stop("Make sure that the column name of the show_final_lat is correct.")
  if(!column_clusters_long %in% colnames(sampling_data)) stop("Make sure that the column name of the show_final_long is correct.")
  if(!column_waypoints_id_rlc %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_ID is correct.")
  if(!column_waypoints_lat %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_lat is correct.")
  if(!column_waypoints_long %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_long is correct.")
  if(!geoshape_column_name %in% colnames(area_data)) stop("Make sure that the column name of the geoshape is correct.")
  
  waypoints <- rlc_data %>% 
    dplyr::select(column_waypoints_id_rlc,column_waypoints_lat,column_waypoints_long) %>% 
    unique() %>% 
    rename("show_cluster_id" = column_waypoints_id_rlc)%>% 
    st_as_sf(coords = c(column_waypoints_long,column_waypoints_lat), crs = crs) 
  # Join sampling data with 
  clusters <- sampling_data %>% 
    dplyr::select(column_cluster_id_sampling,column_clusters_lat,column_clusters_long) %>% 
    st_as_sf(coords = c(column_clusters_long,column_clusters_lat), crs = crs) 
  
  # Join the two data together and calculate the distance between the two points
  data_check <- left_join(clusters %>% as_data_frame(),waypoints%>% as_data_frame(), by = column_cluster_id_sampling)%>% 
    mutate(distance = as.numeric(st_distance(geometry.x,geometry.y, by_element = T)))  %>% filter(distance > 50) %>% 
    mutate(line = st_sfc(mapply(function(a,b) {st_cast(st_union(a,b), "LINESTRING")}, geometry.x, geometry.y, SIMPLIFY = F))) 
    
  area_polygon <- create_rlc_area_geoshape(area_data = area_data,
                                           geoshape_column_name = geoshape_column_name, 
                                           crs = crs)
  # flag
  if(nrow(data_check) > 0) {
    cat(paste("Attention:\n\nThe following waypoints are far from the generated cluster point \nwith a distance larger than 50m.\n"))
    print(knitr::kable(data_check %>%st_drop_geometry() %>% dplyr::select(-c(geometry.x,geometry.y,line))))
    map <- leaflet::leaflet() %>% 
      leaflet::addTiles() %>% 
      leaflet::addPolygons(data = area_polygon) %>% 
      leaflet::addCircleMarkers(
        data = data_check$geometry.y,
        fillColor = "#a1a1a1",
        fillOpacity = 0.8,
        stroke = T,
        color = "#191919",
        weight= 1,
        radius = 4
      ) %>% leaflet::addCircleMarkers(
        data = data_check$geometry.x,
        fillColor = "#a1a1a1",
        fillOpacity = 0.8,
        stroke = T,
        color = "#191919",
        weight= 1,
        radius = 4
      )%>% 
      leaflet::addPolylines(
        data = data_check$line,
        color = "#EE5859",
        fillColor = "black",
        opacity = 1,
        stroke = T,
        weight = 2
      ) 
    return(map)
  } else {
    cat("All your waypoints are close to the generated cluster points.")
  }
}


## Check if hh points are inside the sampled area
check_hh_in_area <- function(area_data, 
                             rlc_data,
                             column_waypoints_id_rlc = "cluster_ID",
                             column_hh_lat = "_household_geopoint_latitude",
                             column_hh_long = "_household_geopoint_longitude",
                             geoshape_column_name = "geoshape", 
                             crs = 4326) {
  
  # Make the dataset dataframes
  area_data <- as.data.frame(area_data)
  rlc_data <- as.data.frame(rlc_data)
  
  # Check columns available in the datasets
  if(!column_waypoints_id_rlc %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_ID is correct.")
  if(!column_hh_lat %in% colnames(rlc_data)) stop("Make sure that the column name of the _household_geopoint_latitude is correct.")
  if(!column_hh_long %in% colnames(rlc_data)) stop("Make sure that the column name of the _household_geopoint_longitude is correct.")
  if(!geoshape_column_name %in% colnames(area_data)) stop("Make sure that the column name of the geoshape is correct.")
  
  # create the area of the geoshape file
  area_polygon <- create_rlc_area_geoshape(area_data = area_data,
                                           geoshape_column_name = geoshape_column_name, 
                                           crs = crs)
  
  # read the household points from rlc and check if points are in polygon
  hh_points <- rlc_data %>% 
    dplyr::select(column_waypoints_id_rlc, column_hh_lat,column_hh_long) %>% 
    group_by(!!sym(column_waypoints_id_rlc)) %>% 
    mutate(hh_id = paste0(!!sym(column_waypoints_id_rlc), "_", row_number())) %>% 
    mutate_at(vars(c(column_hh_lat,column_hh_long)), as.numeric) %>% 
    st_as_sf(coords = c(column_hh_long,column_hh_lat), crs = crs) %>% 
    mutate(intersect = st_intersects(geometry, area_polygon, sparse = F),
           distance = ifelse(!intersect, as.numeric(st_distance(geometry, area_polygon)),0)) %>% 
    mutate(intersect = ifelse(distance > 50, F,T))
  
  if(nrow(hh_points %>% filter(!intersect)) > 0) {
    cat(paste("Attention:\n\nThe following households are located outside of the sampling area \nwith a distance larger than 50m.\n"))
    print(knitr::kable(hh_points %>% filter(!intersect) %>% dplyr::select(-intersect) %>%st_drop_geometry()))
  } else{
    cat("All your hh points are located inside the sampling area.")
  }
  # create quick map to show the polygon and the points with the cluster IDs
  map <- leaflet::leaflet() %>% 
    leaflet::addTiles() %>% 
    leaflet::addPolygons(data = area_polygon) %>% 
    leaflet::addCircleMarkers(
      data = hh_points %>% filter(distance<50),
      fillColor = "#EE5858",
      fillOpacity = 0.8,
      stroke = T,
      color = "#e1e1e1",
      weight= 1,
      radius = 4,
      popup = ~ hh_points$hh_id,
      clusterOptions = leaflet::markerClusterOptions(maxClusterRadius = 20)
    ) %>% 
    leaflet::addCircleMarkers(
      data = hh_points %>% filter(distance>50),
      fillColor = "yellow",
      fillOpacity = 0.8,
      stroke = T,
      color = "#e1e1e1",
      weight= 1,
      radius = 4,
      popup = ~ hh_points$hh_id
    ) 
  return(map)
}



## Check distance from hh to waypoint sample
check_hh_to_waypoint_distance <- function(rlc_data,
                                          area_data,
                                          column_waypoints_id_rlc = "cluster_ID",
                                          column_waypoints_lat = "cluster_lat",
                                          column_waypoints_long = "cluster_long",
                                          column_hh_lat = "_household_geopoint_latitude",
                                          column_hh_long = "_household_geopoint_longitude",
                                          geoshape_column_name = "geoshape", 
                                          crs = 4326) {
  # Make the dataset dataframes
  area_data <- as.data.frame(area_data)
  rlc_data <- as.data.frame(rlc_data)
  
  # Check columns available in the datasets
  if(!column_waypoints_id_rlc %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_ID is correct.")
  if(!column_waypoints_lat %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_lat is correct.")
  if(!column_waypoints_long %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_long is correct.")
  if(!column_hh_lat %in% colnames(rlc_data)) stop("Make sure that the column name of the _household_geopoint_latitude is correct.")
  if(!column_hh_long %in% colnames(rlc_data)) stop("Make sure that the column name of the _household_geopoint_longitude is correct.")
  if(!geoshape_column_name %in% colnames(area_data)) stop("Make sure that the column name of the geoshape is correct.")
  
  hh_data <- rlc_data %>% 
    dplyr::select(column_waypoints_id_rlc,column_hh_lat,column_hh_long) %>% 
    st_as_sf(coords = c(column_hh_long,column_hh_lat), crs = crs) 
  # Join sampling data with 
  waypoints <- rlc_data %>% 
    dplyr::select(column_waypoints_id_rlc,column_waypoints_lat,column_waypoints_long) %>% 
    st_as_sf(coords = c(column_waypoints_long,column_waypoints_lat), crs = crs)
  
  # Join the two data together and calculate the distance between the two points
  data_check <- left_join(hh_data %>% as_data_frame(),waypoints%>% as_data_frame(), by = column_waypoints_id_rlc,relationship = "many-to-many") %>% 
    mutate(distance = as.numeric(st_distance(geometry.x,geometry.y, by_element = T)))  %>% 
    unique()%>% filter(distance > 50)
  
  # flag
  if(nrow(data_check) > 0) {
    cat(paste("Attention:\n\nThe following hh points are far from the waypoint \nwith a distance larger than 50m.\n"))
    print(knitr::kable(data_check  %>%st_drop_geometry() %>%  dplyr::select(-c(geometry.x,geometry.y))))
  } else {
    cat("All your hh points are close to the waypoints.")
  }
  
  # create the lines between the waypoints and the hhs
  data_map <- left_join(hh_data %>% as_data_frame(),waypoints%>% as_data_frame(), by = column_waypoints_id_rlc,relationship = "many-to-many") %>% unique() %>%  
    mutate(line = st_sfc(mapply(function(a,b) {st_cast(st_union(a,b), "LINESTRING")}, geometry.x, geometry.y, SIMPLIFY = F))) 
  
  data_distance <- left_join(hh_data %>% as_data_frame(),waypoints%>% as_data_frame(), by = column_waypoints_id_rlc,relationship = "many-to-many") %>% 
    mutate(distance = as.numeric(st_distance(geometry.x,geometry.y, by_element = T)))  %>% 
    unique()%>% filter(distance > 50)
  
  # create the area of the geoshape file
  area_polygon <- create_rlc_area_geoshape(area_data = area_data,
                                           geoshape_column_name = geoshape_column_name,
                                           crs = crs)
  
  # create the map
  map <- leaflet::leaflet() %>% 
    leaflet::addTiles() %>% 
    leaflet::addPolygons(data = area_polygon) %>% 
    leaflet::addCircleMarkers(
      data = data_map$geometry.y,
      fillColor = "#a1a1a1",
      fillOpacity = 0.8,
      stroke = T,
      color = "#191919",
      weight= 1,
      radius = 4
    ) %>% 
    leaflet::addPolylines(
      data = data_map$line,
      color = "#EE5859",
      fillColor = "black",
      opacity = 1,
      stroke = T,
      weight = 2
    ) 
  
  return(map)
}

## Function to check if any hh is falling in a different 3 closest hh waypoint
check_hh_unique_to_waypoint <- function(area_data,
                                        rlc_data,
                                        column_waypoints_id_rlc = "cluster_ID",
                                        column_waypoints_lat = "cluster_lat",
                                        column_waypoints_long = "cluster_long",
                                        column_hh_lat = "_household_geopoint_latitude",
                                        column_hh_long = "_household_geopoint_longitude",
                                        geoshape_column_name = "geoshape",
                                        crs = 4326) {
  # Make the dataset dataframes
  area_data <- as.data.frame(area_data)
  rlc_data <- as.data.frame(rlc_data)
  
  # Check columns available in the datasets
  if(!column_waypoints_id_rlc %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_ID is correct.")
  if(!column_waypoints_lat %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_lat is correct.")
  if(!column_waypoints_long %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_long is correct.")
  if(!column_hh_lat %in% colnames(rlc_data)) stop("Make sure that the column name of the _household_geopoint_latitude is correct.")
  if(!column_hh_long %in% colnames(rlc_data)) stop("Make sure that the column name of the _household_geopoint_longitude is correct.")
  if(!geoshape_column_name %in% colnames(area_data)) stop("Make sure that the column name of the geoshape is correct.")
  
  hh_data <- rlc_data %>% 
    dplyr::select(column_waypoints_id_rlc,column_hh_lat,column_hh_long) %>% 
    st_as_sf(coords = c(column_hh_long,column_hh_lat), crs = crs) 
  
  waypoints <- rlc_data %>% 
    dplyr::select(column_waypoints_id_rlc,column_waypoints_lat,column_waypoints_long) %>% 
    st_as_sf(coords = c(column_waypoints_long,column_waypoints_lat), crs = crs)
  
  # Join the two data together and calculate the distance between the two points
  data_max_distance <- left_join(hh_data %>% as_data_frame(),waypoints%>% as_data_frame(), by = column_waypoints_id_rlc,relationship = "many-to-many") %>% 
    mutate(distance = as.numeric(st_distance(geometry.x,geometry.y, by_element = T))) %>% unique() %>% 
    group_by(!!sym(column_waypoints_id_rlc))%>% 
    filter(distance == max(distance)) %>% 
    mutate(buffer = st_buffer(geometry.y, distance,nQuadSegs=120)) 
  
  # check buffer intersect
  buffer_intersect <- st_intersects(data_max_distance$buffer, data_max_distance$buffer) %>% unique
  buffer_intersect <- buffer_intersect[lapply(buffer_intersect, length) == 2]
  suppressWarnings(list_buffer_intersect <- as.data.frame(sapply(buffer_intersect, function(x) str_split(x," "))) %>% 
                       mutate_all(., as.numeric) %>% 
                       t() %>% as.data.frame() %>% 
                       mutate_all(., ~sapply(.,function(x){
                         data_max_distance[[column_waypoints_id_rlc]][x]
                       })) %>% 
                     rename(intersecting = "V1",clusters = "V2")
                     )
  # If buffer intersect, check if hhs are sharing clusters
  if(nrow(list_buffer_intersect)>0){
    data_distance <- left_join(hh_data %>% as_data_frame(),waypoints%>% as_data_frame(), by = column_waypoints_id_rlc,relationship = "many-to-many") %>% 
      mutate(distance = as.numeric(st_distance(geometry.x,geometry.y, by_element = T))) %>% unique() %>% 
      group_by(!!sym(column_waypoints_id_rlc)) %>% 
      mutate(hh_id = paste0(!!sym(column_waypoints_id_rlc), "_", row_number())) 
    hh_intersect_buffer <- st_intersects(data_distance$geometry.x, data_max_distance$buffer)
    hh_intersect_buffer <- hh_intersect_buffer[lapply(hh_intersect_buffer, length) == 2]
    if(length(hh_intersect_buffer) > 0 ) {
      suppressWarnings(
        list_hh_buffer_intersect <- as.data.frame(sapply(hh_intersect_buffer, function(x) str_split(x," "))) %>% 
                         mutate_all(., as.numeric)%>% 
                         t() %>% as.data.frame() %>% 
                         mutate(V1 = sapply(V1,function(x){data_distance[["hh_id"]][x]}),
                                V2 = sapply(V2,function(x){data_max_distance[[column_waypoints_id_rlc]][x]}))%>% 
                         rename(household = "V1",clusters = "V2"))
    } else {
      list_hh_buffer_intersect <- data.frame()
    }
  }
  
  if(nrow(list_buffer_intersect)>0 & nrow(list_hh_buffer_intersect) > 0){
    cat(paste("Attention:\n\nThe following clusters have their buffer zone intersecting\n"))
    print(knitr::kable(list_buffer_intersect))
    cat(paste("\nAnd the following hh cluster IDs are falling in other clusters buffers\n"))
    print(knitr::kable(list_hh_buffer_intersect))
  } else if (nrow(list_buffer_intersect)>0 & nrow(list_hh_buffer_intersect) == 0){
    cat(paste("Attention:\n\nThe following clusters have their buffer zone intersecting\nbut none of the hh are sharing clusters\n"))
    print(knitr::kable(list_buffer_intersect))
  } else {
    cat(paste("All clusters are unique and do not intersect with each other."))
  }
  
  # create the lines between the waypoints and the hhs
  data_map <- left_join(hh_data %>% as_data_frame(),waypoints%>% as_data_frame(), by = column_waypoints_id_rlc,relationship = "many-to-many") %>% unique() %>%  
    mutate(distance = as.numeric(st_distance(geometry.x,geometry.y, by_element = T))) %>% unique() %>% 
    group_by(!!sym(column_waypoints_id_rlc))%>% 
    mutate(line = st_sfc(mapply(function(a,b) {st_cast(st_union(a,b), "LINESTRING")}, geometry.x, geometry.y, SIMPLIFY = F))) 
  
  # create the area of the geoshape file
  area_polygon <- create_rlc_area_geoshape(area_data = area_data,
                                           geoshape_column_name = geoshape_column_name, 
                                           crs = crs)
  
  # Create map
  map <- leaflet::leaflet() %>% 
    leaflet::addTiles() %>% 
    leaflet::addPolygons(data = area_polygon) %>% 
    leaflet::addPolygons(data = data_max_distance$buffer,
                         stroke = T,
                         fillColor = "#EE5859",
                         fillOpacity = 0.8,
                         color = "#191919",
                         weight= 1) %>% 
    leaflet::addCircleMarkers(
      data = data_max_distance$geometry.y,
      fillColor = "#a1a1a1",
      fillOpacity = 0.8,
      stroke = T,
      color = "#191919",
      weight= 1,
      radius = 4
    ) %>% 
    leaflet::addPolylines(
      data = data_map$line,
      color = "black",
      fillColor = "black",
      opacity = 1,
      stroke = T,
      weight = 2
    ) 
  
  return(map)
}

create_hh_density_weight <- function(area_data,
                                     rlc_data,
                                     column_waypoints_id_rlc = "cluster_ID",
                                     column_waypoints_lat = "cluster_lat",
                                     column_waypoints_long = "cluster_long",
                                     column_hh_lat = "_household_geopoint_latitude",
                                     column_hh_long = "_household_geopoint_longitude", 
                                     crs = 4326) {
  # Make the dataset dataframes
  rlc_data <- as.data.frame(rlc_data)
  
  # Check columns available in the datasets
  if(!column_waypoints_id_rlc %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_ID is correct.")
  if(!column_waypoints_lat %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_lat is correct.")
  if(!column_waypoints_long %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_long is correct.")
  if(!column_hh_lat %in% colnames(rlc_data)) stop("Make sure that the column name of the _household_geopoint_latitude is correct.")
  if(!column_hh_long %in% colnames(rlc_data)) stop("Make sure that the column name of the _household_geopoint_longitude is correct.")

  ## HH Data
  hh_data <- rlc_data %>% 
    dplyr::select(column_waypoints_id_rlc,column_hh_lat,column_hh_long) %>% 
    st_as_sf(coords = c(column_hh_long,column_hh_lat), crs = crs) 
  
  ## Waypoints
  waypoints <- rlc_data %>% 
    dplyr::select(column_waypoints_id_rlc,column_waypoints_lat,column_waypoints_long) %>% 
    st_as_sf(coords = c(column_waypoints_long,column_waypoints_lat), crs = crs)
  
  # Join the two data together and calculate the distance between the two points
  data_max_distance <- left_join(hh_data %>% as_tibble(),waypoints%>% as_tibble(), by = column_waypoints_id_rlc,relationship = "many-to-many") %>% 
    mutate(distance = as.numeric(st_distance(geometry.x,geometry.y, by_element = T))) %>% unique()%>% 
    group_by(!!sym(column_waypoints_id_rlc))%>% 
    filter(distance == max(distance)) ## Filter for the furthest distance
  
  hh_density_cluster <- data_max_distance%>% 
    st_drop_geometry() %>% 
    mutate(hhd_cluster = (3/(pi * (distance^2)))) 
  
  avg_hhd_total_area <- mean(hh_density_cluster$hhd_cluster, na.rm = T)
    
  hh_density_cluster <- hh_density_cluster %>%
    mutate(weight = hhd_cluster/avg_hhd_total_area) %>% 
    dplyr::select(column_waypoints_id_rlc,hhd_cluster,weight)
  
  if(nrow(hh_density_cluster)>0){
    cat(paste("Here is the generated weights table."))
    print(knitr::kable(hh_density_cluster))
  } else {
    cat(paste0())
  }
  return(hh_density_cluster)
}

create_density_map_idw <- function(area_data,
                               hh_density_cluster,
                               column_waypoints_id_rlc = "cluster_ID",
                               column_waypoints_lat = "cluster_lat",
                               column_waypoints_long = "cluster_long",
                               geoshape_column_name = "geoshape",
                               ncell_raster = 500,
                               power_idw = 2,
                               crs = 4326){
  # Make the dataset dataframes
  hh_density_cluster <- as.data.frame(hh_density_cluster)
  area_data <- as.data.frame(area_data)
  rlc_data <- as.data.frame(rlc_data)
  
  # Check columns available in the datasets
  if(!column_waypoints_id_rlc %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_ID is correct.")
  if(!column_waypoints_lat %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_lat is correct.")
  if(!column_waypoints_long %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_long is correct.")
  if(!geoshape_column_name %in% colnames(area_data)) stop("Make sure that the column name of the geoshape is correct.")
  
  # create polygon
  polygon <- create_rlc_area_geoshape(area_data = area_data,
                                      geoshape_column_name = geoshape_column_name,
                                      crs = crs)
  
  # prepare data
  data_density <- hh_density_cluster %>% 
    left_join(rlc_data %>% 
                dplyr::select(c(column_waypoints_id_rlc,column_waypoints_lat,column_waypoints_long)) %>% 
                unique, by = column_waypoints_id_rlc)
  rp <- shp2raster(shp = polygon,column=".id", ncells = ncell_raster)

  suppressMessages(library(spatstat))
  
  # Transform the raster to class im then owin to enter the ppp function
  r <- raster::res(rp)
  orig <- sp::bbox(rp)[, 1] + 0.5 * r
  dm <- dim(rp)[2:1]
  xx <- unname(orig[1] + cumsum(c(0, rep(r[1], dm[1] - 1))))
  yy <- unname(orig[2] + cumsum(c(0, rep(r[2], dm[2] - 1))))
  im <- im(matrix(raster::values(rp), ncol = dm[1], 
                    nrow = dm[2], byrow = TRUE)[dm[2]:1, ], 
             xcol = xx, yrow = yy)
  owin <- as.owin(im)
  suppressWarnings(
  ppp_data <- ppp(as.numeric(data_density[[column_waypoints_long]]),as.numeric(data_density[[column_waypoints_lat]]),
                  marks =as.numeric(data_density$weight), window = owin)
  )
  
  cat(paste0("The below plot is use the Mean Squared Error (mse) to compute the average squared difference between the powers and detect the optimal power"))
  
  plot(idw(ppp_data, power=0.1, at="pixels"),col=heat.colors(20), main="power = 0.1")
  readline(prompt = "Please [Enter] to continue")
  plot(idw(ppp_data, power=1, at="pixels"),col=heat.colors(20), main="power = 1")
  readline(prompt = "Please [Enter] to continue")
  plot(idw(ppp_data, power=2, at="pixels"),col=heat.colors(20), main="power = 2")
  readline(prompt = "Please [Enter] to continue")
  plot(idw(ppp_data, power=4, at="pixels"),col=heat.colors(20), main="power = 4")
  readline(prompt = "Please [Enter] to continue")
  plot(idw(ppp_data, power=6, at="pixels"),col=heat.colors(20), main="power = 6")
  readline(prompt = "Please [Enter] to continue")
  plot(idw(ppp_data, power=8, at="pixels"),col=heat.colors(20), main="power = 8") 
  readline(prompt = "Please [Enter] to continue")
  
  powers <- seq(0.001, 10, 0.01)
  mse_result <- NULL
  for(power in powers){
    CV_idw <- idw(ppp_data, power=power, at="points")
    mse_result <- c(mse_result,
                    Metrics::mse(ppp_data$marks,CV_idw))
  }
  optimal_power <- powers[which.min(mse_result)]
  
  cat(paste0("The below plot is use the Mean Squared Error (mse) to compute the average squared difference between the powers and detect the optimal power"))

  plot(powers, mse_result) +
    abline(v = optimal_power,
           col = "red") 
  readline(prompt = "Please [Enter] to continue")
  
  cat(paste0("The optimal power detected to be used is: ",optimal_power,"\n\nHowever the function by default set the power to 2.\n\n"))
  cat(paste0("You can change it by going to rlc_sample_template.Rmd, (line 168) and change the 2 to 999 for optimal\n\nor\n\nany other number.\n\nThe plot is your reference.\n\n"))
  cat(paste0("You can change the number of cells generated inside the defined area for the IDW calculation. By default, it is 500.\n\n"))
  
  # cereate the idw raster
  if(as.numeric(power_idw) == 999){
    power_idw <- optimal_power
  }
  
  idw_raster <- raster::raster(idw(ppp_data, power= power_idw,at="pixels"))
  
  cat(paste0("By default, IDW calculation assumes that the data point locations are fixed,\nthat is, the standard error only takes into account the variability in the mark values,\nand not the variability due to randomness of the data point locations.\n"))
  cat(paste0("Here is an example if we set IDW to estimate standard error of points.\n"))
  plot(idw(ppp_data, power= power_idw, se=TRUE)$SE,col=heat.colors(20), main="SE Enabled")
  readline(prompt = "Please [Enter] to continue")
  # set crs for the raster
  crs(idw_raster) <- crs
  
  # Handle NA values
  idw_raster[][which(as.character(idw_raster[]) == "0")] <- NA
  
  # Color paletter
  palRaster <- leaflet::colorNumeric(RColorBrewer::brewer.pal(5,"YlOrRd"), domain = idw_raster[], na.color = "transparent")
  
  # create map
  map <- leaflet::leaflet() %>% 
    leaflet::addTiles() %>% 
    leaflet::addPolygons(data = polygon,
                         color = ) %>%
    leaflet::addRasterImage(idw_raster,colors = palRaster,opacity = 0.9) %>% 
    leaflet::addLegend(position = "bottomright",
                       pal = palRaster,
                       values = idw_raster[],
                       title = "Weights of IDW")
  
  # return map
  return(map)
}

estimate_population_area <- function(area_data,
                              rlc_data,
                              hh_density_cluster,
                              column_waypoints_id_rlc = "cluster_ID",
                              column_area = "sum_area",
                              calculate_area = T,
                              column_num_hh_members = "num_hh_members",
                              geoshape_column_name = "geoshape") {
  # Make the dataset dataframes
  area_data <- as.data.frame(area_data)
  rlc_data <- as.data.frame(rlc_data)
  hh_density_cluster <- as.data.frame(hh_density_cluster)
  # Check columns available in the datasets
  if(!column_area %in% colnames(area_data) & calculate_area) stop("Make sure that the column name of the sum of the area is correct.")
  if(!column_waypoints_id_rlc %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_ID is correct.")
  if(!geoshape_column_name %in% colnames(area_data)) stop("Make sure that the column name of the geoshape is correct.")
  # create the area of the geoshape file
  area_polygon <- create_rlc_area_geoshape(area_data = area_data,
                                           geoshape_column_name = geoshape_column_name)
  if(calculate_area){
    sum_area <- as.numeric(st_area(area_polygon))
  } else {
    sum_area <- as.numeric(area_data[[column_area]])
  }
  
  ## Calculate the average hh
  average_people_per_hh <- mean(as.numeric(rlc_data[[column_num_hh_members]]), na.rm=T)
  ## calculate estimate number of hhs and peoples for total area
  average_hhd_area <- mean(hh_density_cluster$hhd_cluster, na.rm = T)
  #HH
  est_total_hh_area <- round(average_hhd_area * sum_area,0)
  #Peopl
  est_total_people_area <- round(est_total_hh_area * average_people_per_hh,0)
  if(is.null(est_total_hh_area)|is.null(est_total_people_area)|is.null(average_people_per_hh)){
    cat("Estimation of number of household and people couldn't be calcualted.\n\nCheck your inputs.\n\n")
  } else {
    cat(paste0("Estimated number of household for the total area: ", est_total_hh_area,"\n\n"))
    cat(paste0("Estimated number of people for the total area: ", est_total_people_area,"\n\n"))
    cat(paste0("Average number of people per household: ", average_people_per_hh,"\n\n"))
  }
  
}


shp2raster <- function(
    shpname="",    # single file name like "coolstuff.shp". Ignored if shp is given.
    shp=NULL,      # Shapefile Object Spatial*DataFrame. If NULL, it reads shpname with rgdal::readOGR.
    r=NULL,        # Target raster, e.g. with raster::raster(other_tif_file) 
    # if not given, it is infered from the shapefile extend and cellsize
    ncells=99,     # Approximate number of cells in either direction to determine cellsize.
    cellsize=NA,   # Cell size in coordinate units (usually degrees or m). Computed from ncells if NA.
    ncellwarn=1000,# Warn if there will be more cells than this. To prevent e.g. accidental degrees instead of km.
    column="",     # Name of column to use for z dimension in raster. Empty string for interactive selection.
    ascname=NA,    # Output file name. If NA, inferred from shpname or shp.
    verbose=FALSE, # Report readOGR progress?
    ...)           # More arguments passed to raster::rasterize, like overwrite=TRUE
{
  # if shp is missing/default, read shpname:
  if(is.null(shp)) 
  {
    # message("Reading '",shpname,"' ...")
    shp <- rgdal::readOGR(dsn=shpname, 
                          layer=basename(tools::file_path_sans_ext(shpname)),
                          verbose=verbose)
    if(is.na(ascname)) ascname <- sub(".shp", ".asc", shpname)
  } else
    if(is.na(ascname)) ascname <- paste0(deparse(substitute(shp)),".asc")
    
    # target raster extend and resolution:
    # if target raster is given, it is used directly
    if(is.null(r)){
      # message("Computing raster extent")
      e <- raster::extent(shp) 
      if(is.na(cellsize)) cellsize <- mean(c((e@xmax-e@xmin), (e@ymax-e@ymin))/ncells)
      nx <- (e@xmax-e@xmin)/cellsize # this seems revertive from the previous line, but
      ny <- (e@ymax-e@ymin)/cellsize # is needed because ncells differ in both directions
      cont <- TRUE # continue by default
      if(max(nx,ny)>ncellwarn) cont <- readline(paste0("Raster will be large: nx=",
                                                       round(nx,1), ", ny=",round(ny,1)," (with cellsize=", round(cellsize,4),", xmin=",
                                                       round(e@xmin,2), ", xmax=",round(e@xmax,2),"). Continue? y/n: "))
      cont <- tolower(cont) %in% c("y", "yes", "t", "true", "")
      if(!cont) return(list(nx=nx, ny=ny, cellsize=cellsize, extend_shp=e))
      r <- raster(ncol=nx, nrow=ny)
      raster::extent(r) <- extent(shp)
      resdif <- abs((yres(r) - xres(r)) / yres(r) )
      if(resdif > 0.01) stop("Horizontal (",round(xres(r),3),") and vertical (", round(yres(r),3),
                             ") resolutions are too different (diff=",round(resdif,3), ", but must be <0.010).\n",
                             "  Use a smaller cell size to achieve this (currently ",round(cellsize,1),").")
    }
    
    # column selection
    n <- names(shp)
    if(!column %in% n) message("Column '",column, "' is not in Shapefile. Select one of\n", 
                               paste(strwrap(toString(n)), collapse="\n"))
    while(!column %in% n) column <- readline(paste0("Nonexistent column '",column, 
                                                    "'. Type desired name, then hit ENTER: "))
    # actually convert and write to file:
    # newascname <- berryFunctions::newFilename(ascname, quiet = T)
    ras <- raster::rasterize(shp, r, field=column, proj=shp@proj4string, ...)
    # return output
    ras
}

