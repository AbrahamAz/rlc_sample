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
    print(knitr::kable(check_3_hh %>% filter(flag) %>% select(-flag)))
  } else {

    cat(paste("\n\nAll your clusters have a total of 3 HHs.\n\n"))
  }
}

## Function to create the polygon of the geoshape area
create_rlc_area_geoshape <- function(area_data,
                                     geoshape_column_name = "geoshape") {
  # Make the dataset dataframes
  area_data <- as.data.frame(area_data)
  # Check columns available in the datasets
  if(!geoshape_column_name %in% colnames(area_data)) stop("Make sure that the column name of the geoshape is correct.")
  
  # Retrieve Geoshape area
  geoshape_df <- as.data.frame(do.call(rbind, strsplit(matrix(unlist(strsplit(area_data[[geoshape_column_name]], ";")),
                                                              ncol = 1, byrow = T), " "))) %>% 
    select(V1,V2) %>% 
    mutate_all(., as.numeric) %>% 
    mutate(`.id` = 1)
  
  # create Geoshape for the sampled area
  geoshape_sf <- sfheaders::sf_polygon(
    obj = geoshape_df
    , polygon_id = ".id"
    , x = "V2"
    , y = "V1"
  ) %>% st_set_crs(4326)
  return(geoshape_sf)
}

## Check if cluster_IDs are inside the sampled area
check_waypoints_in_area <- function(area_data, 
                                    rlc_data,
                                    column_waypoints_id_rlc = "cluster_ID",
                                    column_waypoints_lat = "cluster_lat",
                                    column_waypoints_long = "cluster_long",
                                    geoshape_column_name = "geoshape") {
  
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
                                           geoshape_column_name = "geoshape")
  
  # read the cluster_ID points from rlc and check if points are in polygon
  cluster_points <- rlc_data %>% 
    select(column_waypoints_id_rlc, column_waypoints_lat,column_waypoints_long) %>% 
    unique() %>% 
    mutate_at(vars(c(column_waypoints_lat,column_waypoints_long)), as.numeric) %>% 
    st_as_sf(coords = c(column_waypoints_long,column_waypoints_lat), crs = 4326) %>% 
    mutate(intersect = st_intersects(geometry, area_polygon, sparse = F),
           distance = ifelse(!intersect, as.numeric(st_distance(geometry, area_polygon)),0)) %>% 
    mutate(intersect = ifelse(distance > 50, F,T))
  
  if(nrow(cluster_points %>% filter(!intersect)) > 0) {
    cat(paste("Attention:\nThe following cluster IDs are located outside of the sampling area \nwith a distance larger than 50m.\n"))
    print(knitr::kable(cluster_points %>% filter(!intersect) %>% select(-intersect) %>%st_drop_geometry()))
  } else{
    cat("All your cluster points are located inside the sampling area.")
  }
  # create quick map to show the polygon and the points with the cluster IDs
  map <- leaflet::leaflet() %>% 
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
  return(map)
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
                                                geoshape_column_name = "geoshape") {
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
    select(column_waypoints_id_rlc,column_waypoints_lat,column_waypoints_long) %>% 
    unique() %>% 
    rename("show_cluster_id" = column_waypoints_id_rlc)%>% 
    st_as_sf(coords = c(column_waypoints_long,column_waypoints_lat), crs = 4326) 
  # Join sampling data with 
  clusters <- sampling_data %>% 
    select(column_cluster_id_sampling,column_clusters_lat,column_clusters_long) %>% 
    st_as_sf(coords = c(column_clusters_long,column_clusters_lat), crs = 4326) 
  
  # Join the two data together and calculate the distance between the two points
  data_check <- left_join(clusters %>% as_data_frame(),waypoints%>% as_data_frame(), by = column_cluster_id_sampling)%>% 
    mutate(distance = as.numeric(st_distance(geometry.x,geometry.y, by_element = T)))  %>% filter(distance > 50) %>% 
    mutate(line = st_sfc(mapply(function(a,b) {st_cast(st_union(a,b), "LINESTRING")}, geometry.x, geometry.y, SIMPLIFY = F))) 
    
  area_polygon <- create_rlc_area_geoshape(area_data = area_data,
                                           geoshape_column_name = "geoshape")
  # flag
  if(nrow(data_check) > 0) {
    cat(paste("Attention:\n\nThe following waypoints are far from the generated cluster point \nwith a distance larger than 50m.\n"))
    print(knitr::kable(data_check %>%st_drop_geometry() %>% select(-c(geometry.x,geometry.y,line))))
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
                             geoshape_column_name = "geoshape") {
  
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
                                           geoshape_column_name = "geoshape")
  
  # read the household points from rlc and check if points are in polygon
  hh_points <- rlc_data %>% 
    select(column_waypoints_id_rlc, column_hh_lat,column_hh_long) %>% 
    group_by(!!sym(column_waypoints_id_rlc)) %>% 
    mutate(hh_id = paste0(!!sym(column_waypoints_id_rlc), "_", row_number())) %>% 
    mutate_at(vars(c(column_hh_lat,column_hh_long)), as.numeric) %>% 
    st_as_sf(coords = c(column_hh_long,column_hh_lat), crs = 4326) %>% 
    mutate(intersect = st_intersects(geometry, area_polygon, sparse = F),
           distance = ifelse(!intersect, as.numeric(st_distance(geometry, area_polygon)),0)) %>% 
    mutate(intersect = ifelse(distance > 50, F,T))
  
  if(nrow(hh_points %>% filter(!intersect)) > 0) {
    cat(paste("Attention:\n\nThe following households are located outside of the sampling area \nwith a distance larger than 50m.\n"))
    print(knitr::kable(hh_points %>% filter(!intersect) %>% select(-intersect) %>%st_drop_geometry()))
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
                                          geoshape_column_name = "geoshape") {
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
    select(column_waypoints_id_rlc,column_hh_lat,column_hh_long) %>% 
    st_as_sf(coords = c(column_hh_long,column_hh_lat), crs = 4326) 
  # Join sampling data with 
  waypoints <- rlc_data %>% 
    select(column_waypoints_id_rlc,column_waypoints_lat,column_waypoints_long) %>% 
    st_as_sf(coords = c(column_waypoints_long,column_waypoints_lat), crs = 4326)
  
  # Join the two data together and calculate the distance between the two points
  data_check <- left_join(hh_data %>% as_data_frame(),waypoints%>% as_data_frame(), by = column_waypoints_id_rlc,relationship = "many-to-many") %>% 
    mutate(distance = as.numeric(st_distance(geometry.x,geometry.y, by_element = T)))  %>% 
    unique()%>% filter(distance > 50)
  
  # flag
  if(nrow(data_check) > 0) {
    cat(paste("Attention:\n\nThe following hh points are far from the waypoint \nwith a distance larger than 50m.\n"))
    print(knitr::kable(data_check  %>%st_drop_geometry() %>%  select(-c(geometry.x,geometry.y))))
  } else {
    cat("All your hh points are close to the waypoints.")
  }
  
  # create the lines between the waypoints and the hhs
  data_map <- left_join(hh_data %>% as_data_frame(),waypoints%>% as_data_frame(), by = column_waypoints_id_rlc,relationship = "many-to-many") %>% unique() %>%  
    mutate(line = st_sfc(mapply(function(a,b) {st_cast(st_union(a,b), "LINESTRING")}, geometry.x, geometry.y, SIMPLIFY = F))) 
  
  # create the area of the geoshape file
  area_polygon <- create_rlc_area_geoshape(area_data = area_data,
                                           geoshape_column_name = "geoshape")
  
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
                                        geoshape_column_name = "geoshape") {
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
    select(column_waypoints_id_rlc,column_hh_lat,column_hh_long) %>% 
    st_as_sf(coords = c(column_hh_long,column_hh_lat), crs = 4326) 
  
  waypoints <- rlc_data %>% 
    select(column_waypoints_id_rlc,column_waypoints_lat,column_waypoints_long) %>% 
    st_as_sf(coords = c(column_waypoints_long,column_waypoints_lat), crs = 4326)
  
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
                                           geoshape_column_name = "geoshape")
  
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
                                     geoshape_column_name = "geoshape") {
  # Make the dataset dataframes
  rlc_data <- as.data.frame(rlc_data)
  
  # Check columns available in the datasets
  if(!column_waypoints_id_rlc %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_ID is correct.")
  if(!column_waypoints_lat %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_lat is correct.")
  if(!column_waypoints_long %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_long is correct.")
  if(!column_hh_lat %in% colnames(rlc_data)) stop("Make sure that the column name of the _household_geopoint_latitude is correct.")
  if(!column_hh_long %in% colnames(rlc_data)) stop("Make sure that the column name of the _household_geopoint_longitude is correct.")
  if(!geoshape_column_name %in% colnames(area_data)) stop("Make sure that the column name of the geoshape is correct.")
  
  ## HH Data
  hh_data <- rlc_data %>% 
    select(column_waypoints_id_rlc,column_hh_lat,column_hh_long) %>% 
    st_as_sf(coords = c(column_hh_long,column_hh_lat), crs = 4326) 
  
  ## Waypoints
  waypoints <- rlc_data %>% 
    select(column_waypoints_id_rlc,column_waypoints_lat,column_waypoints_long) %>% 
    st_as_sf(coords = c(column_waypoints_long,column_waypoints_lat), crs = 4326)
  
  # Join the two data together and calculate the distance between the two points
  data_max_distance <- left_join(hh_data %>% as_data_frame(),waypoints%>% as_data_frame(), by = column_waypoints_id_rlc,relationship = "many-to-many") %>% 
    mutate(distance = as.numeric(st_distance(geometry.x,geometry.y, by_element = T))) %>% unique() %>% 
    group_by(!!sym(column_waypoints_id_rlc))%>% 
    filter(distance == max(distance)) ## Filter for the furthest distance
  
  hh_density_cluster <- data_max_distance%>% 
    st_drop_geometry() %>% 
    mutate(hhd_cluster = (3/(pi * (distance^2)))) 
  
  avg_hhd_total_area <- mean(hh_density_cluster$hhd_cluster, na.rm = T)
    
  hh_density_cluster <- hh_density_cluster %>%
    mutate(weight = hhd_cluster/avg_hhd_total_area) %>% 
    select(column_waypoints_id_rlc,hhd_cluster,weight)
  
  if(nrow(hh_density_cluster)>0){
    cat(paste("Here is the generated weights table."))
    print(knitr::kable(hh_density_cluster))
  } else {
    cat(paste0())
  }
 #  # create the area of the geoshape file
 #  area_polygon <- create_rlc_area_geoshape(area_data = area_data,
 #                                           geoshape_column_name = "geoshape")
 #  
 #  # Create map
 # leaflet::leaflet() %>% 
 #    leaflet::addTiles() %>% 
 #    leaflet::addPolygons(data = area_polygon) %>%
 #    leaflet.extras::addHeatmap(data = hh_density_cluster,
 #                               lng = ~ cluster_long,
 #                               lat = ~ cluster_lat,
 #                               intensity = ~ weight*0.1,
 #                               blur = 15, max = 5, radius = 205)
  return(hh_density_cluster)
}



estimate_population_area <- function(area_data,
                              rlc_data,
                              hh_density_cluster,
                              column_waypoints_id_rlc = "cluster_ID",
                              column_area = "sum_area",
                              calculate_area = T,
                              column_num_hh_members = "num_hh_members") {
  # Make the dataset dataframes
  area_data <- as.data.frame(area_data)
  rlc_data <- as.data.frame(rlc_data)
  hh_density_cluster <- as.data.frame(hh_density_cluster)
  # Check columns available in the datasets
  if(!column_area %in% colnames(area_data) & calculate_area) stop("Make sure that the column name of the sum of the area is correct.")
  if(!column_waypoints_id_rlc %in% colnames(rlc_data)) stop("Make sure that the column name of the cluster_ID is correct.")
  
  # create the area of the geoshape file
  area_polygon <- create_rlc_area_geoshape(area_data = area_data,
                                           geoshape_column_name = "geoshape")
  if(calculate_area){
    sum_area <- as.numeric(st_area(area_polygon))
  } else {
    sum_area <- as.numeric(area_data[[column_area]])
  }
  
  ## Calculate the average hh
  average_people_per_hh <- mean(as.numeric(rlc_data[[column_num_hh_members]]), na.rm=T)
  if(is.null(average_people_per_hh)){
    cat("Average number of people per household couldn't be calcualted.\n\nCheck your inputs.")
  } else {
    cat(paste0("Average number of people per household: ", average_people_per_hh,"\n\n"))
  }
  
  ## calculate estimate number of hhs and peoples for total area
  average_hhd_area <- mean(hh_density_cluster$hhd_cluster, na.rm = T)
  #HH
  est_total_hh_area <- round(average_hhd_area * sum_area,0)
  #Peopl
  est_total_people_area <- round(est_total_hh_area * average_people_per_hh,0)
  if(is.null(est_total_hh_area)|is.null(est_total_people_area)){
    cat("Estimation of number of household and people couldn't be calcualted.\n\nCheck your inputs.\n\n")
  } else {
    cat(paste0("Estimated number of household for the total area: ", est_total_hh_area,"\n\n"))
    cat(paste0("Estimated number of people for the total area: ", est_total_people_area,"\n\n"))
  }
}


