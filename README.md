# Spreadsheet to GTFS

This document proposes a simple approach to create GTFS files from
spreadsheets. To run it, you just need at least one route schedule, in
the form of a CSV file, with the route stops as rows and each trip as a
column, being the first column the stop name. Below is provided an
example.

|                 |          |          |          |          |          |
|-----------------|----------|----------|----------|----------|----------|
| Corroios        | 05:13:00 | 05:43:00 | 06:08:00 | 06:23:00 | 06:38:00 |
| Casa do Povo    | 05:15:00 | 05:45:00 | 06:10:00 | 06:25:00 | 06:40:00 |
| Santo Amaro     | 05:16:00 | 05:46:00 | 06:11:00 | 06:26:00 | 06:41:00 |
| Laranjeiro      | 05:18:00 | 05:48:00 | 06:13:00 | 06:28:00 | 06:43:00 |
| António Gedeão  | 05:20:00 | 05:50:00 | 06:15:00 | 06:30:00 | 06:45:00 |
| Parque da Paz   | 05:21:00 | 05:51:00 | 06:16:00 | 06:31:00 | 06:46:00 |
| Cova da Piedade | 05:23:00 | 05:53:00 | 06:18:00 | 06:33:00 | 06:48:00 |
| Ramalha         | 05:25:00 | 05:55:00 | 06:20:00 | 06:35:00 | 06:50:00 |

All the other parameters, such as agency data and calendars are defined
manually when running the script.

## Procedure

This section describes the procedure to create the GTFS file. Start by
loading the packages. :)

``` r
library(dplyr)
library(tibble)
library(rlang)
```

### Parameters

**This code should be edited before executed.** It is where you define
the parameters of the GTFS feed, namely:

-   The folder at which the CSV files are placed, `folder_input`;

-   The folder at which the generated GTFS should de stored,
    `folder_output`;

-   An **optional** `stops.csv` file, that if provided will be used as
    the base file for the GTFS stops, using the ids of the stops
    provided and generating new ones for those that exist in the routes
    CSV but do not match any;

> **Notice that the `stop_lat` and `stop_lon` values are initialized
> with a default 0 value for every stop that was not provided in the
> `stops.txt`. They must be manually editted!**

-   The agency information, which will be used for the `agency.txt`;

-   The services dates list, which will be used for the `calendar.txt`,
    and for each, the associated `routes`, a list of the CSV files that
    contain each route’s stops and times (without the `.csv` extension).

After editting this, you should be able to run the code

``` r
agency <- list(
  # Folders
  folder_input="MTS/Input",
  folder_output="MTS/Output",
  stops="MTS/Input/stops.txt",
  # Agency data (agency.txt)
  agency_id="MTS",
  agency_name="Metro Sul do Tejo",
  agency_url="https://www.mts.pt",
  agency_timezone="Europe/Lisbon",
  route_type=5, 
  # List all services (calendar.txt)
  services = list(
    service_id=c(0,1,2),
    monday=c(1,0,0),
    tuesday=c(1,0,0),
    wednesday=c(1,0,0),
    thursday=c(1,0,0),
    friday=c(1,0,0),
    saturday=c(0,1,0),
    sunday=c(0,0,1),
    start_date=c(20250101,20250101,20250101),
    end_date=c(20251231,20251231,20251231),
    # List trips per service (one list of trips per service)
    routes=list( # Trip will be trip_id, and MUST ALSO be the name of the .csv file stored at folder_input, defined before!!
      list("Linha1_Cacilhas_Uteis"),
      list("Linha2_Pragal_Sabados"),
      list()
    )
  )
)
summary(agency)
```

    ##                 Length Class  Mode     
    ## folder_input     1     -none- character
    ## folder_output    1     -none- character
    ## stops            1     -none- character
    ## agency_id        1     -none- character
    ## agency_name      1     -none- character
    ## agency_url       1     -none- character
    ## agency_timezone  1     -none- character
    ## route_type       1     -none- numeric  
    ## services        11     -none- list

### GTFS data initialization

The GTFS file will be generated using these variables, which are now
initialized with the parameters defined before in the `agency` variable.
`gtfs_routes`, `gtfs_trips`, `gtfs_stop_times` and `gtfs_stops` are
initialized empty. They will be extended during the iteration of the
`agency$service$routes`, in the next step.

``` r
gtfs_agency = data.frame(agency[c("agency_id", "agency_name", "agency_url", "agency_timezone")])
gtfs_calendar = data.frame(agency$services[c("service_id", "monday", "tuesday", "wednesday", "thursday", "friday", "saturday", "sunday", "start_date", "end_date")])
gtfs_routes = tibble(route_id=character(), route_long_name=character(), route_type=integer())
gtfs_trips = tibble(route_id=character(), service_id=character(), trip_id=character())
gtfs_stop_times = tibble(trip_id=character(), arrival_time=character(), departure_time=character(), stop_id=character(), stop_sequence=integer())

if (is.null(agency$stops)) {
  gtfs_stops = tibble(stop_id=character(), stop_name=character())
} else {
  gtfs_stops = read.csv(agency$stops)[c("stop_id", "stop_name", "stop_lat", "stop_lon")]
}
```

### Iterate through services and routes

Now it’s time to iterate through `agency$service` and for each service,
the defined `$routes`, specified in the `.csv` files.

``` r
# Iterate through services...
route_id <- 0
for (i in seq(length(agency$services$service_id))){
  service_id <- agency$services$service_id[i]
  print(sprintf("Analysing service %s, start %s, end %s, with %d routes...", service_id, agency$services$start_date[i], agency$services$end_date[i], length(agency$services$routes[[i]])))
  
  if( length( agency$services$routes[[i]] ) == 0) {
    print("!! Ignoring service, as it has no routes associated...")
    next
  }
  

  # For each, analyse the several routes...
  for(ti in seq( length( agency$services$routes[[i]] ) ) ) {
    route_name = unlist(agency$services$routes[[i]][ti])
    print(sprintf("> Analysing route %s...", route_name ))
    schedule <- read.csv(sprintf("%s/%s.csv", agency$folder_input, route_name), header=FALSE)
    print(sprintf("> There are %s stops and %d trips...", nrow(schedule), ncol(schedule)-1 ))
    
    route_id <<- route_id + 1
    route_gtfs = tibble(route_id=as.character(route_id), route_long_name=route_name, route_type=agency$route_type)
    gtfs_routes <<- rbind(gtfs_routes, route_gtfs)
    
    # And the associated trips...
    trip_counter <- 0
    
    trips <- lapply(names(schedule)[-1], function(trip_name) { # Iterate through columns (all except first), since each column represents one trip
      trip_counter <<- trip_counter + 1
      trip_id <- sprintf("%s_%s_Trip%03d", service_id, route_name, trip_counter)
      trip_gtfs = tibble(route_id=route_id, service_id=service_id, trip_id=trip_id)
      gtfs_trips <<- rbind(gtfs_trips, trip_gtfs)
      
      stops = schedule[[1]]
      stop_ids = list()
      for(si in seq(length(stops))) {
        # Check if stop already exists in gtfs_stops
        stop_id <- 0
        if (!any(gtfs_stops$stop_name == stops[si])) { # If not, create it
          stop_id <- rlang::hash(stops[si])
          gtfs_stops <<- rbind(gtfs_stops, tibble(stop_id=stop_id, stop_name=stops[si], stop_lat=0.0, stop_lon=0.0))  
        } else { # Otherwise, use id that is already associated to it
          stop_id <- gtfs_stops$stop_id[gtfs_stops$stop_name==stops[si]]
        }
        stop_ids[si] <- stop_id
      }

      data.frame(
        "stop_id" = unlist(stop_ids),
        # stop_id = sapply(schedule[[1]], rlang::hash),
        stop_name = schedule[[1]],
        arrival_time = schedule[[trip_name]],
        departure_time = schedule[[trip_name]],
        trip_id = trip_id,
        stop_sequence=seq(1, length(schedule[[trip_name]]), by=1)
      ) %>%
        filter(!is.na(arrival_time)) %>%  # Remove rows with missing times
        arrange(arrival_time)             # Ensure order by time
    })
    trips_df <- bind_rows(trips)
    
    gtfs_stop_times <<- rbind(gtfs_stop_times, trips_df[c("trip_id", "arrival_time", "departure_time", "stop_id", "stop_sequence")])
  }
}
```

    ## [1] "Analysing service 0, start 20250101, end 20251231, with 1 routes..."
    ## [1] "> Analysing route Linha1_Cacilhas_Uteis..."
    ## [1] "> There are 13 stops and 135 trips..."
    ## [1] "Analysing service 1, start 20250101, end 20251231, with 1 routes..."
    ## [1] "> Analysing route Linha2_Pragal_Sabados..."
    ## [1] "> There are 9 stops and 78 trips..."
    ## [1] "Analysing service 2, start 20250101, end 20251231, with 0 routes..."
    ## [1] "!! Ignoring service, as it has no routes associated..."

``` r
View(gtfs_routes)
```

``` r
# Preventing error when creating shapes.txt (GTFSWizard requires this attribute, even if empty)
gtfs_trips[["shape_id"]] = NA
```

## Build GTFS file

### Create folder with `.txt` files.

The script below will generate a `/GTFS` folder at the `agency$folder`
folder with all the `.txt` files required by the GTFS documentation.

``` r
if (!dir.exists(agency$folder_output)) dir.create(agency$folder_output, recursive=TRUE)

gtfs_folder <- sprintf("%s/GTFS", agency$folder_output)
if (!dir.exists(gtfs_folder)) dir.create(gtfs_folder)

write.csv(gtfs_agency, file.path(gtfs_folder, "agency.txt"), row.names = FALSE)
write.csv(gtfs_calendar, file.path(gtfs_folder, "calendar.txt"), row.names = FALSE)
write.csv(gtfs_stops, file.path(gtfs_folder, "stops.txt"), row.names = FALSE)
write.csv(gtfs_routes, file.path(gtfs_folder, "routes.txt"), row.names = FALSE)
write.csv(gtfs_trips, file.path(gtfs_folder, "trips.txt"), row.names = FALSE, na = "")
write.csv(gtfs_stop_times, file.path(gtfs_folder, "stop_times.txt"), row.names = FALSE)
```

### ZIP it

Once you have performed the final adjustments (like adding the
coordinates to the `stops.txt` file), you can zip it with the command
below. It will generate a `GTFS.zip` file at the `agency$folder_output`
folder.

``` r
gtfs_zip = sprintf("%s.zip", gtfs_folder)
zip(zipfile = gtfs_zip, files = dir(gtfs_folder, full.names = TRUE), flags = "-j")
```

## Generate `shapes.txt`

> Based on
> <https://github.com/U-Shift/SiteSelection/blob/25-include-other-gtfs/R/gtfs_create_shapes.R>

``` r
gtfs_shapes = sprintf("%s_WithShapes.zip", tools::file_path_sans_ext(gtfs_zip))
gtfs_fixed <- GTFSwizard::read_gtfs(gtfs_zip) 
```

    ## get_shapes() reconstructs the shapes table using euclidean approximation, based on the coordinates and sequence of stops for each trip, and may not be accurate.

    ## Joining with `by = join_by(trip_id)`
    ## Joining with `by = join_by(trip_id)`

``` r
# Create temporary files for shapes and the unzipped GTFS contents
temp_shapes <- tempfile(fileext = "_shapes.txt")
temp_dir <- tempfile()
dir.create(temp_dir)

# Write shapes.txt to a temporary file
write.csv(gtfs_fixed$shapes, temp_shapes, row.names = FALSE)

# Unzip GTFS to temporary file
unzip(gtfs_zip, exdir = temp_dir)

# Move the new CSV to the temp folder
file.copy(temp_shapes, file.path(temp_dir, "shapes.txt"))
```

    ## [1] TRUE

``` r
# Create a new ZIP with all files (old + new)
zip::zip(gtfs_shapes, files = list.files(temp_dir, full.names = TRUE), mode = "cherry-pick")

# Clean up temporary files
unlink(temp_shapes)
unlink(temp_dir, recursive = TRUE)
```
