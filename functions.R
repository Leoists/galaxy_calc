library(deSolve)

# The colonization_ode function...

colonization_ode <- function(t, state, parameters) {
  
  # Unpack state variables
  startup_planets <- state[["startup_planets"]]
  building_ship_planets <- state[["building_ship_planets"]]
  colonized_planets <- state[["colonized_planets"]]
  
  # Unpack parameters
  startup_rate  <- parameters[["startup_rate"]]
  build_rate <- parameters[["build_rate"]]
  
  # Differential equations
  d_startup_planets <- colonized_planets * build_rate - startup_rate * startup_planets
  d_building_ship_planets <- startup_rate * startup_planets - build_rate * building_ship_planets
  d_colonized_planets <- build_rate * building_ship_planets
  
  list(c(d_startup_planets, d_building_ship_planets, d_colonized_planets))
}

# The solve_colonization function...

solve_colonization <- function(startup_lag, years_to_build_ship, some_end_time) {
  
  parameters <- list(startup_rate  = 1/startup_lag,
                     build_rate = 1/years_to_build_ship)
  
  # Initial conditions
  state <- c(startup_planets = 0, 
             building_ship_planets = 1, 
             colonized_planets = 0)
  
  # Time sequence
  times <- seq(0, some_end_time, by=1)  # Increment by 1 year for granularity
  
  # Solve the differential equations
  out <- ode(y = state, times = times, func = colonization_ode, parms = parameters)
  
  return(out)
}

# library(deSolve)
# 
# # The colonization_ode function with more descriptive variable names and notation changes...
# 
# colonization_ode <- function(t, state, parameters) {
#   
#   # Unpack state variables
#   startup_planets <- state[["startup_planets"]]
#   building_ship_planets <- state[["building_ship_planets"]]
#   colonized_planets <- state[["colonized_planets"]]
#   transit_ships <- state[["transit_ships"]]
#   
#   # Unpack parameters
#   transit_rate <- parameters[["transit_rate"]]
#   startup_rate  <- parameters[["startup_rate"]]
#   build_rate <- parameters[["build_rate"]]
#   colonize_rate <- parameters[["colonize_rate"]]
#   
#   # Differential equations
#   # d_startup_planets <- transit_rate * transit_ships - startup_rate * startup_planets
#   # d_building_ship_planets <- startup_rate * startup_planets - build_rate * building_ship_planets
#   # d_colonized_planets <- colonize_rate * building_ship_planets
#   # d_transit_ships <- colonize_rate * building_ship_planets - transit_rate * transit_ships
#   d_startup_planets <- colonized_planets * build_rate - startup_rate * startup_planets
#   d_building_ship_planets <- startup_rate * startup_planets - build_rate * building_ship_planets
#   d_colonized_planets <- build_rate * building_ship_planets
#     
#   list(c(d_startup_planets, d_building_ship_planets, d_colonized_planets, d_transit_ships))
# }
# 
# # The solve_colonization function...
# 
# solve_colonization <- function(travel_time, startup_lag, years_to_build_ship, some_end_time) {
#   
#   parameters <- list(transit_rate = 1/travel_time, 
#                      startup_rate  = 1/startup_lag,
#                      build_rate = 1/years_to_build_ship, 
#                      colonize_rate = 1/years_to_build_ship)  # these values may need adjustment
#   
#   # Initial conditions
#   state <- c(startup_planets = 0, 
#              building_ship_planets = 1, 
#              colonized_planets = 1, 
#              transit_ships = 0)
#   
#   # Time sequence
#   times <- seq(0, some_end_time, by=travel_time)  # 'some_end_time' should be set appropriately
#   
#   # Solve the differential equations
#   out <- ode(y = state, times = times, func = colonization_ode, parms = parameters)
#   
#   return(out)
# }

# Example invocation:

# result <- solve_colonization(travel_time = 10, 
#                              startup_lag = 20, 
#                              years_to_build_ship = 10, 
#                              some_end_time = 1000) # Set some_end_time based on an estimation or desired horizon
# 
# print(result)



colonization_time <- function(number_of_stars = 570e9,        
                              fraction_habitable = 0.01,      
                              ship_speed = 0.1,               
                              years_to_build_ship = 10,       
                              startup_lag = 20,               
                              avg_distance_ly = 10,           
                              df = NULL                       
) {
  
  total_habitable <- number_of_stars * fraction_habitable
  
  travel_time <- avg_distance_ly / ship_speed
  startup_ticks <- max(1, round(startup_lag / travel_time))
  shipbuild_ticks <- max(1, round(years_to_build_ship / travel_time))
  
  planet_counts <- integer(startup_ticks + shipbuild_ticks)
  
  if (is.null(df)) {
    years <- 0
    ships_in_transit <- 1
    df <- c()
  } else {
    latest <- tail(df, 1)
    years <- latest$years
    ships_in_transit <- latest$ships_in_transit
    planet_counts <- as.integer(unlist(latest$planet_states))
  }

  on.exit(return(df), add<-TRUE)
  
  while (sum(planet_counts) + ships_in_transit < total_habitable) {
    
    # Increment years with travel time
    years <- years + travel_time
    
    # Move planets through the conveyor belt
    ships_completed <- planet_counts[length(planet_counts)]
    planet_counts <- c(ships_in_transit, planet_counts[-length(planet_counts)])
    planet_counts[startup_ticks + 1] <- planet_counts[startup_ticks + 1] + ships_completed
    
    # Update ships in transit
    ships_in_transit <- ships_completed
    df_latest <- data.frame(years, 
                            colonized_planets=sum(planet_counts), 
                            planets_startup_lag=(planet_counts[1:startup_ticks]), 
                            planets_building_ship=sum(planet_counts[(startup_ticks+1):(startup_ticks+shipbuild_ticks)]),
                            ships_completed, 
                            ships_in_transit);
    df_latest$planet_states <- I(list(planet_counts));
    df <- rbind(df,df_latest);
  }
  
  return(df)
}

