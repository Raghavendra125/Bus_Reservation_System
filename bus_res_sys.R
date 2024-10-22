library(readr)

bus_csv_file <- "bus_details.csv"
reservation_csv_file <- "passenger_reservations.csv"

# Ensure files exist with headers if missing
if (!file.exists(bus_csv_file)) {
  bus_data <- data.frame(
    BusID = integer(0),
    Source = character(0),
    Destination = character(0),
    TotalSeats = integer(0),
    AvailableSeats = integer(0)
  )
  write.csv(bus_data, file = bus_csv_file, row.names = FALSE)
}

if (!file.exists(reservation_csv_file)) {
  reservation_data <- data.frame(
    BusID = integer(0),
    PassengerName = character(0),
    NumSeatsReserved = integer(0)
  )
  write.csv(reservation_data, file = reservation_csv_file, row.names = FALSE)
}

# Add a new bus to the system
add_bus <- function(bus_id, source, destination, total_seats) {
  bus_data <- read.csv(bus_csv_file, stringsAsFactors = FALSE)
  
  # Check if Bus ID already exists
  if (bus_id %in% bus_data$BusID) {
    cat("Error: Bus ID already exists!\n")
    return()
  }
  
  new_bus <- data.frame(
    BusID = bus_id,
    Source = source,
    Destination = destination,
    TotalSeats = total_seats,
    AvailableSeats = total_seats
  )
  
  bus_data <- rbind(bus_data, new_bus)
  write.csv(bus_data, file = bus_csv_file, row.names = FALSE)
  
  cat("Bus added successfully!\n")
}

# Display available buses
display_buses <- function() {
  bus_data <- read.csv(bus_csv_file, stringsAsFactors = FALSE)
  
  available_buses <- bus_data[bus_data$AvailableSeats > 0, ]
  
  if (nrow(available_buses) == 0) {
    cat("No available buses at the moment.\n")
  } else {
    cat("\nAvailable Buses:\n")
    print(available_buses[, c("BusID", "Source", "Destination", "AvailableSeats")])
  }
}

# Make a reservation
make_reservation <- function(bus_id, num_seats) {
  bus_data <- read.csv(bus_csv_file, stringsAsFactors = FALSE)
  
  bus_index <- which(bus_data$BusID == bus_id)
  
  if (length(bus_index) == 0) {
    cat("Error: Bus not found!\n")
    return()
  }
  
  bus <- bus_data[bus_index, ]
  
  if (bus$AvailableSeats < num_seats) {
    cat("Error: Not enough available seats on this bus.\n")
    return()
  }
  
  passenger_name <- readline("Enter Passenger Name: ")
  
  reservation_data <- read.csv(reservation_csv_file, stringsAsFactors = FALSE)
  
  new_reservation <- data.frame(
    BusID = bus_id,
    PassengerName = passenger_name,
    NumSeatsReserved = num_seats
  )
  
  reservation_data <- rbind(reservation_data, new_reservation)
  
  write.csv(reservation_data, file = reservation_csv_file, row.names = FALSE)
  
  # Update available seats
  bus_data$AvailableSeats[bus_index] <- bus$AvailableSeats - num_seats
  write.csv(bus_data, file = bus_csv_file, row.names = FALSE)
  
  cat("Reservation made successfully!\n")
}

# Cancel a reservation
cancel_reservation <- function(bus_id, num_seats) {
  bus_data <- read.csv(bus_csv_file, stringsAsFactors = FALSE)
  
  bus_index <- which(bus_data$BusID == bus_id)
  
  if (length(bus_index) == 0) {
    cat("Error: Bus not found!\n")
    return()
  }
  
  bus <- bus_data[bus_index, ]
  
  if ((bus$AvailableSeats + num_seats) > bus$TotalSeats) {
    cat("Error: Cannot cancel more seats than initially booked.\n")
    return()
  }
  
  bus_data$AvailableSeats[bus_index] <- bus$AvailableSeats + num_seats
  write.csv(bus_data, file = bus_csv_file, row.names = FALSE)
  
  cat("Reservation canceled successfully!\n")
}

# Display all passenger reservations
display_passenger_reservations <- function() {
  reservation_data <- read.csv(reservation_csv_file, stringsAsFactors = FALSE)
  
  if (nrow(reservation_data) == 0) {
    cat("No passenger reservations at the moment.\n")
  } else {
    cat("Passenger Reservations:\n")
    print(reservation_data)
  }
}

# Save bus data
save_bus_data <- function() {
  cat("Bus data saved successfully!\n")
}

# Save reservation data
save_reservation_data <- function() {
  cat("Reservation data saved successfully!\n")
}

# Main menu loop
main_menu <- function() {
  repeat {
    cat("\nBus Reservation System Menu:\n")
    cat("1. Add Bus Details\n")
    cat("2. Display Available Buses\n")
    cat("3. Make Reservation\n")
    cat("4. Cancel Reservation\n")
    cat("5. Display Passenger Reservations\n")
    cat("6. Save Bus Data\n")
    cat("7. Save Reservation Data\n")
    cat("8. Exit\n")
    
    choice <- as.integer(readline("Enter your choice: "))
    
    switch(choice,
           { # Add bus
             bus_id <- as.integer(readline("Enter Bus ID: "))
             source <- readline("Enter Source: ")
             destination <- readline("Enter Destination: ")
             total_seats <- as.integer(readline("Enter Total Seats: "))
             add_bus(bus_id, source, destination, total_seats)
           },
           display_buses(), # Display available buses
           { # Make reservation
             bus_id <- as.integer(readline("Enter Bus ID: "))
             num_seats <- as.integer(readline("Enter Number of Seats to Reserve: "))
             make_reservation(bus_id, num_seats)
           },
           { # Cancel reservation
             bus_id <- as.integer(readline("Enter Bus ID: "))
             num_seats <- as.integer(readline("Enter Number of Seats to Cancel: "))
             cancel_reservation(bus_id, num_seats)
           },
           display_passenger_reservations(), # Display reservations
           save_bus_data(), # Save bus data
           save_reservation_data(), # Save reservation data
           { cat("Exiting system... Goodbye!\n"); break }
    )
  }
}

# Start the system
main_menu()

