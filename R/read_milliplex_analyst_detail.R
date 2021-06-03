# function to read milliplex analyst output
read_milliplex_analyst_detail <- function(path) {
  data <- readxl::read_excel(path, skip = 3, na = c("N/A", "NA"))
  
  # get rid of concentration row because I don't need it
  # NOTE: I'm thinking about developing a bioassay class, which would include
  # concentration as one of its attributes.
  data <- data[-1,]
  # need to set formatting of Location and Sample columns so that they are 
  # easier to work with.
  names(data)[1:2] <- c("Location", "Sample")
  # need to discard everything after the well positions.
  wells <- grep("^(\\d[A-Z]\\d+)$", data$Location)
  data <- data[wells,]
  # Fix the Location column: 
  # All Locations start with 1 followed by the position. I'm not sure why.
  # This feature is useless to me so it makes the most sense for me to
  # fix it here, rather than in a separate function (in contrast with limits of
  # detection, which may be useful to store).
  tidyr::separate(data, Location, into = c("Multiplate", "Position"), sep = 1)
}



