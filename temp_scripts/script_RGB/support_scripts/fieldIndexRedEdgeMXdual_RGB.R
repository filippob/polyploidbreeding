fieldIndexRedEdgeMXdual_RGB <- function (mosaic,
                                     #coastalBlue = 1, Blue = 2, Green531 = 3, Green = 4, Red650 = 5, Red = 6, RedEdge705 = 7, RedEdge = 8, RedEdge740 = 9, NIR = 10, 
                                     Red = 1, Green = 2, Blue = 3,
                                     index = "HUE", 
                                     myIndex = NULL, plot = TRUE) 
{
  Ind <- read.csv(file = system.file("extdata", "Indices.txt", 
                                     package = "FIELDimageR", mustWork = TRUE), 
                  header = TRUE, sep = "\t")
  mosaic <- raster::stack(mosaic)
  num.band <- length(mosaic@layers)
  print(paste(num.band, " layers available", sep = ""))
  if (num.band < 3) {
    stop("At least 3 bands (RGB) are necessary to calculate indices")
  }
  # if (!is.null(RedEdge) | !is.null(NIR)) {
  #   if (num.band < 4) {
  #     stop("RedEdge and/or NIR is/are not available in your mosaic")
  #   }
 # }
  IRGB = as.character(Ind$index)
  if (is.null(index)) {
    stop("Choose one or more indices")
  }
  if (!all(index %in% IRGB)) {
    stop(paste("Index: ", index[!index %in% IRGB], " is not available in FIELDimageR"))
  }
  # NIR.RE <- as.character(Ind$index[Ind$band %in% c("RedEdge", "NIR")])
  # if (any(NIR.RE %in% index) & is.null(NIR)) {
  #   stop(paste("Index: ", NIR.RE[NIR.RE %in% index], " needs NIR/RedEdge band to be calculated", sep = ""))
 # }
  # Bands
  #cB <- mosaic@layers[[coastalBlue]]
  B <- mosaic@layers[[Blue]]
  #G531 <- mosaic@layers[[Green531]]
  G <- mosaic@layers[[Green]]
  #R650 <- mosaic@layers[[Red650]]
  R <- mosaic@layers[[Red]]
  #RE705 <- mosaic@layers[[RedEdge705]]
  # RE <- mosaic@layers[[RedEdge]]
  # RE740 <- mosaic@layers[[RedEdge740]]
  # NIR1 <- mosaic@layers[[NIR]]
  #names(mosaic)[c(coastalBlue,Blue,Green531,Green,Red650,Red,RedEdge705,RedEdge,RedEdge740,NIR)] <- c("coastalBlue","Blue","Green531","Green","Red650","Red","RedEdge705","RedEdge","RedEdge740","NIR")
  names(mosaic)[c(Blue,Green,Red)] <- c("Blue","Green","Red")
  
  #
  for (i in 1:length(index)) {
    mosaic@layers[[(num.band + i)]] <- eval(parse(text = as.character(Ind$eq[as.character(Ind$index) == index[i]])))
    names(mosaic)[(num.band + i)] <- as.character(index[i])
  }
  # myIndex
  if (!is.null(myIndex)) {
    #coastalBlue <- cB
    Blue <- B
    #Green531 <- G531
    Green <- G
    #Red650 <- R650
    Red <- R
    # RedEdge705 <- RE705
    # RedEdge <- RE
    # RedEdge740 <- RE740
    # NIR <- NIR1
    #
    for (m1 in 1:length(myIndex)) {
      mosaic@layers[[(length(mosaic@layers) + 1)]] <- eval(parse(text = as.character(myIndex[m1])))
      if (length(myIndex) == 1) {
        names(mosaic)[(length(mosaic@layers))] <- "myIndex"
      }
      if (length(myIndex) > 1) {
        names(mosaic)[(length(mosaic@layers))] <- paste("myIndex", m1)
      }
    }
  }
  # Plot
  if (plot) {
    raster::plot(mosaic, axes = FALSE, box = FALSE)
  }
  # Return
  mosaic <- stack(mosaic)
  return(mosaic)
}
