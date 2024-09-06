fieldInfoRedEdgeMXdual <- function (mosaic, fieldShape, fun = "function(x, na.rm) c(mean = mean(x, na.rm=na.rm), sd = sd(x, na.rm = na.rm), min = min(x, na.rm=na.rm), max = max(x, na.rm=na.rm), median = median(x,na.rm = na.rm), q0.2=quantile(x,probs=0.2,na.rm=na.rm), q25=quantile(x,probs=0.25,na.rm=na.rm), q5=quantile(x,probs=0.5,na.rm=na.rm), q75=quantile(x,probs=0.75,na.rm=na.rm), q98=quantile(x,probs=0.98,na.rm=na.rm))", # 
                                    plot = FALSE, buffer = NULL, 
                                    n.core = NULL, projection = TRUE) 
{
  if (projection) {
    if (projection(fieldShape) != projection(mosaic)) {
      stop("fieldShape and mosaic must have the same projection CRS. Use fieldRotate() for both files.")
    }
  }
  mosaic <- stack(mosaic)
  num.band <- length(mosaic@layers)
  print(paste("Extracting: ", num.band, " layers.", sep = ""))
  print(paste("You can speed up this step using n.core=",parallel::detectCores(), " or less.", sep = ""))
  CropPlot <- crop(x = mosaic, y = fieldShape)
  if (is.null(n.core)) {
    plotValue <- extract(x = CropPlot, y = fieldShape, fun = eval(parse(text = fun)), 
                         buffer = buffer, na.rm = T, df = T)
  }
  if (!is.null(n.core)) {
    if (n.core > parallel::detectCores()) {
      stop(paste(" 'n.core' must be less than ", detectCores(), sep = ""))
    }
    # package dependecy...
    if (!requireNamespace("foreach", quietly = TRUE)) {
      utils::install.packages("foreach")
    }
    require(foreach, quietly = TRUE)
    #
    cl <- parallel::makeCluster(n.core, output = "", setup_strategy = "sequential")
    doParallel::registerDoParallel(cl)
    plotValue <- foreach(i = 1:length(fieldShape), .packages = c("raster"), 
                         .combine = rbind) %dopar% {
                           single <- fieldShape[i, ]
                           CropPlot <- crop(x = mosaic, y = single)
                           extract(x = CropPlot, y = single, fun = eval(parse(text = fun)), 
                                   buffer = buffer, na.rm = T, df = T)
                         }
    # plotValue$ID <- 1:length(fieldShape)
    parallel::stopCluster(cl)
  }
  metric <- rep(c("mean","sd","min","max","median","q0.2","q0.25","q0.5","q0.75","q0.98"),dim(fieldShape@data)[1])
  Nmetric <- length(unique(metric))
  field_data<-fieldShape@data[rep(seq_len(nrow(fieldShape@data)), each = Nmetric), ]
  field_data <- cbind.data.frame(field_data,metric=as.factor(metric),plotValue[,-1])
  rownames(field_data) <- NULL
  field_data_mean <- field_data[field_data$metric=="mean", ]
  fieldShape@data <- field_data_mean
  Out <- list(fieldShape = fieldShape, plotValue = plotValue, 
              cropPlot = CropPlot, Index_metric = field_data)
  if (plot) {
    if (num.band > 2) {
      plotRGB(RGB.rescale(CropPlot, num.band = 3), r = 1, 
              g = 2, b = 3)
    }
    if (num.band < 3) {
      raster::plot(CropPlot, axes = FALSE, box = FALSE)
    }
    sp::plot(fieldShape, add = T)
  }
  return(Out)
}
