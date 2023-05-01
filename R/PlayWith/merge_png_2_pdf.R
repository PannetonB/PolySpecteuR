merge_png_2_pdf <- function(pdfFile, pngFiles, deletePngFiles=FALSE) {
  
  #### Package Install ####
  pngPackageExists <- require ("png")
    if ( !pngPackageExists ) {
    install.packages ("png")
    library ("png")
    
  }
  
  ok <- require("grid")
  if (!ok) {
    install.packages('grid')
    library("grid")
  }
  
  #########################
  
  pdf(pdfFile)
  
  n <- length(pngFiles)
  
  for( i in 1:n) {
    
    pngFile <- pngFiles[i]
    
    pngRaster <- readPNG(pngFile)
    
    grid.raster(pngRaster, width=unit(0.8, "npc"), 
                      height= unit(0.8, "npc"))
    
    if (i < n) plot.new()
    
  }
  
  dev.off()
  
  if (deletePngFiles) {
    
    unlink(pngFiles)
  }
  
}