#########################################################################################################
#
# Function to calculate zonal stats from rasters for a set of polygons using the velox package.
#
# Arguments:
#
#   segmentation - Polygon layer (opened with st_read) for which zonal statistics are to be calculated.
#                  The layer must have a column named ID containing unique values.
#
#   list.rasters - a list containing a vector of format:
#                  list(layer_name=c(path_to_raster, stat, band1[, band2, ...]), ...)
#
#   tiles        - number of columns and rows for tiling of the raster layers, e.g. tiles=2 will have 2x2 tiles.
#                  Tiling is required for handling large rasters.  The default value tiles=1 is for no tiling.
#
# Returns a table  zonal statistics for each polygon.  The ID field references the original polygon.
# segmented object id.  Column names are taken from list.rasters appended with statistic and band.
#
# Author:       Richard Alexander, Natural England, 26 October 2018
#

library(foreach)
library(sf)
library(velox)
library(rgdal)
library(raster)

#####################################################################################################

zonal.stats.velox <- function(segmentation, list.rasters, tiles=1)
{   
  # Check segmentation is in a supported format
  if (class(segmentation)[1] != "sf")
  {
     stop("Segmented polygons must be a polygon layer opened using 'st_read')")
  }
  
  # Check that list of rasters is valid prior to processing
  for (r in list.rasters)
  {
     for (band in r[3:length(r)])
     {
        raster(r[1], band)
     }
  }
  
  # Convert the segmentation polygons to points
  segmentation.points <- st_centroid(segmentation)
   
   
  # Determine the extent of the segmentation layer
  seg.ext <- st_bbox(segmentation)  
  
  # Tile the segmented polygons    
  segmentation.tiles <- foreach(tile=0:(tiles*tiles-1)) %do% # Not run in parallel as segmentation object is too large
  {
    ix = tile %% tiles
    iy = tile %/% tiles
    
    segmentation.part <- NULL
    
    # Create an extent for the current tile      
    x1 <- seg.ext[1] + (seg.ext[3] - seg.ext[1])/tiles * ix
    x2 <- x1 + (seg.ext[3] - seg.ext[1])/tiles
    y1 <- seg.ext[2] + (seg.ext[4] - seg.ext[2])/tiles * iy
    y2 <- y1 + (seg.ext[4] - seg.ext[2])/tiles
    
    tile.ext <- st_polygon(list(matrix(c(x1,y1,x2,y1,x2,y2,x1,y2,x1,y1), ncol=2, byrow=T)))
    tile.ext <- st_geometry(tile.ext)
    tile.ext <- st_set_crs(tile.ext, st_crs(segmentation))
           
    # Determine which segmentation polygons fall with the tile
    #seg.sel <- segmentation$xx >= x1 & segmentation$xx < x2 & segmentation$yy >= y1 & segmentation$yy < y2
    seg.sel <- st_intersects(tile.ext, segmentation.points, sparse=T)
    segmentation.part <- segmentation[seg.sel[[1]],]
    
    print(paste("Tiling:", ix+iy*tiles+1, "of", tiles*tiles, "Number of polygons:", nrow(segmentation.part)))
                 
    segmentation.part
  }  

zonal.stats.all <- zonal.stats.tiles(segmentation.tiles, list.rasters)  
  
return(zonal.stats.all)

}

##################################################################################################################
#
# Internal function to extract zonal statistics from tiled rasters
#

zonal.stats.tiles <- function(segmentation.tiles, list.rasters)
{
  zonal.stats.all <- NULL
  
  # Iterate through each of the segmented polygon tiles  
  for (tile in 1:length(segmentation.tiles))
  {
     segmentation.tile <- segmentation.tiles[[tile]]
     
     # Get the extent of the segmentated polygons for the tile
     box <- st_bbox(segmentation.tile)
     ext <- extent(box[c(1,3,2,4)])
     
     print(paste("Zonal stats:", tile, "of", length(segmentation.tiles)))
     
     zonal.stats.tile <- data.frame(ID=segmentation.tile$ID)
     if(!is.null(segmentation.tile))
     {
        # Iterate through each of the rasters to be processed
        pb <- txtProgressBar(0, length(list.rasters))
        for (i in 1:length(list.rasters)) 
        {
           list.raster <- list.rasters[[i]]   
           
           file <- list.raster[1]
           fun <- list.raster[2]
           bands <- list.raster[3:length(list.raster)]
           
           # Determine the overlap between the raster and the segmented polygons tile
           info <- GDALinfo(file)
           ext <- intersect(ext, extent(info[4], info[4]+info[2]*info[6], info[5], info[5]+info[1]*info[7]))  
           if (!is.null(ext))
           {
              # Calculate the extent of the overlap between the raster layer and the segmented polygons
              offset.x <- (ext[1] - info[4]) %/% info[6]
              offset.y <- (info[5]+info[1]*info[7]-ext[4]) %/% info[7]   # Assumes ysign=-1
              region.x <- max((ext[2]-ext[1]) %/% info[6],1)
              region.y <- max((ext[4]-ext[3]) %/% info[7],1)
              
              # Create a raster stack 
              s <- NULL
              for (j in 1:length(bands))
              {
                 r <- as(readGDAL(file, band=bands[j], offset=c(offset.y, offset.x), region.dim=c(region.y, region.x)), "RasterLayer")
                 if (is.null(s)) {s <- r} else {s <- stack(s, r)}
              }
                 
              # Convert raster to a velox object
              vx <- velox(s)       
              
              # Extract values for each statistic
              zonal.stats.layer <- vx$extract(segmentation.tile, eval(parse(text=fun)))
              
              # Add ID column
              zonal.stats.layer <- data.frame(zonal.stats.layer)
              
              # Rename columns
              for (j in 1:length(bands))
              {
                 name <- paste0(names(list.rasters)[i],"_",fun)
                 if (length(bands) > 1) name <- paste0(name, "_band", j)
                 
                 colnames(zonal.stats.layer)[j] <- name
              }
              
              zonal.stats.tile <- cbind(zonal.stats.tile, zonal.stats.layer)   
              
           }
           setTxtProgressBar(pb, i)
        }  
        close(pb)
         
      # Merge results              
        zonal.stats.all <- rbind(zonal.stats.all, zonal.stats.tile) # Append to existing zonal stats
     }
  }
  
  return(zonal.stats.all)  
}

##################################################################################################################
#
# Function to calculate the mathematical mode of a vector (most common value)
#

Mode <- function(x, na.rm = TRUE) 
{
   if(na.rm)
   {
      x = x[!is.na(x)]
   }
   
   ux <- unique(x)
   return(ux[which.max(tabulate(match(x, ux)))])
}
