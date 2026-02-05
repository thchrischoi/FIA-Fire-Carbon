


library(dplyr)
library(terra)

setwd('N:/Research/Evangelista/EVANGELISTA GROUP_Data/Projects/FIA_Fire_Carbon/Analysis/Data/TLS_Scans/Trees')


# f = system.file("8_49_20_04/8_49_20_04_T15/trunk.tif", package="terra")
# trunk = terra::rast(f)

# trunk = terra::rast(x = '8_49_37_02/8_49_37_02_T05/8_49_37_02_T05_trunk_downed.tif')

trunk = terra::rast(x = '8_49_20_04/8_49_20_04_T15/8_49_20_04_T15_trunk.tif')

summary(trunk)
plot(trunk)

res(trunk)


# downed.trunk.volume = function(trunk){
#   
#   # find horizontal resolution and convert to cm
#   resolution = res(trunk)[1] / 3.28084 * 100
#   
#   ## remove 0 values
#   
#   
#   ## find minimum of leftover values
#   
#   min.height = terra::minmax(trunk, compute = T)[1,]
#   
#   ## minus all values by the minimum value (new 0)
#   calibrated.height = trunk - min.height
#   
#   ## multiply values by resolution x resolution
#   pixel.volume = calibrated.height * (resolution^2)
#   
#   ## sum of pixel volumes
#   total.volume = sum((pixel.volume |> as.data.frame()), na.rm = T)
#   
#   return(total.volume)
#   
# }
# 
# 
# 
# downed.trunk.volume(trunk)


###############################
## Convert volume to biomass

downed.trunk.biomass = function(trunk){
  
  downed.trunk.volume = function(trunk){
    # find horizontal resolution and convert ft to m to cm
    resolution = res(trunk)[1] / 3.28084 * 100
    
    ## remove 0 values
    
    ## find minimum of leftover values
    min.height = terra::minmax(trunk, compute = T)[1,]
    
    ## minus all values by the minimum value (new 0)
    calibrated.height = trunk - min.height
    
    ## multiply values by resolution x resolution
    pixel.volume = calibrated.height * (resolution^2)
    
    ## sum of pixel volumes
    total.volume = sum((pixel.volume |> as.data.frame()), na.rm = T)
    
    return(total.volume)
  }
  
  
  volume = downed.trunk.volume(trunk)
  
  
  return(volume)
  
  ## Convert to biomass based on lodgepole biomass dry weight
  biomass = volume * 0.39 
  
  ## Convert to lodegepole carbon ratio
  carbon = biomass * 0.503
  
  return(carbon)
  
  
}

downed.trunk.biomass(trunk)

# dr. david schmitt
# ER is not helping
# consult with doctor

