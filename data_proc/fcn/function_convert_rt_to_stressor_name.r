

# the field name of the input file has to be "Stressor"
convert_rt_to_stressor_name <- function(a){


  a$s_name <- "" ## add in one of the options as a default value for column
  a$s_name[a$Stressor == "1"] <- "Cropland"
  a$s_name[a$Stressor == "2"] <- "Impervious Surfaces"
  a$s_name[a$Stressor == "3"] <- "Livestock Density"
  a$s_name[a$Stressor == "4"] <- "Wetland Disconnectivity"
  a$s_name[a$Stressor == "5"] <- "Soil Salination"
  a$s_name[a$Stressor == "6"] <- "Nitrogen Loading"
  a$s_name[a$Stressor == "7"] <- "Phosphorus Loading"
  a$s_name[a$Stressor == "8"] <- "Murcury Deposition"
  a$s_name[a$Stressor == "9"] <- "Pesticide Loading"
  a$s_name[a$Stressor == "10"] <- "Sediment Loading"
  a$s_name[a$Stressor == "11"] <- "Organic Loading"
  a$s_name[a$Stressor == "12"] <- "Potential Acidification"
  a$s_name[a$Stressor == "13"] <- "Thermal Alteration"
  a$s_name[a$Stressor == "14"] <- "Dam Density"
  a$s_name[a$Stressor == "15"] <- "River Fragmentation"
  a$s_name[a$Stressor == "16"] <- "Consumptive Water Loss"
  a$s_name[a$Stressor == "17"] <- "Human Water Stress"
  a$s_name[a$Stressor == "18"] <- "Agricultural Water Stress"
  a$s_name[a$Stressor == "19"] <- "Flow Disruption"
  a$s_name[a$Stressor == "20"] <- "Non-Native Fishes (%)"
  a$s_name[a$Stressor == "21"] <- "Non-Native Fishes (#)"
  a$s_name[a$Stressor == "22"] <- "Fishing Pressure"
  a$s_name[a$Stressor == '23'] <- 'Aquaculture Pressure'

  return(a)
}