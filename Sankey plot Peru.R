# Sankey plot del muestreo

#eBird data filters - filtro a los datos de eBird
library(auk); 
#seven packages in one for data management and visualization - siete paquetes en uno para manipulación y visualización de datos
library(tidyverse);
#Sankey plot with ggplot
library(ggsankey) #should be installed via `devtools::install_github("davidsjoberg/ggsankey")`

# Datos de eBird
Peru_ebd <- "ebd_PE_relFeb-2025/ebd_PE_relFeb-2025.txt"

# Seleccionar las columnas (based on x$col_idx$name) 
# {{Reduce el tamaño del dataset}}
colsE <- c("observer_id", "sampling_event_identifier",
           "group identifier",
           "common_name", "scientific_name",
           "observation_count",
           "country", "state_code", "locality_id", "latitude", "longitude",
           "protocol_type", "all_species_reported",
           "observation_date",
           "time_observations_started",
           "duration_minutes", "effort_distance_km",
           "number_observers")

#generate a temporal file to save the filtering eBird data (f_ebd)
f_ebd <- "temporalFiltering/ebd_Examples.txt" 

All_peru <- auk_ebd(file = Peru_ebd) |>           #archivo de registros
  auk_filter(f_ebd,
             overwrite=T,
             keep = colsE) |>
  read_ebd() |>
  mutate(         
    # effort_distance_km to 0 for non-travelling counts
    effort_distance_km = if_else(protocol_type %in% c("Stationary"),
                                 0, effort_distance_km),# si es estacionario es 0
    #effort_distance_km change to integer no decimals
    effort_distance_kmI = round(effort_distance_km, digits = 0),
    # split date into year, month, week, and day of year
    year = year(observation_date),
    )

saveRDS(All_peru, "All_Peru_eBird.RDS")

# Asignar cada registor (checklist con reporte) por sus metadatos de muestreo

ebd_sankey <- All_peru |>
  mutate(All = "eBird data",
         `Type of list` = case_when(all_species_reported == FALSE ~ "Incomplete",
                                    all_species_reported == TRUE ~"Complete"),
         Protocol = ifelse(protocol_type == "Stationary",  "Stationary",
                    ifelse(protocol_type == "Traveling",  "Traveling",
                           "Other protocol")),
         Year = ifelse(year %in% c(2000:2024), "2000-2024",
                       "Other year"),
         Duration = case_when(duration_minutes %in% c(0:9) ~ "Other duration",
                              duration_minutes %in% c(10:300) ~ "10 min - 5 hrs",
                              duration_minutes > 300 ~ "Other duration",
                              is.na(duration_minutes) ~ "Other duration"),
         Distance = case_when(effort_distance_kmI %in% c(0:5) ~ "<5 km",
                              effort_distance_kmI > 5 ~ "Other distance",
                              is.na(effort_distance_kmI) ~ "Other distance"),
         Decision = ifelse((all_species_reported == "TRUE") &
                             (protocol_type %in% c("Stationary",
                                                   "Traveling")) &
                             (duration_minutes %in% c(10:300)) &
                             (effort_distance_kmI %in% c(0:5)), 
                           "Retained","Excluded"))

saveRDS(ebd_sankey |> filter(Decision == "Retained"), "ebd_PeruRetained.RDS")

dfPE <- ebd_sankey |>
  make_long(All, `Type of list`, Protocol, Year, Duration, Distance, Decision)

dagg <- dfPE |>
  group_by(node) |>
  tally() |>
  mutate(pct = round((n/nrow(All_peru))*100, 1))

sankey_data_n = merge(dfPE, dagg, by.x = 'node', by.y = 'node',
                      all.x = TRUE)

sankey_data_n$node <- factor(sankey_data_n$node, 
                             levels = c("eBird data",
                                        "Incomplete","Complete",
                                        "Other protocol", "Traveling","Stationary", 
                                        "Other year", "2000-2024",
                                        "Other duration","10 min - 5 hrs",
                                        "Other distance","<5 km",
                                        "Excluded", "Retained"))

sankey_data_n$next_node <- factor(sankey_data_n$next_node, 
                                  levels = c("eBird data",
                                             "Incomplete","Complete",
                                             "Other protocol", "Traveling","Stationary", 
                                             "Other year", "2000-2024",
                                             "Other duration","10 min - 5 hrs",
                                             "Other distance","<5 km",
                                             "Excluded", "Retained"))

# code of colors Viridis
scales::show_col(viridis::viridis_pal()(9))

# the figure
Fig_Sankey_Peru <- ggplot(sankey_data_n, aes(x = x,
                          next_x = next_x,
                          node = node,
                          next_node = next_node,
                          fill = factor(node),
                          color = factor(node),
                          label = paste0(node, " (", pct, "%)"))) +
  geom_sankey(flow.alpha = 0.5, node.color = "black", 
              show.legend = F)+
  geom_sankey_label(size = 3, color = "black", fill = "white",
                    alpha = 0.75,hjust = -0.1,vjust = 0) +
  labs(x = NULL) +
  scale_fill_manual(values = c("eBird data" = "#440154",
                               "Incomplete" = "gray",
                               "Complete" = "#472D7B",
                               "Other protocol" = "gray",
                               "Traveling" = "#3B528B",
                               "Stationary" = "#2C728E", 
                               "Other year" = "gray",
                               "2000-2024" = "#21908C",
                               "Other duration" = "gray",
                               "10 min - 5 hrs" = "#27AD81",
                               "No distance"= "gray",
                               "<5 km" = "#5DC863",
                               "1-5 km" = "#AADC32",
                               "Excluded" = "gray",
                               "Retained" = "#FDE725")) +
  scale_color_manual(values = c("eBird data" = "#440154",
                                "Incomplete" = "gray",
                                "Complete" = "#472D7B",
                                "Other protocol" = "gray",
                                "Traveling" = "#3B528B",
                                "Stationary" = "#2C728E", 
                                "Other year" = "gray",
                                "2000-2024" = "#21908C",
                                "Other duration" = "gray",
                                "10 min - 5 hrs" = "#27AD81",
                                "Other distance"= "gray",
                                "<5 km" = "#5DC863",
                                "1-5 km" = "#AADC32",
                                "Excluded" = "gray",
                                "Retained" = "#FDE725")) +
  scale_x_discrete(expand = expansion(add = c(0.2,1)))+
  theme_sankey()+
  theme(axis.text.x = element_text(hjust = -0.1, size = 12))

ggsave(filename = "SankeyPlot.pdf", plot = Fig_Sankey_Peru, width = 11, height = 5)
