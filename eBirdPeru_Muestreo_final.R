#Spatio-temporal patterns of bird species based on the eBird platform in Peru
# Meza-Mori, Gerson; Orlando Acevedo-Charry; Ian James Ausprey; Matteo Anderle, Jaris Veneros

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#### ~~~~ Paquetes ~~~~ ####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
library(auk); # filtros a datos de eBird
library(tidyverse) # siete paquetes en uno para manejo de datos y figuras
library(dggridR) # grillas espaciales hexagonales
library(maps); # cargar mapas
library(ggmagnify); # zoom dentro de figuras
library(terra); library(geodata) #Elevación
library(sf);
library(avesperu) #lista de aves de Perú

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#### ~~~~ Preproceso de datos eBird ~~~~ ####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
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

#generate a temporal file to save the filtering eBird data (f_ebd) and sampling (f_sed)
f_ebd <- "temporalFiltering/ebd_Examples.txt" 
f_sed <- "temporalFiltering/sed_Examples.txt"

#Spatial grid cells - diameters of ~11km (area of 95.98 ~> ~100 km^2)
dggs_100 <- dgconstruct(spacing = 11) 

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#### ~~~~ Filtrando registros de aves de Perú en eBird ~~~~ ####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

Peru_ebd <- "ebd_PE_relFeb-2025/ebd_PE_relFeb-2025.txt"

# Filtrando por criterios especificos
ebd_only <- auk_ebd(file = Peru_ebd) |>
  auk_distance(distance = c(0,5)) |> # 0 a 5 km
  auk_duration(duration = c(10,300)) |> # 10 minutos a 5 horas
  auk_year(year = c(2000:2024)) |>
  auk_complete()|>
  auk_protocol(protocol = c("Traveling","Stationary"))|>
  auk_filter(f_ebd,
             overwrite=T, keep = colsE) |>
  read_ebd()

#ebd_only <- read_ebd(f_ebd)
#2,941,830 registros

# Generemos una grilla de muestreo para Perú, asignandole ID a cada registro
Peru <-  ebd_only |>
  mutate(cell = dgGEO_to_SEQNUM(dggs_100, # ID de cell de muestreo
                                longitude, latitude)$seqnum) |>
  group_by(cell) |>
  mutate(mean_latitude = mean(latitude),
         mean_longitude = mean(longitude),
         # effort_distance_km to 0 for non-travelling counts
         effort_distance_km = if_else(protocol_type %in% c("Stationary"),
                                      0, effort_distance_km),# si es estacionario es 0
         #effort_distance_km change to integer no decimals
         effort_distance_kmI = round(effort_distance_km, digits = 0),
         # split date into year, month, week, and day of year
         year = year(observation_date),
         month = month(observation_date),
         week = week(observation_date),
         day_of_year = yday(observation_date))

# Salvar como RDS permite transferencia eficiente 
saveRDS(Peru, "Peru_eBird_registros_CellID.rds")

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#### ~~~~ Llamar el archivo filtrado ~~~~ ####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
# Peru <- readRDS("Peru_eBird_registros_CellID.rds") |> ungroup()
head(Peru)
class(Peru)

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#### ~~~~ Descripción general de los datos ~~~~####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

##### ~~ Mapa sencillo de registros ~~ ####
world1 <- sf::st_as_sf(maps::map(database = 'world', plot = FALSE, fill = TRUE))

ggplot()+
  geom_sf(data = world1)+
  geom_point(data = Peru, 
             aes(x = longitude, y = latitude), 
             size = 0.5, 
             alpha = 0.25)+
  coord_sf(xlim = c(-81.52, -68.65),
           ylim =  c(-0.11, -18.89)) +
  labs(title = "Muestreos en eBird para Peru")+
  theme_bw() 

##### ~~ como ha cambiado en el tiempo? ~~ ####
TimelineRecordsA <- Peru |> 
  filter(year != 2025) |>
  group_by(year, month) |>
  count() |>
  ggplot(aes(x = year, y = n/1000, 
             fill = factor(month.abb[month], 
                            levels = month.abb))) +
  geom_col() +
  scale_y_continuous(expand = c(0,0))+
  scale_fill_manual(values = hcl.colors(12, "Temps")) +
  labs(x = "Year", 
     tag = "A",
 #      title = "All species",
       y = "Number of checklists \n (*1,000)",
       fill = "Month")+
  theme_classic() +
  theme(legend.position = "inside",
        legend.direction = "horizontal",
        legend.position.inside = c(0.3,0.7))+
  guides(fill = guide_legend(nrow=3,byrow=TRUE))
TimelineRecordsA

Peru |>
  filter(year != 2025) |>
  count(year, month) |>
  ggplot(aes(year, month, fill = n/1000)) +
  geom_tile(alpha = 0.9) +
  scale_y_continuous(
    breaks = 1:12,
    labels = month.abb
  ) +
  scale_fill_viridis_c() +
  labs(x = "Year", 
       #tag = "a)",
       #      title = "All species",
       fill = "Number of checklists \n (*1,000)",
       y = "Month")+
  theme_classic() +
  theme(legen)


#ggsave(filename = "TimeLinePeru.jpg",TimelineRecordsA, dpi = 300,
#       width = 140, height = 70, units = "mm")

# Por mes

TimelineRecordsB <- Peru |> 
  filter(year != 2025) |>
  group_by(year, month) |>
  count() |>
  ggplot(aes(x = factor(month.abb[month], 
                        levels = month.abb), y = n/1000, 
             fill = factor(year),
             color = factor(year))) +
  geom_col(alpha = 0.8) +
  scale_y_continuous(expand = c(0,0))+
  scale_color_manual(values = hcl.colors(25, "Viridis")) +
  scale_fill_manual(values = hcl.colors(25, "Viridis")) +
  labs(x = "Year", 
       tag = "b)",
       #      title = "All species",
       y = "Number of checklists \n (*1,000)",
       fill = "Year", 
       color = "Year")+
  theme_classic() +
  theme(legend.position = "bottom",
        legend.direction = "horizontal",
        legend.position.inside = c(0.3,0.7))+
  guides(fill = guide_legend(ncol=9,byrow=TRUE), 
         color = guide_legend(ncol=9,byrow=TRUE))
TimelineRecordsB

library(patchwork)
TimelineRecordsA / TimelineRecordsB

ggsave(filename = "PostReview/TemporalDynamiceeBirdPeru_.jpg", dpi = 300, width = 190, height = 180, units = "mm")


##### ~~ Cuantas localidades hay? ~~ ####
Peru |>
  group_by(locality_id) |>
  count() |>
  nrow()
# 63854

##### ~~ En cuantas unidades espaciales? (celdas) ~~ ####
Peru |>
  group_by(cell) |>
  count() |>
  nrow()
# 4214

##### ~~ Cuáles especies son las mas representadas en las unidades espaciales? ~~ ####
Peru |> 
  group_by(scientific_name,cell) |> 
  summarise(n_lists = n()) |> 
  ungroup() |> 
  group_by(scientific_name) |> 
  summarise(n_cells = n(), 
            prop_cells = n()/4214) |> # Total numero de cells en Peru
  arrange(desc(n_cells)) # adicionar |> View() # para ver la tabla

##### ~~ Cómo es la representatividad de los protocolos usados? ~~ ####
Peru |> 
  group_by(scientific_name, protocol_type) |> 
  summarise(n_obs = n()) |> 
  pivot_wider(names_from = protocol_type, values_from = n_obs) 
# adicionar |> View() # para ver la tabla
# Stationary & Traveling son los que aportan mas

##### ~~ Para saber el número de listas en cada cell ~~ ####
CellObservations <- Peru |>
  group_by(cell, checklist_id) |>
  summarise(count=n()) |>
  group_by(cell) |>
  summarise(n_lists = n()) |>
  mutate(Log10Lists = log10(n_lists)) # esta transformacion facilita visualización

##### ~~ Generar una grilla del pais ~~ ####
gridPeru <- dgcellstogrid(dggs_100, CellObservations$cell)

# Actualizar las informacion de la cell para incluir el numero de listas
gridPeru <- merge(gridPeru, CellObservations, by.x="seqnum", by.y="cell")

# Handle cells that cross 180 degrees. Something funky happens if no
wrapped_gridPeru = st_wrap_dateline(gridPeru, 
                                    options = c("WRAPDATELINE=YES","DATELINEOFFSET=180"), 
                                    quiet = TRUE)

# guardar el nombre de las cells como caracter, teniendo numero de listas en el dataset
wrapped_gridPeru$cell <- as.character(wrapped_gridPeru$seqnum)

saveRDS(wrapped_gridPeru, "wrapped_gridPeru.rds")

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#### ~~~~ Cobertura y completitud del muestreo en eBird para Perú ~~~~ ####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

# Calcular (y guardar) el número de especies registradas por celda
S <- Peru |>
  group_by(cell,scientific_name) |>
  count() |> #head()
  group_by(cell) |>
  count() 

colnames(S) <- c("cell", "S_m")

# Calcular (y guardar) el número de combinaciones únicas de registros (especies*dates) por celda
N <- Peru|>
  group_by(cell,scientific_name,observation_date) |>
  count() |> #head()
  group_by(cell) |>
  count()

colnames(N) <- c("cell", "N_m")

# Calcular (y guardar) el número de especies registradas en exactamente 1 día en cada celda (a)
a <- Peru |>
  group_by(cell, scientific_name) |>
  summarise(days_recorded = n_distinct(observation_date)) |> 
  filter(days_recorded == 1) |> 
  group_by(cell) |> 
  summarise(a = n())

# Calcular (y guardar) el número de especies registradas en exactamente 2 días en cada celda (b)
b <- Peru |>
  group_by(cell, scientific_name) |>
  summarise(days_recorded = n_distinct(observation_date)) |> 
  filter(days_recorded == 2) |> 
  group_by(cell) |> 
  summarise(b = n())

# Calcular Chao2 y completitud del inventario (C)

Completeness <- S |> 
  left_join(N, by = "cell") |>
  left_join(a, by = "cell") |>
  left_join(b, by = "cell") |> 
  replace_na(list(a = 0,
                  b = 0)) |>
  mutate(Chao2_m = S_m + ((N_m - 1) / N_m) + ((a * (a - 1)) / (2 * (b + 2))),
         C_m = S_m / Chao2_m)

hist(Completeness$C_m)

Completeness$cell <- as.character(Completeness$cell)

# Para saber el número de listas en cada celda
CellObservations <- Peru |>
  group_by(cell, checklist_id) |>
  summarise(count=n()) |>
  group_by(cell) |>
  summarise(n_lists = n()) |>
  mutate(Log10Lists = log10(n_lists)) # esta transformacion facilita visualización

CellObservations$cell <- as.character(CellObservations$cell)

# Llamar el "wrapped grid" RDS, para incluir el valor de completitud
wrapped_gridPeru <- readRDS("wrapped_gridPeru.rds") |>
  left_join(Completeness, by = "cell") |> 
  mutate(C_m = round(C_m,2), 
         C_m_range = ifelse((C_m >= 0 & C_m <= 0.4), "0.0-0.4",
                     ifelse((C_m >= 0.41 & C_m <= 0.8), "0.4-0.8", 
                     ifelse((C_m >= 0.81 & C_m <= 1.0), "0.8-1.0", 
                                   "Other"))),
         S_m_range = ifelse((S_m >= 0 & S_m <= 100), "1-100",
                     ifelse((S_m >= 101 & S_m <= 400), "101-400", 
                     ifelse((S_m >= 401 & S_m <= 800), "401-800", 
                     "Other")))) |> 
  left_join(CellObservations, by = "cell")
  
# llamar el mapa del mundo como base de fondo
world1 <- sf::st_as_sf(maps::map(database = 'world', plot = FALSE, fill = TRUE))

##### ~~~~ Mapa de completitud en Perú ~~ ####
CT <- ggplot() +
  geom_sf(data = world1, fill = "#fbfbfb")+
  geom_sf(data=wrapped_gridPeru,
          aes(fill = C_m_range,
              color = C_m_range)) +
  coord_sf(xlim = c(-81.52, -68.65),
           ylim =  c(-0.11, -18.89)) +
  scale_fill_manual(values = c(#"#FFFFFF50",
                               "#86ADC670",
  #                             "#6396B670",
                               "#4A7C9D70",
  #                              "#39617A",
                               "#1E3F66"))+
  scale_color_manual(values = c(#"#FFFFFF50",
                               "#86ADC670",
    #                           "#6396B670",
                               "#4A7C9D70",
  #                             "#39617A",
                               "#1E3F66"))+
  labs(y = "",
       x = "",       
       tag = "a)") +
  scale_x_continuous(breaks = seq(-180, 180, by = 5)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 5)) +
  theme_classic()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background = element_rect(colour = NA, fill = NA), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) 
CT

# Solo criterios laxo (>0.8) a estricto (>0.9)
ggplot() +
  geom_sf(data = world1, fill = "#fbfbfb")+
  geom_sf(data = wrapped_gridPeru |> filter(C_m > 0.8),
          fill = "#528AAE80",
          color = "#528AAE") +
  geom_sf(data = wrapped_gridPeru |> filter(C_m > 0.9),
          fill = "#1E3F66",
          color = "#1E3F66") +
  coord_sf(xlim = c(-81.52, -68.65),
           ylim =  c(-0.11, -18.89)) +
  labs(y = "Latitude",
       x = "Longitude",       
       title = "Lax (>0.8) to strict (>0.9) criteria",
       subtitle = bquote(cells~of~"~100"~km^2)) +
  theme_classic()+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.box.background = element_rect(colour = "black")) 

##### ~~~~ Mapa de número de listas (log10) en Perú ~~ ####
summary(wrapped_gridPeru$n_lists)

my_breaks <- c(1,90,8960)

LT <- ggplot() +
  geom_sf(data = world1, fill = "#fbfbfb")+
  geom_sf(data=wrapped_gridPeru,
          aes(color = n_lists, 
              fill = n_lists)) +
  coord_sf(xlim = c(-81.52, -68.65),
           ylim =  c(-0.11, -18.89)) +
  scale_fill_gradient(low="#BF86C660", 
                      high="#73397A",
                      trans = "log",  
                      breaks = my_breaks,
                      labels = my_breaks)+
  scale_color_gradient(low="#BF86C660", 
                       high="#73397A",
                       trans = "log", 
                       breaks = my_breaks,
                       labels = my_breaks)+
  scale_x_continuous(breaks = seq(-180, 180, by = 5)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 5)) +
  labs(y = "",
       x = "",       
       tag = "b)") +
  theme_classic()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background = element_rect(colour = NA, fill = NA), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank()) 
LT

##### ~~~~ Mapa de riqueza en Perú ~~ ####
summary(wrapped_gridPeru$S_m)

ST <- ggplot() +
  geom_sf(data = world1, fill = "#fbfbfb")+
  geom_sf(data=wrapped_gridPeru,
          aes(fill = S_m_range,
              color = S_m_range)) +
  coord_sf(xlim = c(-81.52, -68.65),
           ylim =  c(-0.11, -18.89)) +
  scale_fill_manual(values = c(#"#FFFFFF50",
                               "#A1BE7530",
                               #"#8AAE5250",
                               "#6F8C4170",
                               "#617A3990"))+
  scale_color_manual(values = c(#"#FFFFFF50",
                                "#A1BE7540",
                                #"#8AAE5260",
                                "#6F8C4180",
                                "#617A39"))+
  scale_x_continuous(breaks = seq(-180, 180, by = 5)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 5)) +
  labs(y = "",
       x = "",
       tag = "c)") +
  theme_classic()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background = element_rect(colour = NA, fill = NA), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank()) 

ST

##### ~~~~ Mapa de riqueza (log10) en Perú ~~ ####
my_breaks <- c(1,8,80,781)

ggplot() +
  geom_sf(data = world1, fill = "#fbfbfb")+
  geom_sf(data=wrapped_gridPeru,
          aes(color = S_m, 
              fill = S_m)) +
  coord_sf(xlim = c(-81.52, -68.65),
           ylim =  c(-0.11, -18.89)) +
  scale_fill_gradient(low="#A1BE7560", 
                      high="#617A39",
                      trans = "log",  
                      breaks = my_breaks,
                      labels = my_breaks)+
  scale_color_gradient(low="#A1BE7560", 
                       high="#617A39",
                       trans = "log", 
                       breaks = my_breaks,
                       labels = my_breaks)+
  labs(y = "Latitude",
       x = "Longitude",       
       title = "eBird effort in Peru",
       subtitle = bquote(cells~of~"~100"~km^2),
       color = expression(Log["10"]~"species"),
       fill = expression(Log["10"]~"species")) +
  theme_classic()+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.box.background = element_rect(colour = "black"))

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#### ~~~~ Especies endémicas en Perú ~~~~ ####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

splist <- unique(Peru$scientific_name)

# identificar especies no identificadas en la lista de Perú del paquete `avesperu`
splistNA <- search_avesperu(splist = splist, max_distance = 1) |> filter(is.na(status))
splistNA$name_submitted 
# varias escapadas de cautiverio o introducidas, pero otras cambios taxonómicos

# solo corregir las que son endémicas o amenazadas (VU, EN, CR)
splist <- Peru |>
  mutate(scientific_name = ifelse(scientific_name == "Cranioleuca berlepschi",
                                         "Thripophaga berlepschi",
                                  scientific_name)) |>
  dplyr::select(scientific_name) |>
  unique()

PeruEndemics <- search_avesperu(splist = splist$scientific_name, max_distance = 1) |>
  mutate(status = ifelse(status == "Endémico", "Endemic",
                  # Grallaria atuensis Endémico (separado de Grallaria quitensis)
                  ifelse(name_submitted == "Grallaria atuensis", "Endemic",
                  status)),
         scientific_name = ifelse(name_submitted == "Thripophaga berlepschi",
                                  "Cranioleuca berlepschi",
                           name_submitted)) |>
  filter(status == "Endemic") |> 
  dplyr::select(scientific_name, status) |>
  left_join(Peru)

length(unique(PeruEndemics$scientific_name)) #116 especies endémicas

saveRDS(PeruEndemics, "PeruEndemics_eBird_registros_CellID.rds")

##### ~~ como ha cambiado en el tiempo? ~~ ####
TimelineRecordsEndemics <- PeruEndemics |> 
  group_by(year, month) |>
  count() |>
  ggplot(aes(x = year, y = n, fill = factor(month))) +
  geom_col() +
  scale_y_continuous(expand = c(0,0))+
  scale_fill_viridis_d()+
  labs(x = "Year", y = "Number of checklists",
       title = "Endemic species",
       tag = "B",
       fill = "Month")+
  theme_classic() +
  theme(legend.position = "inside",
        legend.direction = "horizontal",
        legend.position.inside = c(0.3,0.7))+
  guides(fill = guide_legend(nrow=3,byrow=TRUE))
TimelineRecordsEndemics

##### ~~~~ Cobertura y completitud del muestreo en eBird para Endemicas de Perú ~~ ####
# Calcular (y guardar) el número de especies registradas por celda
Se <- PeruEndemics |>
  group_by(cell,scientific_name) |>
  count() |> #head()
  group_by(cell) |>
  count() 

colnames(Se) <- c("cell", "S_m")

# Calcular (y guardar) el número de combinaciones únicas de registros (especies*dates) por celda
Ne <- PeruEndemics |>
  group_by(cell,scientific_name,observation_date) |>
  count() |> #head()
  group_by(cell) |>
  count()

colnames(Ne) <- c("cell", "N_m")

# Calcular (y guardar) el número de especies registradas en exactamente 1 día en cada celda (a)
ae <- PeruEndemics |>
  group_by(cell, scientific_name) |>
  summarise(days_recorded = n_distinct(observation_date)) |> 
  filter(days_recorded == 1) |> 
  group_by(cell) |> 
  summarise(a = n())

# Calcular (y guardar) el número de especies registradas en exactamente 2 días en cada celda (b)
be <- PeruEndemics |>
  group_by(cell, scientific_name) |>
  summarise(days_recorded = n_distinct(observation_date)) |> 
  filter(days_recorded == 2) |> 
  group_by(cell) |> 
  summarise(b = n())

# Calcular Chao2 y completitud del inventario (C)

CompletenessE <- Se |> 
  left_join(Ne, by = "cell") |>
  left_join(ae, by = "cell") |>
  left_join(be, by = "cell") |> 
  replace_na(list(a = 0,
                  b = 0)) |>
  mutate(Chao2_m = S_m + ((N_m - 1) / N_m) + ((a * (a - 1)) / (2 * (b + 2))),
         C_m = S_m / Chao2_m)

hist(CompletenessE$C_m)

CompletenessE$cell <- as.character(CompletenessE$cell)

# Para saber el número de listas en cada celda
CellObservationsE <- PeruEndemics |>
  group_by(cell, checklist_id) |>
  summarise(count=n()) |>
  group_by(cell) |>
  summarise(n_lists = n()) |>
  mutate(Log10Lists = log10(n_lists)) # esta transformacion facilita visualización

CellObservationsE$cell <- as.character(CellObservationsE$cell)

# Llamar el "wrapped grid" RDS, para incluir el valor de completitud
wrapped_gridPeruE <- readRDS("wrapped_gridPeru.rds") |>
  left_join(CompletenessE, by = "cell") |> 
  mutate(C_m = round(C_m,2), 
         C_m_range = ifelse((C_m >= 0 & C_m <= 0.4), "0.0-0.4",
                     ifelse((C_m >= 0.41 & C_m <= 0.8), "0.4-0.8", 
                     ifelse((C_m >= 0.81 & C_m <= 1.0), "0.8-1.0", 
                     "Other"))),
         S_m_range = ifelse((S_m >= 0 & S_m <= 5), "1-5",
                     ifelse((S_m >= 6 & S_m <= 15), "6-15", 
                     ifelse((S_m >= 16 & S_m <= 22), "16-22", 
                     "Other")))) |> 
  left_join(CellObservationsE, by = "cell") |>
  filter(!is.na(Log10Lists.y))

saveRDS(wrapped_gridPeruE, "wrapped_gridPeruE.rds")

# llamar el mapa del mundo como base de fondo
world1 <- sf::st_as_sf(maps::map(database = 'world', plot = FALSE, fill = TRUE))

##### ~~~~ Mapa de completitud en Perú (endémicas) ~~ ####
CE <- ggplot() +
  geom_sf(data = world1, fill = "#fbfbfb")+
  geom_sf(data=wrapped_gridPeruE,
          aes(fill = C_m_range,
              color = C_m_range)) +
  coord_sf(xlim = c(-81.52, -68.65),
           ylim =  c(-0.11, -18.89)) +
  scale_fill_manual(values = c(#"#FFFFFF50",
    "#86ADC670",
    #                             "#6396B670",
    "#4A7C9D70",
    #                              "#39617A",
    "#1E3F66"))+
  scale_color_manual(values = c(#"#FFFFFF50",
    "#86ADC670",
    #                           "#6396B670",
    "#4A7C9D70",
    #                             "#39617A",
    "#1E3F66"))+
  scale_x_continuous(breaks = seq(-180, 180, by = 5)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 5)) +
  labs(y = "Latitude",
       x = "",       
       tag = "d)") +
  theme_classic()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background = element_rect(colour = NA, fill = NA), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank()) 

CE  


# Solo criterios laxo (>0.8) a estricto (>0.9)
ggplot() +
  geom_sf(data = world1, fill = "#fbfbfb")+
  geom_sf(data = wrapped_gridPeruE |> filter(C_m > 0.8),
          fill = "#528AAE80",
          color = "#528AAE") +
  geom_sf(data = wrapped_gridPeruE |> filter(C_m > 0.9),
          fill = "#1E3F66",
          color = "#1E3F66") +
  coord_sf(xlim = c(-81.52, -68.65),
           ylim =  c(-0.11, -18.89)) +
  labs(y = "Latitude",
       x = "Longitude",       
       title = "Lax (>0.8) to strict (>0.9) criteria (endemics)",
       subtitle = bquote(cells~of~"~100"~km^2)) +
  theme_classic()+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.box.background = element_rect(colour = "black")) 

##### ~~~~ Mapa de número de listas (log10) en Perú (endémicas) ~~ ####
summary(wrapped_gridPeruE$n_lists)

my_breaks <- c(1,50,1492)

LE <- ggplot() +
  geom_sf(data = world1, fill = "#fbfbfb")+
  geom_sf(data=wrapped_gridPeruE,
          aes(color = n_lists, 
              fill = n_lists)) +
  coord_sf(xlim = c(-81.52, -68.65),
           ylim =  c(-0.11, -18.89)) +
  scale_fill_gradient(low="#BF86C660", 
                      high="#73397A",
                      trans = "log",  
                      breaks = my_breaks,
                      labels = my_breaks)+
  scale_color_gradient(low="#BF86C660", 
                       high="#73397A",
                       trans = "log", 
                       breaks = my_breaks,
                       labels = my_breaks)+
  scale_x_continuous(breaks = seq(-180, 180, by = 5)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 5)) +
  labs(y = "",
       x = "",       
       tag = "e)") +
  theme_classic()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background = element_rect(colour = NA, fill = NA), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank()) 

LE 

##### ~~~~ Mapa de riqueza en Perú (endémicas) ~~ ####
summary(wrapped_gridPeruE$S_m)

wrapped_gridPeruE$S_m_range <- factor(wrapped_gridPeruE$S_m_range,
                                         levels = c("1-5",
                                                    "6-15",
                                                    "16-22"))

SE <- ggplot() +
  geom_sf(data = world1, fill = "#fbfbfb")+
  geom_sf(data=wrapped_gridPeruE,
          aes(fill = S_m_range,
              color = S_m_range)) +
  coord_sf(xlim = c(-81.52, -68.65),
           ylim =  c(-0.11, -18.89)) +
  scale_fill_manual(values = c(#"#FFFFFF50",
    "#A1BE7530",
    #"#8AAE5250",
    "#6F8C4170",
    "#617A3990"))+
  scale_color_manual(values = c(#"#FFFFFF50",
    "#A1BE7530",
    #"#8AAE5250",
    "#6F8C4170",
    "#617A3990"))+
  scale_x_continuous(breaks = seq(-180, 180, by = 5)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 5)) +
  labs(y = "",
       x = "",       
       tag = "f)") +
  theme_classic()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background = element_rect(colour = NA, fill = NA), 
        axis.text.x = element_blank(), 
        axis.title.x = element_blank(), 
        axis.title.y = element_blank(), 
        axis.text.y = element_blank()) 

SE

###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###
#### ~~~~ Especies amenazadas en Perú - AviList IUCN ~~~~ ####
###~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~###

iucn <- read_csv("Avilist_v2025_spp.csv") |>
  rename(scientific_name = Scientific_name,
         iucn = IUCN_Red_List_Category)

# identificar especies no identificadas en la lista de categoria de amenaza para Perú de BirdLife
splistNA <- Peru |>
  dplyr::select(scientific_name) |>
  unique() |>
  left_join(iucn |> dplyr::select(scientific_name, iucn)) |>
  filter(is.na(iucn))

splistNA$scientific_name 
# No hay amenazadas (solo Oressochen jubatus NT)

# Extraer solo amenazadas
PeruThreatened <- Peru |>
  left_join(iucn |> dplyr::select(scientific_name, iucn)) |>
  filter(iucn %in% c("VU",
                     "EN",
                     "CR",
                     "CR (PE)",
                     "CR (PEW)",
                     "EW",
                     "EX"))

length(unique(PeruThreatened$scientific_name)) #68 especies amenazadas

saveRDS(PeruThreatened, "PeruThreatened_eBird_registros_CellID.rds")

##### ~~ como ha cambiado en el tiempo? ~~ ####
TimelineRecordsThreat <- PeruThreatened |> 
  group_by(year, month) |>
  count() |>
  ggplot(aes(x = year, y = n, fill = factor(month))) +
  geom_col() +
  scale_y_continuous(expand = c(0,0))+
  scale_fill_viridis_d()+
  labs(x = "Year", y = "Number of checklists",
       title = "Threatened species",
       tag = "C",
       fill = "Month")+
  theme_classic() +
  theme(legend.position = "inside",
        legend.direction = "horizontal",
        legend.position.inside = c(0.3,0.7))+
  guides(fill = guide_legend(nrow=3,byrow=TRUE))

TimelineRecordsThreat


##### ~~~~ Cobertura y completitud del muestreo en eBird para Amenazadas de Perú ~~ ####
# Calcular (y guardar) el número de especies registradas por celda
St <- PeruThreatened |>
  group_by(cell,scientific_name) |>
  count() |> #head()
  group_by(cell) |>
  count() 

colnames(St) <- c("cell", "S_m")

# Calcular (y guardar) el número de combinaciones únicas de registros (especies*dates) por celda
Nt <- PeruThreatened |>
  group_by(cell,scientific_name,observation_date) |>
  count() |> #head()
  group_by(cell) |>
  count()

colnames(Nt) <- c("cell", "N_m")

# Calcular (y guardar) el número de especies registradas en exactamente 1 día en cada celda (a)
at <- PeruThreatened |>
  group_by(cell, scientific_name) |>
  summarise(days_recorded = n_distinct(observation_date)) |> 
  filter(days_recorded == 1) |> 
  group_by(cell) |> 
  summarise(a = n())

# Calcular (y guardar) el número de especies registradas en exactamente 2 días en cada celda (b)
bt <- PeruThreatened |>
  group_by(cell, scientific_name) |>
  summarise(days_recorded = n_distinct(observation_date)) |> 
  filter(days_recorded == 2) |> 
  group_by(cell) |> 
  summarise(b = n())

# Calcular Chao2 y completitud del inventario (C)

CompletenessT <- St |> 
  left_join(Nt, by = "cell") |>
  left_join(at, by = "cell") |>
  left_join(bt, by = "cell") |> 
  replace_na(list(a = 0,
                  b = 0)) |>
  mutate(Chao2_m = S_m + ((N_m - 1) / N_m) + ((a * (a - 1)) / (2 * (b + 2))),
         C_m = S_m / Chao2_m)

hist(CompletenessT$C_m)

CompletenessT$cell <- as.character(CompletenessT$cell)

# Para saber el número de listas en cada celda
CellObservationsT <- PeruThreatened |>
  group_by(cell, checklist_id) |>
  summarise(count=n()) |>
  group_by(cell) |>
  summarise(n_lists = n()) |>
  mutate(Log10Lists = log10(n_lists)) # esta transformacion facilita visualización

CellObservationsT$cell <- as.character(CellObservationsT$cell)

# Llamar el "wrapped grid" RDS, para incluir el valor de completitud
wrapped_gridPeruT <- readRDS("wrapped_gridPeru.rds") |>
  left_join(CompletenessT, by = "cell") |> 
  mutate(C_m = round(C_m,2), 
         C_m_range = ifelse((C_m >= 0 & C_m <= 0.4), "0.0-0.4",
                     ifelse((C_m >= 0.41 & C_m <= 0.8), "0.4-0.8", 
                     ifelse((C_m >= 0.81 & C_m <= 1.0), "0.8-1.0", 
                     "Other"))),
         S_m_range = ifelse((S_m >= 0 & S_m <= 4), "1-4",
                     ifelse((S_m >= 5 & S_m <= 8), "5-8", 
                     ifelse((S_m >= 9 & S_m <= 13), "9-13", 
                     "Other")))) |> 
  left_join(CellObservationsT, by = "cell") |>
  filter(!is.na(S_m))

saveRDS(wrapped_gridPeruT, "wrapped_gridPeruT.rds")

# llamar el mapa del mundo como base de fondo
world1 <- sf::st_as_sf(maps::map(database = 'world', plot = FALSE, fill = TRUE))

##### ~~~~ Mapa de completitud en Perú (endémicas) ~~ ####
CTh <- ggplot() +
  geom_sf(data = world1, fill = "#fbfbfb")+
  geom_sf(data=wrapped_gridPeruT,
          aes(fill = C_m_range,
              color = C_m_range)) +
  coord_sf(xlim = c(-81.52, -68.65),
           ylim =  c(-0.11, -18.89)) +
  scale_fill_manual(values = c(#"#FFFFFF50",
    #"#86ADC670",
    #                             "#6396B670",
    "#4A7C9D70",
    #                              "#39617A",
    "#1E3F66"))+
  scale_color_manual(values = c(#"#FFFFFF50",
    #"#86ADC670",
    #                             "#6396B670",
    "#4A7C9D70",
    #                              "#39617A",
    "#1E3F66"))+
  scale_x_continuous(breaks = seq(-180, 180, by = 5)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 5)) +
  labs(y = "",
       x = "",       
       tag = "g)") +
  theme_classic()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background = element_rect(colour = NA, fill = NA)) 

CTh


# Solo criterios laxo (>0.8) a estricto (>0.9)
ggplot() +
  geom_sf(data = world1, fill = "#fbfbfb")+
  geom_sf(data = wrapped_gridPeruT |> filter(C_m > 0.8),
          fill = "#528AAE80",
          color = "#528AAE") +
  geom_sf(data = wrapped_gridPeruT |> filter(C_m > 0.9),
          fill = "#1E3F66",
          color = "#1E3F66") +
  coord_sf(xlim = c(-81.52, -68.65),
           ylim =  c(-0.11, -18.89)) +
  labs(y = "Latitude",
       x = "Longitude",       
       title = "Lax (>0.8) to strict (>0.9) criteria (threatened)",
       subtitle = bquote(cells~of~"~100"~km^2)) +
  theme_classic()+
  theme(legend.position = "right",
        legend.direction = "vertical",
        legend.box.background = element_rect(colour = "black")) 

##### ~~~~ Mapa de número de listas (log10) en Perú (amenazadas) ~~ ####
summary(wrapped_gridPeruT$n_lists)

my_breaks <- c(1,50,1569)

LTh <- ggplot() +
  geom_sf(data = world1, fill = "#fbfbfb")+
  geom_sf(data=wrapped_gridPeruT,
          aes(color = n_lists, 
              fill = n_lists)) +
  coord_sf(xlim = c(-81.52, -68.65),
           ylim =  c(-0.11, -18.89)) +
  scale_fill_gradient(low="#BF86C660", 
                      high="#73397A",
                      trans = "log",  
                      breaks = my_breaks,
                      labels = my_breaks)+
  scale_color_gradient(low="#BF86C660", 
                       high="#73397A",
                       trans = "log", 
                       breaks = my_breaks,
                       labels = my_breaks)+
  scale_x_continuous(breaks = seq(-180, 180, by = 5)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 5)) + 
  labs(y = "",
       x = "Longitude",       
       tag = "h)") +
  theme_classic()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background = element_rect(colour = NA, fill = NA), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank()) 

LTh

##### ~~~~ Mapa de riqueza en Perú (amenazadas) ~~ ####
summary(wrapped_gridPeruT$S_m)

STh <- ggplot() +
  geom_sf(data = world1, fill = "#fbfbfb")+
  geom_sf(data=wrapped_gridPeruT,
          aes(fill = S_m_range,
              color = S_m_range)) +
  coord_sf(xlim = c(-81.52, -68.65),
           ylim =  c(-0.11, -18.89)) +
  scale_fill_manual(values = c("#A1BE7540",
                               "#8AAE5260",
                               "#617A3999"))+
  scale_color_manual(values = c("#A1BE7550",
                                "#8AAE5270",
                                "#617A39"))+
  scale_x_continuous(breaks = seq(-180, 180, by = 5)) +
  scale_y_continuous(breaks = seq(-90, 90, by = 5)) +
  labs(y = "",
       x = "",       
       tag = "i)") +
  theme_classic()+
  theme(legend.position = "inside",
        legend.position.inside = c(0.2, 0.2),
        legend.direction = "vertical",
        legend.title = element_blank(),
        legend.background = element_rect(colour = NA, fill = NA), 
        axis.text.y = element_blank(), 
        axis.title.y = element_blank()) 

STh

# Combined figure 3x3 ####

library(patchwork)
plot_list <- list(CT, LT, ST, 
                  CE, LE, SE,
                  CTh, LTh, STh)
wrap_plots(plot_list, ncol = 3, nrow = 3)
ggsave(filename = "PostReview/eBird_Peru_a_i.jpg", dpi = 300,
       height = 290, width = 210, units = "mm")
# End of code ####
