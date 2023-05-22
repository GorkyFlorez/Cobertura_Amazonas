
library(sf)
library(ggplot2)
library(ggspatial)
library(raster)

Peru  <- getData('GADM', country='Peru', level=1) %>% st_as_sf()

Per  <- getData('GADM', country='Peru', level=0) %>% st_as_sf()
Amazonas =  subset(Peru , NAME_1 == "Amazonas")

CobVeg_18061 = st_read("SHP/Amazonas_Cober.geojson")  %>% st_as_sf()
Cob  <- st_transform(CobVeg_18061 ,crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

SurAmerica = st_read("SHP/SurAmerica.geojson")  %>% st_as_sf()
SurAmeric  <- st_transform(SurAmerica  ,
                           crs = st_crs("+proj=longlat +datum=WGS84 +no_defs"))

SurA= ggplot()+
  geom_sf(data = SurAmeric, fill="white", color="black", size=0.01)+
  geom_sf(data = Per , fill="gray", color="black")+
  geom_sf(data = Amazonas, fill="black", color="black")+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "#a2d2ff"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  annotate(geom = "text", x = -60, y = 10, hjust = 0, vjust = 1, 
           label = "a) Sur America",size = 3, family="serif", color = 
             "black",  fontface="italic", face = "bold")+
  annotate(geom = "text", x = -80, y = -40, hjust = 0, vjust = 1, 
           label = "Pacific ocean",size = 3, family="serif", color = 
             "blue",  fontface="italic", angle=90)+
  annotate(geom = "text", x = -55, y = -50, hjust = 0, vjust = 1, 
           label = "Atlantic ocean",size = 3, family="serif", color = 
             "blue",  fontface="italic")+
  annotate(geom = "text", x = -70, y = -10, hjust = 0, vjust = 1, 
           label = "Peru",size = 3, family="serif", color = 
             "black",  fontface="italic")
SurA
SurA.grob  <- ggplotGrob(SurA)

ggplot()+
  geom_sf(data = Cob, aes(fill=CobVeg2013))


col=c("#F0B0CF", # Agricultura costera andina
      "#f94144", # Area Urbana 
      "#ef476f", # Area de no bosuqe amazonico
      "#7209b7", # Bofedal
      "#00734C", # Bosque de colina alta 
      "#98E600", # Bosque de colina baja
      "#267300", # Bosque de montaña
      "#B4D79E", # Bosque de montaña altimontano
      "#ABCD66", # Bosque de montaña basimontano
      "#A5F57A", # Bosque de montaña montano
      "#D3FFBE", # Bosque de palmeras de montaña montano
      "#A8A800", # Bosque de terraza alta
      "#ABCD66", # Bosque de terraza baja
      "#005CE6", # Bosque inindable de palmeras
      "#FFAA00", # Bosque xerico interandino
      "#FFA77F", # Jalca
      "#97DBF2", # lagunas, lago, cochas
      "#734C00", # Matorral arbustivos
      "#895A44", # Matorral esclerofilo de montaña montano
      "#FFFF00", # pajonal andino
      "#D7B09E", # paramo
      "#BEE8FF", # Rio
      "#D7C29E", # Sabana xerica interandina
      "#AAFF00", # Vegetacion de isla
      "#FFEBAF" # Vegetacion esclerofila de arena blanca
      )



library(elevatr)
elev = get_elev_raster(Amazonas, z=10)
#plot(elev)

Poligo_alt    <- crop(elev, Amazonas)                           #   
Poligo_alt   <- Poligo_alt <- mask(Poligo_alt, Amazonas)
#plot(Poligo_alt)

slopee    = terrain(Poligo_alt  , opt = "slope") 
plot(slopee )
aspecte    = terrain(Poligo_alt, opt = "aspect")
#plot(aspecte)
hille     = hillShade(slopee, aspecte, angle = 40, direction = 270)
#plot(hille)


hill.p        <-  rasterToPoints(hille)
hill.pa_      <-  data.frame(hill.p)

colores = c( 
  "#8e9aaf",#celeste
  "#dda15e", # maroon 
  "#faedcd")#amarillo pastel

Geo_data       <-  rasterToPoints(Poligo_alt)
Geo_data_frame <-  data.frame(Geo_data)
colnames(Geo_data_frame) <- c("x","y", "alt")


library(ggnewscale) 

colores<- c("#015B26", "#3F8433", "#8BAB56", "#CFCF7D", "#C49450", "#AA5B32", "#872921")

cortes <- c(200, 500,1000,2000,3000,4000)

Alt_gg= ggplot()+
  geom_sf(data = Amazonas, fill=NA, color="black", size=0.01)+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  geom_raster(data = Geo_data_frame  ,aes(x,y, fill = alt), alpha=0.6)+
  scale_fill_gradientn(colours = colores ,
                       breaks = cortes ,
                       na.value = 'white',
                       name='Elevacion \n(msnm)')+
  guides(fill = guide_legend(nrow = 10, ncol=1))+
  theme(panel.grid.major = element_blank(),
        panel.grid.minor = element_blank(),
        panel.margin = unit(c(0,0,0,0), "cm"),
        plot.margin = unit(c(0,0,0,0), "cm"),
        axis.title = element_blank(),
        axis.text = element_blank(),
        axis.ticks = element_blank(),
        legend.position = "none", 
        
        panel.background = element_rect(fill = "white"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 1))+
  theme(legend.position = c(0.85, 0.75),
        legend.text=element_text(size=6, family="serif"),
        legend.title = element_text(size=6, family="serif"),
        legend.key.size = unit(0.3, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.3,"cm"))
Alt_gg.grob  <- ggplotGrob(Alt_gg)


library(tidyverse)
library(dplyr)
library(scales)

bivariate_color_scale <- tibble(
  "4 - 4" = "#872921", # high inequality, high income
  "3 - 4" = "#FF9254", # high inequality, high income
  "2 - 4" = "#FFC492",
  "1 - 4" = "#FFDCB8", # low inequality, high income
  "4 - 3" = "#E5FFF9", # high inequality, high income
  "3 - 3" = "#D0FFF0", # high inequality, high income
  "2 - 3" = "#BCFFE2",
  "1 - 3" = "#A7FFD5", # low inequality, high income
  "4 - 2" = "#93FFC8",
  "3 - 2" = "#7FFFBB",
  "2 - 2" = "#6AEBAA", # medium inequality, medium income
  "1 - 2" = "#56D28E",
  "4 - 1" = "#42AF75", # high inequality, low income
  "3 - 1" = "#2E8C5C", # high inequality, low income
  "2 - 1" = "#187341",
  "1 - 1" = "#015B26" # low inequality, low income
) %>%
  gather("group", "fill")


bivariate_color_scale %<>%
  separate(group, into = c("gini", "mean"), sep = " - ") %>%
  mutate(gini = as.integer(gini),
         mean = as.integer(mean))

legend <-ggplot() +
  geom_tile(data = bivariate_color_scale, mapping = aes(
    x = gini, y = mean, fill = fill)) +
  scale_fill_identity() +
  labs(x = "<------ manor altitud ",
       y = " menor altitud ------>") +
  theme( axis.title = element_text(size = 10),
         axis.title.x=element_text(color="black"),
         axis.text = element_blank(),
         panel.background = element_rect(fill = NA),
         axis.title.y=element_text(color="black")) +
  
  coord_fixed()
legend

legend.grob <- ggplotGrob(legend)

Mapa =ggplot()+
  geom_sf(data = SurAmeric, fill="#E1E1E1", color="black", size=0.01)+
  geom_sf(data = Peru, fill="white", color="black", size=0.4)+
  geom_raster(data = hill.pa_, aes(x,y, fill = layer), show.legend = F)+
  scale_fill_gradientn(colours=grey(1:100/100))+
  new_scale_fill()+
  geom_sf(data = Cob, aes(fill=CobVeg2013), alpha=0.6, color=NA)+
  scale_fill_manual(values = col,name='Cobertura Vegetal')+
  coord_sf(xlim = c(-79.8, -77.13264), ylim = c(-6.987491 ,-2.984379)) +
  theme_classic()+
  theme(legend.position = c(0.2, 0.78),
        axis.text.x  = element_text(face="bold", color="black", size=10,
                                    family="serif"),
        axis.text.y  = element_text(angle = 90,face="bold", color="black",
                                    family="serif",size=10),
        axis.title = element_text(face="bold", color="black"),
        legend.background = element_rect(fill = "white"),
        legend.text=element_text(size=9, family="serif"),
        legend.title = element_text(size=9, family="serif"),
        legend.key.size = unit(0.2, "cm"), #alto de cuadrados de referencia
        legend.key.width = unit(0.5,"cm"), #ancho de cuadrados de referencia 
        panel.background = element_rect(fill = "#a9def9"),
        panel.border = element_rect( color = "grey20", fill = NA, size = 0.5))+
  guides(fill = guide_legend(nrow = 25, ncol=1))+
  labs(title = '', fill = '',  x = 'Longitud', y = 'Latitud')+
  
  annotation_north_arrow(location="tl",which_north="true",style=north_arrow_fancy_orienteering ())+
  annotation_scale(location = "br",bar_cols = c("grey60", "white"), text_family = "ArcherPro Book")+
  annotate(geom = "text", x = -79.5, y = -3, hjust = 0, vjust = 1, 
           label = "ECUADOR",size = 5, family="serif", color = 
             "black",  fontface="italic")+
  annotate(geom = "text", x = -77.3, y = -4.5, hjust = 0, vjust = 1,face="bold",
           label = "Loreto",size = 3, family="serif", color = 
             "black")+
  annotate(geom = "text", x = -77.3, y = -6.9, hjust = 0, vjust = 1,face="bold", 
           label = "San Martin",size = 3, family="serif", color = 
             "black")+
  annotate(geom = "text", x = -79, y = -6, hjust = 0, vjust = 1,face="bold",
           label = "Cajamarca",size = 3, family="serif", color = 
             "black")+
  annotate(geom = "text", x = -79.7, y = -6.8, hjust = 0, vjust = 1,face="bold",
           label = "Lambayeque",size = 3, family="serif", color = 
             "black")+
  annotate(geom = "text", x = -79.5, y = -5, hjust = 0, vjust = 1,face="bold",
           label = "Piura",size = 3, family="serif", color = 
             "black")+
  annotation_custom(SurA.grob, xmin = -80, xmax = -79.2, ymin =-7.2, ymax=-6.1)+
  annotation_custom(Alt_gg.grob, xmin = -80, xmax = -79.3, ymin =-6, ymax=-4.5)+
  annotation_custom(legend.grob, xmin = -79.1, xmax = -78.4, ymin =-7, ymax=-6.3)+
  
  annotate(geom = "text", x = -75, y = -17, hjust = 0, vjust = 1, angle=45,
           label = "Gorky Florez Castillo            Gorky Florez Castillo        Gorky Florez Castillo",
           size = 7, family="serif", color = "grey20",
           alpha=0.2)


ggsave(plot=Mapa ,"Mapa/Mapa de Covertura Amazonas.png",units = "cm",width = 21, #alto
       height = 29, #ancho
       dpi=1200)


