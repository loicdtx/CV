library(ggplot2)
library(raster)
library(sp)
library(rgeos)
library(magrittr)

# world <- getData(name = 'countries')
world <- readRDS('~/countries_gadm36_sp.rds')
str(world@data)
levels(world@data$UNREG1)
# worldSub <- world[world$UNREG1 %in% c("Western Europe"),]
worldSub <- world[world$NAME_ENGLISH %in% c('France', 'Belgium', 'Netherlands', 'Luxembourg'),]
plot(worldSub)


FRA <- getData(name = 'GADM', country = 'FRA', level = 2)
FRA <- FRA[FRA$NAME_2 %in% c('Ardèche', 'Alpes-de-Haute-Provence', 'Maine-et-Loire', 'Hérault'),]
FRA <- gCentroid(FRA, byid = TRUE)


NL <- getData(name = 'GADM', country = 'NLD', level = 1)
NL <- NL[NL$NAME_1 %in% c('Gelderland'),]
NL <- gCentroid(NL, byid = TRUE)

places <- rbind(NL, FRA) %>%
  spTransform(CRS('+proj=robin')) %>%
  data.frame()

ggEurope <- spTransform(worldSub, CRS('+proj=robin')) %>%
  fortify(region = 'NAME_ISO')

ggplot(ggEurope) +
  geom_polygon(aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(x=long, y=lat, group=group), colour = 'white', size = 0.2, fill = NA) +
  geom_point(data = places, aes(x, y), color = 'red', size = 3) +
  coord_equal() +
  theme_bw() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill = "transparent",colour = NA),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_rect(fill = "transparent",colour = NA))

ggsave(filename = 'img/map.png', width = 4, height = 4, bg = 'transparent')

# Mali
africa <- world[world$CONTINENT == 'Africa',]
mali <- getData(name = 'GADM', country = 'MLI', level = 2)

niono <- mali[mali$NAME_2 == 'Niono',] %>%
  gCentroid(byid = TRUE) %>%
  spTransform(CRS('+proj=robin')) %>%
  data.frame()

ggAfrica <- spTransform(africa, CRS('+proj=robin')) %>%
  fortify(region = 'NAME')

ggplot(ggAfrica) +
  geom_polygon(aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(x=long, y=lat, group=group), colour = 'white', size = 0.2, fill = NA) +
  geom_point(data = niono, aes(x, y), color = 'red', size = 3) +
  coord_equal() +
  theme_bw() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill = "transparent",colour = NA),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_rect(fill = "transparent",colour = NA))

ggsave(filename = 'img/mapAfrica.png', width = 4, height = 4, bg = 'transparent')


# Mexico
MEX_0 <- getData(name = 'GADM', country = 'MEX', level = 0)
MEX_1 <- getData(name = 'GADM', country = 'MEX', level = 1)
MEX_1 <- MEX_1[MEX_1$NAME_1 == 'Distrito Federal',]
MEX_1 <- gCentroid(MEX_1, byid = TRUE)


places <- MEX_1 %>%
  spTransform(CRS('+proj=robin')) %>%
  data.frame()

ggMex <- spTransform(MEX_0, CRS('+proj=robin')) %>%
  fortify(region = 'ISO')

ggplot(ggMex) +
  geom_polygon(aes(x=long, y=lat, group=group)) +
  geom_polygon(aes(x=long, y=lat, group=group), colour = 'white', size = 0.2, fill = NA) +
  geom_point(data = places, aes(x, y), color = 'red', size = 3) +
  coord_equal() +
  theme_bw() +
  theme(axis.line=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks=element_blank(),
        axis.title.x=element_blank(),
        axis.title.y=element_blank(),
        panel.background=element_rect(fill = "transparent",colour = NA),
        panel.border=element_blank(),
        panel.grid.major=element_blank(),
        panel.grid.minor=element_blank(),
        plot.background=element_rect(fill = "transparent",colour = NA))

ggsave(filename = 'img/mapMex.png', width = 4, height = 4, bg = 'transparent')
