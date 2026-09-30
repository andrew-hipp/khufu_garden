library(ggplot2)
library(gridExtra)
library(ggmap)
library(maps)
library(mapdata)
library(ggrepel)
library(rgdal)
library(plyr)
library(proj4)
library(magrittr)

proj4string <- c("+proj=aea", #albers equal area
                 "+lon_0=-82",
                  "+ellps=clrk66",
                  "+lat_1=38",
                   "+lat_2=42",
                   "+lat_0=40",
                   "+units=m",
                   "+x_0=0",
                   "+y_0=0",
                   "+datum=WGS84",
                   "+axis=enu"
                 )
# Source data

if(!exists('mapsLittle')) {
  mapsLittle <- lapply(dir('../data/little-maps'), function(x) {
    temp <- readOGR(dsn=paste('../data/little-maps/', x, sep = ''),
                    layer=strsplit(x, '_', fixed = T)[[1]][2])
    temp@data$id = rownames(temp@data)
    temp.points = fortify(temp, region="id")
    temp.df = join(temp.points, temp@data, by="id")
    temp.df$Species <- paste('Quercus',
                        strsplit(x, '_', fixed = T)[[1]][1])
    names(temp.df)[10:11] <- c('ID1', 'ID2')
    temp.df$spGroup <- paste(strsplit(x, '_', fixed = T)[[1]][1],
                             temp.df$group, sep = '')
    temp.df
  })
  names(mapsLittle) <- sapply(strsplit(dir('../data/little-maps'), '_', fixed = T),
                              function(x) x[1])
  mapsLittle$all <- do.call(rbind,
    mapsLittle[c(
                  #'alba',
                 'macrocarpa',
                 'stellata',
                 'bicolor')]
               )
  xy <- as.data.frame(mapsLittle$all[, c('long', 'lat')])
  pj <- project(xy, proj4string, inverse=TRUE)
  mapsLittle$all$lat <- pj$y
  mapsLittle$all$long <- pj$x

  mapsLittle$macro <- mapsLittle[['macrocarpa']]
  xy <- as.data.frame(mapsLittle$macro[, c('long', 'lat')])
  pj <- project(xy, proj4string, inverse=TRUE)
  mapsLittle$macro$lat <- pj$y
  mapsLittle$macro$long <- pj$x

  mapsLittle$alba <- mapsLittle[['alba']]
  xy <- as.data.frame(mapsLittle$alba[, c('long', 'lat')])
  pj <- project(xy, proj4string, inverse=TRUE)
  mapsLittle$alba$lat <- pj$y
  mapsLittle$alba$long <- pj$x

  # 'michauxii|bicolor|stellata'
  mapsLittle$'michauxii|bicolor|stellata' <- do.call(rbind,
    mapsLittle[c('michauxi',
                 'bicolor',
                 'stellata')]
               )
  xy <- as.data.frame(mapsLittle$'michauxii|bicolor|stellata'[, c('long', 'lat')])
  pj <- project(xy, proj4string, inverse=TRUE)
  mapsLittle$'michauxii|bicolor|stellata'$lat <- pj$y
  mapsLittle$'michauxii|bicolor|stellata'$long <- pj$x

  # 'montana|prinoides|muehlenbergii'
  mapsLittle$'montana|prinoides|muehlenbergii' <- do.call(rbind,
    mapsLittle[c('montana',
                 'muehlenbergii')]
               )
  xy <- as.data.frame(mapsLittle$'montana|prinoides|muehlenbergii'[, c('long', 'lat')])
  pj <- project(xy, proj4string, inverse=TRUE)
  mapsLittle$'montana|prinoides|muehlenbergii'$lat <- pj$y
  mapsLittle$'montana|prinoides|muehlenbergii'$long <- pj$x

}

states <- map_data("state")
counties <- map_data('county')
#cbbPalette <- c("#000000", "#E69F00", "#56B4E9", "#009E73",
#                "#F0E442", "#0072B2", "#D55E00", "#CC79A7")
#names(cbbPalette) <- dat.seq.mapping$Species %>% unique
cbbPalette <- c(
  'Quercus alba' = '#E61E51',
  'Quercus bicolor' = '#3094D1',
  'Quercus macrocarpa' = '#339947',
  'Quercus michauxii' = '#F79750',
  'Quercus montana' = '#E59CC4',
  'Quercus muehlenbergii' = '#6750A0',
  'Quercus prinoides' = '#6750D0',
  'Quercus stellata' = '#E0E21B'
)
p <- ggplot()
p <- p + geom_polygon(data = counties,
                      aes(x = long, y = lat, group = group),
                      fill = 'gray90', color = "white",
                      lwd = 0.1)
p <- p + geom_polygon(data = states,
                      aes(x = long, y = lat, group = group),
                      fill = NA, color = "gray70",
                      lwd=0.3)## figure out line width here
## add in a layer here of light gray for Q. macrocarpa, from GBIF, then overplot with counties, fill = NA
p <- p + coord_fixed(1.3)
p <- p + coord_map('albers', lat0=40, lat1=38)
p <- p + scale_fill_manual(values = cbbPalette)
p <- p + scale_x_continuous('Longitude')
p <- p + scale_y_continuous('Latitude')
p <- p + theme(legend.position = "none", panel.spacing = unit(0.1, 'in'))
  #c(0.12, 0.25))
#p <- p + theme_nothing()

p.map <- vector('list',0)
for(i in c('macro',
           'alba',
           'michauxii|bicolor|stellata',
           'montana|prinoides|muehlenbergii')) {
  p.map[[i]] <- p + geom_polygon(data = mapsLittle[[i]],
    aes(x =long, y =lat, group = spGroup, fill = Species),
        alpha = 0.4
        )

  p.map[[i]] <- p.map[[i]] + geom_point(data = dat.seq.mapping[grep(i, dat.seq.mapping$Species),],
                              mapping = aes(x = jitter(long), y = jitter(lat),
                                            fill = Species
                                            ),
                              shape=24,
                              colour = 'black',
                              size = 2
                            )
}
#p <- p + xlim(-127, -65)
#guides(fill=FALSE)  # do this to leave off the color legend

p.out <- grid.arrange(grobs = p.map)
ggsave('../out/Fig1.map.pdf', plot = p.out,
        width = 11, height = 7.5, units = 'in',
        useDingbats = FALSE)

p.legend.only <- p + geom_point(data = dat.seq.mapping,
                            mapping = aes(x = long, y = lat,
                                          fill = Species
                                          ),
                            shape=24,
                            colour = 'black',
                            size = 2
                          ) +
                  theme(legend.position = "left")
ggsave('../out/Fig1.legendToPaste.pdf', plot = p.legend.only,
        width = 11, height = 7.5, units = 'in',
        useDingbats = FALSE)

### make table 2

xy <- lapply(names(mapsLittle), function(x) as.data.frame(mapsLittle[[x]][, c('long', 'lat')]))
xy.lat <- lapply(xy, project, proj = proj4string, inverse= T)
xy.lat <- lapply(xy.lat, as.data.frame)
names(xy.lat) <- paste('Quercus', gsub('michauxi', 'michauxii', names(mapsLittle)))
table2 <- data.frame(
  N = table(dat.seq.mapping$Species) %>% as.integer,
  'Max sample dist (km)' =
    round(dat.dists[unique(dat.seq.mapping$Species), 3], 1),
  'Median sample dist (km)' =
    round(dat.dists[unique(dat.seq.mapping$Species), 2], 1),
  'Sample latitude range' = sapply(unique(dat.seq.mapping$Species), function(SP) {
    dat.seq.mapping$lat[dat.seq.mapping$Species == SP] %>% range(na.rm = T) %>% round(1) %>% paste(collapse = ', ')
    }),
  'Species latitude range' = sapply(unique(dat.seq.mapping$Species), function(SP) {
    range(xy.lat[[SP]]$y) %>% round(1) %>% paste(collapse = ', ')
    }),
  'Sample longitude range' = sapply(unique(dat.seq.mapping$Species), function(SP) {
    dat.seq.mapping$long[dat.seq.mapping$Species == SP] %>% range(na.rm = T) %>% round(1) %>% paste(collapse = ', ')
    }),
  'Species longitude range' = sapply(unique(dat.seq.mapping$Species), function(SP) {
    range(xy.lat[[SP]]$x) %>% round(1) %>% paste(collapse = ', ')
    }),
  row.names = unique(dat.seq.mapping$Species)
)
write.csv(table2, '../out/table2.csv')
