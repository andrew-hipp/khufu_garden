library(openxlsx)
dat.hyb <- read.xlsx('data/HybSeq_TableS6-r1.xlsx', rowNames = T)
dat.hyb$lat <- as.numeric(dat.hyb$lat)
dat.hyb$lon <- as.numeric(dat.hyb$lon)
touse <- union(
    grep('macrocarpa', dat.hyb$Species.Determination),
    grep('macrocarpa', dat.hyb$Parent.Species)
    )

dat.hyb <- dat.hyb[touse, ]

gardenC <- list(
    MN = c(lat=45.38953, lon=-93.19485),
    IL = c(lat=41.81347, lon=-88.06293),
    OK = c(lat=34.9822, lon=-97.5211)
)

gardenR <- 2 # radius for detecting

dat.khufu <- read.csv('out/vcf.stats.raw.table.csv', row.names = 1)
nums <- sapply(names(gardenC), function(x) {
    sum(!is.na(dat.khufu$MOTHER) & dat.khufu$stateAbb == x)
})

dat.state <- lapply(names(gardenC), function(i) {
    temp <- dat.hyb
    temp <- dat.hyb[
        which((temp$lat < (gardenC[[i]]['lat'] + gardenR)) &
        (temp$lat > (gardenC[[i]]['lat'] - gardenR)) &
        (temp$lon < (gardenC[[i]]['lon'] + gardenR)) &
        (temp$lon > (gardenC[[i]]['lon'] - gardenR))),
    ]
    return(temp)
})
names(dat.state) <- names(gardenC)

pfails = function(p, n) {(1-p)^n} # probability of ??

F1s <- lapply(names(dat.state), function(x) {
  prob <- sum(dat.state[[x]]$F1) / dim(dat.state[[x]])[1]
  paste(x, ': ', 
        round(100*prob, 1), 
        '% of ', dim(dat.state[[x]])[1], 
        ' (prob of no F1s in ', nums[x], ' wild acorns = ', pbinom(0, nums[x], prob), ')\n',
        sep = '')
}) |> unlist()

message(F1s)

