# Install the necessary packages:
install.packages(c("lmtest","NlinTS","rEDM","gdverse"),dep = TRUE)

install.packages("spEDM",
                 repos = c("https://stscl.r-universe.dev",
                           "https://cloud.r-project.org"),
                 dep = TRUE)

# Which came first: the chicken or the egg?
# US chicken population and egg production
# -- An annual time series from 1930 to 1983 with 2 variables.
df = as.data.frame(lmtest::ChickEgg)
head(df)

cor.test(df$chicken,df$egg)

#-------------------------- Granger Causality Test -----------------------

## chickens granger-cause eggs?
lmtest::grangertest(egg ~ chicken, order = 3, data = df)
## eggs granger-cause chickens?
lmtest::grangertest(chicken ~ egg, order = 3, data = df)

#-------------------------- Transfer Entropy -----------------------------

df = as.data.frame(lmtest::ChickEgg)

# Method1: Continuous Transfer Entropy using the Kraskov estimation
## TE: chickens —> eggs
NlinTS::te_cont(df$egg, df$chicken, p = 3, q = 3, k = 6, normalize = F)
## TE: eggs -> chickens
NlinTS::te_cont(df$chicken, df$egg, p = 3, q = 3, k = 6, normalize = F)

# Method2: Pre-discretization
chicken_disc = sdsfun::discretize_vector(df$chicken,n = 5,method = 'natural')
egg_disc = sdsfun::discretize_vector(df$egg,n = 5,method = 'natural')

## TE: chickens —> eggs
NlinTS::te_disc(egg_disc, chicken_disc, p = 3, q = 3, normalize = TRUE)
## TE: eggs -> chickens
NlinTS::te_disc(chicken_disc, egg_disc, p = 3, q = 3, normalize = TRUE)

#-------------------------- Empirical Dynamic Modeling -------------------------

# temporal embeddings:
chickegg = as.data.frame(lmtest::ChickEgg)
m1 = stats::embed(chickegg$chicken,dimension = 3)
head(m1,5)
scatterplot3d::scatterplot3d(m1[,1:3], pch = 16, 
                             color="steelblue")

# spatial embeddings:
columbus = sf::read_sf(system.file("shapes/columbus.gpkg", package="spData"))
m2 = spEDM::embedded(columbus,target = "CRIME", E = 3, tau = 0)
head(m2,5)
scatterplot3d::scatterplot3d(m2[,1:3], pch = 16, 
                             color="red")

# simplex proojection
spEDM:::RcppSimplexForecast(m2,columbus$CRIME,1:49,1:49,4)

# s-mapping
spEDM:::RcppSMapForecast(m2,columbus$CRIME,1:49,1:49,4,theta = 0.1)

# convergent cross-mapping
chickegg = as.data.frame(lmtest::ChickEgg)
ccmres = rEDM::CCM(dataFrame = chickegg, E = 3, columns = "egg", target = "chicken",
                   libSizes = "5 50 5", random = FALSE, noTime = TRUE, showPlot = TRUE)
ccmres

# geographical detector
columbus = sf::read_sf(system.file("shapes/columbus.gpkg", package="spData"))
gdverse::opgd("HOVAL ~ CRIME", data = columbus, discnum = 3:15)
gdverse::opgd("CRIME ~ HOVAL", data = columbus, discnum = 3:15)

# geographical convergent cross mapping
columbus = sf::read_sf(system.file("shapes/columbus.gpkg", package="spData"))
spEDM::simplex(columbus,target = "HOVAL",lib = 1:49)
spEDM::simplex(columbus,target = "CRIME",lib = 1:49)
g = spEDM::gccm(columbus, "HOVAL", "CRIME",
                libsizes = seq(5,40,5), E = c(6,5))
g
