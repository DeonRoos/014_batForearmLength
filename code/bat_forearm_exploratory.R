# Location of forearm length
# P:\EXSE0430 - Mammalian vectors of rabies\Data\Other bats


df <- read.csv("C:\\013_batCMR\\data\\bat_afl.csv", header = TRUE)

ggplot(df) +
  geom_point(aes(x = lon, y = lat))
library(mgcv)
df$Site <- factor(df$Site)
plot(gam(AFL ~ s(x, y) + s(july.mean) + s(july.rain) + s(Site, bs = "re"),
         data = df, 
         method = "REML",
         select = TRUE),
     scheme = 2, ask = FALSE)

ggplot(df) +
  geom_jitter(aes(x = lon, y = lat, size = AFL)) +
  theme_classic()
