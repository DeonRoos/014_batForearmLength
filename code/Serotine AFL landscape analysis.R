###############################################################
# Effect of environment on serotine bat wing length
###############################################################

library(corrplot) # unknown package
library(psych)    # unknown package
library(nlme)     # for mixed effect models
library(mgcv)     # for splines
library(ncf)      # Ottar's package, not sure why this is included


bat <- read.csv("C:\\014_batForearmLength\\data\\bat_afl.csv", header = TRUE)
bat$genetics <- factor(bat$genetics)

# there is one duplicated bat but it is found at different sites so label A and B
bat[duplicated(bat$ID),] 
bat$ID <- as.character(bat$ID)
bat$ID[(bat$ID == "X0655" & bat$Site== "Hurstpeirpoint")] <- "X0655a"
bat$ID[(bat$ID == "X0655" & bat$Site== "Hollingbury")] <- "X0655b"
bat$ID <- factor(bat$ID)

#Summary data
table(bat$Site, exclude=NULL)
table(bat$Year, exclude=NULL)  
table(bat$Site, bat$Year, exclude=NULL)  
table(bat$genetics, exclude=NULL)

#Plot data
plot(bat$AFL)
hist(bat$AFL)
plot(bat$Site, bat$AFL, las=2)
hist(bat$roost_size)
plot(bat$Site, bat$roost_size, las=2)

par(mfrow=c(4,1))
hist(bat$july.mean); hist(bat$july.range);hist(bat$july.rain); hist(bat$july.max)
par(mfrow=c(3,1))
hist(bat$short.grass.area); hist(bat$short.grass.disp); hist(bat$short.grass.stddist) 
hist(bat$woodland.area); hist(bat$woodland.disp); hist(bat$woodland.stddist) 
hist(bat$urban.area); hist(bat$urban.disp);hist(bat$urban.stddist) 
hist(bat$other.grass.area); hist(bat$other.grass.disp); hist(bat$other.grass.stddist)
hist(bat$arable.area); hist(bat$arable.disp); hist(bat$arable.stddist)
hist(bat$other.area); hist(bat$other.disp); hist(bat$other.stddist)

#Spearman rank pairwise correlation 
continous.variables <- bat[,c("Year", "AFL","lon","lat","x","y","alt", "july.mean","july.range","july.rain","july.max", 
                         "short.grass.area", "short.grass.stddist", "short.grass.disp", "woodland.area", "woodland.stddist", "woodland.disp",
                         "urban.area", "urban.stddist", "urban.disp", "other.grass.area", "other.grass.stddist", "other.grass.disp",
                         "arable.area", "arable.stddist", "arable.disp", "other.area", "other.stddist","other.disp", "roost_size")]
cor1 <- cor(continous.variables, use="pairwise", method="spearman")
par(mfrow=c(1, 1))
corrplot(cor1, method = "ellipse", title = "Correlation matrix ellipse plot for continuous variables",diag = TRUE, outline = TRUE, tl.col= "black" )
corrplot(cor1, method = "number", title = "Correlation matrix",diag = TRUE, outline = TRUE, tl.col= "black" )
corrplot(cor1, method = "pie", title = "Correlation matrix",diag = TRUE, outline = TRUE, tl.col= "black" )
corrplot(cor1, method = "shade", title = "Correlation matrix, Spearman",diag = TRUE, outline = TRUE, tl.col= "black" )


corrplot(cor1, method = "color",diag = TRUE, outline = TRUE, tl.col= "black")

#Model
aov_model <- aov(AFL ~ Site, data=bat); summary(aov_model); plot(aov_model)
aov_year_model <- aov(AFL ~ Year, data=bat); summary(aov_year_model); plot(aov_year_model)

bat$Site <- factor(bat$Site)
gamm_model <- gam(AFL ~ s(july.mean) + s(Site, bs = "re"), 
                  family = "gaussian", 
                  data = bat,
                  method = "REML")


Correlog <- spline.correlog(x=bat$lon, y=bat$lat, z=bat$july.mean, max=1000)
plot.spline.correlog(Correlog)
plot(Correlog)
