source("helper.R")


# Mussweiler & Strack, 1999a p-value
1 - pf(q = 4.86, df1 = 1, df2 = 57)


# demographics
#----------------------
# (the Demographics results data file is not made publicly available for anonymity reasons)
demo <- read.table("Demographics results EXP2.txt", sep = "\t", head = T, as.is = T)
length(unique(demo$id))
mean(demo$student == "student")
median(demo$age, na.rm = T)
IQR(demo$age, na.rm = T)
mean(demo$sex == "female")



d1 <- read.table("Anchoring 1 results EXP2.txt", sep = "\t", head = T, as.is = T)
d2 <- read.table("Anchoring 2 results EXP2.txt", sep = "\t", head = T, as.is = T)
d <- cbind(d1, comparison = d2$comparison, absolute = d2$absolute, stringsAsFactors = F)
d$absolute <- ifelse(d$absolute > 10000 & d$item == "price", d$absolute / 10000, d$absolute)
d$absolute <- ifelse(d$absolute > 1000000 & d$item == "population", d$absolute / 1000000, d$absolute)
d$higher <- d$comparison == "higher"
d$anchor.h <- as.numeric(d$anchor == "high")
d$anchor.h.s <- d$anchor.h - 0.5
d$absolute.m <- 0
for(i in unique(d$item)) {
  d$absolute.m[d$item == i] <- mccall(d$absolute[d$item == i])
}


# comparison
#---------------------------------------------------
mean(d$answer[d$anchor == "high"] == "lower")
mean(d$answer[d$anchor == "low"] == "higher")


# true attribute values of the provided examples
#---------------------------------------------------
d$example.true.value <- NA
# first names
ch <- function(name) { # ch is a single letter in czech
  if(length(grep(",", name)) > 0) {return(NA)}
  gsub(pattern = "Ch|ch|CH", replacement = "x", name)  
}

# planets
days.examples <- unique(d$example[d$item == "days"])
days.examples <- days.examples[order(days.examples)]
days.values <- c(5,10,10,10,5,5,5,5,4,4,1,1,10,10,8,8,9,9,9,9,9,6,6,6,7,7,2,2,2,2,3,3,3,3,3,3) # 10 for NA
# from https://solarsystem.nasa.gov/solar-system/our-solar-system/overview/
days.true <- c(0.2408467,0.61519726,1.0000174,1.8808476,11.862615,29.447498,84.016846,164.79132,248,NA) #in years

# months
temperature.examples <- unique(d$example[d$item == "temperature"])
temperature.examples <- temperature.examples[order(temperature.examples)]
temperature.values <- c(13,13,8,8,3,3,6,7,7,6,6,7,7,7,7,12,4,4,4,1,13,6,5,5,5,5,1,1,11,11,3,12,12,10,10,10,8,8,2,2,2,9,9,13) # 13 for NA
# source: https://www.chmi.cz/historicka-data/pocasi/mesicni-data/mesicni-data-dle-z.-123-1998-Sb# , Klementinum, years 2011-2019
temperature.months.true <- c(2.344444444,	2.733333333,	7.066666667,	12.23333333,	16.38888889,	20.55555556,	22.2,	21.76666667,	16.81111111,	
                             11.64444444,	7.155555556,	4.477777778, NA) 

# cities
distance.examples <- unique(d$example[d$item == "distance"])
distance.examples <- distance.examples[order(distance.examples)]
# values from www.distance.to
distance.values <- c(NA,NA,NA,NA,710,710,710,1535,1354,rep(278,5),NA,621,291,291,717,717,717,444,444,444,NA,1463,1302,1302,1302,NA,
                     720,2243,2243,1035,1035,1035,449,1772,1772,993,1668,1668,NA,1117,882,882,882,994,rep(923,4),NA,1067,rep(1054,4),
                     487,517,517,rep(253,7),517,489,489)

# countries
population.examples <- unique(d$example[d$item == "population"])
population.examples <- population.examples[order(population.examples)]
# in thousands from https://en.wikipedia.org/wiki/List_of_countries_by_population_(United_Nations), 1.7.2019 data
population.values <- c(NA,NA,NA,11539,11539,7000,rep(10689,11),NA,NA,rep(5772,3),1326,5532,rep(65130,3),17097,4130,rep(60550,3),NA,38,2760,1907,616,
                       9685,440,rep(83517,5),NA,17097,5379,37888,37888,10266,8955,8955,19365,10473,rep(5457,4),2079,67530,rep(83517,3),46737,46737,10036,67530)

# municipalities
unemployment.examples <- unique(d$example[d$item == "unemployment"])
unemployment.examples <- unemployment.examples[order(unemployment.examples)]
# from https://www.czso.cz/csu/czso/podil-nezamestnanych-osob-v-cr-a-krajich-2019
unemployment.municipality <- data.frame(id = 1:14, 
                                        name = c("Hl. m. Praha", "Středočeský","Jihočeský","Plzeňský", "Karlovarský", "Ústecký",
                                                 "Liberecký","Královéhradecký","Pardubický", "Vysočina", "Jihomoravský",
                                                 "Olomoucký", "Zlínský", "Moravskoslezský"), 
                                        unemployment = c(1.90, 2.44,2.31, 2.33,2.74, 3.90,2.95, 2.38,2.20, 2.70,3.48, 2.94,2.43, 4.44)
                                        )
unemployment.values <- c(NA,NA,11,rep(1,4),NA,rep(3,5),rep(11,3),rep(5,3),10,rep(8,7),NA,rep(7,3),rep(14,6),rep(12,4),rep(14,2),rep(9,4),
                         rep(4,5),rep(1,3),NA,NA,rep(2,8),rep(6,9),10,10,rep(13,3))

# politicians
age.examples <- unique(d$example[d$item == "age"])
age.examples <- age.examples[order(age.examples)]
# dates of birth taken from : https://www.psp.cz/sqw/hp.sqw?k=192 (age was computed from age at 1.1.2020)
age.values <- c(NA,NA,65,NA,NA,NA,NA,65,65,NA,55,rep(65,5),NA,rep(39,3),48,43,51,71,58,53,53,NA,41,23,59,NA,50,23,23,NA,rep(23,5),NA,40,
                32,23,23,NA,NA,rep(23,3),55,55,NA,64,54,41,57,68,38,NA,38,55,39,39,NA,65,59,NA,NA,39,44,55,64,46,59,59,NA,59,43,50,50,50,
                44,24,59,72,56,45,46,39,42,38,51,43,rep(35,6),44,48,59,30,55,69,59,59,63,67,NA,55,NA,67,67,rep(NA,10),65,43,47,47,NA,
                50,39,67,35,35,35,23,55,rep(NA,4),44,NA,51,41,41,54,34,42,55,NA,42,48,NA,52,55,82,47,47,63,63,50,50,50,43,54,NA,55,
                43,41,41,33,63,46,NA,63,63,63,55,40,46,NA)

# cars
price.examples <- unique(d$example[d$item == "price"])
price.examples <- price.examples[order(price.examples)]
# cheapest car offered by the car manufacturer in Czechia at 21.7.2021, for a brand even when a model is listed
# some values found at https://www.carismo.cz/katalog/ other at brand websites
price.values <- c(NA,NA,NA,564,564,564,603,603,603,235,235,325,240,330,4850,4850,251,251,rep(359,4),
                  500,500,rep(240,4),1050,1050,270,NA,NA,5105,799,1766,396,rep(645,4),307,330,320,320,
                  rep(255,4),1577,1577,2877,rep(325,3),7219,395,330,330,NA,620,330,306,rep(330,12),NA,
                  1200,225,303,1191,1191,303,303)

# mammals
weight.examples <- unique(d$example[d$item == "weight"])
weight.examples <- weight.examples[order(weight.examples)]
# -3 1-10g, -2 10-100g, -1 100g-1kg, 0 1-10 kg, 1 10-100kg, 2 100-1000kg, 3 1t-10t
# values from https://www.zoopraha.cz/zvirata-a-expozice/lexikon-zvirat , unlisted from wikipedia
weight.values <- c(NA,NA,2,2,-3,1,0,-1,1,1,2,2,0,1,2,2,NA,-2,3,3,0,1,-2,1,2,2,0,1,1,0,0,0,0,0,
                   1,0,2,2,2,2,2,2,2,2,1,2,2,0,0,2,2,1,1,NA,0,0,0,0,0,1,rep(2,6),NA,0,1,
                   rep(-2,5),-2.5,NA,0,3,3,0,NA,NA,NA,NA,1,1,2,1,2,1,1,0.5,0.5,1,-2,NA,NA,
                   0,1,1,NA,rep(3,4),-1,-1,-1,1,1,2,rep(1,4),0,0,0,1.5,1,1,NA,NA,0,2,4,4,
                   -1,1,1,NA,0,0,2,2,2,NA,2,2,2)

# occupations
salary.examples <- unique(d$example[d$item == "salary"])
salary.examples <- salary.examples[order(salary.examples)]
# taken as an average for Prague from https://www.nsp.cz/vyuziti-pro-zamestnavatele-a-personalisty/nastaveni-mezd
# approximately in tens of thousands CZK
salary.values <- c(NA,3,5,3,3,5,3,4,3,2,3,3,2,2,5,1,1,NA,4,3,2,2,5,5,2,2,5,6,6,2,5,5,4,4,
                   4,3,4,4,3,4,3,3,3,4,4,5,4,2,2,3,3,NA,4,2,2,2,1,4,6,6,6,6,4,1,2,6,6,6,
                   6,4,3,2,3,2,1,3,3,3,3,3,6,2,3,7,7,NA,3,2,2,2,4,4,4,NA,2,2,2,NA,NA,2,2,
                   4,5,5,5,rep(2,9),3,7,5,5,5,5,4,2,4,4,2,2,4,NA,4,5,3,3,3,4,4,4,
                   3,3,3,2,2,3,4,4,NA,5,2,2,4,4,4,4,3,3,2,4,2,3,2,rep(3,12),rep(1,8),
                   3,3,4,3,3,2,2,4,4,4,2,2,5,5)




for(i in 1:nrow(d)) {
  if(d$item[i] == "length") { # length of names in letters
    d$example.true.value[i] <- nchar(ch(d$example[i]))
  } else if(d$item[i] == "days") { # order of the planet
    d$example.true.value[i] <- days.true[days.values[which(days.examples == d$example[i])]]
  } else if(d$item[i] == "temperature") { # average temperature in °C
    d$example.true.value[i] <- temperature.months.true[temperature.values[which(temperature.examples == d$example[i])]]
  } else if(d$item[i] == "distance") { # distance of European capital cities from Prague in km
    d$example.true.value[i] <- distance.values[which(distance.examples == d$example[i])]
  } else if(d$item[i] == "population") { # population of European countries in thousands
    d$example.true.value[i] <- population.values[which(population.examples == d$example[i])]
  } else if(d$item[i] == "unemployment") { # average temperature in °C
    d$example.true.value[i] <- as.numeric(unemployment.municipality$unemployment[unemployment.values[which(unemployment.examples == d$example[i])]])
  } else if(d$item[i] == "age") { # age of politicians in years
    d$example.true.value[i] <- age.values[which(age.examples == d$example[i])]
  } else if(d$item[i] == "price") { # price of cars in thousands CZK
    d$example.true.value[i] <- price.values[which(price.examples == d$example[i])]
  } else if(d$item[i] == "weight") { # weight of mammals in exponents of 10 kg
    d$example.true.value[i] <- weight.values[which(weight.examples == d$example[i])]
  } else if(d$item[i] == "salary") { # salary of various occupations in 10000 CZK
    d$example.true.value[i] <- salary.values[which(salary.examples == d$example[i])]
  } else {
    d$example.true.value[i] <- NA
  }
}


d$example.true.value.s <- NA
d$example.true.value.m <- NA
for(i in unique(d$item)) {
  d$example.true.value.s[d$item == i] <- scale(d$example.true.value[d$item == i])
  d$example.true.value.m[d$item == i] <- mccall(d$example.true.value[d$item == i])
}




# EXAMPLES
###################
# H1
#----
m1 <- glmer(higher ~ anchor.h + (anchor.h|item) + (1|id), data = d, family = "binomial")
summary(m1)
results(m1, 2)

mean(d$higher[d$anchor == "high"])
mean(d$higher[d$anchor == "low"])

# true values z-scores
m1s <- lmer(example.true.value.s ~ anchor.h.s + (0 + anchor.h.s|item) + (1|id), data = d)
summary(m1s)
results(m1s, 2, family = "linear", rounding = 3)

# true values mccall
m1m <- lmer(example.true.value.m ~ anchor.h.s + (0 + anchor.h.s|item) + (1|id), data = d)
summary(m1m)
results(m1m, 2, family = "linear", rounding = 3)

# true values z-scores without three categories
m1s2 <- lmer(example.true.value.s ~ anchor.h.s + (0 + anchor.h.s|item)+ (1|id), data = d, subset = d$item != "salary" & d$item != "weight" & d$item != "price")
summary(m1s2)
results(m1s2, 2, family = "linear", rounding = 3)

# dealing with the singular fit
m1s3 <- lmer(example.true.value.s ~ anchor.h.s + (0 + anchor.h.s|item), data = d, subset = d$item != "salary" & d$item != "weight" & d$item != "price")
summary(m1s3)
results(m1s3, 2, family = "linear", rounding = 3)

# classification based on true values and anchors
m1b <- glmer(higher ~ I(anchor.h-0.5)*example.true.value.s + (anchor.h|item) + (1|id), data = d, family = "binomial")
summary(m1b)
results(m1b, 2)
results(m1b, 3)
results(m1b, 4)


# ESTIMATES
###################
# H2
#----
m2 <- lmer(absolute.m ~ anchor.h + (anchor.h|item) + (1|id), data = d)
summary(m2)
results(m2, 2, family = "linear", rounding = 3)

# H3
#----
m3 <- lmer(absolute.m ~ anchor.h + higher + (anchor.h + higher|item) + (1|id), data = d)
summary(m3)
results(m3, 3, family = "linear", rounding = 3)

m3b <- lmer(absolute.m ~ anchor.h + example.true.value.s + (anchor.h + example.true.value.s|item) + (1|id), data = d)
summary(m3b)
results(m3b, 2, family = "linear", rounding = 3)
results(m3b, 3, family = "linear", rounding = 3)



# FIGURES
###################

old.names <- c("length" , "price" , "age" , "unemployment" , "population" , "days" , "weight" , "salary" , "temperature" , "distance")
new.names <- c("First names","Car brands","Czech MPs","Czech regions","EU countries","Planets","Mammal species","Occupations","Months","European capitals")


# examples
#-------------
df3p <- data.frame(item = unique(d$item), est = 0, low = 0, high = 0, stringsAsFactors = F)
df3 <- data.frame(item = unique(d$item), est = 0, low = 0, high = 0, stringsAsFactors = F)
df3s <- data.frame(item = unique(d$item), est = 0, low = 0, high = 0, stringsAsFactors = F)
for(i in 1:10) {
  temp <- d[d$item == unique(d$item)[i],]
  
  ct <- cor.test(temp$anchor.h, as.numeric(temp$higher))
  df3p[i,"est"] <- ct$estimate
  df3p[i,"low"] <- ct$conf.int[1]
  df3p[i,"high"] <- ct$conf.int[2]
  
  x <- d$example.true.value[d$item == unique(d$item)[i] & d$anchor == "high"]
  y <- d$example.true.value[d$item == unique(d$item)[i] & d$anchor == "low"]
  if(all(is.na(x))) {
    df3[i,"est"] <- NA
    df3[i,"low"] <- NA
    df3[i,"high"] <- NA
    df3s[i,"est"] <- NA
    df3s[i,"low"] <- NA
    df3s[i,"high"] <- NA
    next
  }
  ct <- cor.test(c(x,y),c(rep(1,length(x)), rep(0,length(y))))
  df3[i,"est"] <- ct$estimate
  df3[i,"low"] <- ct$conf.int[1]
  df3[i,"high"] <- ct$conf.int[2]
  sci <- spearman.ci(c(x,y),c(rep(1,length(x)), rep(0,length(y))), nrep = 1000)
  df3s[i,"est"] <- sci$estimate
  df3s[i,"low"] <- sci$conf.int[1]
  df3s[i,"high"] <- sci$conf.int[2]
}
df3 <- df3[order(df3p$est),]
df3s <- df3s[order(df3p$est),]
df3p <- df3p[order(df3p$est),]

png("Fig1.png", width = 1600, height = 800, pointsize = 24)
#tiff(filename = "Fig1.tiff", width = 1400, height = 1000, pointsize = 24)
par(las = 1, mar = c(0,8,2,0.2), cex = 1.3)
plot(0,0, axes = F, ann = F, type = "n", xlim = c(-0.4, 0.4), ylim = c(0,10.5))
axis(side = 3, at = (-4:4)*0.1, labels = (-4:4)*0.1, pos = 10.5, lwd = 2)
lines(c(0, 0), c(0, 10.5), lty = 2, lwd = 2)
for(x in c(-4:-1, 1:4)*0.1) {
  lines(c(x, x), c(0, 10.5), lty = 3, lwd = 1, col = "grey")  
}
for(i in 1:10) {
  points(df3$est[i], i-0.25-0.15, pch = 16, cex = 1.5, col = "red")
  lines(x = c(df3$low[i], df3$high[i]), y = c(i-0.25, i-0.25)-0.15, lwd = 3, lend = 2, col = "red")
  
  points(df3p$est[i], i-0.25+0.15, pch = 16, cex = 1.5)
  lines(x = c(df3p$low[i], df3p$high[i]), y = c(i-0.25, i-0.25)+0.15, lwd = 3, lend = 2)
  
  # points(df3s$est[i], i-0.25, pch = 16, cex = 1.5, col = "blue")
  # lines(x = c(df3s$low[i], df3s$high[i]), y = c(i-0.25, i-0.25), lwd = 3, lend = 2, col = "blue")
  
  j <- which(old.names == df3$item[i])
  mtext(text = new.names[j], side = 2, line = 0, at = i-0.25, cex = 1.5, padj = 0.5)
}

rect(xleft = 0.19, ybottom = 8, xright = 0.41, ytop = 10, col = "white", border = "white")
lines(c(0.19,0.25), c(9.35,9.35), col = "black", lwd = 3, lend = 2)
lines(c(0.19,0.25), c(8.65,8.65), col = "red", lwd = 3, lend = 2)
text(x = 0.26, y = 9.35, labels = "Classification", adj = c(0,0.5), cex = 1.2)
text(x = 0.26, y = 8.65, labels = "True values", adj = c(0,0.5), cex = 1.2)
points(0.22, 9.35, pch = 16, cex = 1.5, col = "black")
points(0.22, 8.65, pch = 16, cex = 1.5, col = "red")

dev.off()



# anchoring
#-------------
df1 <- data.frame(item = unique(d$item), est = 0, low = 0, high = 0, stringsAsFactors = F)
df1e <- data.frame(item = unique(d$item), est = 0, low = 0, high = 0, stringsAsFactors = F)
for(i in 1:10) {
  m.temp <- lm(absolute.m ~ anchor.h + example.true.value.s, data = d, subset = d$item == unique(d$item)[i])
  df1[i,"est"] <- m.temp$coefficients[2]
  CI <- confint(m.temp)[2,]    
  df1[i,"low"] <- CI[1]
  df1[i,"high"] <- CI[2]
  df1e[i,"est"] <- m.temp$coefficients[3]
  CI <- confint(m.temp)[3,]    
  df1e[i,"low"] <- CI[1]
  df1e[i,"high"] <- CI[2]
}
df1e <- df1e[order(df1$est),]
df1 <- df1[order(df1$est),]


png("Fig2.png", width = 1600, height = 800, pointsize = 24)
#tiff(filename = "Fig2.tiff", width = 1400, height = 1000, pointsize = 24)

par(las = 1, mar = c(0,8,2,0.2), cex = 1.3)
plot(0,0, axes = F, ann = F, type = "n", xlim = c(-0.5, 1.5), ylim = c(0,10.5))
axis(side = 3, at = (-2:6)*0.25, labels = (-2:6)*0.25, pos = 10.5, lwd = 2)
lines(c(0, 0), c(0, 10.5), lty = 2, lwd = 2)
for(x in c(-2:-1, 1:6)*0.25) {
  lines(c(x, x), c(0, 10.5), lty = 3, lwd = 1, col = "grey")  
}
for(i in 1:10) {
  points(df1$est[i], i-0.25 + 0.15, pch = 16, cex = 1.5)
  lines(x = c(df1$low[i], df1$high[i]), y = c(i-0.25, i-0.25) + 0.15, lwd = 3, lend = 2)

  points(df1e$est[i], i-0.25 - 0.15, pch = 16, cex = 1.5, col = "red")
  lines(x = c(df1e$low[i], df1e$high[i]), y = c(i-0.25, i-0.25) - 0.15, lwd = 3, lend = 2, col = "red")
    
  j <- which(old.names == df1$item[i])
  mtext(text = new.names[j], side = 2, line = 0, at = i-0.25, cex = 1.5, padj = 0.5)
}

rect(xleft = 0.99, ybottom = 1, xright = 1.51, ytop = 3, col = "white", border = "white")
lines(c(0.84,1.02), c(2.35,2.35), col = "black", lwd = 3, lend = 2)
lines(c(0.84,1.02), c(1.65,1.65), col = "red", lwd = 3, lend = 2)
text(x = 1.05, y = 2.35, labels = "Anchor valence", adj = c(0,0.5), cex = 1.2)
text(x = 1.05, y = 1.65, labels = "Example true value", adj = c(0,0.5), cex = 1.2)
points(0.93, 2.35, pch = 16, cex = 1.5, col = "black")
points(0.93, 1.65, pch = 16, cex = 1.5, col = "red")

dev.off()






