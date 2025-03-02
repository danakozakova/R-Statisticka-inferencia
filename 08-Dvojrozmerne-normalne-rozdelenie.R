# Príklad 8.1: Hustota dvojrozmerného normálneho modelu 
## Naprogramujte funkciu dnorm2(), ktorej vstupy budú hodnotyx, y, mu1, mu2, sigma1, sigma2 a ro
## a výstupom bude hodnota hustoty dvojrozmerného normálneho rozdelenia N2(..)
## Správnosť funkcie otestujte na výpočtoch f(x,y) v hodnotach x a y.
## Výsledky overte s výsledkami funkcie dmvnorm() z knižnice mvtnorm.

## a)
x <- y <- 1
mu1 <- mu2 <- 0
s1 <- s2 <- 1
rho <- 0.5

dnorm2<- function(x, y, mu1, mu2, s1, s2, rho) {
  U <- rbind(x - mu1, y - mu2) # vektor (x - mu1, x - mu2)^T
  rho <- rho*s1*s2
  S <- rbind(cbind(s1^2, rho),  # variancna matica Sigma
             cbind(rho, s2^2))
  Z <- exp(-0.5*t(U)%*%solve(S)%*%U)/(2*pi*sqrt(det(S))) # hustota dvojrozmerneho rozdelenia N2 pre hodnoty x, y
  return(Z)  
}
f1v <- dnorm2(x, y, mu1, mu2, s1, s2, rho)

X <- c(x,y)
Mean <- c(mu1, mu2)
Sig <- matrix(c(s1^2, rho*s1*s2, rho*s1*s2, s2^2), nrow = 2)
f1 <- mvtnorm::dmvnorm(x = X, mean = Mean, sigma = Sig)

## b)
x <- 2.5
y <- 1.5
mu1 <- 3
mu2 <- 2
s1 <- sqrt(9)
s2 <- sqrt(16)
rho <- 0.75

f2v <- dnorm2(x, y, mu1, mu2, s1, s2, rho)

X <- c(x,y)
Mean <- c(mu1, mu2)
Sig <- matrix(c(s1^2, rho*s1*s2, rho*s1*s2, s2^2), nrow = 2)
f2 <- mvtnorm::dmvnorm(x = X, mean = Mean, sigma = Sig)

tab <- c(f1v, f1, f2v, f2)
names(tab) <- c('own f(1,1)', 'built-in f(1,1)', 'own f(2.5,1.5)', 'built-in f(2.5,1.5)')
tab

# Príklad 8.2 (Základné číselné charakteristiky dvojice spojitých znakov)
## Načítajte dátový súbor bivariate_normal_distribution_trunk.txt. Nech náhodná premenná Xpopisuje 
## dĺžku dolnej končatiny (v mm) a náhodná premenná Y popisuje dĺžku trupu u žien. 
## Pomocou bodového diagramu vizualizujte vzťah premenných X a Y. Za predpokladu, že údaje pochádzajú 
## z dvojrozmerného normálneho rozdelenia N2 odhadnite hodnoty parametrov mu1, mu2, sigma1^2, sigma2^2, rho
## Výsledky dôkladne interpretujte.

getwd()
setwd('STUDY/MUNI/PREDMETY/St 18.00 Statisticka interferencia/cvicenia/cvicenie 8/')
data <- read.delim('bivariate_normal_distribution_trunk.txt') 
data_F <- data[data$sex == 'f', c('lowex_L', 'tru_L')] # vyfiltrovanie dat iba pre zeny
dim(data_F)

data_F <- na.omit(data_F) # vyhodenie riadkov s NA hodnotami
dim(data_F)

lowex_LF <- data_F[,'lowex_L'] # vyber vektora dlzok dolnej koncatiny zien
lowex_LF |> head()

tru_LF <- data_F[,'tru_L'] # vyber vektora dlzok trupu zien
tru_LF |> head()

n <- data_F |> nrow() # rozsah nahodneho vyberu
m1 <- lowex_LF |> mean() # vyberovy priemer dlzky dolnej koncatiny
m2 <- tru_LF |> mean() # vyberovy priemer dlzky trupu
s1 <- lowex_LF |> sd() # smerodajna odchylka dlzky dolnej koncatiny
s2 <- tru_LF |> sd() # smerodajna odchylka dlzky trupu
s12 <- cov(lowex_LF, tru_LF) # vyberova kovariancia
r12 <- cor(lowex_LF, tru_LF) # vyberovy koeficient korelacie

tab <- data.frame(rbind(c(n, m1, round(s1, 2), round(s12, 2), round(r12, 4)),
                        c(n, m2, round(s2, 2), round(s12, 2), round(r12, 4))),
           row.names = c('dĺžka dolnej končatiny',
                         'dĺžka trupu')) 
names(tab) <- c('n', 'mean', 'sd', 's12', 'r12')
tab  # suhrnna tabulka vysledkov

  # Na vzorke 100 žien: Dĺžka dolnej končatiny je 940.50 +- 45,47
  # Dĺžka trupu je 423.17 +- 34.02
  # Vzájomná kovariancia je 441.31, korelácia je priama s nízkym stupňom súvislosti 0,2853

### Graph
par(mar = c(4, 5, 1, 1))
plot(lowex_LF, tru_LF, pch = 21, col = 'red', bg = rgb(1, 0, 1, 0.2),
     xlab = '', ylab = '')
mtext('dĺžka dolnej končatiny žien (v mm)', side = 1, line = 3.1)
mtext('dĺžka trupu žien (v mm)', side = 2, line = 3.1)

# Príklad 8.3: Test dvojrozmernej normality
## Načítajte dátový súbor bivariate_normal_distribution_trunk.txt. Nech náhodná premenná X 
## popisuje dĺžku dolnej končatiny a náhodná premenná Y popisuje dĺžku trupu žien. 
## Na hladine významnosti alfa = 0.05 testujte hypotézu o dvojrozmernej normalite 
## vektora (X,T)^2. Na testovanie použite Mardiov test.

## Na hladine významnosti alfa = 0.05 testujeme:
## H0: Dáta pochádzajú z dvojrozmerného normálneho rozdelenia oproti
## H1: Dáta nepochádzajú z dvojrozmerného normálneho rozdelenia.
## Mardiov test – skladá sa z dvoch testov:
###  1. Test šikmosti
### H0: Dáta nie sú kladne ani záporne zošikmené oproti
### H1: Dáta sú kladne alebo záporne zošikmené.

### 2. Test špicatosti
### H0: Dáta nie sú kladne ani záporne zašpicatené oproti
### H1: Dáta sú kladne alebo záporne zašpicatené.

install.packages('MVN')
library('MVN')
MVN::mvn(data_F, mvnTest = 'mardia')$multivariateNormality
  # Výsledky:
  # Test          Statistic           p value Result
  # 1 Mardia Skewness   6.31326657225727 0.176942962210473    YES
  # 2 Mardia Kurtosis -0.207066071208097 0.835958259081491    YES
  # 3             MVN               <NA>              <NA>    YES
  # p-hodnoty pre sikmost aj strmost sú nad 0.05, preto plati H0
MVN::mvn(data_F, mvnTest = 'hz')$multivariateNormality
  # Výsledky:
  # Test        HZ  p value MVN
  # 1 Henze-Zirkler 0.5045323 0.587225 YES
  # p-hodnota nad 0.05, plati H0

MVN::mvn(data_F, mvnTest = 'royston')$multivariateNormality
  # Výsledky: 
  #      Test        H   p value MVN
  # 1 Royston 0.913373 0.6339012 YES
  # p-hodnota nad 0.05, plati H0

# Príklad 8.4: Vizualizácia dát z dvojrozmerného normálneho modelu 

## Načítajte dátový súbor bivariate_normal_distribution_trunk.txt. Nech náhodná premenná X
## popisuje dĺžku dolnej končatiny a náhodná premenná Y popisuje dĺžku trupu žien. 
## Na základe riešení príkladov 8.2 a 8.3 predpokladáme, že dáta pochádzajú z dvojrozmerného
## normálneho rozdelenia N2 s odhadom stredných hodnôt mu1 = 940.59, mu2 = 423.17,
## s odhadom rozptylov s1^2 = 45.47^2 a s2^2 = 34.02^2 a s odhadom korelacneho koeficientu
## rho = 0.2853.

## a) Vytvorte bodový diagram pre dĺžku dolnej končatiny a dĺžku trupu. 
## Bodový diagram superponujte:
### i. kontúrami hustoty dvojrozmerného normálneho rozdelenia (funkcia dnorm2() + contour()),
### ii. jadrovým odhadom dvojrozmernej hustoty (funkcia kde2d() z knižnice MASS + contour()).

  # nacitanie a vypocet zakladnych premennych rovnako ako v 8.3
data <- read.delim('bivariate_normal_distribution_trunk.txt') 
data_F <- data[data$sex == 'f', c('lowex_L', 'tru_L')] # vyfiltrovanie dat iba pre zeny
data_F <- na.omit(data_F) # vyhodenie riadkov s NA hodnotami
lowex_LF <- data_F[,'lowex_L'] # vyber vektora dlzok dolnej koncatiny zien
tru_LF <- data_F[,'tru_L'] # vyber vektora dlzok trupu zien
m1 <- lowex_LF |> mean() # vyberovy priemer dlzky dolnej koncatiny
m2 <- tru_LF |> mean() # vyberovy priemer dlzky trupu
s1 <- lowex_LF |> sd() # smerodajna odchylka dlzky dolnej koncatiny
s2 <- tru_LF |> sd() # smerodajna odchylka dlzky trupu
r12 <- cor(lowex_LF, tru_LF) # vyberovy koeficient korelacie
  # koniec naplnenia ako v 8.3

### vygenerovat siet bodov x od 800 do 1100, y od 300 do 550, napriklad 50 krat 50 bodov
n <- 50
x <- seq(from = 800, to = 1100, length = n) # postupnost bodov osy x; od 800 do 1000 dlzky n
y <- seq(from = 300, to = 550, length = n) # postupnost bodov osy y; od 300 do 550 dlzky n
M <- matrix(NA, nrow = n, ncol = n) # prazdna matica - inicializacia pred for cyklom

### MATICA M = teopretické hodnoty hustoty f(x,y) rozdelenia N2(Mu, Sigma) pre siet bodov 800-1100 x 300-550
for (i in 1:n) {
  for (j in 1:n) {
    M[i,j] <- dnorm2(x[i], y[j], m1, m2, s1, s2, r12)
  }
} 

### alebo cez built-in funkciu:
Mean <- c(m1, m2)
Sig <- matrix(c(s1^2, r12*s1*s2, r12*s1*s2, s2^2), nrow = 2)

for (i in 1:n) {
  for (j in 1:n) {
    M[i,j] <- mvtnorm::dmvnorm(x = c(x[i], y[j]), mean = Mean, sigma = Sig)
  }
}

### GRAF HUSTOTY f(x,y) rozdelenia N2(Mu, Sigma) - z matice M
k <- 8 
par(mar = c(4, 4, 1, 1),
    mfrow = c(1,2))
plot(lowex_LF, tru_LF,
     xlim = c(800, 1100), ylim = c(300, 550),
     pch = 21, 
     col = 'red',bg = rgb(1, 0, 0, 0.2))
contour(x = x, y = y, z = M,
        levels = seq(from = 0, to = max(M), length =  k + 1),
        drawlabels = T, # contours are labeled 
        add = T,        # countour is added to the current plot
        col ='black')

### MATICA Z = jadrovy odhad dvojrozmernej hustoty
library('MASS')
Z <- MASS::kde2d(x = lowex_LF, # x-ová súradnica
                 y = tru_LF,   # y-ová súradnica 
                 lim = c(800, 1100, 300, 550), # hranice pre osi x a y
                 n = n) # počet bodov mriežky v oboch smeroch, môže byť číslo alebo 2-rozmerný vektor

### GRAF JADROVÉHO ODHADU hustoty f(x,y) - z matice Z
plot(lowex_LF, tru_LF,
     xlim = c(800, 1100),
     ylim = c(300, 550),
     pch = 21, 
     col = 'red',
     bg = rgb(1, 0, 0, 0.2))
contour(x = Z$x, y = Z$y, z = Z$z,
        levels = seq(from = 0, to = max(Z$z), length = k + 1),
        drawlabels = T, add = T, col ='black')

## b) Vrstevnicové diagramy:

### jemnejsia siet bodov
n <- 500
x <- seq(from = 800, to = 1100, length = n) # postupnost bodov osy x; od 800 do 1000 dlzky n
y <- seq(from = 300, to = 550, length = n) # postupnost bodov osy y; od 300 do 550 dlzky n
M <- matrix(NA, nrow = n, ncol = n) # prazdna matica - inicializacia pred for cyklom
for (i in 1:n) {
  for (j in 1:n) {
    M[i,j] <- dnorm2(x[i], y[j], m1, m2, s1, s2, r12)
  }
} 

k <- 12 # medzery medzi vrstevnicami
par(mar = c(4,4,1,1),
    mfrow = c(1,2))

### vrstevnicovy diagram hustoty f(x y) rozdelenia N2(Mu, Sig)
image(x, y, M, 
      breaks = seq(0, max(M), length = k + 1),
      xlim = c(800, 1100), ylim = c(300, 550), 
      asp = F, #pomer stran obrazka  sa neurci automaticky, ale bude sa riadit pomerom sirky a vysky definovanych cez parametre xlim a ylim
      col = terrain.colors(k), # nejaka paleta farieb
      las = 1, 
      xlab = 'dĺžka dolne končatiny (v mm)', ylab = 'dĺžka trupu (v mm)') 

### kontury hustoty f(x, y) rozdelenia N2(Mu, Sig)
contour(x, y, M, add = T, drawlabels = T, levels = seq(from = 0, to = max(M), length = k + 1)) 

### vrstevnicovy diagram jadroveho odhadu hustoty f(x, y)
image(Z$x, Z$y, Z$z,
      breaks = seq(0, max(Z$z), length = k + 1),
      xlim = c(800, 1100), ylim = c(300, 550), 
      asp = F, #pomer stran obrazka  sa neurci automaticky, ale bude sa riadit pomerom sirky a vysky definovanych cez parametre xlim a ylim
      col = terrain.colors(k), # nejaka paleta farieb
      las = 1, 
      xlab = 'dĺžka dolne končatiny (v mm)', ylab = 'dĺžka trupu (v mm)') 

### kontury jadroveho odhadu hustoty f(x, y)
contour(Z$x, Z$y, Z$z,
        add = T, drawlabels = T, levels = seq(from = 0, to = max(Z$z), length = k + 1))

### c) 3D-diagramy

# naspat menej bodov do siete
n <- 50
x <- seq(from = 800, to = 1100, length = n) # postupnost bodov osy x; od 800 do 1000 dlzky n
y <- seq(from = 300, to = 550, length = n) # postupnost bodov osy y; od 300 do 550 dlzky n
M <- matrix(NA, nrow = n, ncol = n) # prazdna matica - inicializacia pred for cyklom
for (i in 1:n) {
  for (j in 1:n) {
    M[i,j] <- dnorm2(x[i], y[j], m1, m2, s1, s2, r12)
  }
} 

# pocet riadkov matice M
nrm <- nrow(M) 
# pocet stlpcov matice M
ncm <- ncol(M) 

# stredy pre každú štvoricu susedných buniek v matici M
stredy <- (M[-1, -1] +     # vyluci posledny riadok a posledny stlpec
             M[-1, -ncm] + # vyluci posledny riadok a prvy stlpec
             M[-nrm, -1] + # vyluci prvy riadok a posledny stlpec
             M[-nrm, -ncm]) / 4 # vyluci prvy riadok a prvy stlpec

# rozdelenie stredov do k = 12 ekvidistantnych intervalov
# a kazdej hodnote priradi interval, do ktoreho nalezi
stredy.col <- cut(stredy, k) 

par(mar = c(1,1,1,1)) # okraje grafu 1, 1, 1, 1
color <- terrain.colors(k) # paleta k farieb

# 3D-diagram hudtoty f(x, y) rozdelenia N2(Mu,    Sig)
persp(x, y, M, col = color[stredy.col], # farbypovrchovych ploch
      phi = 30, theta = 20, # uhly definujuce smer pohladu, theta - azimutalny smer, phi - kolatura
      xlab ='dĺžka dolnej končatiny (v mm)', ylab = 'dĺžka trupu (v mm)', 
      zlab = 'f(x,y)') 

nrz <- nrow(Z$z) # pocet riadkov matice Z premennej z
ncz <- ncol(Z$z) # pocet stlpcov matice Z premennej z

color <- terrain.colors(k) # paleta k farieb
stredy <- (Z$z[-1, -1] + Z$z[-1, -ncz] + Z$z[-nrz, -1] + Z$z[-nrz, -ncz]) / 4 # matica stredu siete
stredy.col <- cut(stredy, k) # rozdel rozpatia hodnot do 12 ekvidistantnych intervalov  a kazdej hodnote priradi interval, do ktoreho nalezi
par(mar = c(1,1,1,1)) # okraje grafu 1, 1, 1, 1

# 3D-diagram jadroveho odhadu hustoty f(x, y)
persp(Z$x, Z$y, Z$z,
      col = color[stredy.col], # farbypovrchovych ploch
      phi = 30, theta = 20, # uhly definujuce smer pohladu, theta - azimutalny smer, phi - kolatura
      xlab ='dĺžka dolnej končatiny (v mm)', ylab = 'dĺžka trupu (v mm)', 
      zlab = 'f(x,y)')

# Príklad 8.5: Simulácia dát z dvojrozmerného normálneho rozdelenia 
## Nasimulujte dáta (X, Y)^T z dvojrozmerného normálneho rozdelenia s parametrami mu1 = 940.50, mu2 = 423.17, s1 = 42.472, s2 = 34.022 a rho = 0.2853, n = 100.
## Simuláciu pseudonáhodných čísel z N2(Mu, Sig) vykonajte použitím funkcie rmvnorm() z knižnice mvtnorm. Pre nasimulované dáta vykreslite
## a) histogram náhodnej veličiny X a histogram náhodnej veličiny Y, pričom každý histogram superponujte krivkou marginálneho normálneho rozdelenia 
## N(m1, s1) a N(m2, s2) a krivkou jadrov0ho odhadu hustoty.

mu1 <- 940.50
mu2 <- 423.17
s1 <- 45.472
s2 <- 34.022
rho <- 0.2853
n <- 100

####### toto potom nakoniec
Mu <- c(mu1, mu2) # vektor vyberovych priemerov
Sig <- matrix(c(s1^2, rho*s1*s2, rho*s1*s2, s2^2), nrow = 2) # kovariancna matica
n <- 100 # rozsah nahodneho vyberu n
####### toto potom nakoniec

simnorm2 <- function(n, Mu, Sig, method = "rmvnorm", col1 = 'darkorange3', col2 = 'darkorange4', col3 = 'darkred', k = 12)
{
  ### vygeneruje 100 náhodnych dvojíc z N2 rozdelenia
  if (method == "rmvnorm") { 
  data <- mvtnorm::rmvnorm(n = n, mean = Mu, sigma = Sig) 
  }
  
  ### Vektor x z vygenerovanych dat + Hustota premennej X z rozdelenia N(mu1, sigma1^2) pre vygenerovane body
  x <- data[, 1] 
  xx <- seq(min(x) - 100, max(x) + 100, length = 512) # postupnost bodov na osi x pre krivku f(x) - od min-100 po max+100

  mu1 <- Mu[1]
  sigma1 <- sqrt(Sig[1, 1]) # vyber smerodajnej odchylky si z matice Sig
  xh <- dnorm(xx, mu1, sigma1) # hustota rozdelenia

  ### histogram nasimulovanych dat x - dĺžka dolnej končatiny
  par(mar = c(4,5,1,1), mfrow = c(1,3)) # okraje grafu 4, 5, 1, 1
  hist(x, prob = T, xlab = '', ylab = '', main = '',
       ylim = c(0, max(xh) + 0.005),
       col =  col1, border = col2, density = 50,
       breaks = k) 
  box(bty = 'o') # ram okolo grafu
  lines(density(x), col = col3, lty =1, lwd = 2) # krivka jadroveho odhadu hustoty f(x)
  lines(xx, xh, col = col3, lty =2, lwd = 2) # krivka hustoty N(mu1, sigma1^2)
  mtext('x - dĺžka dolnej končatiny', side = 1, line = 2.1) # popis x
  mtext('f(x)', side = 2, line = 2.4) # popis y
  legend("topright", lty = c(1, 2), lwd = 2, col = col3, legend = c('jadrový', 'hustota')) 

  ### Vektor y (dĺžka trupu) z vygenerovanych dat + Hustota premennej Y z rozdelenia N(mu2, sigma2^2) pre vygenerovane body 
  y <- data[, 2] 
  yy <- seq(min(y) - 100, max(y) + 100, length = 512) # postupnost bodov na osi x pre krivku f(y) - od min-100 po max+100

  mu2 <- Mu[2]
  sigma2 <- sqrt(Sig[2, 2]) # vyber smerodajnej odchylky si z matice Sig
  yh <- dnorm(yy, mu2, sigma2) # hustota rozdelenia

  ## histogram nasimulovaných dát y - dĺžka trupu
  hist(y, prob = T, xlab = '', ylab = '', main = '',
       ylim = c(0, max(yh) + 0.005),
       col = col1, border = col2, density = 50,
       breaks = k)
  box(bty = 'o')
  lines(density(y), col = col3, lty = 1, lwd = 2)
  lines(yy, yh, col = col3, lty = 2, lwd = 2)
  mtext('y - dĺžka trupu', side = 1, line = 2.1) # popis x
  mtext('f(y)', side = 2, line = 2.4) # popis y
  legend('topright', lty =c(1, 2), lwd = 2, col = col3, legend = c('jadrový', 'hustota'))

  ## bodovy graf s jadrovym odhadom
  Z <- MASS::kde2d(x, y, n = 50) # jadrovy odhad hustoty f(x, y)

  plot(x, y, 
       xlim = c(800, 1100), ylim = c(300, 550), 
       xlab = '', ylab = '',
       pch = 21,
       col = 'red',bg = rgb(1, 0, 0, 0.2))
  contour(Z$x, Z$y, Z$z, 
          add = T, 
          drawlabels = T,
          levels = seq(from = 0, to = max(Z$z), length = k + 1))
  mtext('x - dĺžka nohy', side = 1, line = 2.1) # popis x
  mtext('y - dĺžka trupu', side = 2, line = 2.4) # popis y
}

simnorm2(n, Mu, Sig, k = 6)

Tu mi to trochu nesedi. Otazkou je, kde sa ma uplatnovat to k = 6. Ja to mam vo funkcii tak, 
ze to ovplyvnuje aj pocet intervalov v histogramoch, ale podla vzoru to tak nie je. Pocet intervalov 
v histogramoch ma byt teda fixne 12?