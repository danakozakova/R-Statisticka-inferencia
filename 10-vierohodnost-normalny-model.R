# Príklad 10.1 (Maximálne vierohodné odhady parametrov mua sigma2 v normálnom modeli)
## Načítajte dátový súbor normal_distribution_clavicle. Nech náhodná premenná X popisuje dĺžku kľúčnej kosti 
## z ľavej strany u mužov. Za predpokladu, že náhodná veličina X pochádza z normálneho rozdelenia N(mu, sigma2) 

#-------------------------------------------------------------------
## 2. Dosadením do vzorcov pre MLE odhad stanovte presnú hodnotu maximálne vierohodného odhadu parametrov mu a sigma2.
### nacitanie hodnot a odstranenie NA hodnot z vektora
#-------------------------------------------------------------------
data <- read.delim("STUDY/MUNI/PREDMETY/St 18.00 Statisticka interferencia/cvicenia/cvicenie 10/clavicle.txt", sep = "\t") # nacitanie dat
cla_L <- data[data$sex == 'm', 4] # vyber dlzok lavej klucnej kosti muzov
data <- na.omit(cla_L) # odstranenie NA hodnot

### MLE odhady mu a sigma2
n <- data |> length() # dlzka vektora cla.L
mu <- sum(data)/n # MLE odhad parametra mu
sigma_sq <- sum((data - mu)^2)/n # MLE odhad parametra sigma^2

### suhrnna tabulka vysledkov
tab <- cbind(mu, sigma_sq) |> data.frame()
names(tab) <- c('mu', 'sigma^2')
rownames(tab) <- 'MLE odhad'
tab

### suhrnna tabulka pre porovnanie
tab_comp <- cbind(mu, sigma_sq) |> data.frame()

#-------------------------------------------------------------------
## 3a. Pomocou maximalizácie logaritmu vierohodnostnej funkcie normálneho modelu nájdite maximálne vierohodný 
## odhad parametrov mu a sigma2. Maximalizáciu vykonajte pomocou funkcie optim():
#-------------------------------------------------------------------

### logaritmus vierohodnostnej funkcie rozdelenia N(mu, sigma^2)
  # hodnoty (mu, sigma^2) su ako jeden parameter = vektor
lnormOptim <- function(theta, x) { 
  n <- length(x) # dlzka vektora x
  # logaritmus vierohodnostnej funkcie
  like <- (-1/2) * log(2*pi) - (n/2)*log(theta[2]) - (1/(2*theta[2])) * sum((x - theta[1])^2)
  return(like) # vystup; premenna like
}

### MLE odhad vektora parametrov theta = (mu, sigma^2) ako maximum logaritmu vierohodnotnostnej funkcie
OPTtheta <- optim(c(145, 100), lnormOptim, x = cla_L, control = list(fnscale = -1)) 
max_mu <- OPTtheta$par[1] # MLE odhad parametra mu
max_sigma_2 <- OPTtheta$par[2] # MLE odhad parametra sigma^2


### tabulka vysledkov
tab <- cbind(max_mu, max_sigma_2) |> data.frame()
names(tab) <- c('mu', 'sigma^2')
rownames(tab) <- 'odhad ~ max log(L)'
tab

### suhrnna tabulka pre porovnanie
tab_comp <- rbind(cbind(mu, sigma_sq), cbind(max_mu, max_sigma_2))
colnames(tab_comp) = c('mu', 'sigma^2')
rownames(tab_comp) = c('MLE odhad', 'odhad ~ max log(L)')

#-------------------------------------------------------------------
## 3b. Pomocou maximalizácie logaritmu vierohodnostnej funkcie normálneho modelu nájdite maximálne vierohodný 
## odhad parametrov mu a sigma2. Maximalizáciu vykonajte pomocou 
## vlastnoručne naprogramovanej Newton-Raphsonovej metódy NRnorm():
#-------------------------------------------------------------------

#-------------------------------------------------------------------
### logaritmus vierohodnostnej funkcie rozdelenia N(mu, sigma^2)
# na rozdiel od lnormOptim, hodnoty mu a sigma^2 su ako samostatne parametre
#-------------------------------------------------------------------
lnorm <- function(mu, sigma_2, x) { 
  n <- x |> length()  # dlzka vektora x
  # logaritmus vierohodnostnej funkcie
  like <- (-n/2) * log(2*pi) - (n/2)*log(sigma_2) - (1/(2*sigma_2)) * sum((x - mu)^2)
  return(like) # vystup; premenna like
}

#-------------------------------------------------------------------
### skore funkcie rozdelenia - vrati vektor dvoch hodnot skore funkcie (derivacia podla mu a derivacia podla sigma^2)
#-------------------------------------------------------------------
Snorm <- function(mu, sigma_2, x) { # vektor skore funkcii U
  n <- x |> length() # dlzka vektora x
  skore_mu <- (1/sigma_2) * sum(x - mu) # skore funkcia pre parameter mu
  skore_sigma_2 <- (-n/(2*sigma_2)) + (1/(2*sigma_2^2)) * sum((x - mu)^2) # skore funkcia pre parameter sigma^2
  U <- c(skore_mu, skore_sigma_2) # vektor U = (S(mu), S(sigma^2))
  return(U) # vystup; vektor U
}

#-------------------------------------------------------------------
### Fisherova informacna matica I - vrati maticu 2 x 2 hodnot Fisherovej matice pre zadane vstupy (druhe derivacie)
#-------------------------------------------------------------------
Inorm <- function(mu, sigma_2, x) { 
  n <- x |> length()  # dlzka vektora x
  I11 <- n/sigma_2    # I_{11}
  I12 <-  0   # I_{12} = I_{21} # pokus? (1/(sigma_2)^2)*sum(x-mu)
  I22 <-  n/(2*(sigma_2^2)) # I_{22} # pokus? -n/(2*(sigma_2^2)) + 1/((sigma_2)^3) * sum((x-mu)^2)
  I <- matrix(c(I11, I12, I12, I22), ncol = 2) # Fisherova informacna matica I
  return(I) # vystup; matica I
}

#-------------------------------------------------------------------
### samotna optimalizacna funkcia, ktora hlada korene (nulove body) skore funkcie
   # - vrati vektor: [odhad mu ; odhad sigma^2 ; skore funkcie (= log vierohodnostnej) v tychto odhadoch ; pocet interacii]
   # - vstupom je vektor dvoch inicializacnych hodnot (theta1, theta2) a vektor pozorovanych dat (x)
#-------------------------------------------------------------------
NRnorm <- function(theta1, theta2, x) {
  # inicializacna faza funkcie; theta = (mu, sigma^2)
  theta <- c(theta1, theta2) # pociatocne nastavenie vektora theta v kroku 1
    ## poznamka1: Ked uz sme si vytvorili tento vektor theta, tak mozno rovno pouzit funkciu lnormOptim()
  f1 <- 0  # pociatocne nastavenie f1 v kroku 1
  f2 <- lnorm(theta[1], theta[2], x) # pociatocne nastavenie f2 v kroku 1 = hodnota log funkcie v pociatocnom bode (theta1, theta2)
  k <- 0 # pociatocne nastavenie pocitadla k
  
  # telo funkcie; proces prebieha, kym je rozdiel log funkcii mensi ako nastavena hranica 
  # iterujeme, kym skore funkcie, a teda aj rozdiel dvoch skore po sebe nie je takmer 0
  # pre istotu obmedzit aj pocet iteracii v pripade nekonvergencie, chyb zo zaokruhlenia a pod.
  while(abs(f1 - f2) > 0.00001 & k < 1000) {
    k <- k + 1    # zvacsenie pocitadla o 1
    U <- Snorm(theta[1], theta[2], x) # vektor skore funkcii
    H <- Inorm(theta[1], theta[2], x) # Fisherova informacna matica
    (theta <- theta + solve(H) %*% U) # aktualizacia vektora theta 
    f1 <- f2 # aktualizacia f1
    f2 <- lnorm(theta[1], theta[2], x) # aktualizacia f2
  }
  return(list(mu = theta[1], sigma_2 = theta[2], like = f2, k = k)) 
  # vystup; vektor so 4 polozkami
}

NR_MLE <- NRnorm(theta1 = 145, theta2 = 100, cla_L) # MLE odhad (mu, sigma^2)

#-------------------------------------------------------------------
## 3c. Pomocou maximalizácie logaritmu vierohodnostnej funkcie normálneho modelu nájdite maximálne vierohodný 
## odhad parametrov mu a sigma2. Maximalizáciu vykonajte pomocou vlastnoručne naprogramovanej Broydenovej metódy BMnorm():
#-------------------------------------------------------------------
## Broydenova metoda:
  # - dvojrozmerná metóda sečníc
  # - aproximuje Jacobiho maticu parciálnych derivácií bez explicitného výpočtu derivácií
#-------------------------------------------------------------------
### samotna optimalizacna funkcia, ktora hlada korene (nulove body) skore funkcie
  # - vrati vektor: [odhad mu ; odhad sigma^2 ; skore funkcie (= log vierohodnostnej) v tychto odhadoch ; pocet interacii]
  # - vstupom je vektor dvojica inicializacnych hodnot pre kazdy odhadovany parameter (theta10, theta 11, theta20, theta21) a vektor pozorovanych dat (x)
#-------------------------------------------------------------------
theta10 <- 100
theta11 <- 180
theta20 <- 80
theta21 <- 120
BMnorm <- function(theta10, theta11, theta20, theta21, x) {
  # inicializacna faza funkcie; x1 a x2 = (mu, sigma^2)
  theta1 <- c(theta10, theta20) # pociatocne nastavenie vektora theta1 ~ dolna hranica intervalov pre mu, sigma2
  theta2 <- c(theta11, theta21) # pociatocne nastavenie vektora theta2 ~ horna hranica intervalov pre mu, sigma2
  f1 <- lnorm(theta1[1], theta1[2], x) # log. funkcia vierohodnosti pre theta1
  f2 <- lnorm(theta2[1], theta2[2], x) # log. funkcia vierohodnosti pre theta2
  k <- 0 # pociatocne nastavenie pocitadla k
  B <- diag(2) # jednotkova matica dimenzie 2x2
  
  # telo funkcie; proces prebieha, kym podmienka while(...) plati
  while(abs(f1 - f2) > 0.00001 & k < 1000) {
    k <- k + 1 # zvacsenie pocitadla o 1
    U1 <- Snorm(theta1[1], theta1[2], x) # vektor skore funkcii pre theta1
    U2 <- Snorm(theta2[1], theta2[2], x) # vektor skore funkcii pre theta2
    B <- B + c(((U2 - U1) - B %*% (theta2 - theta1)) / sum((theta2 - theta1) ^ 2)) %*% t(c(theta2 - theta1))
    theta <- (theta2 - solve(B) %*% U2) # vypocet vektora theta
    f1 <- f2 # aktualizacia f1
    f2 <- lnorm(theta[1], theta[2], x) # aktualizacia f2
    theta1 <- theta2 # aktualizacia theta1
    theta2 <- theta # aktualizacia theta2
  }
  return(list(mu = theta[1], sigma_2 = theta[2], like = f2, k = k)) 
  # vystup; zoznam so 4 polozkami
}

BM_MLE <- BMnorm(100, 180, 80, 120, x = cla_L) # MLE odhad (mu, sigma^2)
BM_max_mu <- BM_MLE$... # MLE odhad parametra mu
BM_max_sigma_2 <- BM_MLE$... # MLE odhad parametra sigma^2

### suhrnna tabulka pre porovnanie
tab_comp <- rbind(cbind(mu, sigma_sq), 
                  cbind(max_mu, max_sigma_2),
                  cbind(NR_MLE$mu, NR_MLE$sigma_2),
                  cbind(BM_MLE$mu, BM_MLE$sigma_2)) |> round(4)
colnames(tab_comp) = c('mu', 'sigma^2')
rownames(tab_comp) = c('MLE odhad', 'odhad ~ max log(L)', 'odhad - NR metoda', 'odhad - Broyden metoda')
tab_comp

# iteracie
tab_comp <- rbind(cbind(NR_MLE$mu, NR_MLE$sigma_2, NR_MLE$like, NR_MLE$k),
                  cbind(BM_MLE$mu, BM_MLE$sigma_2, BM_MLE$like, BM_MLE$k)) |> round(4)
colnames(tab_comp) = c('mu', 'sigma^2', 'skore fcia', 'pocet iter')
rownames(tab_comp) = c('odhad - NR metoda', 'odhad - Broyden metoda')
tab_comp

#-------------------------------------------------------------------
## 4. Vykreslite 
## a. vrstevnicový diagram
## Pre vykreslenie použite funkciu image() a contour()
#-------------------------------------------------------------------
N <- 50
mu <- seq(146, 157, length = N) # postupnost bodov osi x; od 146 do 157 s dlzkou N
sigma_2 <- seq(80, 120, length = N ) # postupnost bodov osi y; od 80 do 120 s dlzkou N
M <- matrix(NA, N, N) # prazdna matica rozmerov (N x N)

for (i in 1:length(mu)) {
  for (j in 1:length(sigma_2)) {
    M[i, j] <- lnorm(mu[i], sigma_2[j], cla_L)
  }
} # hodnoty logaritmu funkcie vierohodnosti

library(colorspace)
k <- 12 # pocet farieb
par(mar = c(4, 5, 2, 2)) # okraje grafu (spodny, lavy, horny, pravy)
image(mu, sigma_2, M, 
      col = terrain.colors(k), 
      xlab = expression(mu), ylab = expression(sigma^2), 
      breaks = seq(min(M), max(M), length = k + 1), 
      asp = F, las = 1, ylim = c(75, 120))
# vrstevnicovy diagram logaritmu vierohodnostnej funkcie
contour(mu, sigma_2, M, add = T, 
        levels = seq(min(M), max(M), length = k + 1), 
        drawlabels = T) # konturovy diagram
points(max_mu, max_sigma_2, col = 'black', pch = 19, cex = 3) # cierny bod; MLE odhad (mu, sigma^2) z optim
points(NR_MLE$mu, NR_MLE$sigma_2, col = 'red', pch = 19, cex = 2) # cerveny bod; MLE odhad z Newton-Raphson metody
points(BM_MLE$mu, BM_MLE$sigma_2, col = 'blue', pch = 19, cex = 1) # modry bod; MLE odhad z Broydenovej metody
legend('bottom', horiz = TRUE, 
       pch = 19, col = c('black', 'red', 'blue'), 
       legend = c('optim()', 'N-R', 'Broyden')) # legenda


#-------------------------------------------------------------------
## 4b. Vykreslite 
## 3D-diagram logaritmu dvojrozmernej vierohodnostnej funkcie normálneho modeli spolu s maximálne vierohodnostnými 
## odhadmi parametrov a odhadnutými pomocou funkcie optim().
## Pre vykreslenie použite funkciu persp()
#-------------------------------------------------------------------

nrz <- nrow(M) # pocet riadkov matice M
ncz <- ncol(M) # pocet stlpcov matice M
color <- terrain.colors(12) # paleta k farieb
stredy <- (M[-1, -1] + M[-1, -ncz] + M[-nrz, -1] + M[-nrz, -ncz]) / 4 
# matica stredov siete
stredy.col <- cut(stredy, k) # rozdelenie hodnot do k intervalov

par(mar = c(4, 5, 2, 2)) # okraje grafu
persp(x = mu, y = sigma_2, z = M, ticktype = 'simple', main = '', 
      xlab = expression(mu), ylab = expression(sigma^2), 
      zlab = 'lnorm(mu,sigma^2)', col = color[stredy.col], phi = 40, theta = 130) # 3D diagram
