## Dataset 02: n   0   1  2  3  4  >=5 suma
##             447 132 42 21 3  2  647

# Príklad 2.1: Výpočet očakávaných početností za predpokladu Poissonovho modelu
## Vezmite údaje z datasetu 02 a vypočítajte očakávané početnosti výskytu úrazu u robotníkov za predpokladu, že
## početnosti úrazov majú Poissonovo rozdelenie s parametrom lambda

### nacitanie datasetu do premennych
n <- 0:5 # postupnost 0, 1, 2, 3, 4, 5
m_obs <- c(447, 132, 42, 21, 3, 2) # postupnost m_obs 447, 132, 42, 21, 3, 2
M <- sum(m_obs) # celkovy pocet vsetkych jednotiek M (sucet vsetkych cisel m_obs)

### odhad parametra lambda podla vzorca
lambda <- sum(n*m_obs) / sum(M) 
lambda

### vektor s ocakavanymi absolutnymi pocetnosti
#### dpois(x, lambda) - výpočet pravdepodobnostnej funkcie
m_exp <- round(c(dpois(n[1:length(n)-1], lambda), 1 - sum(dpois(n[1:length(n)-1], lambda)))*M)
### kontrola, ci zaokruhlenim sa nestalo, ze sucet teoretickych a empirickych je rozny
sum(m_exp) == M

# Príklad 2.2 (Overdispersion a underdispersion v Poissonovom modeli I)
## V predchádzajúcom príklade Príklad 2.1 sme stanovili očakávané početnosti výskytu úrazov robotníkov v továrni. 
## Teraz do jedného grafu znázorníte hodnoty pozorovaných početností a hodnoty očakávaných početností .
## Pozorované a očakávané početnosti od seba farebne odlíšte. Na základe výsledného grafu stanovte, či v tomto prípade
## došlo k overdispersion (nadmernej variabilite) alebo underdispersion (nedostatočnej variabilite).

par(mar = c(4, 4, 1, 1)) # okraje grafu 4, 4, 1, 1

plot(n, m_exp, type = 'h', ylim = c(0, 450), col = 'red', xlab = '', ylab = 'absolútna početnosť', axes = F) 
# graf s vertikalnymi cervenymi ciarami ocakavanych absolutnych pocetnosti m_exp
box(bty = 'o') # ramik okolo grafu
axis(1, 0:5, labels = c(0:4, '5+')) # os x
axis(2, las = 1) # os y
points(n, m_exp, pch = 21, col = 'red', bg = rgb(1, 0, 0, 0.2)) 
# cervene body absolutnych ocakavanych pocetnosti m_exp

lines(n, m_obs, col = 'blue', type = 'h', lty = 4) 
# modre vertikalne ciary pozorovanych absolutnych pocetnosti m_obs
points(n, m_obs, pch = 21, col = 'blue', bg = rgb(0, 0, 1, 0.2)) 
# modre body pozorovanych absolutnych pocetnosti m_obs

mtext('počet úrazov', side = 1, line = 2.1) # doplnenie popisku osi x pod graf
legend('topright', pch = c(21, 21), col = c('red', 'blue'),
       pt.bg = c(rgb(1, 0, 0, 0.2), rgb(0, 0, 1, 0.2)),
       legend = c(expression(m[exp]), expression(m[obs])), bty = 'n') # legenda

###Oproti očakávaným početnostiam sú pozorované početnosti krajných prípadov vyššie a naopak, pozorované početnosti najčastejších prípadov sú nižšie. Táto situácia ukazuje na vyšší rozptyl pozorovaných početností než teoretických. 
###Ide teda o overdispersion

## Overenie výpočtom
observed <- rep(n, m_obs) # vektor pozorovanych dat: 0, ..., 0, 1, ..., 5, 5
expected <- rep(n, m_exp) # vektor ocakavanych dat: 0, ..., 0, 1, ..., 1, ..., 4
var_obs <- var(observed) # odhad rozptylu na zaklade pozorovanych dat
var_exp <- var(expected) # odhad rozptylu na zaklade ocakavanych dat
tab <- data.frame(var_obs, var_exp) # tabulka vysledkov
tab

### Hodnota rozptylu získaného z pozorovaných dát vyšla 0.6919, 
### hodnota rozptylu získaného z očakávaných dát vyšla 0.4691
### Hodnoty rozptylov sa líšia už na prvom desatinnom mieste. 
### Hodnota rozptylu vypočítaného z pozorovaných dát je približne 1,5-krát vyššia než hodnota rozptylu vypočítaného z očakávaných dát.
### skutočne ide o overdispersion

# Príklad 2.3: Graf pravdepodobnostnej a distribučnej funkcie Poissonovho modelu 
## V príklade Príklad 2.1 sme odhadli hodnotu parametra Poissonovho rozdelenia ako 0,4652
## Nakreslite graf pravdepodobnostnej a distribučnej funkcie Poissonovho rozdelenia v hodnotách 0, 1, 2, 3, 4, 5+

N <- 5 # max pocet
x <- 0:N # postupnost 0, 1, ..., N = 5
lambda 
px <- dpois(x,lambda) # pravdepodobnostna funkcia Poi(lambda) v hodnotach 0, 1, ..., 5
Fx <- ppois(x,lambda) # distribucna funkcia Poi(lambda) v hodnotach 0, 1, ..., 5

## Graf pravdepodobnostnej funkcie p(x):
par(mar = c(4,4,1,1)) # nastavenie okrajov grafu (4, 4, 1, 1)
plot(x, px, type = 'h', ylim = c(0, 0.8), ylab = 'p(x)', xlab = '', axes = F) 
box(bty = 'o')
axis(1, 0:5, labels = c(0:4, "5+"))
axis(2, las = 1)
points(x, px, pch = 21, col = 'black', bg = rgb(1,0,0,0.8))
mtext('x', side = 1, line =  2.1)
mtext(bquote(paste(lambda == .(round(lambda, 4)))), side = 1, line = 3.2) 


# graf distribucnej funkcie (cierne vertikalne ciary)
par(mar = c(4,4,1,1)) # nastavenie okrajov grafu (4, 4, 1, 1)
plot(x, px, type = 'h', ylim = c(0, 0.8), ylab = 'p(x)', xlab = '', axes = F) 
box(bty = 'o')
axis(1, 0:5, labels = c(0:4, "5+"))

plot(x, Fx, type = 'n', xlim = c(-1,6), ylim = c(0, 1), xlab = '', ylab = 'F(x)', axes = F)
box(bty = 'o')
axis(1, -1:5, labels = c(-1:4, "5+"))
axis(2, las = 1)
segments(x, Fx, x + 1, Fx)
arrows(0, 0, -1, 0, length = 0.1)
arrows(5, 1, 6, 1, length = 0.1)

points(x, c(0, Fx[1:N]), col = 'black', pch = 21, bg = 'white')
points(x,Fx, col = 'red', pch = 19)
mtext(bquote(lambda == .(round(lambda, 4))), side = 1, line = 2.1)

# Priklad 2.4: Vypocet pravdepodobnosti na zaklade Poissonovho modelu
## Za predpokladu, že náhodná veličina , ktorá udáva počet úrazov robotníkov v továrni, pochádza z Poissonovho
## rozdelenia s parametrom , vypočítajte pravdepodobnosť, že u náhodne vybraného robotníka dôjde počas
## jedného roka k:
### a) nula úrazom,
lambda <- 0.4652
dpois(0, lambda)

### b) trom alebo štyrom úrazom,
sum(dpois(3, lambda) + dpois(4, lambda))
ppois(4, lambda) - ppois(2, lambda)

### c) najviac dvom úrazom,
ppois(2, lambda)
sum(dpois(0,lambda), dpois(1, lambda), dpois(2, lambda))

### d) aspoň jednému úrazu.
1 - ppois(0, lambda)

# Stredná hodnota a rozptyl náhodnej veličiny z Poissonovho modelu
## Za predpokladu, že náhodná veličina X, ktorá udáva počet úrazov robotníkov v továrni, 
## pochádza z Poissonovho rozdelenia s parametrom lambda = 0.4652, vypočítajte strednú hodnotu E(X)
### a rozptyl Var(X) tejto náhodnej veličiny. Strednú hodnotu a rozptyl porovnajte s ich odhadmi vypočítanými na:
### a) základe očakávaných dát,
### b) základe pozorovaných dát (Príklad 2.1)

## Riesenie:
E_X <- lambda # vypocet strednej hodnoty E[X] rozdelenia Poiss(lambda)
Var_X <- lambda # vypocet rozptylu Var[X] rozdelenia Poiss(lambda)

expected <- rep(0:5, m_exp) # vektor ocakavanych dat
E_exp <- mean(expected) # odhad strednej hodnoty na zaklade ocakakavanych dat
Var_exp <- var(expected) # odhad rozptylu na zaklade ocakavanych dat

observed <- rep(0:5, m_obs) # vektor pozorozovanych dat 
E_obs <- mean(observed) # odhad strednej hodnoty na zaklade pozorozovanych dat
Var_obs <- var(observed) # odhad rozptylu na zaklade pozorozovanych dat

tab <- data.frame(E_X, Var_X, E_exp, Var_exp, E_obs, Var_obs) # tabulka vysledkov
tab

# Príklad 2.6: Simulačná štúdia: Súčet náhodných veličín z Poissonvho modelu 

## Veta 1.: Nech X_1 až X_n sú navzájom nezávislé náhodné veličiny pochádzajúce z Poissonovho rozdelenia Pois(X, lambda_i). 
## Potom náhodná veličina X (ktorá je sumou X_1 až X_n) má Poissonovo rozdelenie s lambdou = suma (lambda_i)

## Na základe simulačnej štúdie overte platnosť Vety. Vygenerujte tri nezávislé náhodné veličiny 
## X1 ~ Poiss(lambda_1 = 10)
## X2 ~ Poiss(lambda_1 = 20)
## X3 ~ Poiss(lambda_1 = 50)
## Pomocou simulačnej štúdie (M = 1000) ukážte, že suma X_i ~ Poiss(lambda_1 + lambda_2 + lambda_3)
## Súčet M náhodných veličín zobrazte pomocou histogramu (hranice triediacich intervalov nastavte tak, 
## aby šírka každého intervalu bola rovná 10) a superponujte ich hodnotami pravdepodobnostnej funkcie 
## Poissonovho rozdelenia s parametrom lambda = lambda_1 + lambda_2 + lambda_3. Hodnoty pravdepodobnostnej funkcie vykreslite vždy v strede každého triediaceho intervalu.

# Riešenie:
M <- 1000 # pocet simulacii M
lambda1 <- 10 # parameter lambda 1
lambda2 <- 20 # parameter lambda 2
lambda3 <- 50 # parameter lambda 3
lambdaS <- lambda1 + lambda2 + lambda3 

X <- replicate(M, sum(rpois(1, lambda1), rpois(1, lambda2), rpois(1, lambda3)))
# 1000 suctov nahodnych velicin X1 + X2 + X3
b <- seq(45, 115, by = 10) # hranice triediacich intervalov; postupnost od 45 do 115 po kroku 10
centr <- seq(40, 110, by = 10) # stredy triediacich intervalov; postupnost od 40 do 110 po kroku 10
y <- dpois(centr, lambdaS) # pravdepodobnostna funkcia rozdelenia Poiss(lam1 + lam2 + lam3) 
# v stredoch triediacich intervalov

par(mar = c(4,4,1,1), mfrow = c(1,1)) # nastavenie okrajov 4, 4, 1, 1
hist(X, prob = T, axes = F, breaks = seq(45, 115, by = 10),  density = 60, col = 'blue', 
     ylim = c(0, max(y) + 0.005), main = '', xlab = '', ylab = 'p(x)') 
# histogram nahodneho vyberu X (v relativnej skale)
box('o') # ramik okolo grafu
axis(1, centr) # os x
axis(2, las = 1) # os y
points(centr, dpois(centr, lambdaS), pch = 21, col = 'black', bg = 'black') # cierne plne body
lines(centr, dpois(centr, lambdaS), type = 'h') # vertikalne cierne ciary
mtext('x', side = 1, line = 2.1) # popis osi x
legend('topright', fill = c('blue', 'black'), density = c(60, 200), legend = c('simulované', 'očakávané'), bty ='n') # legenda

# Príklad 2.7: Výpočet očakávaných početností za predpokladu Poissonovho modelu II 
## Vezmite údaje z datasetu 03 a vypočítajte očakávané početnosti výskytu smrteľných úrazov spôsobených kopnutím koňa 
## za predpokladu, že početnosti úrazov X majú Poissonovo rozdelenie.

## Dataset 03: Pruské armádne jednotky 
### V rámci štúdie z roku 1898 boli spracovávané počty smrteľných úrazov v pruských armádnych jednotkách spôsobených kopnutím koňom. 
### Údaje o smrteľných úrazoch po kopnutí koňom boli zaznamenávané po dobu dvadsiatich rokov u desiatich armádnych jednotiek. 
### Počty úrazov v každej jednotke za jeden rok sú uvedené v nasledujúcej tabuľke. 
### Rozsah náhodného výberu je M = 200 (10 jednotiek × 20 rokov).
### 0	  1	  2	  3	  4	  5+  spolu
### 109	65	22	3	  1	  2	  200	

## Riešenie: 
n <- 0:5
M <- 200 
m_obs <- c(109, 65, 22, 3, 1, 0)

M == sum(m_obs)
lambda <- sum(n*m_obs) / M
lambda
m_exp <- round(dpois(n, lambda)*M)
tab <- data.frame(rbind(c(m_obs, sum(m_obs)), c(m_exp, sum(m_exp))))
names(tab) <- c(0:4, "5+", "spolu")


# Príklad 2.8: Overdispersion a underdispersion v Poissonovom modeli II 
## V predchádzajúcom príklade sme stanovili očakávané početnosti výskytu smrteľných úrazov 
## v pruských armádnych jednotkách. Teraz do jedného grafu znázorníte hodnoty pozorovaných 
## početností a hodnoty očakávaných početností. Pozorované a očakávané početnosti od seba 
## farebne odlíšte. Na základe výsledného grafu stanovte, či v tomto prípade došlo 
## k overdispersion (nadmernej variabilite) alebo underdispersion (nedostatočnej variabilite).
## Záver podložte porovnaním rozptylu vypočítaného z pozorovaných dát s rozptylom vypočítaným 
## z očakávaných dát.

# Riešenie: 
par(mar = c(4, 4, 1, 1)) # okraje grafu 4, 4, 1, 1

plot(n, m_exp, type = 'h', ylim = c(0, 150), col = 'red', xlab = '', ylab = 'absolútna početnosť', axes = F) 
# graf s vertikalnymi cervenymi ciarami ocakavanych absolutnych pocetnosti m_exp
box(bty = 'o') # ramik okolo grafu
axis(1, 0:5, labels = c(0:4, '5+')) # os x
axis(2, las = 1) # os y

points(n, m_exp, pch = 21, col = 'red', bg = rgb(1, 0, 0, 0.2)) 
# cervene body absolutnych ocakavanych pocetnosti m_exp

lines(n, m_obs, col = 'blue', type = 'h', lty = 4) 
# modre vertikalne ciary pozorovanych absolutnych pocetnosti m_obs
points(n, m_obs, pch = 21, col = 'blue', bg = rgb(0, 0, 1, 0.2)) 
# modre body pozorovanych absolutnych pocetnosti m_obs

mtext('počet úrazov', side = 1, line = 2.1) # doplnenie popisku osi x pod graf
legend('topright', pch = c(21, 21), col = c('red', 'blue'),
       pt.bg = c(rgb(1, 0, 0, 0.2), rgb(0, 0, 1, 0.2)),
       legend = c(expression(m[exp]), expression(m[obs])), bty = 'n') # legenda

###Oproti očakávaným početnostiam sa pozorované početnosti takmer nelíšia. Nejde o overdisperziu, ani underdisperziu

## Overenie výpočtom
observed <- rep(n, m_obs) # vektor pozorovanych dat: 0, ..., 0, 1, ..., 5, 5
expected <- rep(n, m_exp) # vektor ocakavanych dat: 0, ..., 0, 1, ..., 1, ..., 4
var_obs <- var(observed) # odhad rozptylu na zaklade pozorovanych dat
var_exp <- var(expected) # odhad rozptylu na zaklade ocakavanych dat
tab <- data.frame(var_obs, var_exp) # tabulka vysledkov
tab

### Hodnota rozptylu získaného z pozorovaných dát vyšla 0.61095, 
### hodnota rozptylu získaného z očakávaných dát vyšla 0.62100ť
### Hodnoty rozptylov sa líšia až na druhom desatinnom mieste. Sú približne rovnaké
