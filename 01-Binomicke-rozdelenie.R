# Príklad 1.1: Výpočet očakávaných početností za predpokladu binomického modelu
## V rámci štúdie pomeru pohlaví u ľudí z roku 1889 bolo na základe záznamov z nemocníc v Sasku 
## zaznamenané rozdelenie počtu chlapcov v dvanásťdetných rodinách. Medzi M = 6115 rodinami s N = 12 deťmi bola pozorovaná početnosť chlapcov.
## Údaje zo štúdie sú uvedené v nasledujúcej tabuľke:
## Tabuľka 1.1: Početnosť chlapcov v rodinách s 12 deťmi
## 0	1	2	3	4	5	6	7	8	9	10	11	12	
## 3	24	104	286	670	1033	1343	1112	829	478	181	45	7	6115

N <- 12 # hodnota parametra N
n <- 0:12 # postupnost 0, 1, ..., N
m_obs <- c(3, 24, 104, 286,	670, 1033,	1343,	1112,	829,	478,	181,	45,	7)  # postupnost m_obs, t.j. 3, 24, ..., 7
M <- sum(m_obs) # pocet vsetkych rodin M (sucet vsetkych cisel v m_obs)
p <- sum(n*m_obs) / (N*M) # odhad parametra p podla vzorca

m_exp_rel <-dbinom(n,N,p) # ocakavanie relativne pocetnosti
m_exp <- round(m_exp_rel*M) # relatívna početnost * pocet rodín, ale zaokruhlit

## Porovnanie empirickych a ocakavanych pocetnosti
M # sucet empirickych pocetnosti
sum(m_exp) # sucet ocakavanych pocetnosti

##Zaokruhlenie podielu, lebo sucet ocakavanych sa od celkoveho poctu empirickych líši o 1
p <- round(sum(n*m_obs) / (N*M),4) 
## a znova porovnanie
m_exp_rel <-dbinom(n,N,p) # ocakavane relativne pocetnosti
m_exp <- round(m_exp_rel*M) # relatívna početnost * pocet rodín, ale zaokruhlit

M # sucet empirickych pocetnosti
sum(m_exp) # sucet ocakavanych pocetnosti

# Priklad 1.2: Overdispersion a underdispersion v binomickom modeli
## zobrazit do jedneho grafu ocakavane aj empiricke pocetnosti

par(mar = c(4, 4, 1, 1),mfrow = c(1, 1)) # nastavenie okraja grafu (vid napoveda funkcie par())
plot(n, m_exp, type = 'h', col = 'red', xlab = ' ', ylab = 'absolutna pocetnost', las = 1) 
# graf s vertikalnymi cervenymi ciarami ocakavanych absolutnych pocetnosti m_exp
points(n, m_exp, pch = 21, col = 'red' , bg = rgb(1, 0, 0, 0.2)) # pch = 21 .. vyplnený krúžok
# cervene body absolutnych ocakavanych pocetnosti m_exp
lines(n, m_obs, col = 'blue', type = 'h', lty = 4) 
# modre vertikalne ciary pozorovanych absolutnych pocetnosti m_obs
points(n, m_obs, pch = 21, bg = rgb(0, 0, 1, 0.2), col = 'blue') 
# modre body pozorovanych absolutnych pocetnosti m_obs 
mtext('pocet chlapcov v rodine', side = 1, line = 2.1) # doplnenie popisku osy x pod graf
legend('topright', pch = c(21, 21), col = c('red', 'blue'), pt.bg = c(rgb(1,0,0,0.2), rgb(0,0,1,0.2)),
       legend = c(expression(m[exp]), expression(m[obs])), bty = 'n') # legenda

## pozorovane m_observed su vyssie na okrajoch, teda variabilita pozorovanych je vyssia ==> overdispersion
## Vypocet variability:
obs <- rep(n, m_obs) # vektor pozorovanych dat: 0, 0, 0, 1, ..., 1, ..., 12, ..., 12
exp <- rep(n, m_exp) # vektor ocakavanych dat: 0, 0, 0, 1, ..., 1, ..., 12, ..., 12
var_obs <- var(obs) # odhad rozptylu na zaklade pozorovanych dat
var_exp <- var(exp) # odhad rozptylu na zaklade ocakavanych dat
(tab <- data.frame(var_obs, var_exp)) # tabulka vysledkov

# Príklad 1.3 Graf pravdepodobnostnej a distribučnej funkcie binomického modelu
N <- 12 # hodnota parametra N
x <- 0:N # postupnost 0, 1, ..., N
p <- 0.5192 # zaokrouhleny odhad p
px <- dbinom(x,N,p) # pravdepodobnostna funkcia Bin(N, p) v hodnotach 0, 1, ..., N
Fx <- pbinom(x,N, p) # distribucna funkcia Bin(N, p) v hodnotach 0, 1, ..., N

## Graf pravdepodobnostnej funkcie p(x):
par(mar = c(4,4,1,1)) # nastavenie okrajov grafu (4, 4, 1, 1)
plot(x, px, type = 'h', ylim = c(0, 0.25), ylab = 'p(x)', xlab = '', las = 1) 
# graf pravdepodobnostnej funkcie (cierne vertikalne ciary)
points(x, px, col = 'red', pch = 19) # cervene body
mtext('x', side = 1, line =  2.1) # popis osy x
mtext(bquote(paste(N == .(N), ' ; ', p == .(p))), side = 1, line = 3.2) 
# druhy popis osy x: N = .., p = ...

## Graf distribučnej funkcie F(x):
plot(x, Fx, type = 'n', xlim = c(-1, N + 1), ylim = c(0, 1), xlab = ' ', ylab = '...', las = 1) 
# priprava prazdneho grafu
segments(x, Fx, x + 1, Fx) # vodorovne cierne cary
arrows(0, 0, -1, 0, length = 0.1) # dolna sipka
arrows(x[13], Fx[13], x[13]+1, Fx[13], length = 0.1) # horna sipka
points(x, c(0, Fx[1:N]), pch = 21, col = 'black', bg = 'white') # prazdne body
points(x, Fx, col = 'red', pch = 19) # plne cervene body
mtext('x', side = 1, line = 2.1) # popis osy x
mtext(bquote(paste(N == .(N), '; ', p == .(p))), side = 1, line = 3.2) # druhy popis osy x: N = ..., p = ...

# Príklad 1.4 Výpočet pravdepodobností na základe binomického modelu
## Za predpokladu, že náhodná veličina, ktorá udáva počet chlapcov v rodine s dvanástimi deťmi, pochádza z binomického rozdelenia s parametrami a 
## vypočítajte pravdepodobnosť, že v rodine s dvanástimi deťmi bude:

## a) práve deväť chlapcov,

N <- 12 # hodnota parametra N
p <- 0.5192 # zaokrouhleny odhad p

dbinom(9, N, p) # vypocet pravdepodobnosti - práve 9 chlapcov

## b) najviac štyria chlapci:
  
pbinom(4, N, p) # vypocet pravdepodobnosti - najviac 4 chlapci - 1. sposob
sum(dbinom(c(0:4), N, p)) # vypocet pravdepodobnosti - 2. sposob

## c) aspoň osem chlapcov:
  
1 - pbinom(7, N, p) # vypocet pravdepodobnosti - aspoň 8 chlapcov - 1. sposob
sum(dbinom(c(8:12), N, p)) # vypocet pravdepodobnosti - 2. sposob

## d) štyria, päť, šesť alebo sedem chlapcov:
  
sum(dbinom(c(4:7), N, p)) # vypocet pravdepodobnosti - 4, 5, 6 alebo 7 chlapci - 1. spososb
pbinom(7, N, p) - pbinom(3, N, p) # vypocet pravdepodobnosti - 2. sposob

# Príklad 1.5: Stredná hodnota a rozptyl náhodnej veličiny z binomického modelu
## Za predpokladu, že náhodná veličina X, ktorá udáva počet chlapcov v rodine s dvanástimi deťmi, 
## pochádza z binomického rozdelenia s parametrami N = 12 a p = 0.5192, 
## vypočítajte strednú hodnotu E[X] a rozptyl Var[X] tejto náhodnej veličiny X. 
## Strednú hodnotu a rozptyl porovnajte s ich odhadmi vypočítanými:
## a) na základe očakávaných dát,
## b) na základe pozorovaných dát

N <- 12 # hodnota parametra N
p <- 0.5192 # zaokruhleny odhad p
E_X <- N * p # vypocet strednej hodnoty E[X] rozdelenia Bin(N, p)
Var_X <- N * p * (1 - p) # vypocet rozptylu Var[X] rozdelenia Bin(N, p)

exp <- rep(n, m_exp) # vektor ocakavanych dat: 0, 0, 0, 1, ..., 1, ..., 12, ..., 12
E_exp <- mean(exp) # odhad strednej hodnoty na zaklade ocakavanych dat
Var_exp <- var(exp) # odhad rozptylu na zaklade ocakavanych dat

obs <- rep(n, m_obs) # vektor pozorovanych dat: 0, 0, 0, 1, ..., 1, ..., 12, ..., 12
E_obs <- mean(obs)  # odhad strednej hodnoty na zaklade pozorovanych dat
Var_obs <- var(obs) # odhad rozptylu na zaklade pozorovanych dat

tab <- data.frame(E_X, Var_X, E_exp, Var_exp, E_obs, Var_obs) # tabulka vysledkov
tab

# Príklad 1.6: Simulačná štúdia pre binomický model
## Sekciu o binomickom rozdelení zakončíme simulačnou štúdiou, ktorá modeluje správanie očakávaných 
## početností náhodnej veličiny X, ktorá predstavuje počet chlapcov v rodinách s dvanástimi deťmi,
## za predpokladu, že X pochádza z binomického rozdelenia s parametrami N = 12 a p = 0.5192.
# Vygenerujte pseudonáhodné čísla X (početnosti úspechov) opakované M-krát (M = 6115)
## z binomického rozdelenia s parametrami N = 12 a p = 0.5192. 
## Vytvorte histogram vygenerovaných pseudonáhodných čísel a porovnajte ho 
## s hodnotami očakávaných (teoretických) početností za predpokladu, že X ~ Bin(N, p).

M <- 6115 # pocet vsetkych rodin M
N <- 12 # hodnota parametra N
n <- 0:12 # postupnost 0, 1, ..., N
p <- 0.5192 # zaokruhleny odhad p
X <- rbinom(M, N, p) # vygenerovanie nahodneho vyberu o dlzke M z rozdelenia Bin(N, p)
p_exp <- dbinom(n, N, p) # pravdepodobnostna funkcia Bin(N, p) v hodnotach 0, 1, ..., N
m_exp <- round(p_exp * M) # vektor ocakavanych absolutnych pocetnosti

par(mar = c(4,4,1,1), mfrow = c(1, 2)) # nastavenie okrajov 4, 4, 1, 1; volba: mfrow = c(1, 2) = 2 obrázky vedľa seba
hist(X, prob = F, breaks = seq(-0.5, 12.5, by = 1), density = 60, col = rgb(0, 0, 1, 0.1), 
     ylim = c(0, 1500), xlab = '', ylab = 'absolútna početnosť', main ='', las = 1) 
# histogram nahodneho vyberu X (v absolutnych pocetnostiach)
box(bty = 'o') # ramik okolo grafu

lines(n, m_exp, type = 'h') # vertikalne ciary ocakavanych absolutnych pocetnosti m_exp
points(n, m_exp, pch = 20) # body ocakavanych absolutnych pocetnosti m_exp
mtext('pocet chlapcov', side = 1, line = 2.1) # popis osy x
mtext(bquote(paste(N == .(N), '; ', p == .(p))), side = 1, line = 3.2) # druhy popis osy x: N = ..., p = ...
legend('topright', fill = c(rgb(0,0,0,0.1),'black'), density = c(60, 200),
       legend = c('simulovane', 'ocakavane'), bty = 'n') # legenda

hist(X, prob = T, breaks = seq(-0.5, 12.5, by = 1), density = 60, col = rgb(0,0,1, 0.1), ylim = c(0, 0.25), xlab = '', 
     ylab = 'relatívna početnosť', main = '', las = 1) 
# histogram nahodneho vyberu X (v relativnych pocetnostiach)
box(bty = 'o') # ramik okolo grafu
lines(n, p_exp, type = 'h') # vertikalne ciary ocakavanych relativnych pocetnosti p_exp
points(n, p_exp, pch=20) # body ocakavanych relativnych pocetnosti p_exp
mtext('pocet chlapcov', side = 1, line = 2.1) # popis osy x
mtext(bquote(paste(N == .(N), '; ', p == .(p))), side = 1, line = 3.2) # druhy popis osy x: N = ..., p = ..
legend('topright', fill = c(rgb(0,0,1, 0.1), 'black'), density = c(60, 200), legend = c('simulovane', 'ocakavane') , bty = 'n') 
# legenda
