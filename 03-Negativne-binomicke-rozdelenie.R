# Teoria - Negativne binomicke rozdelenie
## vychadzam z X ~ Bin(n, p), kde 
### n = pocet pokusov, 
### p = pravdepodobnost nastatia javu
### NP X predstavuje pocet nastati javu pri n pokusoch

## Negatívne binomické rozdelenie X ~ NegBin(k, p)
### pricom k = stanoveny pocet neuspechov (nenastatia javu)
### p = pravdepodobnost uspechu (nastatia javu)
### NP X predstavuje pocet uspechov dosiahnutych skor ako k neuspechov, resp. pocet uspechov pred dosiahnutim k.-teho neuspechu


#Priklad 3.1 Pravdepodobnostna funkcia
## Naprogramovat funkciu dNegBinom(k,p)
## porovnat ju so zabudovanou funkciou dnbinom(x, k, p)

## funkcia s povinnymi vstupnymi argumentami x, k, p 
dNegBinom <- function(x, k, p){
  px <- choose(x + k - 1, x) * p^x * (1-p)^k  # pravdepodobnostna funkcia rozdelenia NegBinom(k, p)
  return(px) 
}
### overit pre vstupy x = 0, 1, 2, NegBin(k = 5, p = 0.4)
x <- 0:2
k <- 5
p <- 0.4
p_own <- dNegBinom(x, k, p) # vypocet pravdepodobnosti s vlastnou funkciu
p_impl <- dnbinom(x, k, 1 - p) # vypocet pravdepodobnosti so zabudovanou funkciou 

# tabulka vysledkov vypocitanych hodnot (podla vzorca a cez zabudovanu funkciu)
tab <- data.frame(rbind(p_own, p_impl)) 
names(tab) <- x
tab

# !!! do implementovanej funkcie dnbinom treba zadat ako 3. parameter pravdepodobnost neuspechu, 1 - p !!!

# Priklad 3.2 Vypocet pravdepodobnosti na zaklade negativne binomickeho modelu
## Pravdepodobnosť zostrelenia terča u biatlonistky A dosahuje až 95%. 
## Pravdepodobnosť zásahu terča u biatlonistky B sa pohybuje okolo 65%. 
## Porovnajte, aká je pravdepodobnosť, že počas tréningu každá z biatlonistiek zostrelí pred prvými dvoma neúspechmi:

### Riesenie:
#### nastavenie hodnot
k <- 2 # budeme skumat pocet uspechov pred dvomi neuspechmi 
pA <- 0.95
pB <- 0.65

#### a) prave 5 tercov
x <-5
p__A <- dnbinom(x, k, 1 - pA) # vypocet pravdepodobnosti - A
p__B <- dnbinom(x, k, 1 - pB) # vypocet pravdepodobnosti - B

#### b) najviac 10 tercov
x<-10
p__A <- c(p__A, pnbinom(x, k, 1 - pA))
p__B <- c(p__B, pnbinom(x, k, 1 - pB))

#### c) d) e) aspon 7, 15, 25 - cez invertovanie pravdepodobnosti
x<- c(6, 14, 24)
p__A <- c(p__A, 1 - pnbinom(x, k, 1 - pA))
p__B <- c(p__B, 1 - pnbinom(x, k, 1 - pB))

tab <- data.frame(rbind(p__A, p__B))
names(tab) <- c('prave 5', 'najviac 10', 'aspon 7', 'aspon 15', 'aspon 25')
round(tab, 4)

# Priklad 3.3 Výpočet očakávaných početností za predpokladu negatívne binomického modelu
## Tabuľka: Pocet urazov u robotnikov v továrni
### n     0	   1	  2	  3	  4	  >=5  spolu
### m_obs 447	 132	42	21	3 	2	   647	
### V priklade 2.2 sme odhadovali model cez Poissonovo rozdelenie, kde sme identifikovali overdisperziu pozorovanych pocetnosti 
### v takych situaciach je vhodne modelovat data cez negativne binomicke rozdelenie. 
### Vypocitajte odhady k a p pre taketo rozdelenie NegBin(k, p)

## Riesenie:
##### nasetovanie premennych
N <- 5 # maximalny uvazovany pocet urazov jedneho robotnika
n <- 0:5
m_obs <- c(447, 132, 42, 21, 3, 2) # postupnost m_obs
M <- 647 # celkovy pocet vsetkych robotnikov M

#### odhad strednej hodnoty a rozptylu
observed <- rep (0:5, m_obs) # vektor pozorovanych dat: 0, 0, ..., 5, 5
m <- mean(observed) # odhad strednej hodnoty E[X] ziskany na zaklade vektora pozorovanych dat
v <- var(observed) # odhad rozptylu Var[X] ziskany na zaklade vektora pozorovanych dat

#### odhad parametrov k a p pre NeBinom
k <- m^2/(v - m) # odhad parametra k dopocitany podla vzorca
p <- 1 - (m/v) # odhad parametra p dopocitany podla vzorca
tab <- data.frame(k,p) # tabulka vyslednych parametrov k a p
tab

#### vypocet ocakavanych poctov za predpokladu Poissonovho a NegBinom rozdelenia
#### a porovnanie s pozorovanymi hodnotami

#### vektor ocakavanych absolutnych pocetnosti za predpokladu X ~ Poiss(0.4652)
lambda <- 0.4652
m_exp_p <- round(c(dpois(n[1:length(n)-1], lambda), 1 - sum(dpois(n[1:length(n)-1], lambda)))*M)

#### vektor ocakavanych absolutnych pocetnosti za predpokladu rozdelenia NegBin(k, p)
m_exp_n <- round(c(dnbinom(n[1:length(n)-1], k, 1-p), 1 - sum(dnbinom(n[1:length(n)-1], k, 1-p))) * M) 

#### tabulka vysledkov (pozorovanych a ocakavanych absolutnych pocetnosti)
tab <- data.frame(rbind(m_obs, m_exp_p, m_exp_n)) 
names(tab) <- 0:5
tab
#### Z tabulky vidno, ze odhadovane pocetnosti pre negativne binomicke rozdelenie sa viac zhoduju 
#### s experimentalnymi datami ako pocetnosti pre Poissonovo rozdelenie.

# Priklad 3.4 Overdispersion and underdispersion v negativne binomickom modeli
## Vykreslit pocetnosti (za predpokladu Po(lambda), NegBin(k, p), ako aj pozorovane) do jedneho grafu
## Vyslovit predpoklad overdispersion / underdispersion o NegBin a overit ho vypoctom disperzii

par(mar = c(4, 4, 1, 1)) # nastavenie okraja grafu 4, 4, 1, 1
### graf s vertikalnymi cervenymi ciarami pozorovanych absolutnych pocetnosti 
plot(0:N, m_obs, type = 'h', col = 'red', xlab = '', ylab = 'Absolútna početnosť', las = 1) 
### (NegBin) zelene vertikalne ciary ocakavanych absolutnych pocetnosti (posunute o 0.1 dolava, aby sa neprekryvali)
lines(0:N - 0.1, m_exp_n, col = 'green', type = 'h')  
### (Poiss) modre vertikalne ciary ocakavanych absolutnych pocetnosti (posunute o 0.1 doprava)
lines(0:N + 0.1,  m_exp_p, col='blue', type='h') 
points(0:N, m_obs, pch = 21, col = 'red', bg = rgb(1, 0, 0, 0.2)) # cervene body
points(0:N - 0.1, m_exp_n, pch = 21, col = 'green', bg = rgb(0, 1, 0, 0.2)) # zelene body
points(0:N + 0.1, m_exp_p, pch = 21, col = 'blue', bg = rgb(0,0,1, 0.2)) # modre body
mtext('Počet úrazov', side = 1, line = 2.1) # popis osy x
legend('topright', lty = 4, pch = 21, col = c('red', 'green', 'blue'), 
       pt.bg = c(rgb(1, 0, 0, 0.2), rgb(0, 1, 0, 0.2), rgb(0, 0, 1, 0.2)), 
       legend = c('m_obs', 'm_exp_NegBin', 'm_exp_Poiss'), bty = 'n') # legenda

### Z grafov je zrejme to, co z tabulky. Odhadovane pocty na zaklade NegBinom su blizke pozorovanym udajom.
### Observed (pozorovane) maju o malicko vacsi rozptyl. Asi overdisperzia, ale velmi slaba.
### vypocet rozptylov
observed <- rep (0 : N, m_obs) # vektor pozorovanych dat
expected_n <- rep(0 : N, m_exp_n) # vektor ocakavanych dat na zaklade NegBin rozdelenia
round (tab <- data.frame(Var_obs = var(observed), Var_exp_n = var(expected_n)), 4)
### Hodnota rozptylu pozorovanych dat je o trochu vyssia (0.6919) oproti ocakavanym datam (0.6700), 
### ale rozdiel je velmi maly. Preto nemozno hovorit o overdisperzii.

# Priklad 3.5 Graf pravdepodobnostnej a distribucnej funkcie negativne binomickeho modelu

## Riesenie:
### vektor ocakavanych relativnych pocetnosti za predpokladu rozdelenia NegBin(k, p)
px_n <- c(dnbinom(n[1:length(n)-1], k, 1-p), 1 - sum(dnbinom(n[1:length(n)-1], k, 1-p)))

### graf hustoty
par(mar = c(4,4,1,1), mfrow = c(1, 1)) # nastavenie okrajov grafu (4, 4, 1, 1)
plot(n, px_n, type = 'h', ylim = c(0, 0.7), ylab = 'p(x)', xlab = '', axes = F) 
box(bty = 'o')
axis(1, 0:5, labels = c(0:4, "5+"))
axis(2, las = 1)
points(n, px_n, pch = 21, col = 'purple', bg = 'purple')
mtext('x', side = 1, line = 2.1)
mtext(bquote(paste(k == .(round(k, 4)), '; ', p == .(round(p,4)))), side = 1, line = 3.2) 

### vektor ocakavanych kumulativnych relativnych pocetnosti za predpokladu rozdelenia NegBin(k, p)
Fx_n <- c(pnbinom(n[1:length(n)-1], k, 1-p), 1)

### graf distribucnej funkcie
par(mar = c(4,4,1,1)) # nastavenie okrajov grafu (4, 4, 1, 1)
plot(n, Fx, type = 'n', xlim = c(-1,6), ylim = c(0, 1), xlab = '', ylab = 'F(x)', axes = F)
box(bty = 'o')
axis(1, -1:5, labels = c(-1:4, "5+"))
axis(2, las = 1)
segments(n, Fx_n, n + 1, Fx_n)
arrows(0, 0, -1, 0, length = 0.1)
arrows(5, 1, 6, 1, length = 0.1)

points(n, c(0, Fx_n[1:N]), col = 'black', pch = 21, bg = 'white')
points(n,Fx_n, col = 'purple', pch = 19)
mtext('x', side = 1, line = 2.1)
mtext(bquote(paste(k == .(round(k, 4)), '; ', p == .(round(p, 4)))), side = 1, line = 3.1)

# Priklad 3.6
## dnbinom(x, k, 1-p) - hustota
## pnbinom(x, k, 1-p) - distribucna
## Za predpokladu, že náhodná veličina X, ktorá udáva počet úrazov robotníkov v továrni, 
## pochádza z negatívne binomického rozdelenia s parametrom k = 0,9548 a p = 0.3276, 
## t.j. X ~ NegBin(k,p) Vypočítajte pravdepodobnosť, že u náhodne vybraného robotníka dôjde 
## počas jedného roka k:
## a) nula úrazom,
## b) trom alebo štyrom úrazom,
## c) najviac dvom úrazom,
## d) aspoň jednému úrazu.
## Vysledky porovnajte s pravdepodobnostami vypocitanymi na zaklade Poissonovho rozdelenia

## Riesenie:
k <- 0.9548 # parameter k 
p <- 0.3276 # parameter p
## a) nula
p0 <- dnbinom(0, k, 1-p)
## b) 3 alebo 4 úrazom
p3_4 <- dnbinom(3, k, 1-p) + dnbinom(4, k, 1-p)
## c) najviac dvom
p_max2 <- pnbinom(2, k, 1-p)
## d) aspon jednemu
p_min1 <- 1 - p0
(round(data.frame(p0, p3_4, p_max2, p_min1),4))

## na zaklade Poissonovho rozdelenia
lambda <- 0.4652241
p0 <- dpois(0, lambda)
p3_4 <- dpois(3, lambda) + dpois(4,lambda)
p_max2 <- ppois(2, lambda)
p_min1 <- 1 - p0
(round(data.frame(p0, p3_4, p_max2, p_min1), 4))

# Priklad 3.7 Stredná hodnota a rozptyl NP z negatívneho binomického modelu
## Za predpokladu, že náhodná veličina X, ktorá udáva počet úrazov robotníkov v továrni, 
## pochádza z negatívne binomického rozdelenia s parametrom k = 0.9548 a p = 0.3276 
## vypočítajte strednú hodnotu a rozptyl tejto náhodnej veličiny. Porovnajte ich 
## s odhadmi vypočítanými na:
## a) základe očakávaných dát,
## b) základe pozorovaných dát

## Riesenie:
k <- 0.9548 # parameter k 
p <- 0.3276 # parameter p

### Vypocet cez vzorce pre negativne binomicke rozdelenie
E_X <- k*p/(1-p) # vypocet strednej hodnoty E[X] rozdelenia NegBin(k, p)
Var_X <- k*p/(1-p)^2 # vypocet rozptylu Var[X] rozdelenia NegBin(k, p)

### Vypocet na zaklade ocakavanych dat
expected <- rep(0:5, m_exp_n) # vektor ocakavanych dat
E_exp <- mean(expected) # odhad strednej hodnoty na zaklade ocakakavanych dat
Var_exp <- var(expected) # odhad rozptylu na zaklade ocakavanych dat

### Vypocet na zaklade pozorovanych dat
observed <- rep(0:5, m_obs) # vektor pozorovanych dat 
E_obs <- mean(observed) # odhad strednej hodnoty na zaklade pozorovanych dat
Var_obs <- var(observed) # odhad rozptylu na zaklade pozorovanych dat

(round(data.frame(E_X, Var_X, E_exp, Var_exp, E_obs, Var_obs), 4)) # tabulka vysledkov

# Priklad 3.8 Simulačná štúdia pre negatívny binomický model a Poissonov model
## Vytvorte simulačnú štúdiu modelujúcu správanie očakávaných počtov náhodnej veličiny 
## popisujúcej počet úrazov pracovníkov v továrni za predpokladu, že:
## a) X ~ Poiss(lambda = 0.4652)
## b) X ~ NegBin(k = 0.9548, p = 0.3276) 
## Vygenerujte pseudonáhodné čísla X (počty úspechov) opakované M-krát (M = 647)
## Pre každé rozdelenie vytvorte histogram vygenerovaných pseudonáhodných čísel 
## a superponujte ho hodnotami očakávaných (teoretických počtov).

## Riesenie:

### Simulacia cez Poissonovo rozdelenie
N <- 5 # maximalny uvazovany pocet urazov jedneho robotnika
M <- 647 # celkovy pocet vsetkych robotnikov M
lambda <- 0.4652 # lambda 

par(mar = c(5, 4, 3, 2)) # nastavenie okrajov 5, 4, 3, 2
X <- rpois(1:M, lambda)  # vektor M pseudonáhodných čísel z rozdelenia Poiss(lambda)
px_pois <- dpois(0:N, lambda) # pravdepodobnostna funkcia Poiss(lambda) v hodnotach 0, 1, ..., N
hist(X, prob = T, breaks = seq(-0.5, max(X) + 0.5, by = 1), density = 60, col = 'red', 
     ylim = c(0, max(px_pois) + 0.05), las = 1, main = 'Poisson', xlab = '', ylab = 'relatívna početnosť') 
# histogram nahodneho vyberu X (v relativnej skale)
box('o') # ram okolo grafu
points(0:N, px_pois, pch = 19, col='black') # cierne plne body; pravdepodobnostna funkcia Poiss(lambda)
lines(0:N, px_pois, type = 'h') # cierne vertikalne ciary; pravdepodobnostna funkcia Poiss(lambda)
mtext('x', side = 1, line = 2.1) # popis osy x
mtext(bquote(paste(lambda == .(lambda))), side = 1, line = 3.2) # druhy popis osy x
legend('topright', fill = c('red', 'black'), density = c(60, 200), legend = c('simulované', 'očakávané'), 
       bty ='n') # legenda

### Simulacia cez Negativne binomicke rozdelenie
k <- 0.9548 # parameter k
p <- 0.3276 # parameter p
Y <- rnbinom(1:M,k, 1-p) # vektor M pseudonahodnych cisel z rozdelenia NegBin(k, p)
py <- dnbinom(0:N, k, 1-p) # pravdepodobnostna funkcia rozdelenia NegBin(k, p) v hodnotach 0, 1, ..., N
hist(Y, prob = T, breaks = seq(-0.5, max(Y) + 0.5, by = 1), , density = 60, col = 'red', las = 1,
     ylim = c(0, max(px_pois) + 0.05), main = 'Negatívne binomické', xlab = '', ylab = 'relatívna početnosť') # histogram nahodneho vyberu vyberu Y (v rel. skale) 
box('o') # ram okolo grafu
lines(0:N, py, type = 'h' ) # cierne vertikalne ciary
points(0:N, py, pch = 19, col = 'black') # cierne plne body; 
mtext('x', side = 1, line = 2.1) # popis osy 
mtext(bquote(paste(k == .(k), '; ', p == .(p))), side = 1, line = 3.2) # druhy popis osy x
legend('topright', fill = c('red', 'black'), density = c(60, 200), legend = c('simulované', 'očakávané'), 
       bty ='n') # legenda
