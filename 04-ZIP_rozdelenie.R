# ZIP rozdelenie
## Ide o modifikáciu Poissonovho rozdelenia, kedy náhodný výber obsahuje veľké množstvo núl.
## Náhodná veličina X pochádza zo ZIP rozdelenia, ak X = 0 s pravdepodobnosťou p a 
## X ~ Pois(lambda) s pravdepodobnostou 1-p
## Pravdepodobnostná funkcia: p(x) = pI(x = 0) + (1-p) * f(x, lambda) pre x = 0, 1...
## Vlastnosti:
### E[X] = (1-p)*lambda
### Var[X] = (1-p)*lambda*(1+p*lambda)
  
install.packages("VGAM")

# VGAM::dzipois(x, lambda, p) - výpočet pravdepodobnostnej funkcie
# VGAM::pzipois(x, lambda, p) - výpočet distribučnej funkcie
# VGAM::rzipois(M, lambda, p) - generovanie pseudonáhodných čísel

# Príklad 4.1: Pravdepodobnostná funkcia ZIP modelu 
## Naprogramujte funkciu dzip(x, lambda, p), ktorá vypočíta hodnoty pravdepodobnostnej funkcie 
## ZIP rozdelenia ZIP(lambda, p) pre hodnotu x. Správnosť funkcie otestujte na výpočte p(x) pre x = 0, 1, 2
## v prípade, že X ~ ZIP(lambda, p), kde lambda = 0.89 a p = 0.5. Výsledky porovnajte s výsledkami funkcie VGAM::dzipois(...).

## Riešenie:
dzip <- function(x, lambda, p){ # funkcia s povinnymi vstupnymi argumentami x, lambda, p 
  px <- p * (x == 0) + (1-p)*lambda^x*exp(-lambda)/factorial(x)  # pravdepodobnostna funkcia rozdelenia ZIP(lambda, p)
  return(px) # vystupom funkcie bude hodnota ulozena v premennej px
}

### pre x = 0
x <- 0
dzip0 <- dzip(x=0, lambda = 0.89, p = 0.5); # vypocet pravdepodobnostnej funkcie rozdelenia ZIP(0.89, 0.5) (funkcia dzip())
vdzip0 <- VGAM::dzipois(0, 0.89, 0.5); # vypocet pravdepodobnostnej funkcie rozdelenia ZIP(0.89, 0.5) 
# (funkcia dzipois())
tab <- data.frame(dzip0, vdzip0)
names(tab) <- c('own function', 'VGAM::dzipois')
row.names(tab) <- paste('for x = ', x)

### pre x = 1
dzip1 <- dzip(x=1, lambda = 0.89, p = 0.5);
vdzip1 <- VGAM::dzipois(1, 0.89, 0.5); 
data.frame(dzip1, vdzip1)

### pre x = 2
dzip2 <- dzip(x=2, lambda = 0.89, p = 0.5);
vdzip2 <- VGAM::dzipois(2, 0.89, 0.5); 
data.frame(dzip2, vdzip2)

# Dataset 4 Operácia bedrového kĺbu 
## V rámci národnej štúdie boli zaznamenané počty primárnych a revíznych operácií bedrového kĺbu 
## vykonaných na Slovensku v období od 1. januára 2003 do 31. decembra 2012. Celkovo boli získané 
## údaje o 42349 operáciách, pričom v 41317 prípadoch išlo o operácie primárne (operácii nepredchádzala 
## žiadna iná operácia bedrového kĺbu) a v 1032 prípadoch išlo o operáciu revíznu (operácii predchádzala 
## aspoň jedna operácia bedrového kĺbu). Presné údaje o počte predchádzajúcich operácií u každého 
## pacienta sú uvedené v nasledujúcej tabuľke:
## Tabuľka 4.1: Operácia bedrového kĺbu
### 0	      1	    2	    3	    4	  spolu
### 41 317	907	  106	  18	  1	  42 349

#Príklad 4.2: Výpočet očakávaných početností za predpokladu ZIP modelu 
## Vezmite údaje z datasetu 04 a vypočítajte očakávané početnosti výskytu primárnych a revíznych 
## operácií bedrového kĺbu za predpokladu, že početnosti úrazov majú:
## a) ZIP rozdelenie ZIp(Lambda, p)
## odhad_p = (Var[n*m_obs] - E[n*m_obs])/(E[n*m_obs]^2 + Var[n*m_obs]- E[n*m_obs])
## odhad_lambda = E[n*m_obs] + Var[n*m_obs]/E[n*m_obs] - 1
## Var [..] a E[..] sú odhady z empirických dát
## b) Poissonovo rozdelenie Poiss(lambda_p) s parametrom:
## lambda_p = suma(n*m_obs)/suma(m_obs) .. de facto priemer

## Riesenie:
### nasetovanie dat
m_obs <- c(41317, 907, 106, 18, 1) # postupnost m_obs
n <- 0:4 # n

### odhad strednej hodnoty a rozptylu
observed <- rep(n, m_obs) # vektor pozorovanych dat: 0, ..., 1, ..., 1, ..., 4
M <- sum(m_obs) # celkovy pocet vsetkych operacii
m <- mean(observed) # odhad strednej hodnoty E[X] ziskany na zaklade vektora pozorovanych dat
v <- var(observed) # odhad rozptylu Var[X] ziskany na zaklade vektora pozorovanych dat

### odhad parametrov pre Poissonove a ZIP rozdelenie
#### Poisson rozdelenie
lambda_P <- sum(n*m_obs)/M # odhad parametra lambda Poissonovho rozdelenia, t.j. vlastne m
#### ZIP rozdelenie
p <- (v - m)/(m^2 +v - m) # odhad parametra p ZIP rozdelenia 
lambda <- m + v/m - 1 # odhad parametra lambda ZIP rozdelenia 
tab <- data.frame(lambda, p, lambda_P) # sumarizacna tabulka vysledkov
tab

### vypocet ocakavanych pocetnosti
#### vektor ocakavanych absolutnych pocetnosti za predpokladu Poissonovho rozdelenia
m_exp_pois <- round(dpois(n, lambda_P)*M)

#### vektor ocakavanych absolutnych pocetnosti za predpokladu ZIP rozdelenia
m_exp_zip <- round(VGAM::dzipois(n,lambda, p)*M) # vektor ocakavanych absolutnych pocetnosti 

tab <- data.frame(rbind(m_obs, m_exp_pois, m_exp_zip)) # tabulka pozorovanych a ocakavanych absolutnych pocetnosti 
tab <- data.frame(cbind(tab, apply(tab, 1, sum)), row.names = c("m_obs", "m_exp_Poiss", 
                                                                "m_exp_ZIP"))
names(tab) <- c(n, 'sum') # nazvy stlpcov tabulky tab
tab

#### Z tabuľky vidíme, že očakávané počty vypočítané za predpokladu ZIP modelu lepšie 
#### popisujú správanie pozorovaných počtov než počty vypočítané za predpokladu Poissonovho 
#### modelu.

# Príklad 4.3: Overdispersion a underdispersion v ZIP modeli 
## V príklade Príklad 4.2 sme vypočítali hodnoty očakávaných absolútnych počtov operácií 
## za podmienky, že údaje pochádzajú 
## a) zo ZIP rozdelenia ZIP(lambda = 0.2821, p = 0.9015)
## b) Poissonovho modelu Poiss(lambda_p = 0.0277) 
## Do jedného grafu teraz znázornite hodnoty pozorovaných počtov m_exp, 
## hodnoty očakávaných počtov za predpokladu Poissonovho rozdelenia m_exp_poiss
## a hodnoty očakávaných počtov za predpokladu ZIP rozdelenia m_exp_zip. 
## Na základe výsledného grafu určte, či v prípade použitia ZIP modelu dochádza 
## k overdisperzii alebo underdisperzii.
## Záver podložte porovnaním rozptylu vypočítaného z pozorovaných dát s rozptylom 
## vypočítaným z očakávaných dát pri využití ZIP i Poissonovho modelu.

par(mar = c(4,5,1,1)) # okraje grafu 4, 5, 1, 1
# graf s vertikalnymi cervenymi ciarami pozorovanej absolutnej pocetnosti 
plot(n, m_obs, type = 'h', col = 'red', xlab = 'poradie operácie', ylab = '', las = 1) 
# graf s vertikalnymi cervenymi ciarami pozorovanej absolutnej pocetnosti
lines(n - 0.1, m_exp_zip, col = 'orange', type = 'h') # oranzove vertikalne ciary ocakavanej absolutnej pocetnosti (ZIP) 
lines(n + 0.1, m_exp_pois, col = 'blue', type = 'h') # modre vertikalne ciary ocakavanej absolutnej pocetnosti (Poisson) 
points(n, m_obs, pch = 21, col = 'red',bg = rgb(1, 0, 0, 0.2), cex = 0.8) # cervene body
points(n - 0.1, m_exp_zip, pch = 21, col = 'orange', bg = rgb(1, 0.5, 0, 0.2),  cex = 0.8) # oranzovo-zlte body
points(n + 0.1, m_exp_pois, pch = 21, col = 'blue', bg = rgb(0, 0, 1, 0.2),  cex = 0.8) # modre body
mtext('absolútna početnosť', side = 2, line = 4) # popis osi y
legend('topright', lty = 1, pch = 19, col = c('red', 'orange', 'blue'), 
       pt.bg = c(rgb(1, 0, 0, 0.2), rgb(1, 0.5, 0, 0.2), rgb(0, 0, 1, 0.2)), 
       legend = c('m_obs', 'm_exp_ZIP', 'm_exp_Poiss'), bty = 'n') # legenda

## Pretože dátový súbor obsahuje mnoho núl a málo hodnôt odlišných od nuly, v grafe nie je 
## príliš zreteľný rozdiel v pozorovaných a očakávaných absolútnych početnostiach. Z tohto dôvodu 
## zmeníme škálu osi na logaritmickú.
log_m_exp_p <- log(m_exp_pois) 
log_m_exp_p[log_m_exp_p < 0] <- 0

log_m_exp_z <- log(m_exp_zip) 
log_m_exp_z[log_m_exp_z < 0] <- 0

log_m_obs <- log(m_obs) 
log_m_obs[log_m_obs < 0] <- 0

par(mar = c(4,5,1,1)) # okraje grafu 4, 5, 1, 1
# graf s vertikalnymi cervenymi ciarami pozorovanej absolutnej pocetnosti 
plot(n, log_m_obs, type = 'h', col = 'red', xlab = 'poradie operácie', ylab = 'absolútna početnosť', las = 1) 
# graf s vertikalnymi cervenymi ciarami pozorovanej absolutnej pocetnosti
lines(n - 0.1, log_m_exp_z, col = 'orange', type = 'h') # oranzove vertikalne ciary ocakavanej absolutnej pocetnosti (ZIP) 
lines(n + 0.1, log_m_exp_p, col = 'blue', type = 'h') # modre vertikalne ciary ocakavanej absolutnej pocetnosti (Poisson) 
points(n, log_m_obs, pch = 21, col = 'red',bg = rgb(1, 0, 0, 0.2), cex = 0.8) # cervene body
points(n - 0.1, log_m_exp_z, pch = 21, col = 'orange', bg = rgb(1, 0.5, 0, 0.2),  cex = 0.8) # oranzovo-zlte body
points(n + 0.1, log_m_exp_p, pch = 21, col = 'blue', bg = rgb(0, 0, 1, 0.2),  cex = 0.8) # modre body
legend('topright', lty = 1, pch = 21, col = c('red', 'orange', 'blue'), 
       pt.bg = c(rgb(1, 0, 0, 0.2), rgb(1, 0.5, 0, 0.2), rgb(0, 0, 1, 0.2)), 
       legend = c('log m_obs', 'log m_exp_ZIP', 'log m_exp_Poiss'), bty = 'n') # legenda

## Vypocitane variability:
observed <- rep(n, m_obs) # vektor pozorovanych dat
expected_zip <- rep(n, m_exp_zip) # vektor ocakavanych dat ZIP
expected_poiss <- rep(n, m_exp_pois) # vektor ocakavanych dat Poiss
tab <- data.frame(Var_obs = var(observed), Var_exp_ZIP = var(expected_zip), var_exp_Poiss = var(expected_poiss))
names(tab) <- c('observed', 'expected ZIP', 'expected Poisson')
tab               
### Pozorovane data su mierne overdispersed oproti Poissonovmu. Ale pri porovnani so ZIP je disperzia velmi podobna

# Príklad 4.4 (Graf pravdepodobnostnej a distribučnej funkcie ZIP modelu)
## Riesenie pre ZIP model
### vektor ocakavanych relativnych pocetnosti za predpokladu ZIP rozdelenia ZIP(lambda, p)
px_n <- VGAM::dzipois(0:4, lambda, p)

### graf hustoty
par(mar = c(4,4,1,1), mfrow = c(1, 1)) # nastavenie okrajov grafu (4, 4, 1, 1)
plot(n, px_n, type = 'h', ylab = 'p(x)', xlab = '', las = 1) 
points(n, px_n, pch = 21, col = 'purple', bg = 'pink')
mtext('x', side = 1, line = 2.1)
mtext(bquote(paste(lambda == .(round(lambda, 4)), '; ', p == .(round(p,4)))), side = 1, line = 3.2) 

### vektor ocakavanych kumulativnych relativnych pocetnosti za predpokladu ZIP rozdelenia ZIP(lambda, p)
Fx_n <- VGAM::pzipois(n, lambda, p)

### graf distribucnej funkcie
par(mar = c(4,4,1,1)) # nastavenie okrajov grafu (4, 4, 1, 1)
plot(n, Fx_n, type = 'n', xlim = c(-1,5), ylim = c(0, 1), xlab = '', ylab = 'F(x)', axes = F)
box(bty = 'o')
axis(1, -1:5, labels = c(-1:4, "5+"))
axis(2, las = 1)
segments(n, Fx_n, n + 1, Fx_n)
arrows(0, 0, -1, 0, length = 0.1)
arrows(4, 1, 5, 1, length = 0.1)

points(n, c(0, Fx_n[1:4]), col = 'black', pch = 21, bg = 'white')
points(n,Fx_n, col = 'purple', pch = 19, bg = 'pink')
mtext('x', side = 1, line = 2.1)
mtext(bquote(paste(lambda == .(round(lambda, 4)), '; ', p == .(round(p,4)))), side = 1, line = 3.2) 


## Riesenie pre Poissonov model
### vektor ocakavanych relativnych pocetnosti za predpokladu rozdelenia Poisson(lambda)
px_n <- dpois(0:4,lambda_P)

### graf hustoty
par(mar = c(4,4,1,1), mfrow = c(1, 1)) # nastavenie okrajov grafu (4, 4, 1, 1)
plot(n, px_n, type = 'h', ylab = 'p(x)', xlab = '', las = 1) 
points(n, px_n, pch = 21, col = 'purple', bg = 'pink')
mtext('x', side = 1, line = 2.1)
mtext(bquote(paste(lambda[p] == .(round(lambda_P, 4)))), side = 1, line = 3.2) 

### vektor ocakavanych kumulativnych relativnych pocetnosti za predpokladu rozdelenia NegBin(k, p)
Fx_n <- ppois(n, lambda_P)

### graf distribucnej funkcie
par(mar = c(4,4,1,1)) # nastavenie okrajov grafu (4, 4, 1, 1)
plot(n, Fx_n, type = 'n', xlim = c(-1,5), ylim = c(0, 1), xlab = '', ylab = 'F(x)', axes = F)
box(bty = 'o')
axis(1, -1:5, labels = c(-1:4, "5+"))
axis(2, las = 1)
segments(n, Fx_n, n + 1, Fx_n)
arrows(0, 0, -1, 0, length = 0.1)
arrows(4, 1, 5, 1, length = 0.1)

points(n, c(0, Fx_n[1:4]), col = 'black', pch = 21, bg = 'white')
points(n,Fx_n, pch = 21, col = 'purple', bg = 'pink')
mtext('x', side = 1, line = 2.1)
mtext(bquote(paste(lambda[p] == .(round(lambda_P, 4)))), side = 1, line = 3.2) 

# Príklad 4.5: Výpočet pravdepodobností na základe ZIP modelu
## Za predpokladu, že náhodná veličina X , ktorá udáva počet primárnych 
## a revíznych operácií, pochádza z ZIP rozdelenia, X ~ ZIP(lambda - 0.2821, p = 0.9015),
## vypočítajte pravdepodobnosť, že náhodne vybraný pacient podstúpil:
## a) iba primárnu operáciu,
az <- VGAM::dzipois(0, lambda, p) # vypocet pravdepodobnosti (ZIP)
ap <- dpois(0, lambda_P) # vypocet pravdepodobnosti (Poiss)

## b) presne jednu revíznu operáciu,
bz <- VGAM::dzipois(1, lambda, p) # vypocet pravdepodobnosti (ZIP)
bp <- dpois(1, lambda_P) # vypocet pravdepodobnosti (Poiss)

## c) minimálne jednu a najviac dve revízne operácie,
cz <- sum(VGAM::dzipois(1:2, lambda, p)) # vypocet pravdepodobnosti (ZIP)
cp <- sum(dpois(1:2, lambda_P)) # vypocet pravdepodobnosti (Poiss)

## d) aspoň jednu revíznu operáciu,
dz <- 1 - VGAM::dzipois(0, lambda, p) # vypocet pravdepodobnosti (ZIP)
dp <- 1 - dpois(0, lambda_P) # vypocet pravdepodobnosti (Poiss)

## e) najviac dve revízne operácie.
ez <- sum(VGAM::dzipois(0:2, lambda, p)) # vypocet pravdepodobnosti (ZIP)
ep <- sum(dpois(0:2, lambda_P)) # vypocet pravdepodobnosti (Poiss)

tab <- round(data.frame(rbind(c(az, bz, cz, dz, ez), c(ap, bp, cp, dp, ep)),
                        row.names = c('ZIP', 'Poisson')),
             4)
names(tab) = c('len primarna', 'práve 1 revízna', '1 až 2 revízne', 'aspoň 1 revízna', 'najviac 2 revízne')
tab

# Príklad 4.6: Stredná hodnota a rozptyl náhodnej veličiny zo ZIP modelu 
## Za predpokladu, že náhodná veličina , ktorá udáva počet primárnych a revíznych operácií, pochádza
## zo ZIP rozdelenia, t.j. X ~ ZIP(lambda = 0.2821, p = 0.9015) vypočítajte strednú hodnotu a rozptyl 
## tejto náhodnej veličiny . Strednú hodnotu a rozptyl porovnajte s ich odhadmi vypočítanými na:
## a) základe očakávaných dát,
## b) základe pozorovaných dát (Príklad 4.2).
## Výsledky porovnajte s odhadmi vypočítanými za predpokladu Poissonovho modelu, kde lambda_p = 0.0278

## Riesenie:
## vektor ocakavanych abs. pocetnosti je v m_exp_zip
## vektoro pozorovanych abs. pocetnosti je v observed

### Vypocet cez vzorce
E_X <- (1-p)*lambda # stredna hodnota E[X] rozdelenia ZIP(lambda, p) = (1-p)*lambda
Var_X <- (1-p)*lambda*(1+p*lambda) # rozptyl Var[X] rozdelenia ZIP(lambda, p) = (1-p)*lambda*(1+p*lambda)

### Vypocet na zaklade pozorovanych dat
observed <- rep(n, m_obs) # vektor pozorozovanych dat
E_obs <- mean(observed) # odhad strednej hodnoty na zaklade pozorozovanych dat.
Var_obs <- var(observed) # odhad rozptylu na zaklade pozorozovanych dat

### Vypocet na zaklade odhadnutych dat
expected_zip <- rep(n, m_exp_zip) # vektor ocakavanych dat ZIP(lambda, p)
E_exp_z <- mean(expected_zip) # odhad strednej hodnoty na zaklade ocakakavanych dat ZIP(lambda, p)
Var_exp_z <- var(expected_zip)  # odhad rozptylu na zaklade ocakavanych dat ZIP(lambda, p)
tab <- round(data.frame(E_X, E_obs, E_exp_z, Var_X, Var_obs, Var_exp_z), 4) # sumarizacna tabulka vysledkov
tab

### Porovnanie s Poissonom
expected_p <- rep(n, m_exp_pois) # vektor ocakavanych dat (Poiss)
E_exp_p <- mean(expected_p) # odhad strednej hodnoty na zaklade ocakavanych dat (Poiss)
Var_exp_p <- var(expected_p) # odhad rozptylu na zaklade ocakavanych dat (Poiss)
tab <- round(data.frame(rbind(E_obs, E_exp_z, E_exp_p), 
                  rbind(Var_obs, Var_exp_z, Var_exp_p),
                  row.names = c('pozorované dáta', 'očakávané dáta: ZIP', 'očakávané dáta: Poiss')
                  ), 4) 
names(tab) <- c('E(X)', 'Var(X)')
tab

# Príklad 4.7: Simulačná štúdia pre ZIP model a Poissonov model
## Pomocou simulačnej štúdie preskúmajte správanie náhodnej veličiny X, ktorá popisuje počet primárnych 
## a revíznych operácií bedrového kĺbu u náhodne vybraného pacienta. Vygenerujte náhodný výber o rozsahu M = 42 349,
## ktorý pochádza zo ZIP rozdelenia s parametrami lambda = 0.2821 a p = 0.9015.
## Vykreslite histogram tohto náhodného výberu a superponujte ho:
## a) očakávanými početnosťami za predpokladu ZIP modelu s parametrami a odhadnutými z náhodného výberu,
## b) očakávanými početnosťami za predpokladu Poissonovho modelu s parametrom odhadnutým tiež z náhodného výberu.
## Hodnoty očakávaných početností a pozorovaných početností navzájom porovnajte

## Riesenie:
M <- 42349 # celkovy pocet operacii M
X <- VGAM::rzipois(1:M, lambda, p) # vektor M pseudonahodnych cisel z rozdelenia ZIP(lambda, p)
N <- max(X) # maximalne vygenerovane cislo
m <- mean(X) # odhad strednej hodnoty E[X] ziskany z vektoru pseudonahodnych cisel
v <- var(X) # odhad rozptylu Var[X] ziskany z vektoru pseudonahodnych cisel

### Odhad parametrov
lambda_p <- m # odhad parametra lambda_p (Poiss) z vektoru pseudonahodnych cisel
lambda_z <- m + v/m - 1 # odhad parametra lambda (ZIP) z vektoru pseudonahodnych cisel
p <- (v - m)/(m^2 + v - m) # odhad parametra p (ZIP) z vektoru pseudonahodnych cisel

### Vypocet absolutnych pocetnosti
m_exp_z <- round(VGAM::dzipois(0:N, lambda_z, p)*M) # absolutne ocakavane pocetnosti (ZIP)
m_exp_p <- round(dpois(0:N, lambda_p)*M) # absolutne ocakavane pocetnosti (Poiss)

par(mar = c(5, 5, 1, 1)) # okraje grafu 5, 5, 1, 1
hist(X, breaks = seq(-0.5, N + 0.5, by = 1), prob = F, col = 'orange', density = 60, las = 1, xlab = '', ylab = '')
# histogram nahodneho vyberu v absolutnej skale
box(bty = 'o') # ramcek okolo grafu
x <- 0:(length(m_exp_z) - 1) # pomocny vektor
lines(x - 0.1, m_exp_zip, type = 'h', col = 'orange') # vertikalne ciary ocakavanych absolutnych pocetnosti (ZIP)
lines(x + 0.1, m_exp_pois, type = 'h', col = 'blue' ) # vertikalne ciary ocakavanych absolutnych pocetnost (Poiss)
points(x - 0.1, m_exp_zip, pch = 21, col = 'orange', bg = rgb(0.95,0.75,0.25,0.8)) # oranzovo-zlte body
points(x + 0.1, m_exp_pois, pch = 21, col = 'blue', bg = rgb(0.75,0.75,1,1)) # modre body
mtext('x', side = 1, line = 2.1) # popis osy x
mtext(bquote(paste(lambda == .(round(lambda_z, 4)), "; ",
                   p == .(round(p,4)), "; ", 
                   lambda[p] == .(round(lambda_p, 4)))),
      side = 1, line = 3.3) # druhy popis osy
mtext('Absolútna početnosť', side = 2, line = 3.6) # popis osy y
legend('topright', 
       lty = 1, #ciarocky
       pch = 21, # kruznice
       col = c('orange', 'blue'),
       pt.bg = c(rgb(0.95,0.75,0.25,0.8), rgb(0.75,0.75,1,1)),
       legend = c('ZIP', 'Poisson'),
       bty = 'n') # legenda
