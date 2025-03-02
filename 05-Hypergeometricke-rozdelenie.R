# Uvod
## Npop - populacia
## M - pocet jedincov s charakteristickou vlastnostou v populacii Npop
## Npop - M = pocet jedincov, ktori tu charakteristicku vlastnost nemaju

## nahodny vyber velkosti N z populacie Npop
## X ~ pocet jedincov s charakteristikou vo vybere velkosti N
## X ~ HyperGeom(N, p) p = M / Npop
## ohranicenie pre x:
###  x patri <max(N -(Npop - M), 0); min(M, N)>
  
# Príklad 5.1: Pravdepodobnostná funkcia hypergeometrického modelu 
  dhypergeom <- function(x, Npop, M, N){ # funkcia s povinnymi vstupnymi argumentami x, Npop, M, N
    px <- choose(M, x)*choose(Npop - M, N - x)/choose(Npop, N) # pravdepodobnostna funkcia rozdelenia HyperGeom
    return(px)
  }

dhypergeom(45, 350, 240, 70) # pravdepodobnostna funkcia rozdelenia HyperGeom(N = 70, M=240/Npop=350) (funkcia dhypergeom())
dhyper(45, 240, 350 - 240, 70) # pravdepodobnostna funkcia rozdelenia HyperGeom(70, 240/350)
## dhyper(x, M, Npop - M, N) - výpočet pravdepodobnostnej funkcie
tab <- data.frame(rbind(dhypergeom(45, 350, 240, 70), dhyper(45, 240, 350 - 240, 70)),
                  rbind(dhypergeom(50, 350, 240, 70), dhyper(50, 240, 350 - 240, 70)),
                  rbind(dhypergeom(53, 350, 240, 70), dhyper(53, 240, 350 - 240, 70)))
names(tab) <- c('x=45', 'x=50', 'x=53')
tab

# Príklad 5.2: Výpočet pravdepodobností na základe hypergeometrického modelu I 
# X = pocet mliecnych vo vybere
# X ~ HyperGeom(N,p)
# populacia = vsetky cokolady v adv kalendari
# charakteristika = cokolada je mliecna

Npop = 24
M = 12
N = 8 #Kajov vyber

## a) všetky mliečne čokolády:
p1 <- dhyper(8, M, Npop-M, N) # dhyper(x, M, Npop - M, N) - výpočet pravdepodobnostnej funkcie

## b) maximálne dve horké čokolády
### t.j. pocet mliecnych bude 6, 7 alebo 8, teda 1 - distribucnapre(5)
### phyper(x, M, Npop - M, N) - výpočet distribučnej funkcie
### polopate: phyper(x, splna v populacii, nesplna v populacii, velkost vyberu)

p2 <- 1 - phyper(5, M, Npop-M, N)

## c) viac než polovicu čokolád mliečnych:
### pocet mliecnych 5 az 8
p3 <- sum(dhyper((5:8), M, Npop-M, N))

tab <- data.frame(cbind(p1, p2, p3)) # tabulka vysledkov
tab

# Príklad 5.3: Odhad parametra hypergeometrického modelu 
Npop <- 1184381 # rozsah populacie
M <- 379275 # pocet statistickych jednotiek so sledovanou charakteristikou
N <- 10 # parameter N
p <- M/Npop # odhad parametra p

# Príklad 5.4: Graf pravdepodobnostnej a distribučnej funkcie hypergeometrického modelu
## vektor pravdepodobnosti pre x patri [0, 10] za predpokladu HyperGeom(N = 10, Npop = 350, M = 240)
px_n <- dhyper((0:N), M, Npop-M, N)

### graf hustoty
par(mar = c(6,4,1,1), mfrow = c(1, 1)) # nastavenie okrajov grafu (4, 4, 1, 1)
plot((0:N), px_n, type = 'h', ylim = c(0, 0.30), ylab = 'p(x)', xlab = '', axes = F) 
box(bty = 'o')
axis(1, las = 1)
axis(2, las = 1)
points((0:N), px_n, pch = 21, col = 'purple', bg = 'purple')
mtext('x', side = 1, line = 2.1)
mtext(bquote(paste(N[pop] == .(Npop), '; ', M == .(M))), side = 1, line = 3.4)
mtext(bquote(paste(N == .(N), '; ', p == .(round(p, 4)))), side = 1, line = 4.5) 

### vektor kumulativnych pravdepodobnosti za predpokladu HyperGeom rozdelenia
Fx_n <- phyper((0:N), M, Npop-M, N)

### graf distribucnej funkcie
par(mar = c(6,4,1,1)) # nastavenie okrajov grafu (4, 4, 1, 1)
plot((0:N), Fx_n, type = 'n', xlim = c(-1,N+1), ylim = c(0, 1), xlab = '', ylab = 'F(x)', axes = F)
box(bty = 'o')
axis(1, las = 1)
axis(2, las = 1)

segments((0:N), Fx_n, (1:(N+1)), Fx_n)
arrows(0, 0, -1, 0, length = 0.1)
arrows(N, 1, N+1, 1, length = 0.1)

points((0:N), c(0, Fx_n[1:N]), col = 'black', pch = 21, bg = 'white')
points((0:N),Fx_n, col = 'purple', pch = 19)
mtext('x', side = 1, line = 2.1)
mtext(bquote(paste(N[pop] == .(Npop), '; ', M == .(M))), side = 1, line = 3.4)
mtext(bquote(paste(N == .(N), '; ', p == .(round(p, 4)))), side = 1, line = 4.5) 

# Príklad 5.5: Výpočet pravdepodobností na základe hypergeometrického modelu II 
Npop <- 1184381 # rozsah populacie
M <- 379275 # pocet statistickych jednotiek so sledovanou charakteristikou
N <- 10 # parameter N
## a) najviac traja obyvatelia z okresu Brno-město,
p1 <- phyper(3, M, Npop - M, N)

## b) aspoň šesť obyvateľov z okresu Brno-město,
p2 <- 1 - phyper(5, M, Npop - M, N)

## c) žiadny obyvateľ z okresu Brno-město,
p3 <- dhyper(0, M, Npop - M, N)

## d) aspoň sedem obyvateľov z iného okresu,
## t.j. najviac 3 z Brno mesto
p4 <- phyper(3, M, Npop - M, N)
## iny sposob
p4 <- 1 - phyper(6, Npop - M, M, N)

## e) najviac štyria obyvatelia z iného okresu,
## t.j. aspon 6 z Brno mesto
p5 <- 1 - phyper(5, M, Npop - M, N)
p5 <- phyper(4, Npop - M, M, N)

## f) všetci obyvatelia z iného okresu.
## t.j. 0 z Brno mesto
p6 <- dhyper(0, M, Npop - M, N)
p6 <- dhyper(N, Npop - M, M, N)

tab_B <- data.frame(p1, p2, p3)
names(tab_B) <- c('najviac 3', 'aspoň 6', 'žiadny')
tab_B
tab_C <- data.frame(p4, p5, p6)
names(tab_C) <- c('aspoň 7', 'najviac 4', 'všetci')
tab_C

#Príklad 5.6: Aproximácia hypergeometrického modelu binomickým - stanovenie maximálneho rozsahu
Npop <- 1184381 # rozsah populacie
fS <- 0.05 # hodnota vyberoveho pomeru fs
N_opt <- fS * Npop # vypocet maximalneho rozsahu reprezentativnej vzorky
# Maximálny rozsah reprezentatívnej vzorky, pri ktorej je ešte možné aproximovať hypergeometrický model binomickým modelom je:
trunc(N_opt)
