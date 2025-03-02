# Príklad 6.1 (Pravdepodobnostná funkcia multihypergeometrického modelu)
## Naprogramujte funkciu dmultihypergeom(x, M), ktorá vypočíta hodnoty pravdepodobnostnej funkcie
## multihypergeometrického rozdelenia kde p = (5/30, 10/30, 15/30)
## Správnosť funkcie otestujte na výpočte, pricom vektor x je:
## a) x = (3, 6, 9)

M1 <- 5
M2 <- 10
M3 <- 15
M <- c(M1, M2, M3) # vektor M

dmultihyper <- function(x, M){
  Npop <- sum(M) # celkovy rozsah populacie vypocitany pomocou vektora M, Npop <- M1 + M2 + M3
  N <- sum(x) # rozsah nahodneho vyberu vypocitany pomocou vektora x
  px <- prod(choose(M,x)) / choose(Npop, N) # pravdepodobnostna funkcia MultiHyperGeom() 
  return(px)
}

## a) x = (3, 6, 9)
x <- c(3, 6, 9)
N <- sum(x)
cat('Vypocet pravdepodobnosti pre vektor x = (', x, '):', '\n', 
    'pomocou vlastnej funkcie:', round(dmultihyper(x, M), 4), '    ', 
    'pomocou vstavanej funkcie dmvhyper:', round(extraDistr::dmvhyper(x, M, sum(x)), 4))   ## (x = vyber, n = pocty M1, M2, M3, t.j. M; k = pocet vybranych, t.j. N = suma(x))

## b) x = (4, 5, 9)
x <- c(4, 5, 9)
N <- sum(x)
cat('Vypocet pravdepodobnosti pre vektor x = (', x, '):', '\n', 
    'pomocou vlastnej funkcie:', round(dmultihyper(x, M), 4), '    ', 
    'pomocou vstavanej funkcie dmvhyper:', round(extraDistr::dmvhyper(x, M, sum(x)), 4))   ## (x = vyber, n = pocty M1, M2, M3, t.j. M; k = pocet vybranych, t.j. N = suma(x))

## c) x = (5, 6, 7)
x <- c(5, 6, 7)
N <- sum(x)
cat('Vypocet pravdepodobnosti pre vektor x = (', x, '):', '\n', 
    'pomocou vlastnej funkcie:', round(dmultihyper(x, M), 4), '    ', 
    'pomocou vstavanej funkcie dmvhyper:', round(extraDistr::dmvhyper(x, M, sum(x)), 4))   ## (x = vyber, n = pocty M1, M2, M3, t.j. M; k = pocet vybranych, t.j. N = suma(x))


## d) x = (7, 6, 5)
x <- c(7, 6, 5)
N <- sum(x)
cat('Vypocet pravdepodobnosti pre vektor x = (', x, '):', '\n', 
    'pomocou vlastnej funkcie:', round(dmultihyper(x, M), 4), '    ', 
    'pomocou vstavanej funkcie dmvhyper:', round(extraDistr::dmvhyper(x, M, sum(x)), 4))   ## (x = vyber, n = pocty M1, M2, M3, t.j. M; k = pocet vybranych, t.j. N = suma(x))


# Príklad 6.2 (Výpočet pravdepodobností na základe multihypergeometrického modelu)
## Jana, Bára a Kája dostali horko-mliečny adventný kalendár, v ktorom je tretina čokolád horkých, tretina čokolád mliečnych a tretina čokolád biela. Príchute čokolád sú v kalendári rozmiestnené náhodne. Deti sa rozhodli čokolády rozdeliť rovným dielom, ale pretože je Kája najmenší, dovolili mu sestry, aby svoj diel čokolád zjedol ako prvý. Vypočítajte, aká je pravdepodobnosť, že Kája, bude mať vo svojom diele:
## a) dve horké, dve biele a štyri mliečne čokolády,
## b) štyri mliečne a štyri horké čokolády,
## c) maximálne dve čokolády horké,
## d) viac než polovicu čokolád mliečnych.

## Riesenie:
### CH1 horka
### CH2 mliecna
### CH3 biela

library(extraDistr) # nacitanie kniznice extraDistr
Npop <- 24 # celkovy pocet cokolad 
M <- c(8, 8, 8) # pocet roznych cokolad
N <- 8 # rozsah nahodneho vyberu cokolad
## a) dve horké, dve biele a štyri mliečne čokolády:
x = c(2, 2, 4)
p1 <- extraDistr::dmvhyper(x, M, N) # vypocet pravdepodobnosti

## b) štyri mliečne a štyri horké čokolády:
x = c(4, 4, 0)
p2 <- extraDistr::dmvhyper(x, M, N) # vypocet pravdepodobnosti

## c) maximálne dve čokolády horké:
x = 2 # !! len hypergeom
M = 8
p3 <- phyper(x, M, Npop - M, N) # vypocet pravdepodobnosti  

## d) viac než polovicu čokolád mliečnych:
x = 4
M = 8
p4 <- 1 - phyper(x, M, Npop - M, N) # vypocet pravdepodobnosti

round(data.frame(p1, p2, p3, p4), 4)  

# Príklad 6.3 (Pravdepodobnostná funkcia multihypergeometrického modelu)
## V príklade Príklad 6.2 sme stanovili, že počet horkých, mliečnych a bielych čokolád v Kájovom podiele sa bude riadiť multihypergeometrickým modelom
## Vykreslite graf pravdepodobnostnej funkcie rozdelenia 

## Riesenie:

M <- c(8,8,8)    # pocet horkych, mliecnych a bielych cokolad v celej populacie
N <- 8

### Priprava matice dat - 3 stlpce: pocet horkych, pocet mliecnych, pravdepodobnost
#### najprv matica 9 x 9 s poctami horkzy a mliecnych
X <- rep(0:8, 9) # postupnost 0, ..., 8, 0, ..., 8, 0, ..., 8 deväťkrát za sebou
  ## X reprezentuje pocet horkych
Y <- rep(0:8, rep(9, 9)) # takto alebo s parametrom each
Y <- rep(0:8, each = 9) # postupnost 0, ..., 0, 1, ..., 1, 8, ..., 8,
  ## rep(9, 9) je vektor (9,9,9,9,9,9,9,9,9), cize 9-krát 0, potom 9-krát 1...
  ## Y reprezentuje pocet mliecnych
  ## pocet bielych netreba, to vzdy ten zvysok do vyberoveho poctu N = 8
  
#### matica s hodnotami pravdepodobnosti
fxy <- matrix(NA, nrow = 9, ncol = 9) # priprava prazdnej matice NA hodnot o dimenzii 9x9
for (i in 0:8){ # vnorene cykly; kazdej dvojici x a y vypocitaju hodnotu p(x, y) 
  for(j in 0:8){
    fxy[i + 1, j + 1] <- extraDistr::dmvhyper(c(i, j, N - (i + j)), M, N)
    ## polia su od 1,1 po 9,9, pricom hodnoty su od 0,0 po 8,8, preto shift 
    ## na kazde miesto matice dame pravdepodobnost
  }
}

#### "vysledna" matica ako data pre graf: 1. stlpec horke, 2. stlpec mliecne, 3. stlpec pravdepodobnost
s_obs <- cbind(X, Y, c(fxy)) # spojenie vektorov X, Y, a c(fxy) po stlpcoch
  ## c(fxy) vrati jednorozmerny vektor z matice fxy

### Graf
barvy <- vypln <- rep('black', 81) # nastavenie ciernej farby vsetkych bodov
  ## takto mozno nastavit dve premenne
barvy[s_obs[, 3] != 0] <- 'darkred' 
# zmena farby bodov s nenulovou pravdepodobnostnou funkciou na cervenu 
vypln[s_obs[, 3] != 0] <- 'red' 
# zmena vyplne bodov s nenulovou pravdepodobnostnou funkciou na cervenu

library(scatterplot3d) # nacitanie kniznici scatterplot3d             
par(mar = c(4,4,1,1)) # nastavenie okraju grafu na 5, 4, 0, 1                
scatterplot3d(s_obs, type   = 'h', angle = 71, lwd = 1, pch = 16, color = barvy, 
              bg = vypln, xlab = 'počet horkých čokolád', ylab = 'počet mliečnych čokolád', zlab = 'absolútna početnosť')
# graf pravdepodobnostnej funkcie rozdelenia MultiHyperGeom(8, (1/3, 1/3, 1/3))           
  ## type = 'h' je zvisla ciara, 'p' by boli body
  ## angle = uhol otocenia 
  ## lwd = hrubka ciary
  ### pch = symbol pre bod (1 prazdny kruh, 16 plny kruh)


# Príklad 6.4 (Multihypergeometrický model)
## Podľa údajov uvedených v datasete 05 má Juhomoravský kraj k dátumu 30.6.2018 celkovo 1 184 381 obyvateľov,
## z toho 108 641 obyvateľov patrí do okresu Blansko, 379 275 obyvateľov patrí do okresu Brno-mesto, atď. 
## Predpokladajme, že chceme zostaviť reprezentatívnu vzorku obyvateľov pochádzajúcich z Juhomoravského 
## kraja. Náhodný vektor X = (X1, X2, ... Xk), ktorý popisuje rozdelenie počtu obyvateľov z jednotlivých 
## okresov Juhomoravského kraja v reprezentatívnej vzorke, má potom multihypergeometrické rozdelenie,
## teda X ~ MultiHyperGeom(N, p), kde p je vektor pravdepodobností výskytu obyvateľov z jednotlivých okresov 
## Juhomoravského kraja.
## 1) Vypočítajte odhad vektora p.

## Riesenie:
M <- c(108641, 379275, 221200, 115728, 154183, 91483, 113871) # pocet obyvatelov z jednotlivych okresov
Npop <- sum(M) # rozsah populacie  
okresy <- c('Blansko', 'Brno-město', 'Brno-venkov', 'Břeclav', 'Hodonín', 'Vyškov', 'Znojmo') 
  # nazvy 7 okresov 
p <- M/Npop # vektor parametrov p = (p1, ..., p7)
tab <- data.frame(rbind(p))
names(tab) <- okresy
round(tab, 4)

## 2) Určte, aké bude rozdelenie počtu obyvateľov z jednotlivých okresov v reprezentatívnej vzorke za predpokladu, že rozsah reprezentatívnej vzorky 
##bude: a) 580, b) 58 000, c) 550 000, d) 580 000, e) 900 000, f) 1 100 000.
## Riesenie:

juhom_kraj <- function(Npop, M, N){ # funkcia s povinnymi vstupnymi argumentami Npop, M - vektor, N - vyber
  p <- M/Npop  # vektor parametra p = (p1, ..., p7)
  pocet <- round(N %*% t(p)) # ocakavane absolutne pocetnosti v reprezentativnej vzorke 
  ## t(matrix) - vrati transponovanu maticu
  return(pocet)
}

N <- c(580, 58000, 550000, 580000, 900000, 1100000) # rozsah nahodneho vyberu
tab <- juhom_kraj(Npop, M, N) # tabulka ocakavanych absolutnych pocetnosti v reprezentativnej vzorke
tab <- data.frame(tab, row.names = paste('N =', N)) # prevod tab na data.frame
tab <- cbind(tab, apply(tab, 1, sum)) # doplnenie riadkovych suctov
names(tab) <- c(okresy, 'Sum') # zmena nazvu stlpcov tabulky tab (okresy + 'Sum')
tab

## 3) Stĺpcový diagram očakávaných absolútnych početností:
## Riesenie:

N <- 58000 # rozsah nahodneho vyberu N (pre graf (a))
n   <- c(juhom_kraj(Npop, M, N)) # ocakavane pocetnosti v reprezentativnaj vzorky N obyvatelov JM kraja
col <- c('bisque', 'brown4', 'firebrick1', 'darkorange', 'goldenrod1', 'darkseagreen', 'darkolivegreen1') # vektor 7 farieb http://www.stat.columbia.edu/~tzheng/files/Rcolor.pdf

par(mar = c(4,4,3,1)) # nastavenie okrajov grafu na 6, 4, 1, 1
barplot(n, col = col, axes = F, xlab = "", space = 0.3, width = 1, xlim = c(0.4, 9), 
        ylim = c(0, max(n) + 3000), density = 80,
        main = paste('Odhad početností pri výbere N = ', N, 'obyvateľov')) # stlpcovy diagram absolutnych pocetnosti
  ## space = medzera medzi stlpcami, 
  ## width = sirka stlpcov
  ## xlim = rozsah osi
  ## density = hustota srafovania
text(seq(0.8, 9, by = 1.3), n + 1500, n, cex = 0.8) # popis nad stlpcami
  ## seq = x-ové súradnice pre popisky
  ## n + 1500 = y-ov0 súradnice pre popisky
  ## n = hodnoty
  ## cex = velkost textu v % oproti standardnemu
text(seq(0.8, 9, by = 1.3), n + 750, paste(' (', round(n / N, 4) * 100, '%)', sep = ''),   
     cex = 0.8) # popis nad stlpcami
box(bty = 'o') # ram okolo grafu
text(seq(0.8, 9, by = 1.3), -700, okresy, cex.axis = 0.9, xpd = T, srt = - 47, adj = 0)  # popisky stlpcov pod osou x otocene o 47° smerom dolu
  ## cex.axis = velkost textu popiskov osi
  ## xpd = T - povoluje vykreslenie mimo plochu grafu
axis(2, las = 1) # os y        
  
  