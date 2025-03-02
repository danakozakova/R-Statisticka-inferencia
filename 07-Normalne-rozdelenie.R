# Normalne rozdelenie
## Základné funkcie v R:
## ! vstupom je sigma, nie sigma^2
## default mu = 0, sigma = 1
## dnorm(x, mu, sigma) - výpočet hustoty
## pnorm(x, mu, sigma) - výpočet distribučnej funkcie
## rnorm(M, mu, sigma) - generovanie pseudonáhodných čísel
## qnorm(alpha, mu, sigma) - kvantily

# Príklad 7.1 (Hustota normálneho modelu)
## Naprogramujte funkciu dnormal(x, mu, sigma2), ktorá počíta hodnoty hustoty normálneho rozdelenia  
## v hodnote x. Správnosť funkcie otestujte na výpočtochf(x), kde x = -1, 0, 1 pre X ~ N(mu, sigma2), 
## kde mi = 0, sigma^2 = 2

dnormal <- function(x, mu, sigma2) { # funkcia s povinnymi vstupnymi argumentami x, mu, sigma2
  sigma <- sqrt(sigma2)
  fx <- exp(-(x-mu)^2/(2*sigma2))/sqrt(2*pi*sigma2) # hustota rozdelenia N(mu, sigma^2)
  return(fx)
}

x <- c(-1, 0, 2)
mu <- 0
sigma2 <- 2
dnormal(x, mu, sigma2) # hustota rozdelenia N(0, 2); funkcia dnormal()
dnorm(x, mu, sqrt(sigma2)) # hustota rozdelenia N(0, 2); funkcia dnorm()

cat('Vypocet pravdepodobnosti pre vektor x = (', x, '):', '\n', 
    'pomocou vlastnej funkcie:', round(dnormal(x, mu, sigma2), 4), '    ', '\n',
    'pomocou vstavanej funkcie dmvhyper:', round(dnorm(x, mu, sqrt(sigma2)), 4))   ## (x = vyber, n = pocty M1, M2, M3, t.j. M; k = pocet vybranych, t.j. N = suma(x))

# Príklad 7.2 (Základné číselné charakteristiky spojitého znaku)
## Načítajte dátový súbor normal_distribution_clavicle. Nech je náhodná premenná X dĺžka kľúčnej kosti z ľavej 
## strany u mužov. Premenná X je spojitého typu. Pre dĺžku kľúčnej kosti z ľavej strany u mužov vytvorte tabuľku 
## základných číselných charakteristík.

getwd()
data <- read.delim("STUDY/MUNI/PREDMETY/St 18.00 Statisticka interferencia/cvicenia/cvicenie 7/clavicle.txt", sep = "\t") # nacitanie datoveho suboru
names(data)
str(data)
data_M <- na.omit(data[data$sex == "m", ]) # udaje pre muzov + odstranenie NA
l_LM <- data_M$length_L # vyber dlzok klucnych kosti z lavej strany
n <- length(l_LM) # rozsah nahodneho vyberu = dlzka lavej klucnej kosti u muzov 
m <- mean(l_LM) # vyberovy priemer
s <- sd(l_LM) # vyberova smerodajna odchylka
v <- s/m # vyberovy variacny koeficient
min <- min(l_LM) # minimalna namerena hodnota
max <- max(l_LM) # maximalna namerena hodnota
x0_25 <- quantile(l_LM, probs = 0.25, type = 2) # dolny kvartil
x0_50 <- quantile(l_LM, probs = 0.5, type = 2) # median
x0_75 <- quantile(l_LM, probs = 0.75, type = 2) # horny kvartil
IQR <- x0_75 - x0_25 # interkvartilove rozpatie
sikmost <- e1071::skewness(l_LM, type = 3) # koeficient sikmosti b1
spicatost <- e1071::kurtosis(l_LM, type = 3) # koeficient spicatosti b2
Xtab <- data.frame(rbind(c(round(n), 
                           round(m, 1), 
                           round(s, 2),
                           round(v, 2),
                           round(sikmost, 2), 
                           round(spicatost, 2)))) # suhrnna tabulka vysledkov
rownames(Xtab) <- 'lava' 
colnames(Xtab) <- c('   n', 'mean', 'std_dev', ' var_%', ' skew', ' kurt' )
Xtab

Xtab <- data.frame(rbind(c(min,
                           x0_25, 
                           x0_50,
                           x0_75,
                           max,
                           IQR))) # suhrnna tabulka vysledkov
rownames(Xtab) <- 'lava' 
colnames(Xtab) <- c('   min', 'lower_q', ' median', 'upper_q', '    max', '    IQR' )
Xtab

# Príklad 7.3 (Vizualizácia dát z normálneho rozdelenia)

##Načítajte dátový súbor normal_distribution_clavicle. Nech náhodná premenná X popisuje dĺžku kľúčnej 
## kosti z ľavej strany mužov. Pomocou histogramu a krabicového diagramu vhodne vizualizujte rozdelenie dĺžky 
## kľúčnej kosti z ľavej strany mužov. Histogram superponujte:
## a) krivkou jadrového odhadu hustoty,
## b) krivkou teoretickej hustoty normálneho rozdelenia N(mu, sigma^2). Hodnoty parametrov odhadnite na základe dát.

xfit <- seq(min, max,length.out = 512) # postupnost od min do max o dlzke 512
yfit <- dnorm(xfit, m, s ) # hustota rozdelenia N(m, s^2) pre postupnost xfit
r <- round(3.3 * log10(n) + 1) # Sturgesovo pravidlo;
# pocet tr. intervalu = 7, 176 - 130 = 46 -> 47 / 7 = 7... 
# opt. sirka = 7 -> seq(128, 177, by = 7)

par(mar = c(4, 4, 1, 1)) # okraje grafu 4, 4, 1, 1
hist(l_LM, prob = T, breaks = seq(128, 177, by = 7), axes = F, ylim = c(0, 0.05), col = 'yellow', 
     border = 'black', density = 60, main = "", xlab = 'dĺžka ľavej kľúčnej kosti (v mm) - muži', ylab = 'relatívna početnosť') 
## histogram dlzok lavych klucnych kosti
box(bty = 'o') # ram okolo grafu
axis(1, seq(131.5, 173.5, by = 7)) # os x, stredy triediacich intervalov
axis(2) # os y
lines(density(l_LM), col = "darkolivegreen2", lwd = 2) # krivka jadroveho odhadu hustoty
lines(xfit, yfit, col = "orange3", lwd = 2) # krivka hustoty N(m, s^2)

## boxplot
boxplot(l_LM, type = 2, xlab = "dĺžka ľavej kľúčnej kosti (v mm) - muži", 
        las = 1, horizontal = T, col = 'yellow', 
        border = 'black', medcol = 'darkkhaki')
points(m, 1, pch = 16, col = 'red') # aritmeticky priemer ako bod
legend("bottomright", lty = c(1, NA), pch = c(NA, 20), lwd = c(2, NA),
       col = c('darkkhaki', 'red'), legend = c('medián', 'priemer'), bty = 'n') # legenda


# Príklad 7.4 (Výpočet pravdepodobnosti na základe normálneho modelu I)
## Za predpokladu, že náhodná veličina X udávajúca dĺžku kľúčnej kosti z ľavej strany u mužov 
## pochádza z normálneho rozdelenia N(153,6; 9,95^2), vypočítajte pravdepodobnosť, že dĺžka kľúčnej 
## kosti z ľavej strany je:
## a) menšia než 140 mm,
## b) väčšia než 160 mm,
## c) v rozmedzí 150–160 mm,
## d) rovná 155 mm.

mu <- 153.6
sigma <- 9.95

p1 <- pnorm(140, mu, sigma)
p2 <- 1 - pnorm(160, mu, sigma)
p3 <- pnorm(160, mu, sigma) - pnorm(150, mu, sigma)
p4 <- 0
round(data.frame(p1, p2, p3, p4), 4)  

# Príklad 7.5 (Výpočet pravdepodobnosti na základe normálneho modelu II)
## Za predpokladu, že náhodná veličina X udávajúca dĺžku kľúčnej kosti z ľavej strany u mužov 
## pochádza z normálneho rozdelenia N(153,6; 9,95^2), vypočítajte pravdepodobnosť, že priemerná dĺžka 
## dĺžka piatich kľúčnych kostí z ľavej strany u mužov je:
## a) menšia než 140 mm,
## b) väčšia než 160 mm,
## c) v rozmedzí 150–160 mm,
## d) rovná 155 mm.

mu <- 153.6
sigma5 <- sqrt(9.95^2/5)

p1 <- pnorm(140, mu, sigma5)
p2 <- 1 - pnorm(160, mu, sigma5)
p3 <- pnorm(160, mu, sigma5) - pnorm(150, mu, sigma5)
p4 <- 0
round(data.frame(p1, p2, p3, p4), 4)  

# Príklad 7.6 (Graf funkcie hustoty a distrubučnej funkcie normálneho modelu)
## V príkladoch Príklad 7.2 a Príklad 7.3 sme odhadli hodnoty parametrov mu a sigma^2 normálneho 
## rozdelenia pre znak X, ktorý predstavuje dĺžku kľúčnej kosti z ľavej strany u mužov
## Nakreslite graf hustoty a distribučnej funkcie tohto normálneho rozdelenia.

mu <- 153.6
sigma <- 9.95
n <- 50
n1 <- 5
sigma5 <- sqrt(sigma^2/n1)
xfit <- seq(120, 180,length.out = 512) # postupnost od min 120 do max 180, s krokom 10

fx <- dnorm(xfit, mu, sigma) # hustota rozdelenia N(mu, sigma ^ 2) pre postupnost xfit
fx5 <- dnorm(xfit, mu, sigma5) # hustota rozdelenia N(mu, sigma ^ 2 / n) pre postupnost xfit

Fx <- pnorm(xfit, mu, sigma) # distribucna funkcia rozdelenia N(mu, sigma ^ 2) pre postupnost xfit
Fx5 <- pnorm(xfit, mu, sigma5) # distribucna funkcia rozdelenia N(mu, sigma ^ 2 / n) pre postupnost xfit

par(mar = c(5,4,1,1)) # okraje grafu 5, 4, 1, 1
plot(xfit, fx5, type = 'l', xlab = '', ylab = 'f(x)', col = 'red', lwd = 2, las = 1, ylim = c(0, 0.11)) 
# krivka hustoty N(mu, sigma ^ 2 / n); cervena

lines(xfit, fx, col='orange', lwd = 2) # krivka hustoty N(mu, sigma ^ 2); oranzova
mtext('dĺžka ľavej kľúčnej kosti (v mm) - muži', side = 1, line = 2.1) # popis osy x
mtext(paste('n = ', n), side = 1, line = 3.2) # druhy popis osy x
legend('topleft', lwd = c(2, 2), col = c('red', 'orange'), bty = 'n',
       legend = c('1 kľúčna kosť', 'priemer 5 kľúčnych kostí')) # legenda

plot(xfit, Fx5, type = 'l', xlab = '', ylab = 'f(x)', col = 'red', lwd = 2, las = 1, ylim = c(0, 1.2)) 
  ## krivka distribucnej funkcie N(mu, sigma ^ 2 / n); cervena
lines(xfit, Fx, col='orange', lwd = 2) # krivka distribucnej fukncie N(mu, sigma ^ 2); oranzova
mtext('dĺžka ľavej kľúčnej kosti (v mm) - muži', side = 1, line = 2.1) # popis osy x
mtext(paste('n = ', n), side = 1, line = 3.2) # druhy popis osy x
legend('topleft', lwd = c(2, 2), col = c('red', 'orange'), bty = 'n',
       legend = c('1 kľúčna kosť', 'priemer 5 kľúčnych kostí')) # legenda

# Príklad 7.7 (Simulačná štúdia)
## Na základe simulačnej štúdie overte, že ak X ~ (mu, sigma^2), potom Xpriemer ~ (mu, sigma^2/n). 
## Zvolte mu = 153.60 a sigma^2 = 9.952:
##  a) n = 5,
##  b) n = 50,
##  c) n = 100.
  
##  Vygenerujte M pseudonáhodných výberov, kde M = 1000. Pre každý výber vypočítajte realizáciu 
## aritmetického priemeru xpriemer_mn. Následne vygenerujte histogram pre hodnoty xpriemer_mn a superponujte
## ho teoretickou krivkou hustoty pre Xpriemer_n.Pre všetky tri prípady a), b) a c) vypočítajte 
## Pr(Xpriemer_n > 152) na zaklade empirickeho a teoretickeho rozdelenia Xpriemer_n. Pravdepodobnosti porovnajte.


simulace_mean <- function(n, mu, sigma, M = 1000, vypis = TRUE) {
  m <- replicate(1000, mean(rnorm(n, mean = mu, sd = sigma))) # M = 1000 vyberovy priemer n hodnot
  xfit <- seq(min(m)-5, max(m)+5, length.out = 512) # postupnost od min(m) - 5 do max(m) + 5 o dlzke 512
  yfit <- dnorm(xfit, mu, sqrt(sigma^2/n)) # hustota rozdelenia N(mu, sigma ^ 2 / n) pre xfit
  par(mar = c(5, 4, 1, 1)) # okraje grafu 5, 4, 1, 1
  hist(m, prob = T, ylim = c(0, max(hist(m, plot = F)$density) + 0.025),
       col = 'gold', xlab = '', ylab = 'f(x)', main = '') 
  ## histogram 1000 vyberovych priemerov
  box(bty = 'o') # ram okolo grafu
  lines(xfit, yfit, col = 'red', lwd = 2) # krivka hustoty N(mu, sigma ^ 2 / n)
  mtext(expression(paste(bar(X)[n])), side = 1, line = 2.1) 
  ## popis osy x
  mtext(bquote(paste('X - N(', 
                     .(mu), 
                     ', ', 
                     .(round(sigma, 2))^2,
                     '); n = ',
                     .(n))), 
        side = 1, line = 3.2, cex = 0.9) 
  ## druhy popis osy x
  tab <- data.frame(teor = 1 - pnorm(152, mu, sqrt(sigma^2 / n)), exact = sum(m > 152) / M) 
  if (vypis == T) {return(tab)} # pokial vypis == T, vrati ako vystup premennu tab
}

m <- 153.60
s <- 9.952

sim1 <- simulace_mean(n = 5, mu = m, sigma = s) # simulacia pre a) n = 5
sim2 <- simulace_mean(n = 50, mu = m, sigma = s) # simulacia pre b) n = 50
sim3 <- simulace_mean(n = 100, mu = m, sigma = s) # simulacia pre c) n = 500
tab <- round(data.frame(t(rbind(sim1, sim2, sim3))), 4) # suhrnna tabulka vysledkov troch simulacii
names(tab) <- c("n = 5", "n = 50", "n = 100") # pomenovanie stlpcov tabulky tab
tab

