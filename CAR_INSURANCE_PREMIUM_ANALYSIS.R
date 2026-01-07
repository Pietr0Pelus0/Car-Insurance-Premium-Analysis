INS<-read.csv(file="car_insurance_premium_dataset.csv")


# Questo file contiene 1.000 righe di dati sulle assicurazioni auto. Ogni riga rappresenta un unico conducente e il relativo premio assicurativo, calcolato in base a diversi fattori come l'età, l'esperienza di guida, la storia degli incidenti e altro ancora.

# Driver age (AGE): 
# L'età del conducente in anni. È un fattore chiave che influenza il premio assicurativo.
AGE<-INS$Driver.Age

# Driver Experience (EXP): 
# Il numero di anni di guida del conducente. Limitata a "massimo valore di età del conducente - 18 ".
EXP<-INS$Driver.Experience

# Previous Accidents (ACC):
# Il numero di incidenti in cui il conducente è stato coinvolto nel corso della sua storia di guida.
ACC<-INS$Previous.Accidents

# Annual Mileage (x1000 km):
# Il chilometraggio annuale totale percorso dal conducente, misurato in migliaia di chilometri.
MIL<-INS$Annual.Mileage..x1000.km.

# Car Manufacturing Year:
# L'anno di produzione dell'auto. Le auto più vecchie possono aumentare leggermente il premio assicurativo.
CY<-INS$Car.Manifacturing.Year

# Car Age:
# L'età dell'auto in anni, calcolata come 2025 - anno di produzione dell'auto.
CA<-INS$Car.Age

# Insurance Premium ($):
# Il premio assicurativo finale calcolato per il conducente, in base alle altre caratteristiche.
PREMIUM<-INS$Insurance.Premium

# Rinominiamo per semplicità le colonne del dataset con i nuovi nomi assegnati alle variabili
colnames(INS)<-c("AGE","EXP","ACC","MIL","CY","CA","PREMIUM")

head(INS)


# 1) ANALISI DESCRITTIVA DEL DATASET ---------------------------------------------------------------------------------------------------
# "Fare un’analisi descrittiva del dataset intero e, nel caso possa avere senso, anche delle variabili divise in base a una variabile qualitativa presente nel dataset."


# Cominciamo visualizzando la matrice di scatterplots dele variabili del dataset
par(mfrow=c(2,3), mar=c(2,2,2,2))
K=1
for(i in 1:2)
{
  for(j in 1:3)
  {
    plot(PREMIUM~INS[,K], main=paste0(colnames(INS)[K]," vs PREMIUM"), type="p",col="black", xlab=paste0("C",i),ylab=paste0("C",j))
    K=K+1
  }
}

# Visualizziamo qualche informazione su indici di posizione e quantili di ciascuna variabile
summary(INS)

# Identifichiamo le distribuzioni delle variabili chiave
ECDF_PR<-ecdf(PREMIUM)
plot(ECDF_PR, main="FUNZIONE DISTRIBUZIONE 'PREMIUM'")

ECDF_AGE<-ecdf(AGE)
plot(ECDF_AGE, main="FUNZIONE DISTRIBUZIONE 'AGE'")

ECDF_ACC<-ecdf(ACC)
plot(ECDF_ACC, main="FUNZIONE DISTRIBUZIONE 'ACC'")

# Osserviamo la densità di probabilità della variabile PREMIUM
par(mfrow=c(1,1), mar=c(2,2,2,2))
DENS_PR<-density(PREMIUM)
hist(PREMIUM,, main="DENSITÀ DI PROBABILITÀ 'PREMIUM'", xlab="Premio",ylab="Densità", breaks=25, col=rgb(0.2,0.4,0.6,0.5), freq=FALSE)
lines(DENS_PR)

# Grafichiamo il Boxplot della variabile PREMIUM sovrapposto allo Stripchart per vedere la distribuzione dei dati
boxplot(PREMIUM, main="BOXPLOT di 'PREMIUM'", 
        xlab="($)",
        col = "red",
        border = "black", 
        horizontal=TRUE)
#par(new=TRUE)
#stripchart(PREMIUM, method="stack", offset=.8, at =0.6, pch=20)

# Identifichiamo gli outliers attraverso la definizione con la distanza interquantile
Q1<-quantile(PREMIUM, 0.25)
Q3<-quantile(PREMIUM, 0.75)
IQR<-Q3-Q1
outliers<-PREMIUM[PREMIUM<(Q1-1.5*IQR) | PREMIUM>(Q3+1.5*IQR)]
print(outliers)

# Notiamo che non ce ne sono

# Suddividiamo il dataset in fasce d'età
U25<-sum(AGE<=25)
U35<-sum(AGE>25 & AGE<=35)
U45<-sum(AGE>35 & AGE<=45)
U55<-sum(AGE>45 & AGE<=55)
U65<-sum(AGE>55 & AGE<=65)

# Registriamo le frequenze assolute, relative e percentuali di ciascuna fascia
FREQ_ASS<-c(U25,U35,U45,U55,U65)
print(FREQ_ASS)
FREQ_REL<-(FREQ_ASS/1000)
print(FREQ_REL)
FREQ_PERC<-round(FREQ_REL*100,2)
print(FREQ_PERC)

NAMES<-c("U25","U35","U45","U55","U65")

#Valutiamo la segmetazione per età

BAR_PERC<-barplot(FREQ_PERC, 
                  names.arg=NAMES, 
                  main="FREQUENZA PERCENTUALE FASCE D'ETÀ",
                  xlab = "Range d'età", 
                  ylab = "Frequenza percentuale",
                  col = "orange", 
                  border = "black",
                  ylim = c(0, max(FREQ_PERC) + 10))

text(x = BAR_PERC, 
     y = FREQ_PERC, 
     labels = paste(FREQ_PERC, "%"), 
     pos = 3, 
     cex = 1.1, 
     col = "black")

# Consideriamo la segmentazione per età anche sui premi per osservare eventuali variazioni
PR_U25<-PREMIUM[AGE<=25]
PR_U35<-PREMIUM[AGE>25 & AGE<=35]
PR_U45<-PREMIUM[AGE>35 & AGE<=45]
PR_U55<-PREMIUM[AGE>45 & AGE<=55]
PR_U65<-PREMIUM[AGE>55 & AGE<=65]

BOX_PR_AGE<-boxplot(PR_U25,PR_U35,PR_U45,PR_U55,PR_U65,
                    names=NAMES,
                    main="DISTRIBUZIONE PREMI PER FASCE D'ETÀ",
                    xlab = "Range d'età",
                    ylab = "Premio($)",
                    col = "magenta",
                    border = "black")

# Osserviamo che all'aumentare dell'età (e, conseguentemente, dell'esperienza di guida), c'è una diminuzione della mediana dei premi assicurativi tra le varie fasce.


# Valutiamo variazioni nello storico di incidenti considerando la suddivisione per fasce d'età alla ricerca di eventuali correlazioni tra le due variabili
ACC_U25<-ACC[AGE<=25]
ACC_U35<-ACC[AGE>25 & AGE<=35]
ACC_U45<-ACC[AGE>35 & AGE<=45]
ACC_U55<-ACC[AGE>45 & AGE<=55]
ACC_U65<-ACC[AGE>55 & AGE<=65]


BOX_ACC<-boxplot(ACC_U25,ACC_U35,ACC_U45,ACC_U55,ACC_U65,
                 names=NAMES, 
                 main="STORICO INCIDENTI PER FASCE D'ETÀ",
                 xlab = "Range d'età", 
                 ylab = "N° Incidenti",
                 col = "light blue", 
                 border = "black")

# Ricaviamo che l'età non influisce significativamente sulla realizzazione di incidenti.


# Segmentiamo ancora i dati in categorie di rischio, sulla base dello storico di sinistri registrati
BASSO_RISCHIO<-PREMIUM[ACC==0]
MEDIO_RISCHIO<-PREMIUM[ACC>0 & ACC<=2]
ALTO_RISCHIO<-PREMIUM[ACC>=3]

BOX_PR_ACC<-boxplot(BASSO_RISCHIO, MEDIO_RISCHIO, ALTO_RISCHIO,
                    names=c("Basso","Medio","Alto"),
                    main="DISTRIBUZIONE PREMI PER RISCHIO",
                    xlab="Fascia di Rischio",
                    ylab="Premio($)",
                    col = c("green","yellow","red"), 
                    border = "black")

# Osserviamo che all'aumentare del rischio (sulla base della classe a cui si appartiene), c'è un aumento della mediana dei premi assicurativi tra le varie fasce


# Calcoliamo la matrice di correlazione per valutare il rapporto tra le variabili del dataset.
# install.packages("corrplot")
library(corrplot)
corr_mat<-cor(INS)
corrplot(corr_mat, main="\n      MATRICE DI CORRELAZIONE",tl.col="black", tl.srt=45)





# 2) MODELLIZZAZIONE DEL DATASET ---------------------------------------------------------------------------------------------------


# "Pensare ad un possibile modello lineare (standard o generalizzato) a partire dal dataset fornito. 
# Interpretare le relative statistiche (e i p-values); 
# Valutare se è il caso di rimuovere alcune variabili dal modello lineare considerato e ripetere una stima per un modello aggiustato in base a queste considerazioni; procedere con un’accurata analisi dei residui del modello considerato migliore."


# Cominciamo cercando di costruire un Modello di Regressione Lineare Multipla che possa ben interpretare i dati.


LM_MULT_TOT<-lm(PREMIUM ~ AGE + EXP + ACC + MIL + CA)
summary(LM_MULT_TOT)
plot(LM_MULT_TOT)

# Il p-value relativo all'F-statistica per valutare i risultati del test di dipendenza lineare globale è infinitesimo: è possibile dunque asserire che esiste almeno un coefficiente relativo ad uno dei regressori scelto non nullo.
# A supporto di ciò, i p-values relativi all'efficienza dei singoli regressori sono bassissimi, testimoniando una grande efficacia nella modellizzazione.

# Circa l'adattamento del modello ai dati, osserviamo di trovarci in un caso limite per cui il valore di R^2 risulta essere proprio pari a 1 (probabilmente dovuto alla natura artificiale dei dati), che si interpreta col fatto che la variabilità dei premi assicurativi è descritta al 100% dal modello considerato.

# Circa i residui, analizziamo i grafici di diagnosi realizzati in automatico.

# · Residuals vs Fitted verifica l'ipotesi di linearità e omoschedasticità dei residui; l'aspettativa è di osservare una distribuzione omogenea dei punti attorno allo zero, senza pattern evidenti.
# Tale pretesa è pienamente rispettata nel caso in analisi; si osserva anche che i residui sono dell'ordine di 10^(-12), praticamente infinitesimi.

# · Normal Q-Q testa la normalità del vettore dei residui, necessaria alla fase di test di ipotesi e intervalli di confidenza; in un buon modello i punti dovrebbero seguire la linea guida del grafico, proprio come accade nel nostro caso.

# · Scale-Location approfondisce l'analisi sull'omogeneità della varianza dei residui, tenendo come guida la direzione della linea rossa: se rimane orizzontale, non riscontra trend crescenti/decrescenti, sottolineando una buona distribuzione dei dati.

# · Residuals vs Leverage identifica eventuali osservazioni influenti che possono avere un peso sproporzionato sulla regressione. Un buon modello presenta una concentrazione dei punti attorno all'origine degli assi del riferimento considerato, cioè con basso leverage e bassi residui.


# Nel nostro modello di riferimento, in conclusione, l'analisi conferma che i residui relativi risultano praticamente infinitesimi, lineari, omoschedastici e perfettamente distribuiti secondo legge Gaussiana.


# Riduciamo la dimensionalità del modello basandoci sulla matrice di correlazione e selezionando le varibili più esplicative.
LM_MULT_AEA<-lm(PREMIUM ~ AGE + EXP + ACC)
summary(LM_MULT_AEA)
plot(LM_MULT_AEA)

# Anche in questo caso i p-values relativi ai test di dipendenza lineare e di significatività dei regressori restituiscono risultati positivi.
# Pur avendo ridotto il numero di regressori di due, il valore dell'R^2 rimane comunque alto, arrivando a spiegare circa il 97% della variabilità del dataset.
# I residui risultano lineari e con varianza uniforme, distribuiti non proprio con legge normale ma con code leggermente più leggere.



# Cerchiamo a questo punto di creare un Modello Lineare Generalizzato.
# Vanno scelti la distribuzione di riferimento nella famiglia esponenziale e la relativa link function.
# Per effettuare una scelta accurata, analizziamo dapprima la distribuzione della variabile risposta.

# Verifichiamo la similitudine della variabile PREMIUM con una distribuzione Gamma attraverso un QQ-plot.

# Calcolo i parametri di forma e di scala
theta<-(mean(PREMIUM)/sd(PREMIUM))^2
k<-var(PREMIUM)/mean(PREMIUM)

n<-length(PREMIUM)

# Calcolo i quantili della distribuzione teorica
gamma_quant<-qgamma(ppoints(n), shape=theta, scale=k)

qqplot(sort(PREMIUM), gamma_quant, main="QQ-PLOT PREMIUM vs GAMMA", xlab="Quantili empirici", ylab="Quantili teorici")
abline(0,1, col="red")

# Osserviamo una chiara compatibilità della variabile PREMIUM con la distribuzione Gamma.

# Verifichiamo la simmetria dei dati, per ricorrere eventualmente a delle trasformazioni che li simmetrizzino.

MEDIA<-mean(PREMIUM)
DEV<-sd(PREMIUM)

#Confronto media-mediana
CFR<-(MEDIA-median(PREMIUM))/DEV
print(CFR)
# Risulta un valore negativo, ma abbastanza centrale.

NUM<-sum(PREMIUM-MEDIA)^3
SKEWNESS<-DEV^(-3)*(NUM)/1000
print(SKEWNESS)
# Pur non essendo uno 0 nominale, l'asimmetria si attesta a valori infinitesimi.
# Dunque non sono necessarie trasformazioni.

# Scegliamo come distribuzione la distribuzione Gamma e come link function la funzione logaritmo.

GLM_TOT<-glm(PREMIUM ~  AGE + EXP + ACC + MIL + CA,
             family=Gamma(link="log"))
summary(GLM_TOT)
plot(GLM_TOT)

# I residui utilizzati di default nei grafici diagnostici risultano più adatti a modelli di regressione lineare che a glm, a causa della dipendenza della varianza della variabile risposta dalla media.
# Dunque occorre optare per una nuova definizione di residui.

#install.packages("statmod")
library(statmod)

resid<-residuals(GLM_TOT)
resid_deviance<-residuals(GLM_TOT, type="deviance")
resid_pearson<-residuals(GLM_TOT, type="pearson")
resid_quantile<-qresid(GLM_TOT)

fit_val<-fitted(GLM_TOT)

head(data.frame(STD=resid, DEV=resid_deviance, PEARS=resid_pearson, QNT=resid_quantile))

# Residuals vs Fitted
plot(fit_val, resid_deviance,
     main="Deviance Residuals vs Fitted",
     xlab="Fitted Values",
     ylab="Deviance Residuals")
abline(h=0, lty=2)

plot(fit_val, resid_pearson,
     main="Pearson Residuals vs Fitted",
     xlab="Fitted Values",
     ylab="Pearson Residuals")
abline(h=0, lty=2)

plot(fit_val, resid_quantile,
     main="Quantile Residuals vs Fitted",
     xlab="Fitted Values",
     ylab="Quantile Residuals")
abline(h=0, lty=2)

# QQ-Plot
qqnorm(resid_deviance, main="QQ-PLOT DEVIANCE RESIDUALS")
qqline(resid_deviance, col="red")

qqnorm(resid_pearson, main="QQ-PLOT PEARSON RESIDUALS")
qqline(resid_pearson, col="red")

qqnorm(resid_quantile, main="QQ-PLOT QUANTILE RESIDUALS")
qqline(resid_quantile, col="red")

# Scale-Location
plot(fit_val, sqrt(abs(resid_deviance)),
     main="SCALE-LOCATION (DEVIANCE)",
     xlab="Fitted Values",
     ylab="√|Deviance Residuals|")
abline(h=0, col="red")

plot(fit_val, sqrt(abs(resid_pearson)),
     main="SCALE-LOCATION (PEARSON)",
     xlab="Fitted Values",
     ylab="√|Pearson Residuals|")
abline(h=0, col="red")

plot(fit_val, sqrt(abs(resid_quantile)),
     main="SCALE-LOCATION (QUANTILE)",
     xlab="Fitted Values",
     ylab="√|Quantile Residuals|")
abline(h=0, col="red")

# Residuals vs Leverage
plot(GLM_TOT, which=5)



#GLM con distribuzione inversa gaussiana e link function logaritmo
GLM_TOT_2<-glm(PREMIUM ~ AGE + EXP + ACC + MIL + CA, data = INS, family = inverse.gaussian(link = "log"))
summary(GLM_TOT_2)
plot(GLM_TOT_2)

resid_quantile<-qresid(GLM_TOT_2)
fit_val<-fitted(GLM_TOT_2)

plot(fit_val, resid_quantile,
     main="Quantile Residuals vs Fitted",
     xlab="Fitted Values",
     ylab="Quantile Residuals")
abline(h=0, lty=2)

qqnorm(resid_quantile, main="QQ-PLOT QUANTILE RESIDUALS")
qqline(resid_quantile, col="red")

plot(fit_val, sqrt(abs(resid_quantile)),
     main="SCALE-LOCATION (QUANTILE)",
     xlab="Fitted Values",
     ylab="√|Quantile Residuals|")
abline(h=0, col="red")

plot(GLM_TOT_2, which=5)





# 3) ANALISI IN COMPONENTI PRINCIPALI ---------------------------------------------------------------------------------------------------


# "Fare un’analisi in componenti principali".

INS_SCALED<-scale(INS)
PCA<-prcomp(INS_SCALED, center=TRUE, scale.=TRUE)
summary(PCA)

# Analizziamo la matrice dei loadings, cioè la matrice del cambiamento di base da quella canonica a quella delle componenti prncipali appena calcolate.
# Tale matrice ha per colonne i vettori calcolati dall'algoritmo, corrispondenti agli autovettori della matrice di varianza/covarianza del dataset.
print(PCA$rotation)


SDE<-summary(PCA)$importance[1,]
plot(SDE, main="DEVIAZIONE STANDARD COMPONENTI PRINCIPALI", type="b", xlab="Componenti", ylab="SDE", lwd=2)

# Osserviamo che la deviazione standard delle prime cinque componenti principali si aggira su un ordine di grandezza completamente differente rispetto alle rimanenti due.

# Di particolare interesse è la vicinanza dei valori di sde tra PC1 e PC2 e in seguito PC3 e PC4.

PROP_VAR<-summary(PCA)$importance[2,]*100
VAR<-barplot(PROP_VAR, names.arg=c("C1","C2","C3","C4","C5","C6","C7"), 
             main="% VARIANZA SPIEGATA DALLE PC",
             #type="b",
             xlab = "Componenti Principali", 
             ylab = "% Varianza Cumulata",
             col = "green", 
             border = "black",
             ylim=c(0,50))

text(x = VAR, 
     y = PROP_VAR, 
     labels = paste(round(PROP_VAR,1), "%"), 
     adj = 2,
     pos = 3, 
     cex = 1, 
     col = "black")

lines(VAR, PROP_VAR, type="o")

# Allo stesso modo, non si osserva una concentrazione drastica della percentuale di varianza spiegata dalle prime componenti, che risulta quasi omogeneamente distribuita per coppie:
# · PC1 36%,
# · PC2 28%,
# e soprattutto
# · PC3 15%,
# · PC4 14%.

# Risulta dunque che la varianza complessiva spiegata è solo del 65% per le prime due componenti, migliorando accettabilmente all'80% se si considera anche la terza e del 94% aggiungendo infine PC4.

# "Denotiamo con Ci la i-esima componente principale. 
# Rappresentare i plot della proiezione dei dati nei piani CiCj con i, j = 1, 2, 3 (chiaramente i≠j). 
# Se possibile, disporre questi plot in una matrice di grafici. 
# Confrontare questi grafici." 

SCORES<-PCA$x

par(mfrow=c(3,3), mar=c(2,2,2,2))
for(i in 1:3)
{
  for(j in 1:3)
  {
    if(i==j)
    {
      plot(PCA, col="green")
    }
    else
    {
      plot(SCORES[,i],SCORES[,j], 
           main=paste0("PROIEZIONE SU C",i,"C",j), 
           type="p",col="black", 
           xlab=paste0("C",i),
           ylab=paste0("C",j),
           asp=1)
      
    }
  }
}


# "Realizzare un plot rappresentante la proporzione di varianza cumulata spiegata dalle prime i componenti. Le proiezioni sul piano C1C2 che proporzione di varianza spiegano?"

par(mfrow=c(1,1), mar=c(2,2,2,2))
PROP_VAR_CUM<-summary(PCA)$importance[3,]*100

VAR_PERC<-barplot(PROP_VAR_CUM,
                  names.arg=c("C1","+C2","+C3","+C4","+C5","+C6","+C7"), 
                  main="% VARIANZA CUMULATA DELLE PC",
                  type="b",
                  xlab = "Componenti Principali", 
                  ylab = "% Varianza Cumulata",
                  col = "red", 
                  border = "black",
                  ylim=c(0,110))

text(x = VAR_PERC, 
     y = PROP_VAR_CUM, 
     labels = paste(round(PROP_VAR_CUM,1), "%"), 
     pos = 3, 
     cex = 1.1, 
     col = "black")

# Poichè  le componenti principali sono tra loro scorrelate, la percentuale di varianza spiegata dal piano C1C2 è pari alla somma della proporzione di varianza spiegata dalle prime due componenti principali, cioè ~65%.

# "Rappresentare nel plot delle proiezioni su C1C2 anche la bontà della rappresentazione tramite gli indicatori cos2.
# Ci sono dati che sono mal rappresentati?
# Rappresentare nel plot delle variabili i loro contributi alle componenti principali tramite gli indicatori cos2.
# Quali variabili partecipano maggiormente in queste componenti?"


#install.packages("factoextra")
library(ggplot2)
library(factoextra)

# Per sintetizzare le informazioni ottenute attraverso l'implemetazione della PCA, costruiamo un biplot.
fviz_pca_var(PCA, 
             label="var", 
             col.var="cos2", 
             gradient.cols=c("#C00000","#FFC000","#00A650"), 
             arrow.size=10,
             labelsize=5, repel=TRUE) +
  scale_x_continuous(name="PC1 (36.1%)") +
  scale_y_continuous(name="PC2 (28.5%)")

fviz_pca_ind(PCA, 
             label="var", 
             col.ind="cos2",
             gradient.cols=c("#C00000","#FFC000","#00A650"), 
             arrow.size=10,
             labelsize=7, repel=TRUE) +
  scale_x_continuous(name="PC1 (36.1%)") +
  scale_y_continuous(name="PC2 (28.5%)") +
  theme(aspect.ratio = 1)

# Il biplot mostra il piano generato dalle prime due componenti principali, PC1 sulle ascisse e PC2 sulle ordinate.
# All'interno del piano vengono rappresentate sia le proiezioni dei punti, che rappresentano gli individui,  sia le variabili del dataset originario.

# Circa gli individui, è la loro posizione che descrive un'interpretazione più o meno accurata nel piano: più si allontanano dall'origine, meglio sono individuate dalle componenti in esame.
# Per quanto riguarda le variabili, la lunghezza delle frecce misura il loro contributo nella determinazione delle componenti, mentre la direzione determina la correlazione.

# Per valutare la qualità della rappresentazione di individui e variabili attraverso le due componenti principali, si adoperano gli indicatori cos2.
# Nella rappresentazione degli individui, i cos2 quantificano la percentuale di varianza spiegata dalla proiezione.
# Nella rappresentazione delle variabili, invece, non sono altro che i coseni direttori direttori del cambio di base da quella canonica a quella individuata dalle componenti principali.

# In entrambi i casi, più l'indicatore è vicino a 1 (cromaticamente, più ci si avvicina al verde), più la rappresentazione è accurata.

# Nello specifico, nell'analisi del biplot del nosto dataset possiamo osservare che le variabili che partecipano attivamente alla determinazione di PC1 e PC2 sono PREMIUM, CY e CA e con un contributo poco inferiore EXP e AGE.
# Viceversa risultano mal rappresentate le variabili MIL e ACC.

# In particolare osserviamo che PREMIUM incide in modo molto positivo nella PC1, mentre EXP e AGE contribuiscono in senso negativo e lievemente inferiore.
# Con identica proporzione ma senso opposto, invece, osserviamo che CA e CY presentano una buona correlazione con la PC2.

# Osserviamo anche che quest'ultime hanno stessa giacitura nel piano, ma verso opposto, a testimonianza di una correlazione totale ma in senso negativo: ciò risulta evidente considerando il loro significato, in quanto all'aumentare dell'anno di produzione dell'auto diminuisce la sua età.
# Nella giacitura quasi perfettamente ortogonale troviamo invece le variabili EXP, AGE e PREMIUM.
# Le prime due risultano quasi parallele e concordi in verso, ad evidenziare una buona correlazione: risulta infatti chiaro che all'aumentare dell'età aumenti anche l'esperienza alla guida.
# In direzione simile ma verso opposto troviamo PREMIUM, negativamente correlata alle altre due; ciò va interpretato come una proporzionalità inversa tra le variabili, in quanto all'aumentare dell'età e dell'esperienza il premio assicurativo risulta inferiore e viceversa.
# Questo testimonia anche l'importanza di EXP e AGE nell'interpretazione di PREMIUM, come già sottolineato nella fase di modellizzazione,





# 4) CLUSTER ANALYSIS ---------------------------------------------------------------------------------------------------


# Effettuare una cluster analysis sul dataset mediante un metodo gerarchico e uno non gerarchico a scelta (il numero di gruppi da far considerare a questa analisi deve dipendere da una scelta fatta in base al dataset che si sta studiando, ossia si deve scegliere una variabile in base alla quale si divide il dataset, ed essa va rimossa dalla matrice di dati).

# Confrontare i risultati della cluster analysis con la variabile rimossa già nota.


INS_CLUSTERING<-INS[,-7]
INS_CLUST_SCALED<-as.data.frame(scale(INS_CLUSTERING))
DIST<-dist(INS_CLUST_SCALED, method="euclidean")

# • CLUSTERING GERARCHICO CON METODO DI WARD

GER_CLUST_W<-hclust(DIST, method="ward.D")

par(mar = c(2, 2.5, 2.5, 2))
plot(GER_CLUST_W, 
     main = "CLUSTERING GERARCHICO - METODO WARD",
     hang=-0.1,
     frame.plot = TRUE,
     labels = FALSE)

# Visualizziamo gli indici di aggregazione in modo da interpretare il numero di cluster idoneo: il dendrogramma andrà tagliato dove tale indice compie "un salto".

#GER_CLUST_W$height

# Osserviamo che c'è una variazione sostanziale nell'aggregazione finale delle due classi a formarne un'unica, quindi considereremo due cluster.

GER_CLUST_CUT_W<-cutree(GER_CLUST_W, k=2)
rect.hclust(GER_CLUST_W, k=2, border="red") #disegna rettangoli rossi sui cluster
C1<-INS_CLUST_SCALED[GER_CLUST_CUT_W==1,]
C2<-INS_CLUST_SCALED[GER_CLUST_CUT_W==2,]

# Valutiamo l'efficacia del metodo di Ward calcolando la percentuale di inerzia fra classi.

I_TOT<-sum(apply(INS_CLUSTERING,2,var))*nrow(INS_CLUSTERING)
I_INT <- sum(sapply(unique(GER_CLUST_CUT_W), function(c) {
  INS_CLUSTER <- INS_CLUSTERING[GER_CLUST_CUT_W == c, , drop=FALSE]
  sum(apply(INS_CLUSTER, 2, var)) * (nrow(INS_CLUSTER) - 1)}))

I_FRA<-I_TOT-I_INT

cat("INERZIA FRA CLASSI: ",PERC_I_FRA<-I_FRA*100/I_TOT)


# • CLUSTERING GERARCHICO SECONDO DISTANZA MINIMA

GER_CLUST_S<-hclust(DIST, method="single")

par(mar = c(2, 2.5, 2.5, 2))
plot(GER_CLUST_S, 
     main = "CLUSTERING GERARCHICO - DISTANZA MINIMA",
     hang=-0.1,
     frame.plot = TRUE,
     labels = FALSE)

#GER_CLUST_S$height

GER_CLUST_CUT_S<-cutree(GER_CLUST_S, k=2)
rect.hclust(GER_CLUST_S, k=2, border="red")
C1<-INS_CLUST_SCALED[GER_CLUST_CUT_S==1,]
C2<-INS_CLUST_SCALED[GER_CLUST_CUT_S==2,]

# Percentuale di inerzia fra classi.

I_TOT<-sum(apply(INS_CLUSTERING,2,var))*nrow(INS_CLUSTERING)

I_INT <- sum(sapply(unique(GER_CLUST_CUT_S), function(c) {
  INS_CLUSTER <- INS_CLUSTERING[GER_CLUST_CUT_S == c, , drop=FALSE] 
  sum(apply(INS_CLUSTER, 2, var)) * (nrow(INS_CLUSTER) - 1)
  
}))

I_FRA<-I_TOT-I_INT

cat("INERZIA FRA CLASSI: ",PERC_I_FRA<-I_FRA*100/I_TOT)


# • CLUSTERING GERARCHICO SECONDO DISTANZA MASSIMA

GER_CLUST_C<-hclust(DIST, method="complete")

par(mar = c(2, 2.5, 2.5, 2))
plot(GER_CLUST_C, 
     main = "CLUSTERING GERARCHICO - DISTANZA MASSIMA",
     hang=-0.1,
     frame.plot = TRUE,
     labels = FALSE)

#GER_CLUST_C$height

GER_CLUST_CUT_C<-cutree(GER_CLUST_C, k=2)

rect.hclust(GER_CLUST_C, k=2, border="red")
C1<-INS_CLUST_SCALED[GER_CLUST_CUT_C==1,]
C2<-INS_CLUST_SCALED[GER_CLUST_CUT_C==2,]

# Percentuale di inerzia fra classi.

I_TOT<-sum(apply(INS_CLUSTERING,2,var))*nrow(INS_CLUSTERING)

I_INT <- sum(sapply(unique(GER_CLUST_CUT_C), function(c) {
  INS_CLUSTER <- INS_CLUSTERING[GER_CLUST_CUT_C == c, , drop=FALSE] 
  sum(apply(INS_CLUSTER, 2, var)) * (nrow(INS_CLUSTER) - 1)
  
}))

I_FRA<-I_TOT-I_INT

cat("INERZIA FRA CLASSI: ",PERC_I_FRA<-I_FRA*100/I_TOT)



# • CLUSTERING GERARCHICO SECONDO DISTANZA MEDIA

GER_CLUST_A<-hclust(DIST, method="average")

par(mar = c(2, 2.5, 2.5, 2))
plot(GER_CLUST_A, 
     main = "CLUSTERING GERARCHICO - DISTANZA MEDIA",
     hang=-0.1,
     frame.plot = TRUE,
     labels = FALSE)

#GER_CLUST_A$height

GER_CLUST_CUT_A<-cutree(GER_CLUST_A, k=2)
rect.hclust(GER_CLUST_A, k=2, border="red")
C1<-INS_CLUST_SCALED[GER_CLUST_CUT_A==1,]
C2<-INS_CLUST_SCALED[GER_CLUST_CUT_A==2,]

# Percentuale di inerzia fra classi.

I_TOT<-sum(apply(INS_CLUSTERING,2,var))*nrow(INS_CLUSTERING)

I_INT <- sum(sapply(unique(GER_CLUST_CUT_A), function(c) { 
  INS_CLUSTER <- INS_CLUSTERING[GER_CLUST_CUT_A == c, , drop=FALSE] 
  sum(apply(INS_CLUSTER, 2, var)) * (nrow(INS_CLUSTER) - 1)
  
}))

I_FRA<-I_TOT-I_INT

cat("INERZIA FRA CLASSI: ",PERC_I_FRA<-I_FRA*100/I_TOT)


library(ggplot2)

library(factoextra)

# Visualizziamo i cluster

fviz_cluster(list(data = INS_CLUST_SCALED, cluster = GER_CLUST_CUT_W), geom="point", main="CLUSTERING GERARCHICO- METODO WARD")
fviz_cluster(list(data = INS_CLUST_SCALED, cluster = GER_CLUST_CUT_S), geom="point", main="CLUSTERING GERARCHICO- DISTANZA MINIMA")
fviz_cluster(list(data = INS_CLUST_SCALED, cluster = GER_CLUST_CUT_C), geom="point", main="CLUSTERING GERARCHICO- DISTANZA MASSIMA")
fviz_cluster(list(data = INS_CLUST_SCALED, cluster = GER_CLUST_CUT_A), geom="point", main="CLUSTERING GERARCHICO- DISTANZA MEDIA")

# Calcoliamo i baricentri di ciascun cluster

BARIC_W<-by(INS_CLUST_SCALED, GER_CLUST_CUT_W, colMeans) # Ward
BARIC_S<-by(INS_CLUST_SCALED, GER_CLUST_CUT_S, colMeans) # Minima
BARIC_C<-by(INS_CLUST_SCALED, GER_CLUST_CUT_C, colMeans) # Massima
BARIC_A<-by(INS_CLUST_SCALED, GER_CLUST_CUT_A, colMeans) # Media


# Per avere un ulteriore parametro di valutazione del clustering, calcoliamo l'indice di silhouette.
# Tale indice quantifica la bontà del metodo adoperato valutando la coerenza di assegnazione di ciascun punto al cluster.
# Per ciascun punto, se l'indice si avvicina a 1 c'è una buona coerenza, se si avvicina a 0 c'è indecisione (vicinanza al confine), se negativo c'è incoerenza.

library(cluster)

SIL_W<-silhouette(GER_CLUST_CUT_W, DIST)
plot(SIL_W, col=1:2, border=NA, main = "INDICE SILHOUETTE - METODO WARD\n")

SIL_S<-silhouette(GER_CLUST_CUT_S, DIST)
plot(SIL_S, col=1:2, border=NA, main = "INDICE SILHOUETTE - DISTANZA MINIMA\n")

SIL_C<-silhouette(GER_CLUST_CUT_C, DIST)
plot(SIL_C, col=1:2, border=NA, main = "INDICE SILHOUETTE - DISTANZA MASSIMA\n")

SIL_A<-silhouette(GER_CLUST_CUT_A, DIST)
plot(SIL_A, col=1:2, border=NA, main = "INDICE SILHOUETTE - DISTANZA MEDIA\n")

# Complessivamente non si riscontra una buona suddivisione dei punti.

# Analizziamo adesso la natura dei due cluster ottenuti implementando il metodo della Distanza Massima

INS_CLUSTERED<-data.frame(INS, Cluster=factor(GER_CLUST_CUT_C))

aggregate(PREMIUM~Cluster, data=INS_CLUSTERED, summary)

library(ggplot2)
library(reshape2)

INS_MELTED<-melt(INS_CLUSTERED, id.vars="Cluster")

ggplot(INS_MELTED, aes(x=Cluster, y=value, fill=Cluster)) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free") +
  theme_minimal() + labs(title="CONFRONTO VARIABILI TRA CLUSTER - DISTANZA MASSIMA")

# Osserviamo che i due cluster catturano le differenze d'età dei veicoli di ciascuna unità.
# Risulta anche interessante la variazione del valore mediano degli incidenti dei membri di ciascun cluster.
# Il primo cluster potrebbe identificare guidatori medianamente più cauti, mentre il secondo quelli con uno stile di guida meno rigoroso.
# Tuttavia tale disparità non si riflette nei valori del premio assicurativo, che risulta pressochè identico.



# • CLUSTERING GERARCHICO SECONDO DISTANZA MASSIMA(alternativa)

#Rimuoviamo le variabili AGE,CY e CA dal dataset e rieseguiamo clustering secondo distanza massima.

INS_CLUSTERING2 <- INS[, -c(1, 4, 5)]
INS_CLUST_SCALED2<-as.data.frame(scale(INS_CLUSTERING2))
DIST<-dist(INS_CLUST_SCALED2, method="euclidean")

GER_CLUST_C2<-hclust(DIST, method="complete")

par(mar = c(2, 2.5, 2.5, 2))
plot(GER_CLUST_C2, 
     main = "CLUSTERING GERARCHICO - DISTANZA MASSIMA MODIFICATO",
     hang=-0.1,
     frame.plot = TRUE,
     labels = FALSE)

GER_CLUST_CUT_C2<-cutree(GER_CLUST_C2, k=2)

rect.hclust(GER_CLUST_C2, k=2, border="red")

C1<-INS_CLUST_SCALED2[GER_CLUST_CUT_C2==1,]
C2<-INS_CLUST_SCALED2[GER_CLUST_CUT_C2==2,]

# Percentuale di inerzia fra classi.

I_TOT<-sum(apply(INS_CLUSTERING2,2,var))*nrow(INS_CLUSTERING2)

I_INT <- sum(sapply(unique(GER_CLUST_CUT_C2), function(c) {
  INS_CLUSTER <- INS_CLUSTERING2[GER_CLUST_CUT_C2 == c, , drop=FALSE] 
  sum(apply(INS_CLUSTER, 2, var)) * (nrow(INS_CLUSTER) - 1)
  
}))

I_FRA<-I_TOT-I_INT

cat("INERZIA FRA CLASSI: ",PERC_I_FRA<-I_FRA*100/I_TOT)

library(factoextra)

fviz_cluster(list(data = INS_CLUST_SCALED2, cluster = GER_CLUST_CUT_C2), geom="point", main="CLUSTERING GERARCHICO- DISTANZA MASSIMA MODIFICATO")


INS_CLUSTERED2<-data.frame(INS, Cluster=factor(GER_CLUST_CUT_C2))

aggregate(PREMIUM~Cluster, data=INS_CLUSTERED2, summary)

library(ggplot2)
library(reshape2)

INS_MELTED2<-melt(INS_CLUSTERED2, id.vars="Cluster")

ggplot(INS_MELTED2, aes(x=Cluster, y=value, fill=Cluster)) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free") +
  theme_minimal() + labs(title="CONFRONTO VARIABILI TRA CLUSTER - DISTANZA MASSIMA MODIFICATO")

#Osserviamo che la classificazione ottenuta in questo caso suddivide la popolazione prevalentemente per età, e quindi esperienza alla guida, rilevando in corrispondenza di ciò una sensibile discrepanza tra i valori dei premi assicurativi degli automobilisti di ciascuna classe. 





# • CLUSTERING NON GERARCHICO COL METODO KMEANS

# Essendo il dataset abbastanza esteso e non soggetto a numerosi outliers, adoperiamo il Kmeans per la ridotta complessità computazionale.

library(factoextra)

fviz_nbclust(INS_CLUST_SCALED, kmeans, method = "silhouette") + labs(title="INDICE DI SILHOUETTE - N° OTTIMALE CLUSTER")
KMEANS_CLUST<-kmeans(INS_CLUST_SCALED, centers=2, nstart=50)
set.seed(123)   # Impostiamo un seme per la selezione casuale dei centroidi in partenza

C1<-INS[KMEANS_CLUST$cluster=="1",]
C2<-INS[KMEANS_CLUST$cluster=="2",]

# Percentuale di inerzia fra classi.

I_TOT<-sum(apply(INS_CLUSTERING,2,var))*nrow(INS_CLUSTERING)

I_INT <- sum(sapply(unique(KMEANS_CLUST$cluster), function(c) { 
  INS_CLUSTER <- INS_CLUSTERING[KMEANS_CLUST$cluster == c, , drop=FALSE] 
  sum(apply(INS_CLUSTER, 2, var,na.rm=TRUE)) * (nrow(INS_CLUSTER) - 1)
  
}))

I_FRA<-I_TOT-I_INT

cat("INERZIA FRA CLASSI: ",PERC_I_FRA<-I_FRA*100/I_TOT)

BARIC_KM<-KMEANS_CLUST$centers

fviz_cluster(KMEANS_CLUST, INS_CLUST_SCALED,geom="point", main="CLUSTERING NON GERARCHICO - KMEANS")

SIL_KM<-silhouette(KMEANS_CLUST$cluster, DIST)
plot(SIL_KM, col=1:2, border=NA, main = "INDICE SILHOUETTE - KMEANS\n")

# Analizziamo adesso la natura dei due cluster

INS_CLUSTERED<-data.frame(INS, Cluster=factor(KMEANS_CLUST$cluster))

aggregate(PREMIUM~Cluster, data=INS_CLUSTERED, summary)

INS_MELTED<-melt(INS_CLUSTERED, id.vars="Cluster")

ggplot(INS_MELTED, aes(x=Cluster, y=value, fill=Cluster)) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free") +
  theme_minimal() + labs(title="CONFRONTO VARIABILI TRA CLUSTER - KMEANS")

# Ricaviamo che il clustering suddivide la popolazione tenendo conto dell'età dell'auto di ciascun individuo.
# È interessante osservare come al cluster d'appartenenza delle auto meno recenti corrisponda un premio assicurativo lievemente superiore nel valore mediano.



# • CLUSTERING NON GERARCHICO COL METODO KMEANS (alternativa)

#Rimuoviamo le variabili AGE,CY e CA dal dataset e rieseguiamo il K-means.

INS_CLUSTERING2 <- INS[, -c(1, 4, 5)]
INS_CLUST_SCALED2<-as.data.frame(scale(INS_CLUSTERING2))


fviz_nbclust(INS_CLUST_SCALED2, kmeans, method = "silhouette") + labs(title="INDICE DI SILHOUETTE - N° OTTIMALE CLUSTER")
KMEANS_CLUST2<-kmeans(INS_CLUST_SCALED2, centers=2, nstart=50)
set.seed(123)   # Impostiamo un seme per la selezione casuale dei centroidi in partenza

C1<-INS[KMEANS_CLUST2$cluster=="1",]
C2<-INS[KMEANS_CLUST2$cluster=="2",]

# Percentuale di inerzia fra classi.

I_TOT<-sum(apply(INS_CLUSTERING2,2,var))*nrow(INS_CLUSTERING2)

I_INT <- sum(sapply(unique(KMEANS_CLUST2$cluster), function(c) { 
  INS_CLUSTER2 <- INS_CLUSTERING2[KMEANS_CLUST2$cluster == c, , drop=FALSE] 
  sum(apply(INS_CLUSTER2, 2, var,na.rm=TRUE)) * (nrow(INS_CLUSTER2) - 1)
  
}))

I_FRA<-I_TOT-I_INT

cat("INERZIA FRA CLASSI: ",PERC_I_FRA<-I_FRA*100/I_TOT)

BARIC_KM2<-KMEANS_CLUST2$centers

fviz_cluster(KMEANS_CLUST2, INS_CLUST_SCALED2,geom="point", main="CLUSTERING NON GERARCHICO - KMEANS MODIFICATO")

SIL_KM2<-silhouette(KMEANS_CLUST2$cluster, DIST)
plot(SIL_KM2, col=1:2, border=NA, main = "INDICE SILHOUETTE - KMEANS\n")


# Analizziamo adesso la natura dei due cluster

INS_CLUSTERED2<-data.frame(INS, Cluster=factor(KMEANS_CLUST2$cluster))

aggregate(PREMIUM~Cluster, data=INS_CLUSTERED2, summary)

INS_MELTED2<-melt(INS_CLUSTERED2, id.vars="Cluster")

ggplot(INS_MELTED2, aes(x=Cluster, y=value, fill=Cluster)) +
  geom_boxplot() +
  facet_wrap(~variable, scales="free") +
  theme_minimal() + labs(title="CONFRONTO VARIABILI TRA CLUSTER - KMEANS MODIFICATO")


#La prima classe è formata da persone più adulte, con più esperienza e più prudenza alla guida a cui è associato un premio assicurativo più basso. 
#La seconda classe invece comprende persone più giovani, con meno esperienza e una guida meno attenta con conseguente premio sensibilmente più alto.
