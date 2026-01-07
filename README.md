# Car Insurance Premium Analysis

![R](https://img.shields.io/badge/R-%23276DC3.svg?style=flat&logo=r&logoColor=white)
![License: MIT](https://img.shields.io/badge/License-MIT-yellow.svg)
![Statistical Analysis](https://img.shields.io/badge/Analysis-GLM%20%26%20Inference-blue.svg)


## Abstract
Questo progetto sviluppa un'analisi statistica completa in **R** per identificare i fattori che influenzano il calcolo dei premi assicurativi automobilistici. Attraverso l'uso di modelli lineari, modelli generalizzati (GLM) e tecniche di analisi multivariata, il progetto mira a profilare il rischio e prevedere i costi dei premi.

L'analisi si basa su un dataset di 1.000 conducenti e utilizza diverse metodologie statistiche per estrarre insight significativi:
- **Modellizzazione:** Confronto tra regressione lineare e modelli GLM.
- **Riduzione della dimensionalità:** Analisi delle Componenti Principali (PCA).
- **Segmentazione:** Clustering per identificare profili di guidatori omogenei.

## Descrizione del Dataset e Data Availability
Il dataset utilizzato per questa analisi è incluso integralmente in questo repository al fine di garantire la riproducibilità dei risultati. È possibile trovare i dati nel file:
- `car_insurance_premium_dataset.csv`.

Si specifica che i dati contenuti in questo progetto hanno una **natura puramente didattica e illustrativa**.

Il file `car_insurance_premium_dataset.csv` contiene le seguenti variabili:
- `AGE`: Età del conducente.
- `EXP`: Anni di esperienza di guida.
- `ACC`: Numero di incidenti precedenti.
- `MIL`: Chilometraggio annuo (x1000 km).
- `CY/CA`: Caratteristiche temporali del veicolo (anno di produzione/età).
- `PREMIUM`: Valore del premio assicurativo (Variabile Target).

## Fasi dell'Analisi

### 1. Exploratory Data Analysis (EDA)
- Studio delle correlazioni tra età, esperienza e costo del premio.
- Visualizzazioni avanzate con `ggplot2` e matrici di correlazione.

### 2. Modellizzazione Statistica
- **LM (Linear Models):** Regressione lineare multipla per una baseline predittiva.
- **GLM (Generalized Linear Models):** Utilizzo di distribuzioni **Gamma** e **Inverse Gaussian** con funzione di legame `log` per gestire la natura asimmetrica dei premi.
- **Diagnostica:** Analisi dei residui tramite pacchetto `statmod` per validare la robustezza dei modelli.

### 3. PCA & Clustering
- **PCA:** Analisi delle componenti principali per ridurre la collinearità e visualizzare la varianza spiegata.
- **Clustering:** Implementazione di Clustering Gerarchico (Metodo di Ward) e K-means per raggruppare i conducenti in base al profilo di rischio.

## Requisiti
Per eseguire l'analisi, installare i seguenti pacchetti in R:
```r
install.packages(c("ggplot2", "corrplot", "statmod", "factoextra", "reshape2", "cluster"))
```
## Licenza
Questo progetto è distribuito sotto la **Licenza MIT**. Ciò significa che sei libero di utilizzare, copiare, modificare e distribuire il codice, a condizione che venga fornita l'attribuzione all'autore originale.

## **Autori**
- Laraia Letizia
- Peluso Pietro
