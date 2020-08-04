---
title: Ajuste de modelos lineales
summary: "Guia para el plantamiento y ajuste de modelos lineales: explicación y código para desarrollarlos"
tags:
- Modelos
- Regresión lineal
date: "2020-01-27T00:00:00Z"

# Optional external URL for project (replaces project detail page).
external_link: '/rls.html'

image:
  caption: 
  focal_point: Center
  preview_only: true
url_pdf: ''
url_slides: '/rls.html'
featured: true
links:
- icon: github
  icon_pack: fab
  name: materials
  url: /rls.html
bibliography: GLMs.bib
---



```{r setup, include=FALSE}
knitr::opts_chunk$set(echo = TRUE, fig.width=6, fig.height=4.5)
```


Como se mencionó en la sección anterior, la tabla de datos de emisiones de CO<sub>2</sub> fue ensamblada con el fin de establecer cuál es la importancia relativa del tamaño de la población y de la economía en la generación de emisiones de CO<sub>2</sub>. Esta pregunta parte de las premisas de que ambas variables constituyen una causalidad de las emisiones: se espera que la cantidad de CO<sub>2</sub> emitida por un país se incremente a medida que crece su población (posiblemente bajo el argumento de que personas adicionales implican el uso de recursos adicionales) y a medida que crece el tamaño de la actividad económica (a mayor actividad de producción, mayor cantidad de procesos que generan emisiones). La **Figura 1 ** sugiere una  asociación entre las emisiones de CO<sub>2</sub> y la población, en concordancia con una de las premisas del ejercicio. ¿Podemos sintetizar esta relación empleando un modelo estadístico? Por su puesto que sí. De hecho, existen diferentes tipos de modelos estadísticos para resumir esta relación. En la presente sesión haremos uso de una estrategia (herramienta) de modelación muy común: **la regresión lineal simple**, la cual constituye un caso particular de los **modelos lineales generales**.

```{r Figura 1, warning=FALSE, message = FALSE, echo = FALSE, fig.height = 3, fig.width = 5, fig.cap = "**Figura 1.** Relación entre las emisiones de CO<sub>2</sub> y la población. La línea corresponde al ajuste de un modelo de regresión lineal."}
library(tidyverse)

emi <- read.csv("CO2 2014.csv", na.strings = "")

emi <- emi %>% mutate(logpop = log10(Population.millions),
                      logemi = log10(CO2.emissions.Tg),
                      loggdp = log10(GDP.million.US.dollar))

fexp <- ggplot(emi, aes(logpop, logemi)) +
  theme(panel.background = element_blank(), legend.position="none", axis.line = element_line(colour = "black")) +
  labs(y = expression('Emisiones de CO'[2] * ' (log Tg)'), x = "Población (log millones)")

fexp + 
  geom_point(col ="blue", alpha=0.75) +
  geom_line(stat="smooth", method="lm", se=FALSE, col ="blue", size=1)
```

# Recordando la línea recta

Cualquier línea recta puede describirse a través de la siguiente función matemática:

$$
\begin{aligned}
y = f(x) = \beta_{0} + \beta_{1}x
\end{aligned}
$$


En esta fórmula, la variable $y$ está en función de la variable $x$ a través de dos parámetros: el **intercepto** ($\beta_{0}$) y la **pendiente** ($\beta_{1}$). El intercepto establece cuál es el valor de la variable $y$ cuando $x = 0$. Por su parte, la pendiente establece en cuántas unidades se incrementa el valor de Y por cada unidad de incremento en la variable X. Dependiendo del valor específico que tomen estos parámetros, la línea recta se trazará en una posición y/o con una inclinación diferente, como se ilustra en la **Figura 2**. 

```{r Figura 2, warning=FALSE, message=FALSE, echo = FALSE, fig.height = 3, fig.width = 8, fig.cap = "**Figura 2.** Ejemplos de líneas rectas con diferentes interceptos y misma pendiente (a), o diferentes pendientes con el mismo intercepto (b). La línea vertical punteada indica el valor de x = 0 y las horizontales los valores del intercepto."}
library(ggpubr)

df1 <- data.frame(ymin1=c(-0.2899, 0.4101, 1.1101), xmin1=rep(0,3), ymax1=c(-0.2899, 0.4101, 1.1101), xmax1=rep(-2.25,3))

fexp.intercepts <- ggplot(emi, aes(logpop, logemi)) +
  geom_point(color="white") +
  theme(panel.background = element_blank(), legend.position="none", axis.line = element_line(colour = "black")) +
  labs(y = "y", x = "x") +
  scale_x_continuous(expand = c(0,0), limits=c(-2.25,3.3)) +
  scale_y_continuous(expand = c(0,0), limits=c(-2.3,4.3)) +
  geom_abline(slope = 0.8815, intercept = -0.2899, color="darkolivegreen4", size=1) +
  geom_abline(slope = 0.8815, intercept = 0.4101, color="darkolivegreen3", size=1) +
  geom_abline(slope = 0.8815, intercept = 1.1101, color="darkolivegreen2", size=1) +
  geom_vline(xintercept = 0, color="grey50", size=1, linetype=2) +
geom_segment(aes(x = xmin1, y = ymin1, xend = xmax1, yend = ymax1), data = df1, colour = "grey50", linetype=2)

df2 <- data.frame(ymin1=0.4101, xmin1=0, ymax1=0.4101, xmax1=-2.25)

fexp.slopes <- ggplot() +
  theme(panel.background = element_blank(), legend.position="none", axis.line = element_line(colour = "black")) +
  labs(y = "y", x = "x") +
  scale_x_continuous(expand = c(0,0), limits=c(-2.25,3.3)) +
  scale_y_continuous(expand = c(0,0), limits=c(-2.3,4.3)) +
  geom_abline(slope = 1.1815, intercept = 0.4101, color="darkorange4", size=1) +
  geom_abline(slope = 0.8815, intercept = 0.4101, color="darkorange3", size=1) +
  geom_abline(slope = 0.5815, intercept = 0.4101, color="darkorange2", size=1) +
  geom_vline(xintercept = 0, color="grey50", size=1, linetype=2) +
geom_segment(aes(x = xmin1, y = ymin1, xend = xmax1, yend = ymax1), data = df2, colour = "grey50", linetype=2)

ggarrange(fexp.intercepts, fexp.slopes, 
          labels = c("a)", "b)"),
          ncol = 2, nrow = 1, align = "h")

```


# La línea recta aplicada a "respuestas ruidosas": Regresión lineal

La línea recta constituye un modelo completamente determinístico: cada valor de $x$ predice un único valor posible de $y$ (**Figura 2**). Sin embargo, la mayoría de los datos reales no se comportan de esta forma: un mismo valor de $x$ puede estar asociado a uno o más valores de $y$, la mayoría de las veces claramente diferentes al valor de $y$ establecido por la línea recta. Tal es el caso de las emisiones de CO<sub>2</sub>: si bien las emisiones siguen un patrón claro respecto a la población, éstas pueden tomar valores contrastantes y/o muy alejados de los valores esperados a partir del valor de población correspondiente (**Figura 1**). Esto se debe a que las emisiones responden no solo al valor de la población, sino de otros factores no considerados hasta el momento y cuya acción parece ser completamente azarosa (aleatoria).

La **regresión lineal simple** es un modelo estadístico que permite conocer el valor específico que toma una variable $y$ al incorporar tanto el componente determinístico (relación entre $x$ y $y$, ambas contínuas), como la variación aleatoria en torno a esta relación. El componente determinístico describe cuál sería el valor esperado de la variable $y$ para cada valor de la variable $x$. En otras palabras, este componente "idealiza" el comportamiento de $y$ en relación a $x$. Dicha idealización o valor esperado de la variable $y$ para cada valor de la variable $x$ se denota como $\hat{y}$, por lo que estrictamente el modelo de la línea recta debe re-escribirse como:

$$
\begin{aligned}
\hat{y} = f(x) = \beta_{0} + \beta_{1}x
\end{aligned}
$$

¿Cómo saber cuál línea es la que mejor representa la tendencia general de la relación? En el caso particular de nuestro ejemplo, ¿cuáles deben de ser los valores de los parámetros para obtener la mejor idealización posible de la relación entre emisiones y población? Existen diferentes métodos matemáticos para establecer cuáles deben ser los valores de los parámetros para obtener el modelo más adecuado, de los cuales los más comunes son el método de mínimos cuadrados y el de máxima verosimilitud (ver Capítulo 6 en Bolker [-@Bolker2008]). Por ahora, basta con saber que las estimaciones de los parámetros de la línea recta en la **Figura 1** son $\beta_{0} = 0.4101$ (intercepto) y $\beta_{1} = 0.8815$ (pendiente) y que el componente determinístico del modelo de regresión lineal puede ser escrito entonces como:

$$
\begin{aligned}
\log_{10}(\hat{CO_{2}}) = 0.4101 + 0.8815* \log_{10}(pob)
\end{aligned}
$$

Para convertir este modelo determinístico en uno probabilístico (y por ende estadístico), se requiere la inclusión de un término que represente la variación de las emisiones reales en torno a esta tendencia central o valor esperado. A ese término se le denomina **error** o **residuo** y no es más que la diferencia entre el valor real de $CO_{2}$ para cada observación y el valor *esperado* o *idealizado* de las emisiones ($\hat{CO_{2}}$) dado el valor de $x$:

$$
\begin{aligned}
error_{i} = \log_{10}(CO_{2_{i}}) - \log_{10}(\hat{CO_{2_{i}}})  \ \ \ \ \ \ \ \ i: 1 \dots n
\end{aligned}
$$

El subíndice $i$ indica la observación particular a la cual se hace referencia y por ende varía entre 1 (la primera observación) y $n$ (la última observación). Remplazando el valor de $\hat{CO_{2_{i}}}$ de la Eq.1 en la Eq. 2 y despejando para $CO_{2_{i}}$ se obtiene :

$$
\begin{aligned}
\log_{10}(CO_{2_{i}}) &= 0.4101 + 0.8815*\log_{10}(pob_{i})\ + \ error_{i}
\end{aligned}
$$

Ésta es la expresión del modelo de regresión lineal aplicado a la relación entre las emisiones de CO<sub>2</sub> y la población. Esta expresión permite obtener con precisión el valor de emisiones de $CO_{2_{i}}$ de un país cualquiera en la tabla de datos a partir de su valor de población (ambas en escala logarítmica de base 10) y de su error particular. El *error* corresponde a la porción aleatoria, es decir, aquella porción del valor de $CO_{2}$ que no es explicable o predecible por el tamaño de la población. Podrían existir países que tengan exactamente la misma población pero no las mismas emisiones de CO<sub>2</sub>. La razón para dicha variación es hasta este momento desconocida, y por ende, se entiende que su valor es resultado únicamente del azar. 

Veamos la representación gráfica de esta expresión (**Figura 3**). En este caso, se resalta el dato correspondiente a los Estados Unidos de América, cuyo número de registro en la tabla de datos es el 201. El punto rojo con relleno azul claro indica el valor observado de emisiones ($CO_{2_{201}} = 3.72$, ver la tabla de datos *emi*). El punto rojo relleno corresponde al valor "idealizado", es decir, el valor predicho al remplazar el valor de población en la Eq.1. ($\hat{CO_{2_{201}}} = 2.62$). 

```{r Figura 3, warning=FALSE, echo = FALSE, fig.height = 4, fig.width = 7, fig.cap = "**Figura 3.** Representación gráfica del modelo de la ecuación 3. Relación entre las emisiones de CO<sub>2</sub> y la población. La línea corresponde al ajuste de un modelo de regresión lineal."}

lmexp <- lm(logemi ~ logpop, data = emi, na.action = na.exclude)
emi <- emi %>% mutate(pred.lmexp = predict(lmexp),
                      resid.lmexp = residuals(lmexp))

selobs <- c(201)
emisel <- slice(emi, selobs)

fexp +
  scale_x_continuous(expand = c(0,0), limits=c(-2.1,3.2), breaks=round(c(-2,-1,0,1,2,emisel$logpop, 3), digits=2)) +
  scale_y_continuous(expand = c(0,0), limits=c(-2.2,4.2), breaks=round(c(-2,0,2,emisel$pred.lmexp, emisel$logemi,4), digits=2)) +
  geom_point(col ="blue", alpha=0.25, size=3) + 
  geom_line(stat="smooth", method="lm", se=FALSE, col ="blue", alpha = 0.25, size=1) +
  geom_point(data=emisel, aes(logpop, logemi), col="red", shape=1, size=3) +
  geom_point(data=emisel, aes(logpop, pred.lmexp), col="red", alpha=0.75, size=3) +
  geom_segment(data=emisel, aes(x = logpop, y = logemi, xend = logpop, yend = pred.lmexp, col = "red")) +
  annotate(geom = "text", x=emisel$logpop, y=emisel$logemi, label="CO[2[201]]", parse=TRUE, hjust="outward", vjust="outward") +
  annotate(geom = "text", x=emisel$logpop, y=emisel$pred.lmexp, label="hat(CO)[2[201]]", parse=TRUE, hjust="outward", vjust="inward") +
  geom_segment(aes(x = emisel$logpop, y = emisel$logemi, xend = -2.1, yend = emisel$logemi), colour = "grey50", linetype=2) +
  geom_segment(aes(x = emisel$logpop, y = emisel$pred.lmexp, xend = -2.1, yend = emisel$pred.lmexp), colour = "grey50", linetype=2) +
  geom_segment(aes(x = emisel$logpop, y = emisel$pred.lmexp, xend = emisel$logpop, yend = -2.1), colour = "grey50", linetype=2)

```

La población de los Estados Unidos de América (en escala log10) es de 2.50. Empleando la Eq. 2 podemos calcular el valor *predicho* o *esperado* de emisiones de $CO_{2}$ para un país con dicha población:


$$
\begin{aligned}
\log_{10}(\hat{CO_{2_{201}}}) &= 0.4101 + 0.8815 * 2.50 \\
\log_{10}(\hat{CO_{2_{201}}}) &= 2.62
\end{aligned}
$$

Las emisiones reales de Estados Unidos de América son superiores al valor esperado a partir del tamaño de su población. Las razones por las cuales esto ocurre son hasta ahora desconocidas. La diferencia entre estos dos valores está indicada por la línea roja en la **Figura 3**, y su valor (*error*) está dado por:

$$
\begin{aligned}
error_{201} &= 3.72 - 2.62\\
error_{201} &= 1.10
\end{aligned}
$$

La Eq. 4. expresada para los Estados Unidos de América corresponde a:

$$
\begin{aligned}
3.72 &= 0.4101 + 0.8815 * 2.50 + 1.10
\end{aligned}
$$

# El modelo lineal general

El modelo de la Eq.4 puede ser generalizado para el caso de cualquier variable respuesta aleatoria Y y cualquier predictor x:


$$
\begin{aligned}
y_{i} = \beta_{0} + \beta_{1}x_{i} + \ \epsilon_{i}
\end{aligned}
$$

A este modelo se le conoce como **modelo lineal general**. El modelo es lineal no porque la relación se describa usando una línea recta, sino porque la variable $y$ es una función lineal de los parámetros del modelo $\beta_{0},\beta_{1}$. Dicho de forma más simple, las parámetros aparecen como múltiplos en cada término. Los modelos lineales pueden incluir relaciones no lineales, como en el caso de un polinomio de segundo orden.


$$
\begin{aligned}
y_{i} = \beta_{0} + \beta_{1}x^{2}_{i} + \ \epsilon_{i}
\end{aligned}
$$

En este caso la variable $y$ se incrementa de manera no lineal respecto a $x$. Sin embargo, el modelo es lineal en la medida en que sus parámetros $\beta_{0},\beta_{1}$ aparecen como múltiplos en sus términos correspondientes. Por el contrario, el modelo

$$
\begin{aligned}
y_{i} = \beta_{1}x^{\beta_{2}}_{i} + \ \epsilon_{i}
\end{aligned}
$$

no es un modelo lineal, pues $\beta_{2}$ aparece como potencia de $x$ y no como múltiplo de ésta.

Los modelos lineales generales tienen una serie de supuestos fundamentales:

1. Los valores $y_{i}$ son independientes entre sí.
2. $\epsilon$ es una variable aleatoria con distribución normal con media cero.
3. La varianza de $\epsilon$ es constante a través de los valores predichos $\hat{y}$.
4. la variable $x$ se mide sin error.

Todos estos supuestos surgen del método empleado para la estimación de los parámetros del modelo y por ende, para que dichas estimaciones sean válidas (y en particular su error estándar asociado), se requiere que los supuestos se cumplan.

# Ajuste del modelo lineal en R

Se le denomina **ajuste** al proceso de estimación de los parámetros de un modelo estadístico cuando éste se aplica a un conjunto de datos específico. El proceso parte de que la formulación del modelo describe de manera adecuada la relación que se quiere modelar. En el caso de los datos de emisiones, vimos previamente que la asociación entre emisiones y población (ambos en escala logarítmica de base 10) puede modelarse adecuadamente empleando una línea recta (**Figura 1**). Ajustaremos entonces el modelo lineal general definido en la Eq. 4 para modelar la relación entre estas dos variables. La función más ampliamente utilizada para ajustar modelos lineales generales en R es `lm`.

```{r lm}
lm(log10(CO2.emissions.Tg) ~ log10(Population.millions), data = emi)
```

La función se emplea usando el operador `~`, que se lee "en función de". A la izquierda del operador se coloca la variable respuesta y a la derecha la variable predictora (o variables predictoras, como veremos más adelante). El argumento *data* sirve para especificar donde se encuentran almacenadas las variables. El llamado de la función genera la estimación de los parámetros del modelo, llamados *Coefficients*. El primer parámetro, llamado *Intercept*, corresponde a la estimación de $\beta_{0}$, es decir, el intercepto de la regresión lineal. El segundo parámetro, llamado *log10(Population.millions)*, corresponde a la estimación de $\beta_{1}$, que es la pendiente de la relación entre emisiones y población. El nombre se debe a que ese parámetro define el efecto de la población sobre la variable respuesta. Con estos valores se definió la línea de regresión de la **Figura 1**.

Existen una serie de funciones adicionales que permiten obtener más información sobre el modelo ajustado. Para ello es conveniente guardar el ajuste del modelo en un objeto.

```{r Summary.lm}
mod1 <- lm(log10(CO2.emissions.Tg) ~ log10(Population.millions), data = emi)
summary(mod1)
```

La función `summary` sirve para obtener un resumen ampliado del ajuste del modelo: 1) genera la fórmula empleada en el ajuste; 2) un resumen de los errores (*Residuals*) del modelo ; 3) un cuadro con estadísticas relacionadas a los parámetros del modelo (*Coefficients*), donde se observa de nuevo su estimación en la columna *Estimate*, el error estándar asociado a cada estimación (*Std.Error*) y valores del estadístico t (*t value*) y su probabilidad asociada (*Pr(>|t|)*); y 4) una serie de estadísticas adicionales del modelo, tales como el coeficiente de determinación R<sup>2</sup> y el estadístico *F* y valor de *p* asociado a la prueba de hipótesis del modelo completo.

Es posible extraer los valores estimados de los parámetros, los valores de emisión predichos por el modelo para cada país con base en su población ($\hat{CO_{2_{i}}}$), o el error asociado a cada país ($\epsilon_{i}$).

```{r Adicionales lm, eval=FALSE}
# Para extraer la estimación de los parámetros
coefficients(mod1)
# Para extraer las predicciones del modelo
predict(mod1)
# Para extraer los residuos del modelo
residuals(mod1)
```


# Validación del ajuste del modelo lineal

Como se mencionó previamente, todo modelo lineal tiene cuatro supuestos básicos: 1) independencia de datos, 2) normalidad de errores, 3) homogeneidad de varianza de los errores (homocedasticidad), y 4) variable independiente medida sin error. La función `plot` aplicada al modelo lineal genera cuatro gráficos que, además ayudar a comprobar el cumplimiento de los supuestos 2 y 3, permite diagnosticar otro tipo de situaciones relacionadas con el ajuste del modelo, tales como la idoneidad del modelo empleado (que la forma que describe es adecuada para el patrón de relación entre las variables) o la presencia de datos muy influyentes en el ajuste del modelo.

```{r Figura 4, fig.height=8, fig.width=10, fig.cap = "**Figura 4.** Gráficos diagnósticos del modelo lineal obtenidos usando la función `plot`."}
par(mfrow=c(2,2))
plot(mod1, col="blue", pch=21)
```

En la **Figura 4**, el gráfico en la esquina superior izquierda se denomina comúnmente **gráfico de residuos** o **gráfico de errores** y sirve para diagnosticar la homogeneidad de varianza, así como la ideneidad del modelo ajustado para describir la forma de la relación entre las variables. En él los valores predichos por el modelo ($\hat{CO_{2}}$) aparecen en el eje horizontal y los errores correspondientes ($\epsilon$) en el vertical. El gráfico a su derecha se denomina **gráfico Q-Q** y sirve principalmente para diagnosticar si los errores siguen la distribución de probabilidades esperada (en este caso, normal). En éste se emplean los residuos estandarizados, es decir, divididos por la desviación estándar. El gráfico inferior izquierdo es un **scale-location plot**, que muestra la raiz cuadrada del valor absoluto de los residuos estandarizados en el eje vertical y los valores predichos en el horizontal. Este gráfico también permite diagnosticar la homogeneidad de varianza. Por último, el gráfico inferior derecho es un gráfico de **residuos contra leverage**. El leverage (algunas veces traducido como **apalancamiento**) se refiere a la distancia de un dato respecto al resto de datos en el eje de la variable predictora. Un dato en el eje predictor que se encuentra muy alejado de los demás puede llegar a influir de forma importante el resultado del ajuste. Por ello, esta gráfica ayuda a detectar datos altamente influyentes.

¿Qué se debería ver en estos gráficos? Para darse una idea, en la **Figura 5** se presentan los mismos gráficos diagnósticos pero para diferentes situaciones particulares. En el escenario ideal la relación entre un par de variables $x$ y $y$ es adecuadamente descrita por la línea recta asociada al modelo lineal empleado hasta el momento (gráfica superior de la columna *Ideal*); . adicionalmente, los errores o residuos del modelo siguen una distribución normal con varianza homogénea. En este caso, la distribución de los residuos respecto a los valores predichos por el modelo (segunda línea de la primera columna) es homogénea a lo largo del rango de valores predichos, el gráfico-qq muestra los puntos dispuestos a lo largo de una línea recta punteada, y la gráfica de residuos contra leverage no señala ninguna tendencia en los residuos ni alguna observación cuyo valor de distancia de Cook sea cercano a 0.5. este es, como su nombre lo dice, el escenario ideal.

```{r Figura 5, echo =FALSE, eval = TRUE, message=FALSE, fig.width=16, fig.height=14, fig.cap = "**Figura 5.** Gráficos diagnósticos para datos con características particulares. La columna *Ideal* corresponde a una relación entre variables x-y que cumple los supuestos de normalidad y homocedasticidad de los errores, y para la que el modelo lineal describe adecuadamente la relación. La columna *Heterocedasticidad* presenta una situación con errores heterocedásticos, cuya varianza se incrementa con el valor de x. En *No linealidad* se presenta el caso de una relación en la que las variables x y y presentan una asociación no lineal. Por último, en la columna de *Datos extremos* se presenta un caso en el que dos de los datos incluidos presentan valores extremos de la variable y."}

library(ggpubr)

set.seed(1234321)
xaleat <- runif(n=100, min=0, max=10) # Generamos valores del predictor x aleatoriamente distribuidos en el rango de 0 a 10
y1 <- 0.4 + 10*xaleat + rnorm(100, mean = 0, sd = 10) # La variable dependiente y1 se relaciona linealmente con xaleat, y presenta un error con distribución normal y desviación estándar (i.e. varianza) homogénea.
y2 <- 0.4 + 10*xaleat + rnorm(100, mean = 0, sd = xaleat) # La variable dependiente y2 se relaciona linealmente con xaleat, pero la varianza del arror se incrementa con el promedio de y2
y3 <- 10 + xaleat^2 + rnorm(100, mean = 0, sd = 10) # La variable y3 depende no linealmente de xaleat, varianza homogenea
y4 <- c(-10,0.4 + 10*xaleat[2:99] + rnorm(98, mean = 0, sd = 10), 120) # La variable x4 se relaciona linealmente con xaleat, la varianza es homogénea pero hay dos datos extremos
dat <- data.frame(xaleat,y1, y2, y3,y4)


lm1 <- lm(y1~xaleat, data =dat)

xy1.gr <- ggplot(dat, aes(y=y1, x=xaleat)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="x", y="y") +
  geom_point(alpha=0.35, color="green4", size=3) +
  geom_smooth(method="lm", se=FALSE)

res1.gr <- ggplot(lm1, aes(.fitted, .resid)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="Fitted values", y="Residuals") +
  geom_point(alpha=0.35, color="green4", size=3) +
  geom_smooth(se=FALSE, color="red") +
  geom_hline(yintercept = 0, linetype=2)

qq1.gr <- ggplot(lm1, aes(sample=.stdresid)) + 
  stat_qq(alpha=0.35, color="green4", size=3) + 
  geom_abline(slope = 1, linetype=2) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="Theoretical quantiles", y="Standardized residuals")

rl1.gr <- ggplot(lm1, aes(.hat, .stdresid)) +
  geom_point(aes(size = .cooksd),alpha=0.35, color="green4") +
  geom_smooth(se = FALSE, color="red") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x="Leverage", y="Standardized residuals", size="Cook's \ndistance") 

lm2 <- lm(y2~xaleat, data =dat)

xy2.gr <- ggplot(dat, aes(y=y2, x=xaleat)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="x", y="y") +
  geom_point(alpha=0.35, color="yellow3", size=3) +
  geom_smooth(method="lm", se=FALSE)

res2.gr <- ggplot(lm2, aes(.fitted, .resid)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="Fitted values", y="Residuals") +
  geom_point(alpha=0.35, color="yellow3", size=3) +
  geom_smooth(se=FALSE, color="red") +
  geom_hline(yintercept = 0, linetype=2)

qq2.gr <- ggplot(lm2, aes(sample=.stdresid)) + 
  stat_qq(alpha=0.35, color="yellow3", size=3) + 
  geom_abline(slope = 1, linetype=2) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="Theoretical quantiles", y="Standardized residuals")

rl2.gr <- ggplot(lm2, aes(.hat, .stdresid)) +
  geom_point(aes(size = .cooksd),alpha=0.35, color="yellow3") +
  geom_smooth(se = FALSE, color="red") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x="Leverage", y="Standardized residuals", size="Cook's \ndistance")

lm3 <- lm(y3~xaleat, data =dat)

xy3.gr <- ggplot(dat, aes(y=y3, x=xaleat)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="x", y="y") +
  geom_point(alpha=0.35, color="orange3", size=3) +
  geom_smooth(method="lm", se=FALSE)

res3.gr <- ggplot(lm3, aes(.fitted, .resid)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="Fitted values", y="Residuals") +
  geom_point(alpha=0.35, color="orange3", size=3) +
  geom_smooth(se=FALSE, color="red") +
  geom_hline(yintercept = 0, linetype=2)

qq3.gr <- ggplot(lm3, aes(sample=.stdresid)) + 
  stat_qq(alpha=0.35, color="orange3", size=3) + 
  geom_abline(slope = 1, linetype=2) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="Theoretical quantiles", y="Standardized residuals")

rl3.gr <- ggplot(lm3, aes(.hat, .stdresid)) +
  geom_point(aes(size = .cooksd),alpha=0.35, color="orange3") +
  geom_smooth(se = FALSE, color="red") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x="Leverage", y="Standardized residuals", size="Cook's \ndistance")

lm4 <- lm(y4~xaleat, data =dat)

xy4.gr <- ggplot(dat, aes(y=y4, x=xaleat)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="x", y="y") +
  geom_point(alpha=0.35, color="red4", size=3) +
  geom_smooth(method="lm", se=FALSE)

res4.gr <- ggplot(lm4, aes(.fitted, .resid)) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="Fitted values", y="Residuals") +
  geom_point(alpha=0.35, color="red4", size=3) +
  geom_smooth(se=FALSE, color="red") +
  geom_hline(yintercept = 0, linetype=2)

qq4.gr <- ggplot(lm4, aes(sample=.stdresid)) + 
  stat_qq(alpha=0.35, color="red4", size=3) + 
  geom_abline(slope = 1, linetype=2) + 
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) +
  labs(x="Theoretical quantiles", y="Standardized residuals")

rl4.gr <- ggplot(lm4, aes(.hat, .stdresid)) +
  geom_point(aes(size = .cooksd),alpha=0.35, color="red4") +
  geom_smooth(se = FALSE, color="red") +
  theme(panel.background = element_blank(), axis.line = element_line(colour = "black")) + 
  labs(x="Leverage", y="Standardized residuals", size="Cook's \ndistance")

fig5 <- ggarrange(xy1.gr, xy2.gr+rremove("ylab"), xy3.gr+rremove("ylab"), xy4.gr+rremove("ylab"),
          res1.gr, res2.gr+rremove("ylab"), res3.gr+rremove("ylab"), res4.gr+rremove("ylab"), 
          qq1.gr,  qq2.gr+rremove("ylab"),  qq3.gr+rremove("ylab"),  qq4.gr+rremove("ylab"),  
          rl1.gr, rl2.gr+rremove("ylab"), rl3.gr+rremove("ylab"), rl4.gr+rremove("ylab"),
          ncol = 4, nrow = 4)

annotate_figure(fig5,
                top = text_grob("                  Ideal                      Heterocedasticidad                No linealidad                    Datos extremos    ", size=24))


```

Ahora veamos que pasa cuando la situación no es la ideal. En la columna *Heterocedasticdad* de la **Figura 5** se hace referencia a una situación en la cual los errores no presentan varianza homogénea, sino que por el contrario la varianza de los residuos se incrementa a medida que los valores predichos se incrementan (gráfico en la segunda línea). Asociado a ello, el gráfico--qq muestra una desviación importante respecto a la línea recta punteada. Este patrón es bastante común al ajustar modelos lineales generales e implica una violación a uno de los supuestos fundamentales de éstos, la homocedaticidad. En estos casos, se requiere hacer alguna modificación durante la modelación, ya sea através de la transformación de la variable respuesta, o mediante la modelación de la varianza (veremos algunos casos de ello más adelante).

Otra situación no ideal ocurre cuando el modelo lineal empleado no representa adecuadamente la relación entre las variables, como se muestra en el primer gráfico de la columna *Bo linealidad* (**Figura 5**). En este caso, la relación es claramente no lineal, tal vez de tipo polinomial de segundo orden (que puede ser modelada empleando un modelo lineal), de tipo exponencial, en cuyo caso existen modelos más adecuados (como veremos en los modelos lineales generalizados, o con modelos no lineales). El último caso de situación no-ideal presentado acá es el de la presencia de datos extremos, como se muestra en la última columna de la **Figura 5**. En este ejemplo hay dos datos cuyo valor para la variable $y$ es muy alto o muy bajo dado su valor de $x$. En este caso, tanto el gráfico de residuos contra predicho como el gráfico qq muestran claramente que estos dos datos se salen de la masa central de datos. El gráfico de Leverage presenta claramente como la distancia de Cook para estos dos datos tiene valores cercanos o incluso superiores a 0.5, indicando que son datos altamente influyentes. En estos casos, es adecuado revisar la tabla de datos para ver si se trata de un error en la transcripción de la información. Si no lo es, hay que considerar si es adecuado o no remover dichos datos del análisis; aunque esta opción no es la ideal, el ajuste del modelo y las conclusiones que se sacan del mismo pueden depender drásticamente de su inclusión.  Si la decisión es excluirlos, se debe de reportar tal situación durante la presentación de los resultados del ajuste del modelo.

# Inferencia estadística respecto al modelo

Si el diagnóstico gráfico de los modelos es positivo, de tal forma que se considera que el modelo es adecuado y que se cumplen los supuestos del mismo, es adecuado proceder entonces a la inferencia estadística. La **inferencia estadística** se refiere a la toma de decisión respecto al efecto hipotetizado de la(s) variable(s) predictoras sobre la respuesta. En términos estadísticos, la inferencia se refiere a llegar a conclusiones respecto a propiedades de una población (estadística) a partir de una muestra. Existen al menos tres aproximaciones diferentes a la inferencia estadística en la actualidad: 1) las pruebas de hipótesis basadas en estadística frecuentista, 2) la selección de modelos empleando criterios de información, y 3) la estadística bayesiana. En este curso abordaremos las dos primeras.  

## Pruebas de hipótesis

Las pruebas de hipótesis consisten, en términos generales, en calcular la probabilidad asociada a observar un evento específico si se toma por cierta una hipótesis particular, que generalmente es la de no existencia de efecto o relación (hipótesis nula). Si dicha probabilidad calculada es baja (< 0.05), entonces se presume que es debido la hipótesis de partida no es cierta en realidad, por lo cual se rechaza dicha hipótesis nula y se toma como cierta la hipótesis alterna. En el caso de los modelos lineales existen dos tipos generales de pruebas de hipótesis. El primero corresponde a las pruebas sobre los parámetros específicos del modelo, las cuales se obtienen en la sección de coefficientes del resumen del modelo.

```{r}
coef(summary(mod1))
```

En estas pruebas de hipótesis se prueba si cada parámetro estimado es diferente de cero (hipótesis nula) empleando para ello el estadístico *t de Student* (*t value*) y su probabilidad asociada. (*Pr(>|t|)*). A estas pruebas se les conoce como **pruebas t de Wald**. La conclusión en este caso es que hay evidencia suficiente para rechaz<ar la hipótesis nula de que tanto el intercepto como la pensiente son diferentes de cero.

El segundo tipo de prueba de hipótesis corresponde a la prueba del modelo en su conjunto: se prueba si la capacidad explicativa del modelo ajustado es superior a la de un modelo nulo que no posee variables predictoras y en la que por ende la variación en la respuesta es completamente debida al azar. El estadístico *F* y su probabilidad *p* asociada que se muestran en el resumen del modelo presenta corresponden a dicha prueba de hipótesis. 

```{r}
# Extracción del estadístico F asociado al modelo completo y su valor de *p*
f <- summary(mod1)$fstatistic
f
pf(f[1],f[2],f[3], lower.tail = FALSE)
```

La conclusicón es este caso es que el modelo tiene mayor capacidad explicativa de las emisiones de CO<sub>2</sub> que un modelo nulo que no tiene predictores. Es posible generar el mismo resultado si se ajusta el modelo nulo (sin predictores) y se comparan el modelo original y el nulo empelando la función `anova`

```{r}
#Ajuste del modelo nulo sin predictores
mod0 <- lm(log10(CO2.emissions.Tg) ~ 1, data = emi)

# Comparación de los modelos 
anova(mod1,mod0)
```

Nótese que el valor de F obtenido y su probabilidad asociada es el mismo en ambos casos. A este tipo de prueba se le conoce como **prueba de razón de verosimilitud** y puede ser usada para contrastar dos o más **modelos anidados**. Un modelo está anidado en otro cuando su conjunto de variables predictores es subconjunto de los predictores incluidos en el modelo más grande. En este caso, el modelo nulo está anidado en el modelo original, pues sus predictores (conjunto vacío), son un subconjunto de los predictores en el modelo original (población). Usaremos este tipo de prueba de manera recurrente más adelante para modelos más complejos.

## Selección de modelos basada en criterios de información

Cuando tenemos un conjunto de explicaciones alternativas (hipótesis), representadas en diferentes modelos estadísticos, es necesario poder comparar el desempeño de dichos modelos en la recreación de la realidad (datos). Los criterios de información proveen estimaciones de la evidencia empírica relativa en favor de cada modelo, es decir, cuál de los modelos está, en términos relativos, mejor sustentado por los datos. El criterio de información más ampliamente utilizado es el de Akaike (**AIC**), el cual está constuido a partir de dos términos, uno que mide qué tan bien el modelo reproduce la realidad y otro que mide la complejidad del modelo:

$$
\begin{equation}
 AIC = -2log(\mathcal{L}(\hat{\theta})|datos) + 2K
\end{equation}
$$

El término $-2log(\mathcal{L}(\hat{\theta})|datos)$ es sencillamente el logaritmo del valor de la función de verosimilitud. Como lo vimos previamente, la verosimilitud es una medida relativa de la posibilidad de obtener un modelo con los parámetros especificados, dados los datos que tenemos a la mano. Un modelo que describa mejor un conjunto de datos tiene un valor más grande para la función de verosimilitud en relación a otros modelos, de tal forma que el término $-2log(\mathcal{L}(\hat{\theta})|datos)$ se hace más pequeño (más negativo) a medida que incrementamos la *calidad* del modelo. Sin embargo, es conocido que entre más parametros tiene un modelo mejor se ajusta a los datos y por ende mayor es su valor de verosimilud. El término $2K$ adiciona 2 unidades por cada parámetro adicional en el modelo, *penalizando* modelos cada vez más complejos. El valor de **AIC** es entonces un indicador del ajuste relativo de los modelos al conjunto de datos, de tal forma que modelos con AIC más bajos (mayor valor de verosimilitud y menor número de parámetros) son considerados mejores en términos relativos [@Anderson2008].

El criterio de Akaike puede ser calculado empleando la función `AIC`. Sin embargo, en la práctica es muy común que el conjunto de datos analizado sea pequeño (por ejemplo, menos de 50 observaciones diferentes). Cuando la muestra es pequeña el valor de AIC tiene un sesgo que favorece a modelos con más parámetros, por lo que comúnmente se utiliza una formulación del AIC corregida para muestras pequeñas, **AICc**:


$$
AIC = -2log(\mathcal{L}(\hat{\theta})|datos) + 2K\Big(\frac{n}{n-k-1}\Big)
$$

El **AICc** puede ser calculado empleando varias funciones en R. En adelante emplearemos la función `AICc` de la librería `MuMIn`. Veamos entonces cuáles son los valores de AIC para los modelos ajustados a los datos de emisiones de CO<sub>2</sub>:

```{r warning=FALSE}
library(MuMIn)
AICc(mod0,mod1)
```

Claramente el valor de AICc para el modelo que incluye como predictor a la población es inferior al modelo nulo que no incluye predictores. En este caso, existe mucha más evidencia relativa en favor del modelo alternativo que del modelo nulo. Otra función útil para comparar dos o más modelos empleando el AIcc es la función `model.sel`:

```{r}
model.sel(mod0,mod1)
```

Esta función despliega una tabla que presenta cada modelo incluido en una fila diferente y en cada columna información sobre los mismos. Se presentan los valores de los parámetros estimados (columnas *(Int)* y *l10(Ppl.mll)*), los grados de libertad asociados a cada modelo, su valor de verosimilitud *(logLik)*, el valor del criterio de Akaike corregido para muestras pequeñas (*AICc*), la diferencia en el valor de (*AICc*) entre cada modelo y el modelo con el AICc más bajo (*delta*) y el peso de evidencia relativo asociado a cada modelo (*weight*). Estas dos últimas columnas son importantes para hacer la inferencia respecto a los modelos.

El $\Delta AICc$ indica qué tanto apoyo tiene un modelo en relación al modelo con mayor evidencia a su favor. Entre más cercano sea este valor a cero, quiere decir que dicho modelo se parece mucho al mejor modelo y que existe poca evidencia que favorezca a uno de los dos en particular. Si bien se ha usado como regla práctica que cualquier modelo con $\Delta AIC > 2$ puede ser descartado como un modelo interesante o informativo, no existe en realidad un punto de corte para descartar modelos. La posibilidad de descartar un modelo debe ser evaluada a la luz de otros argumentos [@Anderson2008]. Por su parte, la columna de pesos de evidencia $w$ representa una medida de la probabilidad relativa de los modelos. El modelo con menor valor de AIC obtiene el valor más alto de $w$. Los valores de $w$ suman en total 1, por lo que proveen evidencia de la probabilidad relativa de cada modelo de representar el proceso que generó los datos.  

# Visualización del ajuste del modelo

Una vez se ha tomado una decisión respecto a la pertinencia de un modelo para representar la relación entre las variables es ideal visualizar el ajuste del modelo, ya sea como parte del reporte de los resultados, o simplemente para confirmar que el modelo constituye una representación adecuada de los datos y así evaluar si es suficiente o si se requiere emplear otras aproximaciones para la modelación. En el caso de la regresión lineal simple, la visualización del ajuste es relativamente sencilla. En la sección anterior vimos como aplicando un `geom_smooth` en una gráfica de `ggplot` podemos obtener rápidamente una representación del modelo de regresión:

```{r warning=FALSE}
ggplot(emi, aes(log10(Population.millions), log10(CO2.emissions.Tg))) + 
  geom_point() +
  geom_smooth(method = "lm")
```

La opción "lm" en el argumento *method* permite especificar que el modelo que se quiere representar es un modelo lineal, en este caso una regresión lineal simple. La línea azul representa la estimación derivada de la porción determinística del modelo, mientras que el área gris a su alrededor el intervalo de confianza del 95% para dicha estimación. Si bien esta opción es muy práctica, tiene restricciones en cuanto al tipo y detalles de los modelos que se pueden desplegar (ver la ayuda de la función, `?geom_smooth`). Una forma más general de graficar el ajuste de un modelo es mediante la predicción de nuevos valores a partir del modelo empleando la función `predict`.

```{r warning=FALSE}
# Generación de una tabla de datos con nuevos valores de población y PIB
nuevos <- data.frame(Population.millions = seq(from = min(emi$Population.millions, na.rm = TRUE), to = max(emi$Population.millions, na.rm = TRUE), length.out = 1000),
                     GDP.million.US.dollar = seq(from = min(emi$GDP.million.US.dollar, na.rm = TRUE), to = max(emi$GDP.million.US.dollar, na.rm = TRUE), length.out = 1000))

# Generación de las estimaciones y su intervalo de confianza
predichos <- predict(mod1, newdata = nuevos, interval = c("confidence"))

# Integración de las variables predictoras y los valores estimados en una tabla
emi.nue <- data.frame(nuevos, predichos)

# Generación de la gráfica
ggplot(emi, aes(log10(Population.millions), log10(CO2.emissions.Tg))) + 
  geom_point() +
  geom_line(data=emi.nue, aes(log10(Population.millions), fit), col="blue3", size=1) +
  geom_line(data=emi.nue, aes(log10(Population.millions), lwr), col="blue1", size=1, linetype=2) +
  geom_line(data=emi.nue, aes(log10(Population.millions), upr), col="blue1", size=1, linetype=2)
  
```

Una última opción muy práctica, particularmente para modelos más complejos es la función `visreg`en la biblioteca del mismo nombre:

```{r warning=FALSE}
library(visreg)
visreg(mod1, xtrans = log)
```

El uso del argumento *xtrans* es necesario en este caso porque la función `visreg` grafica por descarte el predictor en su escala original, no transformada.

# ¿Qué reportar?

En el caso de un modelo de regresión lineal simple, posiblemente lo más sencillo es presentar una gráfica que muestre el ajuste del modelo y que incluya información adicional, como la ecuación del modelo y el valor del coeficiente de determinación $R^2$. Esto permite además evaluar la calidad del ajuste, al ver el comportamiento del modelo en relación ala nube de puntos. Si el objetivo es tan solo mencionar que existe el efecto (por ejemplo, cuando el análisis no es central en los resultados), se puede incluir directamente en el texto un resumen del procedimiento de inferencia (valores del estadístico de prueba y probabilidad asociados, o la probabilidad $w$ asociada al modelo si se empleó selección de modelos por criterios de información). En cualquier caso, siempre es altamente recomendable reportar si el modelo cumplió con los supuestos (en primera instancia sí, o de lo contrario sería un error reportarlo), o mejor aún, presentar como mqaterial suplementario dicha evidencia. Esto con el fin de mostrar que el ajuste del modelo es adecuado y que las inferencias que de él se derivan son correctas [@Zuur2010] 

## Ejercicios

1. ¿Existe relación entre las emisiones de CO<sub>2</sub> y el PIB? Ajuste un modelo de regresión lineal para la evaluar dicha relación siguiendo la serie de pasos desarrollados en esta sección.

2. La instalación básica de R trae integradas algunas tablas de datos, entre las que se encuentra `airquality`. Dicha tabla contiene información de cinco variables ambientales medidas en la ciudad de Nueva York. El objetivo original era probar como varía la concentración de ozono en el aire (gas contaminante cuando se presenta a nivel del suelo) en relación a las condiciones de radiación solar, velocidad del viento y la temperatura. Ajuste un modelo de regresión lineal simple a la relación entre ozono y velocidad del viento. ¡Es este un modelo adecuado? ¿Cómo se podría mejorar el modelo?




# Bibliografía



