
library(mlbench)
library(arules)
data(Zoo)
dim(Zoo)
str(Zoo)
head(Zoo)
#cols <- c(1:12, 14:16)
#Zoo[,cols] <- lapply(Zoo[,cols], as.factor)
#Zoo[["legs"]] <- ordered(cut(Zoo[["legs"]], c(-1, 1, 2, 4, max(Zoo[["legs"]]))))
#Zoo[["legs"]]
#zooT <- as(Zoo, "transactions")
#zooT

# cuantas piernas tienen los animales
table(Zoo[["legs"]])
# generamos etiquetas en función de si tiene piernas
has_legs <- Zoo[["legs"]]>0
has_legs
table(has_legs) # 23 no tienen patas, 78 sí
Zoo[["legs"]] <- has_legs
#pasamos a factores todas las columnas
Zoo[,1:16]<-lapply(Zoo[, 1:16],as.factor)
str(Zoo)
# Convertimos el data.frame en un conjunto de transacciones
zoo<-as(Zoo,"transactions")
summary(zoo) #101 transacciones y 23 items, items frecuentes: backbone, breathes,legs,tail,toothed
# representamos la distribución de los items en las trans
image(zoo) # algunos items (pej 10,20) aparecen en muchas trans, mientras que otros (p ej 40) aparecen en muy pocas
# representamos items frecuentes
itemFrequencyPlot(zoo, support=0.2)
# entrenar Apriori para extraer itemsets frecuentes
izoo <- apriori(zoo, parameter = list(support = 0.2, target="frequent"))
# ordenar por valor de soporte
izoo <- sort(izoo, by="support")
inspect(head(izoo,n=20))
# consultar el tamaño de los itemsets frecuentes
size(izoo)
barplot(table(size(izoo)), xlab="itemset size", ylab="count")
inspect(izoo[size(izoo)==2])
# extaer itemsets maximales
imaxZoo <- izoo[is.maximal(izoo)]
inspect(head(sort(izoo, by="support")))
# extaer itemsets cerrados
icloZoo <- izoo[is.closed(izoo)]
inspect(head(sort(icloZoo, by="support")))
barplot( c(frequent=length(izoo), closed=length(icloZoo),
           maximal=length(imaxZoo)), ylab="count", xlab="itemsets")
# Apriori para extraer reglas con minSop=0.2 ,conf=0.8,minlen=2
rules <- apriori(Zoo, parameter = list(support = 0.2, confidence = 0.9, minlen = 6))
summary(rules)
# inspeccionar reglas
inspect(head(rules))
# evaluar calidad
quality(head(rules))
# ordenar reglas según interés
rulesSorted = sort(rules, by = "lift")
inspect(head(rulesSorted))
# seleccionar subconjunto que reglas
ruleslegs <- subset(rules, subset = lhs %in% "legs=TRUE" & lift > 2.5)
inspect(head(ruleslegs))
# eliminar reglas redundantes
subsetMatrix <- is.subset(ruleslegs, ruleslegs)
subsetMatrix[lower.tri(subsetMatrix, diag=TRUE)] <- FALSE
redundant <- colSums(subsetMatrix, na.rm=TRUE) >= 1
rulesPruned <- ruleslegs[!redundant]
inspect(head(rulesPruned))
# otras medidas de interés
mInteres <- interestMeasure(rulesPruned, measure=c("hyperConfidence", "leverage"
                                                   ,"phi", "gini"), transactions=Zoo)
quality(rulesPruned) <- cbind(quality(rulesPruned), mInteres)
inspect(head(sort(rulesPruned, by="phi")))

# inspect(head(rulesPruned))
#lhs                            rhs              support   confidence lift     count
#[1] {milk=TRUE,type=mammal}     => {breathes=TRUE}  0.4059406 1          1.262500 41
#[2] {milk=TRUE,type=mammal}     => {feathers=FALSE} 0.4059406 1          1.246914 41
#[3] {milk=TRUE,type=mammal}     => {backbone=TRUE}  0.4059406 1          1.216867 41
#[4] {milk=TRUE,type=mammal}     => {venomous=FALSE} 0.4059406 1          1.086022 41
#[5] {breathes=TRUE,type=mammal} => {feathers=FALSE} 0.4059406 1          1.246914 41
#[6] {breathes=TRUE,type=mammal} => {backbone=TRUE}  0.4059406 1          1.216867 41


#inspect(head(ruleslegs))
#lhs                            rhs              support   confidence lift     count
#[1] {hair=FALSE,legs=FALSE}     => {airborne=FALSE} 0.2178218 1.0000000  1.311688 22
#[2] {airborne=FALSE,legs=FALSE} => {hair=FALSE}     0.2178218 0.9565217  1.665667 22
#[3] {hair=FALSE,legs=FALSE}     => {feathers=FALSE} 0.2178218 1.0000000  1.246914 22
#[4] {feathers=FALSE,legs=FALSE} => {hair=FALSE}     0.2178218 0.9565217  1.665667 22
#[5] {hair=FALSE,legs=FALSE}     => {domestic=FALSE} 0.2079208 0.9545455  1.095558 21
#[6] {legs=FALSE,domestic=FALSE} => {hair=FALSE}     0.2079208 0.9545455  1.662226 21

# CONCLUSION: HAY ALGÚN ANIMAL QUE  which(Zoo$feathers==FALSE & Zoo$legs==FALSE & Zoo$hair==TRUE)
Zoo[75,]


#[1] {hair=FALSE,feathers=FALSE,airborne=FALSE,legs=FALSE} => {domestic=FALSE} 0.2079208 0.9545455  1.095558 21    0.04411765
#leverage   phi    gini
#[1] 0.01813548 0.1312 0.003860832

# CONCLUSION  which(Zoo$feathers==FALSE & Zoo$legs==FALSE & Zoo$hair==FALSE & Zoo$airborne==FALSE & Zoo$domestic==TRUE)

#lhs                                                              rhs             support   confidence lift  count
#[1] {eggs=TRUE,milk=FALSE,airborne=TRUE,breathes=TRUE,legs=TRUE}  => {toothed=FALSE} 0.2178218 1          2.525 22
#[2] {eggs=TRUE,milk=FALSE,airborne=TRUE,fins=FALSE,legs=TRUE}     => {toothed=FALSE} 0.2178218 1          2.525 22
#[3] {eggs=TRUE,airborne=TRUE,breathes=TRUE,fins=FALSE,legs=TRUE}  => {toothed=FALSE} 0.2178218 1          2.525 22
#[4] {milk=FALSE,airborne=TRUE,breathes=TRUE,fins=FALSE,legs=TRUE} => {toothed=FALSE} 0.2178218 1          2.525 22
#hyperConfidence leverage  phi       gini
#[1] 0.8382353       0.1315557 0.6516775 0.203162
#[2] 0.8382353       0.1315557 0.6516775 0.203162
#[3] 0.8382353       0.1315557 0.6516775 0.203162
#[4] 0.8382353       0.1315557 0.6516775 0.203162


# ITEMS NEGADOS
library(mlbench)
library(arules)
data(Zoo)
ZooNegs<-Zoo
has_legs <- Zoo[["legs"]]>0
has_no_legs<-!has_legs # creamos item negado
index<-which(colnames(Zoo)=="legs")
ZooNegs<-ZooNegs[,-index]
ZooNegs<-cbind(ZooNegs,has_legs)
ZooNegs<-cbind(ZooNegs,has_no_legs)
dim(ZooNegs)
#pasamos a factores todas las columnas
ZooNegs[,1:18]<-lapply(ZooNegs[, 1:18],as.factor)

zooNegs<-as(ZooNegs,"transactions")
summary(zooNegs) #101 transacciones y 23 items, items frecuentes: backbone, breathes,legs,tail,toothed
# representamos la distribución de los items en las trans
image(zooNegs) # algunos items (pej 10,20) aparecen en muchas trans, mientras que otros (p ej 40) aparecen en muy pocas
# representamos items frecuentes
itemFrequencyPlot(zooNegs, support=0.2)
# entrenar Apriori para extraer itemsets frecuentes
izooNeg <- apriori(zooNegs, parameter = list(support = 0.2, target="frequent"))
# ordenar por valor de soporte
izooNeg <- sort(izooNeg, by="support")
inspect(head(izooNeg,n=20))
# consultar el tamaño de los itemsets frecuentes
size(izooNeg)
barplot(table(size(izooNeg)), xlab="itemset size", ylab="count")
inspect(izooNeg[size(izooNeg)==3])
# extaer itemsets maximales
imaxZooNeg <- izooNeg[is.maximal(izooNeg)]
inspect(head(sort(izooNeg, by="support")))
# extaer itemsets cerrados
icloZooNeg <- izooNeg[is.closed(izooNeg)]
inspect(head(sort(icloZooNeg, by="support")))
barplot( c(frequent=length(izooNeg), closed=length(icloZooNeg),
           maximal=length(imaxZooNeg)), ylab="count", xlab="itemsets")
# Apriori para extraer reglas con minSop=0.2 ,conf=0.8,minlen=2
rules <- apriori(ZooNegs, parameter = list(support = 0.2, confidence = 0.8, minlen = 2))
summary(rules)
# inspeccionar reglas
inspect(head(rules))
# evaluar calidad
quality(head(rules))
# ordenar reglas según interés
rulesSorted = sort(rules, by = "lift")
inspect(head(rulesSorted))
# seleccionar subconjunto que reglas
ruleslegs <- subset(rules, subset = lhs %in% "type=mammal" & lhs %in% "hair=TRUE" & lift > 2.3)
inspect(head(ruleslegs))
# eliminar reglas redundantes
subsetMatrix <- is.subset(ruleslegs, ruleslegs)
subsetMatrix[lower.tri(subsetMatrix, diag=TRUE)] <- FALSE
redundant <- colSums(subsetMatrix, na.rm=TRUE) >= 1
rulesPruned <- ruleslegs[!redundant]
inspect(head(rulesPruned))
# otras medidas de interés
mInteres <- interestMeasure(rulesPruned, measure=c("hyperConfidence", "leverage"
                                                   ,"phi", "gini"), transactions=Zoo)
quality(rulesPruned) <- cbind(quality(rulesPruned), mInteres)
inspect(head(sort(rulesPruned, by="phi")))

#{eggs=TRUE,milk=FALSE,predator=FALSE,breathes=TRUE,fins=FALSE}                => {toothed=FALSE} 0.2079208 0.9545455  2.410227 21
#[2] {feathers=FALSE,aquatic=FALSE,predator=FALSE,has_legs=TRUE,has_no_legs=FALSE} => {hair=TRUE}     0.2277228 0.8518519  2.000861 23
#[3] {feathers=FALSE,aquatic=FALSE,predator=FALSE,breathes=TRUE,has_legs=TRUE}     => {hair=TRUE}     0.2277228 0.8518519  2.000861 23
#[4] {feathers=FALSE,aquatic=FALSE,predator=FALSE,fins=FALSE,has_legs=TRUE}        => {hair=TRUE}     0.2277228 0.8518519  2.000861 23
#[5] {feathers=FALSE,aquatic=FALSE,predator=FALSE,breathes=TRUE,has_no_legs=FALSE} => {hair=TRUE}     0.2277228 0.8518519  2.000861 23
#[6] {feathers=FALSE,aquatic=FALSE,predator=FALSE,fins=FALSE,has_no_legs=FALSE}    => {hair=TRUE}     0.2277228 0.8518519  2.000861 23


#lhs                                     rhs          support   confidence lift     count
#[1] {hair=TRUE,type=mammal}              => {milk=TRUE}  0.3861386 1.000000   2.463415 39
#[2] {hair=TRUE,type=mammal}              => {eggs=FALSE} 0.3762376 0.974359   2.343101 38
#[3] {hair=TRUE,milk=TRUE,type=mammal}    => {eggs=FALSE} 0.3762376 0.974359   2.343101 38
#[4] {hair=TRUE,eggs=FALSE,type=mammal}   => {milk=TRUE}  0.3762376 1.000000   2.463415 38
#[5] {hair=TRUE,catsize=TRUE,type=mammal} => {milk=TRUE}  0.2970297 1.000000   2.463415 30
#[6] {hair=TRUE,toothed=TRUE,type=mammal} => {milk=TRUE}  0.3762376 1.000000   2.463415 38

#lhs                                     rhs          support   confidence lift     count
#[1] {hair=TRUE,type=mammal}              => {milk=TRUE}  0.3861386 1.000000   2.463415 39
#[2] {hair=TRUE,type=mammal}              => {eggs=FALSE} 0.3762376 0.974359   2.343101 38
#[3] {hair=TRUE,milk=TRUE,type=mammal}    => {eggs=FALSE} 0.3762376 0.974359   2.343101 38
#[4] {hair=TRUE,eggs=FALSE,type=mammal}   => {milk=TRUE}  0.3762376 1.000000   2.463415 38
#[5] {hair=TRUE,catsize=TRUE,type=mammal} => {milk=TRUE}  0.2970297 1.000000   2.463415 30
#[6] {hair=TRUE,toothed=TRUE,type=mammal} => {milk=TRUE}  0.3762376 1.000000   2.463415 38

#which(ZooNegs$type=="mammal" & ZooNegs$hair==TRUE & ZooNegs$eggs==TRUE)
#[1] 64
#> ZooNegs[64,]
#hair feathers eggs milk airborne aquatic predator toothed backbone breathes venomous  fins tail domestic catsize
#platypus TRUE    FALSE TRUE TRUE    FALSE    TRUE     TRUE   FALSE     TRUE     TRUE    FALSE FALSE TRUE    FALSE    TRUE
#type has_legs has_no_legs
#platypus mammal     TRUE       FALSE