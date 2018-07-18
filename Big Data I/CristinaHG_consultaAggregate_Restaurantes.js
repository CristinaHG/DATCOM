db.runCommand({
aggregate: "restaurants",
pipeline : [
{$match: {"grades.grade" : {$eq :"A" }}},
//Agrupa por código de pais y le añade los arrays ciudad1 y ciudad2 con los datos de las ciudades de ese pais
{$group: {_id: "$cuisine", "restaurante1":{$push: {name: "$name", address:"$address"}},
"restaurante2":{$push: {name: "$name", address:"$address"}}}},
{$unwind: "$restaurante1"}, //Desanida ciudad1, crea un documento por cada elemento del array ciudad1
{$unwind: "$restaurante2"}, //Desanida ciudad2, crea un documento por cada elemento del array ciudad2
//Calcula la distancia entre cada par de ciudades en el campo “distancia”, devuelve otros datos necesarios.
{$project: {_id: 0, Cocina: "$cuisine", restaurante1: "$restaurante1.name", restaurante2: "$restaurante2.name",
distancia:{ $sqrt: {$sum: [{$pow: [{$subtract: ["$restaurante1.address.coord[0]","$restaurante2.address.coord[0]"]},2 ]},
{$pow: [{ $subtract: ["$restaurante1.address.coord[1]","$restaurante2.address.coord[1]" ]},2 ]}]}}}},
// Eliminamos parejas de ciudades redundantes y aquellas parejas que están a distancia 0.
{$redact: {"$cond": [{$and:[{"$lt": ["$restaurante1", "$restaurante2"]},{"$ne":["$distancia",0.0]}]},"$$KEEP","$$PRUNE"]}},
{$group: {_id: "$Cocina", "dist_min": {$min: "$distancia"}, // Obtenemos las distancia mínima para cada país
// Añadimos a la salida un “array” con los datos de todas las parejas de ciudades de cada País
"parejas":{$push: {restaurante1: "$restaurante1", restaurante2: "$restaurante2", distancia: "$distancia"}}}},
{$unwind: "$parejas"}, // Desanidamos el “array” parejas
// Nos quedamos con aquellas parejas cuya distancia coincide con la distancia mínima de ese país
{$redact: {"$cond": [{"$eq": ["$dist_min", "$parejas.distancia"]}, "$$KEEP", "$$PRUNE"]}},
// Proyectamos sobre los datos solicitados
{$project: {_id: 0, "cuisine": "$_id", "restaurante1": "$parejas.restaurante1", "restaurante2": "$parejas.restaurante2",
"distancia": "$dist_min"}}
],
allowDiskUse: true, // Permite el uso de disco para operaciones intermedias que no quepan en memoria
cursor: { batchSize: 100 }});

