// Mª Cristina Heredia Gómez
db.runCommand({ mapReduce: "restaurants",
 map : function Map() {
		var key = this.cuisine;
		emit(key, {
				"data":
				[
					{
						"name" : this.name,
						"address" : this.address
					}
				]
		});
 },
 reduce : function Reduce(key, values) {
		var reduced = {"data":[]};
		for (var i in values) {
				var inter = values[i];
				for (var j in inter.data) {
					reduced.data.push(inter.data[j]);
				}
		}
		return reduced;
 },
 finalize : function Finalize(key, reduced) {
		if (reduced.data.length == 1) {
			return { "message" : "Solo hay un restaurante con este tipo de cocina" };
		}
		var min_dist = 999999999999;
		var restaurante1 = { "name": "" };
		var restaurante2 = { "name": "" };
		var r1;
		var r2;
		var dist;
		for (var i in reduced.data) {
				for (var j in reduced.data) {
					if (i>=j) continue;
					r1 = reduced.data[i];
					r2 = reduced.data[j];
					address1=r1.address.coord;
					address2=r2.address.coord;
					dist = (address1[0]-address2[0])*(address1[0]-address2[0])+(address1[1]-address2[1])*(address1[1]-address2[1]);
					if (dist < min_dist && dist > 0) {
						min_dist = dist;
						restaurante1 = r1;
						restaurante2 = r2;
					}
				}
		}
		return {"restaurante1": restaurante1.name,
			"restaurante2": restaurante2.name, 
			"address1": restaurante1.address.street.concat(", ").concat(restaurante1.address.building ), 
			"address2": restaurante2.address.street.concat(", ").concat( restaurante2.address.building ),
			"dist": Math.sqrt(min_dist),
			"numRestaurantes": reduced.data.length				
		};
 },
 query : { "grades.grade" : {"$eq" :"A" }},
 out: { merge: "rest_mapreduce" }
});
