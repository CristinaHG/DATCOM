//--packages djgg:PCARD:1.3

import org.apache.spark.{SparkConf, SparkContext}
import org.apache.spark.mllib.regression.LabeledPoint
import org.apache.spark.mllib.linalg.{Vector, Vectors}


//Load train and test

val pathTrain = "file:////home/spark/datasets/susy-10k-tra.data"
val rawDataTrain = sc.textFile(pathTrain)

val pathTest = "file:////home/spark/datasets/susy-10k-tst.data"
val rawDataTest = sc.textFile(pathTest)

val train = rawDataTrain.map{line =>
    val array = line.split(",")
    var arrayDouble = array.map(f => f.toDouble) 
    val featureVector = Vectors.dense(arrayDouble.init) 
    val label = arrayDouble.last 
    LabeledPoint(label, featureVector)
}.persist

train.count
train.first

val test = rawDataTest.map { line =>
    val array = line.split(",")
    var arrayDouble = array.map(f => f.toDouble) 
    val featureVector = Vectors.dense(arrayDouble.init) 
    val label = arrayDouble.last 
    LabeledPoint(label, featureVector)
}.persist

test.count
test.first


//Class balance

val classInfo = train.map(lp => (lp.label, 1L)).reduceByKey(_ + _).collectAsMap()


//Decision tree

import org.apache.spark.mllib.tree.DecisionTree
import org.apache.spark.mllib.tree.model.DecisionTreeModel

// Train a DecisionTree model.
//  Empty categoricalFeaturesInfo indicates all features are continuous.
val numClasses = 2
val categoricalFeaturesInfo = Map[Int, Int]()
val impurity = "gini"
val maxDepth = 5
val maxBins = 32

val model = DecisionTree.trainClassifier(train, numClasses, categoricalFeaturesInfo, impurity, maxDepth, maxBins)

// Evaluate model on test instances and compute test error
val labelAndPreds = test.map { point =>
  val prediction = model.predict(point.features)
  (point.label, prediction)
}
val testAcc = 1 - labelAndPreds.filter(r => r._1 != r._2).count().toDouble / test.count()
println(s"Test Accuracy = $testAcc")


//Random Forest

import org.apache.spark.mllib.tree.RandomForest
import org.apache.spark.mllib.tree.model.RandomForestModel

// Train a RandomForest model.
// Empty categoricalFeaturesInfo indicates all features are continuous.
val numClasses = 2
val categoricalFeaturesInfo = Map[Int, Int]()
val numTrees = 100
val featureSubsetStrategy = "auto" // Let the algorithm choose.
val impurity = "gini"
val maxDepth = 4
val maxBins = 32

val model = RandomForest.trainClassifier(train, numClasses, categoricalFeaturesInfo, numTrees, featureSubsetStrategy, impurity, maxDepth, maxBins)

// Evaluate model on test instances and compute test error
val labelAndPreds = test.map { point =>
  val prediction = model.predict(point.features)
  (point.label, prediction)
}
val testAcc = 1 - labelAndPreds.filter(r => r._1 != r._2).count.toDouble / test.count()
println(s"Test Accuracy = $testAcc")


//PCARD

import org.apache.spark.mllib.tree.PCARD

val cuts = 5
val trees = 10

val pcardTrain = PCARD.train(train, trees, cuts)

val pcard = pcardTrain.predict(test)


def avgAcc(labels: Array[Double], predictions: Array[Double]): (Double, Double) = {
  var cont = 0
  for (i <- labels.indices) {
    if (labels(i) == predictions(i)) {
      cont += 1
    }
  }
  (cont / labels.length.toFloat, 1 - cont / labels.length.toFloat)
}

print("PCARD Accuracy: " + avgAcc(test.map(_.label).collect(), pcard)._1)

val labelAndPreds = sc.parallelize(pcard).zipWithIndex.map{case (v,k) => (k,v)}.join(test.zipWithIndex.map{case (v,k) => (k,v.label)}).map(_._2)

//Metrics

import org.apache.spark.mllib.evaluation.MulticlassMetrics

val metrics = new MulticlassMetrics(labelAndPreds)
val precision = metrics.precision
val cm = metrics.confusionMatrix
