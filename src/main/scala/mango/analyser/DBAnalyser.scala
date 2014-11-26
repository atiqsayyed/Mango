package mango.analyser

import mango.ui.Config

class DBAnalyser {

  def mergeSchemas(acc: Seq[Field], schemas: Seq[Field]): Seq[Field] = {
    val accFields = schemas.foldLeft(acc) { (accVal, schema) =>
      if (accVal.exists(accField => isFieldSimilar(accField, schema))) {
        accVal.map(accField => {
          if (isFieldSimilar(schema, accField)) {
            val modifiedField =
              if (schema.fieldType == "Object") {
                val existingCollectionField = accField.asInstanceOf[CollectionField]
                existingCollectionField.updateValue(mergeSchemas(
                  existingCollectionField.value.toSeq,
                  schema.asInstanceOf[CollectionField].value.toSeq).toSet)
              } else {
                accField
              }
            modifiedField.incrementCount
          } else {
            accField
          }
        })
      } else {
        accVal :+ schema
      }
    }
    accFields
  }

  def isFieldSimilar(f1: Field, f2: Field) = f1.name == f2.name && f1.fieldType == f2.fieldType

  def analyse(collectionSchemas: Seq[CollectionSchema]) = {
    collectionSchemas.map { collectionSchema =>
      val inferredSchema = collectionSchema.rowFields.foldLeft[Seq[Field]](Nil)(mergeSchemas).sortBy(-_.count)
      CollectionMergedSchema(collectionSchema.collectionName, inferredSchema)
    }
  }
}

object AnalyzerTest extends App {
  private var config = Config(None, "MongoDB", Config.host, Config.port)
  val mongoParser = config.makeParser
  val parsedData = mongoParser.parseDB("mongoschooltest")
  val dbAnalyser = new DBAnalyser()
  val analysis = dbAnalyser.analyse(parsedData)
  analysis.foreach { collectionSchema =>
    println("\n----------\nInferredSchema for " + collectionSchema.collectionName + ":\n")
    collectionSchema.fields.foreach(println)
  }
  println("----------------------------------------------------------------------------------------------------------")
  val couchParser = config.makeParser
  val couchParsedData = couchParser.parseDB("couchschooltest")
  val couchAnalysis = dbAnalyser.analyse(couchParsedData)
  couchAnalysis.foreach { collectionSchema =>
    println("\n----------\nInferredSchema for " + collectionSchema.collectionName + ":\n")
    collectionSchema.fields.foreach(println)
  }
}
