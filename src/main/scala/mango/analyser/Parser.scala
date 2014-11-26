package mango.analyser

abstract class Parser {
  def getdbNames : Seq[String]
  def getCollectionNames(dbName: String): Seq[String]

  def parseDB(dbName: String) :  List[CollectionSchema]
  def parseCollection(dbName: String, collectionName: String) : Iterator[Seq[Field]]
}