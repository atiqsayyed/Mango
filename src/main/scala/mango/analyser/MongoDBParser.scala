package mango.analyser

import java.util.Date
import scala.collection.JavaConverters.asScalaSetConverter
import org.bson.types.ObjectId
import com.mongodb.BasicDBList
import com.mongodb.BasicDBObject
import com.mongodb.DBObject
import com.mongodb.casbah.MongoConnection

class MongoDBParser(host:String, port:Int = 27017) extends Parser{
  private val mongoConn = MongoConnection(host, port)

  def getdbNames = {
    mongoConn.databaseNames
  }

  def getCollectionNames(dbName: String) = {
    val db = getDb(dbName)
    db.collectionNames.toSeq
  }
  
  def getDb(dbName: String) = {
    val db = mongoConn.getDB(dbName)
    db
  }

  def getCollection(dbName: String, collName: String) = {
    val db = getDb(dbName)
    db(collName)
  }

  def parseDB(dbName: String) = {
    val collectionNames = getCollectionNames(dbName)
    collectionNames.toList.filter(_ != "system.indexes").map(collectionName => {
      val rowFields = parseCollection(dbName, collectionName)
      CollectionSchema(collectionName, rowFields.toList)
    })
  }

  def parseCollection(dbName: String, collectionName: String) = {
    val coll = getCollection(dbName, collectionName)
    val rows = coll.find()
    rows.map(row => {
      val keys = row.keySet().asScala
      val fields = keys.map(key => {
        mkField(0, key, row.get(key))
      })
      fields.toSeq
    })
  }
  
  private def getValues(level: Int, dbObject: DBObject, name: Option[String]) = {
    val keys = dbObject.keySet().asScala
    keys.map(x => mkField(level, name.getOrElse(x), dbObject.get(x))).toSet
  }

  def mkField(level: Int, name: String, obj: Any): Field = obj match {
    case d: Double => DoubleField(level, name, d)
    case l: Long => LongField(level, name, l)
    case b: Boolean => BooleanField(level, name, b)
    case s: String => StringField(level, name, s)
    case i: Int => IntegerField(level, name, i)
    case o: BasicDBObject => ObjectField(level, name, getValues(level + 1, o, None))
    case a: BasicDBList => ArrayField(level, name, getValues(level + 1, a, Some("[i]")))
    case id: ObjectId => ObjectIdField(level, name, id, 1)
    case date: Date => DateField(level, name, date)
    case null => new NullField(level, 0)
    case x => println("Unknown field value and type:", x, x.getClass); new NullField(level, 0)
  }
}