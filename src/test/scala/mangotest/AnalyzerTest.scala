package mangotest

import org.bson.types.ObjectId
import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import com.mongodb.casbah.commons.MongoDBObject

import mango.analyser.ArrayField
import mango.analyser.DoubleField
import mango.analyser.IntegerField
import mango.analyser.LongField
import mango.analyser.MongoDBParser
import mango.analyser.ObjectField
import mango.analyser.ObjectIdField
import mango.analyser.StringField

class AnalyzerTest extends FlatSpec with ShouldMatchers {

  val parser = new MongoDBParser("localhost")
  private val db = parser.getDb("mango_test")
  db.dropDatabase

  "Parse Collection for embedded Collection with Array" should " Return the parsed embebded collection" in {
    val testCollectionName = "parser_test"
    db.getCollection(testCollectionName).drop
    val collection = db(testCollectionName)
    
    collection += MongoDBObject(
        "_id" -> new ObjectId("50b7051c06361a93339658a4"),
        "name" -> "name",
        "address" -> MongoDBObject(
          "city" -> "vapi",
          "state" -> "gujarat"
        ),
        "document" -> MongoDBObject(
          "document1" -> MongoDBObject("docname" -> "document"),
          "array" -> ("a1", "a2")
        ),
        "age" -> 10,
        "year" -> 2000L,
        "energy" -> 10.10d,
        "power" -> 100d
      )

     val parseCollection = parser.parseCollection("mango_test", testCollectionName).next.toSet
     val testCollection = Set(new ObjectIdField(0, "_id", new ObjectId("50b7051c06361a93339658a4"), 1),
       new ObjectField(0, "address", Set(
         new StringField(1, "city", "vapi", 1),
         new StringField(1, "state", "gujarat", 1)),
         1),
       new StringField(0, "name", "name", 1),
       new ObjectField(0, "document", Set(
         new ObjectField(1, "document1", Set(new StringField(2, "docname", "document", 1)), 1),
         new ArrayField(1, "array", Set(
             new StringField(2, "[i]", "a1", 1),
             new StringField(2, "[i]", "a2", 1)),
             1
           )
       )),
       new IntegerField(0, "age", 10),
       new LongField(0, "year", 2000L),
       new DoubleField(0, "energy", 10.10d),
       new DoubleField(0, "power", 100d)
     )

    parseCollection should equal(testCollection)
  }

}