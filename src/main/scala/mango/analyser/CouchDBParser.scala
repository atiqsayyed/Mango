package mango.analyser

import scala.Array.canBuildFrom
import scala.collection.JavaConverters.asScalaBufferConverter
import scala.collection.JavaConverters.asScalaSetConverter
import com.fourspaces.couchdb.Session
import net.sf.json.JSONObject
import mango.ui.Config

class CouchDBParser(config: Config) extends Parser{

  def parseDB(dbName: String) = {
    val dbSession = new Session(config.host, config.port);
    val db = dbSession.getDatabase(dbName);
    val allViews = db.getAllDesignDocuments.getResults().asScala
    val viewsDoc = allViews.map(doc => {
      val document = doc.getJSONObject().getJSONObject("doc").get("views")
      val funName = document match {
        case json: JSONObject => json.keySet
      }
      val viewFunName = funName.iterator().next()
      val viewRes = db.view(doc.getViewDocumentId + "/" + viewFunName)
      val result = viewRes.getResults.asScala.toList
      val resultDocs = result.map(document => {
        val fullDocument = db.getDocument(document.getString("id"))
        val keys = fullDocument.keySet.asScala
        val field = keys.map(key => {
          mkField(0, key.toString, fullDocument.get(key).toString)
        }).toSeq
        field
      })
      CollectionSchema(doc.getViewDocumentId, resultDocs.toList)
    }).toList
    println("Schema ===>>>" + viewsDoc)
    viewsDoc
  }

  def parseCollection(dbName: String, collectionName: String) = {
    val dbSession = new Session(config.host, config.port.toInt);
    val db = dbSession.getDatabase(dbName);
    val allViews = db.getAllDesignDocuments.getResults().asScala
    val viewsDoc = allViews.filter(doc => {collectionName.equalsIgnoreCase(doc.getViewDocumentId)}).head

    val document = viewsDoc.getJSONObject().getJSONObject("doc").get("views")
    val funName = document match {
      case json: JSONObject => json.keySet
    }
    val viewFunName = funName.iterator().next()
    val viewRes = db.view(viewsDoc.getViewDocumentId + "/" + viewFunName)
    val resultDoc = viewRes.getResults.asScala.toList
    val resultDocs = resultDoc.map(document => {
      val fullDocument = db.getDocument(document.getString("id"))
      val keys = fullDocument.keySet.asScala
      val field = keys.map(key => {
        mkField(0, key.toString, fullDocument.get(key).toString)
      }).toSeq
      field
    }).toIterator
    resultDocs
  }

  def mkField(level: Int, name: String, obj: String): Field = {
    try {
      val intVal = obj.toInt
      IntegerField(level, name, intVal)
    } catch {
      case _ => {
        try {
          val booleanVal = obj.toBoolean
          BooleanField(level, name, booleanVal)
        } catch {
          case _ => {
            try {
              val doubleVal = obj.toDouble
              DoubleField(level, name, doubleVal)
            } catch {
              case _ => {
                try {
                  obj match {
                    case _ if ((obj.startsWith("[")) && (obj.endsWith("]"))) => {
                      val arrayVal = obj.replace("[", "").replace("]", "").replace("\"", "").split(",")
                      ArrayField(level, name, getValues(level + 1, arrayVal, Some("[i]")))
                    }
                    case _ => StringField(level, name, obj)
                  }
                }
              }
            }
          }
        }
      }
    }
  }

  private def getValues(level: Int, arrVal: Array[String], name: Option[String]) = {
    arrVal.map(x => mkField(level, name.getOrElse(""), x.toString)).toSet
  }

  private val dbSession = new Session(config.host, config.port.toInt);

  def getdbNames = {
    dbSession.getDatabaseNames.asScala.seq
  }

  def getCollectionNames(dbName: String) : Seq[String] = {
    val db = dbSession.getDatabase(dbName);
    val allViews = db.getAllDesignDocuments.getResults().asScala
    val viewNames = allViews.map(doc => {
      doc.getViewDocumentId
    }).toSet
    viewNames.toSeq
  }

}