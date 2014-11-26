package mango.analyser
import com.esyfur.{ rql => r }
import scala.collection.mutable.ArrayBuffer
import java.util.Date

class RethinkDBParser(host: String, port: Int = 28015) extends Parser {

  def toStringSeq(anySeq: Seq[Any]) = anySeq.map(_.toString)

  val conn = r.connect(host).repl().use("");

  def getdbNames: Seq[String] = {
    implicit def toArrayBuffer(arrayList: Any) = {
      arrayList.asInstanceOf[scala.collection.GenTraversableOnce[_]]
    }
    toStringSeq(r.dbList.run().toBuffer.flatten.toList)
  }
  def getCollectionNames(dbName: String): Seq[String] = {
    implicit def toArrayBuffer(arrayList: Any) = {
      arrayList.asInstanceOf[scala.collection.GenTraversableOnce[_]]
    }
    toStringSeq(r.db(dbName).tableList.run().toBuffer.flatten.toList)
  }

  def parseDB(dbName: String) = {
    val collectionNames = getCollectionNames(dbName)
    collectionNames.toList.filter(_ != "system.indexes").map(collectionName => {
      val rowFields = parseCollection(dbName, collectionName)
      CollectionSchema(collectionName, rowFields.toList)
    })
  }

  def getListData(tableData: Any) = {
    val listData = tableData match {
      case a: ArrayBuffer[_] => a.toList
      case _ => List()
    }
    listData.asInstanceOf[List[Map[String, String]]]
  }

  def getCollection(dbName: String, collName: String) = {
    val tableData = r.db(dbName).table(collName).run()
    tableData.map(x => {
      getListData(x)
    })
  }

  def parseCollection(dbName: String, collectionName: String) = {
    val rows = getCollection(dbName, collectionName).flatten.toList
    rows.map(row => {
      val keys = row.keySet
      val fields = keys.map(key => {
        mkField(0, key, row.getOrElse(key, "").asInstanceOf[Any])
      })
      fields.toSeq
    }).toIterator
  }

  def mkField(level: Int, name: String, obj: Any): Field = obj match {
    case i: Int => IntegerField(level, name, i)
    case d: Double => DoubleField(level, name, d)
    case l: Long => LongField(level, name, l)
    case b: Boolean => BooleanField(level, name, b)
    case s: String => StringField(level, name, s)
    case date: Date => DateField(level, name, date)
    case array: ArrayBuffer[_] => ArrayField(level, name, getValues(level + 1, array.toArray, Some("[i]")))
    case obj: Map[_, _] => ObjectField(level, name, getObjectValues(level + 1, obj.asInstanceOf[Map[String, String]], None))
    case null => new NullField(level, 0)
    case x => {
      println("Unknown field value and type:", x, x.getClass)
      new NullField(level, 0)
    }
  }
  private def getValues(level: Int, arrVal: Array[Any], name: Option[String]) = {
    arrVal.map(x => mkField(level, name.getOrElse(""), x)).toSet
  }

  private def getObjectValues(level: Int, objVal: Map[String, String], name: Option[String]) = {
    val keys = objVal.keySet
    keys.map(key => {
      mkField(level, key, objVal.getOrElse(key, "").asInstanceOf[Any])
    })

  }

}