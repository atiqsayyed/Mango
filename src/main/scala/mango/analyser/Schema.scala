package mango.analyser

import java.util.Date
import scala.collection.JavaConverters.asScalaSetConverter
import org.bson.types.ObjectId
import com.mongodb.BasicDBList
import com.mongodb.BasicDBObject
import com.mongodb.DBObject

abstract class Field(val level: Int, val name: String, val fieldType: String) {
  protected final val indent = "   " * level
  val count: Int
  override def toString = { indent + name + " : " + fieldType + " (" + count + ")" }

  def getChildDepth = 1

  def incrementCount: Field

  def isSimilarTo(that: Field) = name == that.name && fieldType == that.fieldType
}

case class ObjectIdField(levelIn: Int, nameIn: String, value: ObjectId, val count: Int = 1) extends Field(levelIn, nameIn, "ObjectId") {
  override def incrementCount = this.copy(count = this.count + 1)
}
case class IntegerField(levelIn: Int, nameIn: String, value: Int, val count: Int = 1) extends Field(levelIn, nameIn, "Integer") {
  override def incrementCount = this.copy(count = this.count + 1)
}
case class DoubleField(levelIn: Int, nameIn: String, value: Double, val count: Int = 1) extends Field(levelIn, nameIn, "Double") {
  override def incrementCount = this.copy(count = this.count + 1)
}
case class LongField(levelIn: Int, nameIn: String, value: Long, val count: Int = 1) extends Field(levelIn, nameIn, "Long") {
  override def incrementCount = this.copy(count = this.count + 1)
}
case class BooleanField(levelIn: Int, nameIn: String, value: Boolean, val count: Int = 1) extends Field(levelIn, nameIn, "Boolean") {
  override def incrementCount = this.copy(count = this.count + 1)
}
case class StringField(levelIn: Int, nameIn: String, value: String, val count: Int = 1) extends Field(levelIn, nameIn, "String") {
  override def incrementCount = this.copy(count = this.count + 1)
}
case class DateField(levelIn: Int, nameIn: String, value: Date, val count: Int = 1) extends Field(levelIn, nameIn, "Date") {
  override def incrementCount = this.copy(count = this.count + 1)
}
case class NullField(levelIn: Int, val count: Int = 0) extends Field(levelIn, "NULL", "NULL") {
  override def incrementCount = this.copy(count = this.count + 1)
}

abstract class CollectionField(level: Int, name: String) extends Field(level, name, "Object") {
  val value: Set[Field]
  def updateValue(newValue: Set[Field]): CollectionField
  override def getChildDepth = 1 + (if (value.isEmpty) 0 else value.map(_.getChildDepth).max)
}

case class ObjectField(levelIn: Int, nameO: String, val value: Set[Field], val count: Int = 1) extends CollectionField(levelIn, nameO) {

  override def updateValue(newValue: Set[Field]) = this.copy(value = newValue)

  override def toString = {
    indent + nameO + " (" + count + ") => {\n" + value.mkString("\n") + "\n" + indent + "}"
  }
  override def incrementCount = this.copy(count = this.count + 1)
}

case class ArrayField(levelIn: Int, nameIn: String, val value: Set[Field], val count: Int = 1) extends CollectionField(levelIn, nameIn) {
  override def updateValue(newValue: Set[Field]) = this.copy(value = newValue)
  override def toString = {
    val firstType = value.headOption.map(_.fieldType).getOrElse("NULL")
    val allSame = value.forall(_.fieldType == firstType)
    if (allSame) {
      indent + name + "(" + count + ") Array[" + firstType + "] (" + value.size + ")"
    } else {
      indent + name + "(" + count + ") => Array[\n" + value.mkString("\n") + "\n" + indent + "]"
    }
  }
  def getMergedFields = {
    val dbAnalyser = new DBAnalyser
    value.map(Seq(_)).foldLeft[Seq[Field]](Nil)(dbAnalyser.mergeSchemas)
  }
  override def incrementCount = this.copy(count = this.count + 1)
}

class Schema(val collectionName: String, val fields: Seq[Field])

case class CollectionSchema(val collectionName: String, val rowFields: Seq[Seq[Field]])
case class CollectionMergedSchema(val collectionName: String, val fields: Seq[Field])