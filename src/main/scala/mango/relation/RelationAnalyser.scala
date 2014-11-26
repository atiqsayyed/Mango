package mango.relation

import scala.Option.option2Iterable

import mango.analyser.ArrayField
import mango.analyser.CollectionSchema
import mango.analyser.Field
import mango.analyser.NullField
import mango.analyser.ObjectField
import mango.analyser.ObjectIdField
import mango.analyser.Parser
import mango.analyser.StringField

case class FieldRef(collectionName: String, field: Field) {
  override def toString = collectionName + " : " + (field match {
    case os: ObjectField => {
      val res = os.value.map(f => f match {
        case s: StringField => os.name + "." + s.name
        case ar: ArrayField => os.name + "." + ar.name
        case _ => os.name
      }).toSet
      res.head
    }
    case _ => field.name
  })

  override def equals(that: Any) = {
    that match {
      case FieldRef(thatCollection, thatField) => (collectionName, field.name) equals (thatCollection, thatField.name)
      case _ => false
    }
  }
}

case class Association(field1: FieldRef, field2: FieldRef) {
  override def toString = field1 + " <----> " + field2
}

case class Relation(association: Association, confidence: Float, relation: (String, String)) {
  override def equals(that: Any) = {
    that match {
      case Relation(thatAssociation, _, thatRelation) => (
        ((association.field1, association.field2) equals (thatAssociation.field2, thatAssociation.field1)) ||
        ((association.field1, association.field2) equals (thatAssociation.field1, thatAssociation.field2)))
      case _ => false
    }
  }
}

class RelationAnalyser(dbParser: Parser, dbName: String) {

  def getRelations(): Set[Relation] = {
    val parsedData = dbParser.parseDB(dbName)
    val associations = parsedData.flatMap(collection => {
      collection.rowFields.flatMap(row => {
        row.flatMap(field => {
          findAssociations(field, collection.collectionName, parsedData)
        })
      })
    })

    val associationSet = associations.toSet
    val relationsFinalSet = associationSet.map(f => {
      val countLeft = associations.count(c => (c.field1.collectionName equals f.field1.collectionName) && (c.field1.field.name equals f.field1.field.name))
      val countRight = associations.count(c => (c.field2.collectionName equals f.field2.collectionName) && (c.field2.field.name equals f.field2.field.name))
      val confidence = (associations.count(_ equals f).toFloat / associations.count(_ => true).toFloat)
      Relation(f, confidence, if (countLeft > 1) ("1", "n") else (if (countLeft == 1) ("1", "1") else ("0", "0")))
    })

    relationsFinalSet
  }

  def getCollRelations(collNames: List[String]) = {
    if (collNames.size >= 2) {
      val parseData = collNames.map(coll => {
        val rowFields = dbParser.parseCollection(dbName, coll)
        CollectionSchema(coll, rowFields.toList)
      })
      val collAssociations = parseData.flatMap(collection => {
        collection.rowFields.flatMap(row => {
          row.flatMap(field => {
            getCollAssociations(field, collection.collectionName, collNames, parseData)
          })
        })
      })

      val collAssociationsSet = collAssociations.toSet
      val collRelationsFinalSet = collAssociationsSet.map(f => {
        val count = collAssociations.count(_ equals f)
        val confidence = (collAssociations.count(_ equals f).toFloat / collAssociations.count(_ => true).toFloat)
        Relation(f, confidence, if (count > 1) ("1", "n") else (if (count == 1) ("1", "1") else ("0", "0")))
      })
      collRelationsFinalSet
    } else {
      Set(Relation(Association(FieldRef(" ", NullField(0, 0)), FieldRef(" ", NullField(0, 0))), 0.0f, ("0", "0")))
    }
  }

  def getCollAssociations(keyValue: Field, collection: String, collNames: List[String], parseData: List[CollectionSchema]): List[Association] = {
    parseData.filter(coll => (coll.collectionName != collection) && (collNames.contains(coll.collectionName))).flatMap { collectionInner =>
      collectionInner.rowFields.flatMap { row =>
        row.flatMap { field =>
          if (compareValue(keyValue, field)) Some(Association(FieldRef(collection, keyValue), FieldRef(collectionInner.collectionName, field)))
          else None
        }
      }
    }
  }

  def compareValue(f1: Field, f2: Field): Boolean = {
    (f1, f2) match {
      case (o1: ObjectIdField, o2: ObjectIdField) => (o1.value.equals(o2.value) && !(o1.value.equals("")))
      case (o1: ObjectIdField, s2: StringField) => (o1.value.equals(s2.value) && !(o1.value.equals("")))
      case (s1: StringField, s2: StringField) => (s1.value.equals(s2.value) && !(s1.value.equals("")))
      case (s1: StringField, o2: ObjectIdField) => (s1.value.equals(o2.value) && !(s1.value.equals("")))
      case (a1: ArrayField, s2: StringField) => {
        a1.value.exists({
          case s: StringField => (s.value.equals(s2.value) && !(s.value.equals("")))
          case _ => false
        })
      }
      case (s1: StringField, a2: ArrayField) => {
        a2.value.exists({
          case s: StringField => (s.value.equals(s1.value) && !(s.value.equals("")))
          case _ => false
        })
      }
      case (s1: StringField, ob2: ObjectField) => {
        compareValue(s1, getField(ob2))
      }
      case (ob1: ObjectField, s2: StringField) => {
        compareValue(getField(ob1), s2)
      }
      case _ => false
    }
  }

  def getField(objField: ObjectField): Field = {
    val field = objField.value.map(f => {
      f match {
        case objId: ObjectIdField => objId
        case objStr: StringField => objStr
        case objArr: ArrayField => objArr
        case _ => new StringField(0, "", "", 1)
      }
    })
    field.head
  }

  def findAssociations(keyValue: Field, collection: String, parsedData: List[CollectionSchema]): List[Association] = {
    parsedData.filter(_.collectionName != collection).flatMap { collectionInner =>
      collectionInner.rowFields.flatMap { row =>
        row.flatMap { field =>
          if (compareValue(keyValue, field)) Some(Association(FieldRef(collection, keyValue), FieldRef(collectionInner.collectionName, field)))
          else None
        }
      }
    }
  }
}
