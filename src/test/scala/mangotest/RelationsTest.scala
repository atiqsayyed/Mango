package mangotest

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers

import com.mongodb.casbah.MongoDB
import com.mongodb.casbah.commons.MongoDBObject

import mango.analyser.ArrayField
import mango.analyser.MongoDBParser
import mango.analyser.ObjectField
import mango.analyser.StringField
import mango.relation.Association
import mango.relation.FieldRef
import mango.relation.Relation
import mango.relation.RelationAnalyser

class RelationsTest extends FlatSpec with ShouldMatchers {
  val mongodbParser = new MongoDBParser("localhost")
  private val db = mongodbParser.getDb("mongoschooltest")

  "Parse relations for collections" should "relations between collections" in {

    db.dropDatabase
    createDatabase(db)

    val relationAnalyser = new RelationAnalyser(mongodbParser, "mongoschooltest")
    val relation = relationAnalyser.getRelations
    val testRelation = Set(
      Relation(
        Association(
          FieldRef("class", StringField(0, "ClassTeacher", "T1", 1)),
          FieldRef("teacher", StringField(0, "TeacherId", "T1", 1))),
        0.125f, ("1", "n")),
      Relation(
        Association(
          FieldRef("class", ObjectField(0, "Student", Set(
            ArrayField(1, "Id", Set(
              StringField(2, "[i]", "S2", 1), StringField(2, "[i]", "S3", 1), StringField(2, "[i]", "S1", 1), StringField(2, "[i]", "S4", 1)), 1)), 1)),
          FieldRef("student", StringField(0, "StudentId", "S1", 1))),
        0.375f, ("1", "n")))
    relation should equal(testRelation)

    val collRelation = relationAnalyser.getCollRelations(List("class", "teacher", "student"))
    val testCollRelation = Set(
      Relation(
        Association(
          FieldRef("class", StringField(0, "ClassTeacher", "T1", 1)),
          FieldRef("teacher", StringField(0, "TeacherId", "T1", 1))),
        0.125f, ("1", "n")),
      Relation(
        Association(
          FieldRef("class", ObjectField(0, "Student", Set(
            ArrayField(1, "Id", Set(
              StringField(2, "[i]", "S2", 1), StringField(2, "[i]", "S3", 1), StringField(2, "[i]", "S1", 1), StringField(2, "[i]", "S4", 1)), 1)), 1)),
          FieldRef("student", StringField(0, "StudentId", "S1", 1))),
        0.375f, ("1", "n")))

    collRelation should equal(testCollRelation)

    db.dropDatabase
  }

  def createDatabase(db: MongoDB) {

    val teacherCollectionName = "teacher"
    db.getCollection(teacherCollectionName).drop
    val collectionTeacher = db(teacherCollectionName)
    collectionTeacher += MongoDBObject(("TeacherId", "T1"), ("Name" -> "Teacher1"), ("Age", 30), ("Subject", "Maths"))
    collectionTeacher += MongoDBObject(("TeacherId", "T2"), ("Name" -> "Teacher2"), ("Age", 35), ("Subject", "English"))

    val studentCollectionName = "student"
    db.getCollection(studentCollectionName).drop
    val collectionStudent = db(studentCollectionName)
    collectionStudent += MongoDBObject(("StudentId", "S1"), ("Name" -> "Student1"), ("Age", 10))
    collectionStudent += MongoDBObject(("StudentId", "S2"), ("Name" -> "Student2"), ("Age", 10))
    collectionStudent += MongoDBObject(("StudentId", "S3"), ("Name" -> "Student3"), ("Age", 10))
    collectionStudent += MongoDBObject(("StudentId", "S4"), ("Name" -> "Student4"), ("Age", 10))
    collectionStudent += MongoDBObject(("StudentId", "S5"), ("Name" -> "Student5"), ("Age", 11))
    collectionStudent += MongoDBObject(("StudentId", "S6"), ("Name" -> "Student6"), ("Age", 11))
    collectionStudent += MongoDBObject(("StudentId", "S7"), ("Name" -> "Student7"), ("Age", 11))
    collectionStudent += MongoDBObject(("StudentId", "S8"), ("Name" -> "Student8"), ("Age", 12))
    collectionStudent += MongoDBObject(("StudentId", "S9"), ("Name" -> "Student9"), ("Age", 12))

    val classCollectionName = "class"
    db.getCollection(classCollectionName).drop
    val collectionClass = db(classCollectionName)

    collectionClass += MongoDBObject(("ClassId", "C1"), ("Name" -> "C-5A"), ("ClassTeacher", "T1"), ("Student" -> MongoDBObject(("Id" -> ("S1", "S2", "S3", "S4")))))
    collectionClass += MongoDBObject(("ClassId", "C2"), ("Name" -> "C-6B"), ("ClassTeacher", "T2"), ("Student" -> MongoDBObject(("Id" -> ("S5", "S6", "S7")))))
    collectionClass += MongoDBObject(("ClassId", "C3"), ("Name" -> "C-7A"), ("ClassTeacher", "T1"), ("Student" -> MongoDBObject(("Id" -> ("S8", "S9", 10)))))

  }

}