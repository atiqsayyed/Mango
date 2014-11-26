package mangotest

import org.scalatest.FlatSpec
import org.scalatest.matchers.ShouldMatchers
import com.fourspaces.couchdb.Document
import com.fourspaces.couchdb.Session
import mango.analyser.ArrayField
import mango.analyser.CouchDBParser
import mango.analyser.ObjectField
import mango.analyser.StringField
import mango.relation.Association
import mango.relation.FieldRef
import mango.relation.Relation
import mango.relation.RelationAnalyser
import mango.ui.Config

class CouchDBTest extends FlatSpec with ShouldMatchers {

  private var config = Config(None, Config.dbs.head, Config.host, 5984)
  val dbSession = new Session(config.host, config.port.toInt)
  
  "Parse relations for collections" should "relations between collections" in {

    val dbName = "couchschooltest";
    dbSession.deleteDatabase(dbName)
    createDatabase(dbName)

    val couchdbParser = new CouchDBParser(config)
    val relationAnalyser = new RelationAnalyser(couchdbParser, dbName)
    val relation = relationAnalyser.getRelations
    val testRelation = Set(
      Relation(
        Association(
          FieldRef("Class", StringField(0, "ClassTeacher", "T1", 1)),
          FieldRef("Teacher", StringField(0, "TeacherId", "T1", 1))),
        0.125f, ("1", "n")),
      Relation(
        Association(
          FieldRef("Class", ObjectField(0, "Students", Set(
            ArrayField(1, "Id", Set(
              StringField(2, "[i]", "S2", 1), StringField(2, "[i]", "S3", 1), StringField(2, "[i]", "S1", 1), StringField(2, "[i]", "S4", 1)), 1)), 1)),
          FieldRef("Student", StringField(0, "StudentId", "S1", 1))),
        0.375f, ("1", "n")))
    relation should equal(testRelation)

    val collRelation = relationAnalyser.getCollRelations(List("Class", "Teacher", "Student"))
    val testCollRelation = Set(
      Relation(
        Association(
          FieldRef("Class", StringField(0, "ClassTeacher", "T1", 1)),
          FieldRef("Teacher", StringField(0, "TeacherId", "T1", 1))),
        0.125f, ("1", "n")),
      Relation(
        Association(
          FieldRef("Class", ObjectField(0, "Students", Set(
            ArrayField(1, "Id", Set(
              StringField(2, "[i]", "S2", 1), StringField(2, "[i]", "S3", 1), StringField(2, "[i]", "S1", 1), StringField(2, "[i]", "S4", 1)), 1)), 1)),
          FieldRef("Student", StringField(0, "StudentId", "S1", 1))),
        0.375f, ("1", "n")))

    collRelation should equal(testCollRelation)

  }

  def createDatabase(dbName: String) {

    val db = dbSession.createDatabase(dbName)

    val newC1 = new Document
    newC1.put("Type", "Class")
    newC1.put("ClassId", "C1")
    newC1.put("ClassName", "C-2A")
    newC1.put("ClassTeacher", "T1")
    newC1.accumulate("Students", "S1");
    newC1.accumulate("Students", "S2");
    newC1.accumulate("Students", "S3");
    db.saveDocument(newC1)

    val newC2 = new Document
    newC2.put("Type", "Class")
    newC2.put("ClassId", "C2")
    newC2.put("ClassName", "C-2B")
    newC2.put("ClassTeacher", "T2")
    newC2.accumulate("Students", "S4");
    newC2.accumulate("Students", "S5");
    newC2.accumulate("Students", "S6");
    db.saveDocument(newC2)

    val newC3 = new Document
    newC3.put("Type", "Class")
    newC3.put("ClassId", "C3")
    newC3.put("ClassName", "C-2C")
    newC3.put("ClassTeacher", "T2")
    newC3.accumulate("Students", "S7");
    newC3.accumulate("Students", "S8");
    newC3.accumulate("Students", "S9");
    db.saveDocument(newC3)

    val newT1 = new Document
    newT1.put("Type", "Teacher")
    newT1.put("TeacherId", "T1")
    newT1.put("TeacherName", "Teacher1")
    newT1.put("Subject", "Maths")
    db.saveDocument(newT1)

    val newT2 = new Document
    newT2.put("Type", "Teacher")
    newT2.put("TeacherId", "T2")
    newT2.put("TeacherName", "Teacher2")
    newT2.put("Subject", "English")
    db.saveDocument(newT2)

    val newS1 = new Document
    newS1.put("Type", "Student")
    newS1.put("StudentId", "S1")
    newS1.put("StudentName", "Student1")
    newS1.put("Age", "8")
    newS1.put("LastYearPercentage", "68.40")
    newS1.put("Sport", "true")
    db.saveDocument(newS1)

    val newS2 = new Document
    newS2.put("Type", "Student")
    newS2.put("StudentId", "S2")
    newS2.put("StudentName", "Student2")
    newS2.put("Age", "8")
    newS2.put("LastYearPercentage", "60.04")
    newS2.put("Sport", "false")
    db.saveDocument(newS2)

    val newS3 = new Document
    newS3.put("Type", "Student")
    newS3.put("StudentId", "S3")
    newS3.put("StudentName", "Student3")
    newS3.put("Age", "8")
    newS3.put("LastYearPercentage", "78.90")
    newS3.put("Sport", "false")
    db.saveDocument(newS3)

    val newS4 = new Document
    newS4.put("Type", "Student")
    newS4.put("StudentId", "S4")
    newS4.put("StudentName", "Student4")
    newS4.put("Age", "8")
    newS4.put("LastYearPercentage", "75.86")
    newS4.put("Sport", "true")
    db.saveDocument(newS4)

    val newS5 = new Document
    newS5.put("Type", "Student")
    newS5.put("StudentId", "S5")
    newS5.put("StudentName", "Student5")
    newS5.put("Age", "8")
    newS5.put("LastYearPercentage", "55.40")
    newS5.put("Sport", "false")
    db.saveDocument(newS5)

    val newS6 = new Document
    newS6.put("Type", "Student")
    newS6.put("StudentId", "S6")
    newS6.put("StudentName", "Student6")
    newS6.put("Age", "8")
    newS6.put("LastYearPercentage", "82.70")
    newS6.put("Sport", "true")
    db.saveDocument(newS6)

    val newS7 = new Document
    newS7.put("Type", "Student")
    newS7.put("StudentId", "S7")
    newS7.put("StudentName", "Student7")
    newS7.put("Age", "8")
    newS7.put("LastYearPercentage", "48.90")
    newS7.put("Sport", "false")
    db.saveDocument(newS7)

    val newS8 = new Document
    newS8.put("Type", "Student")
    newS8.put("StudentId", "S8")
    newS8.put("StudentName", "Student8")
    newS8.put("Age", "8")
    newS8.put("LastYearPercentage", "88.10")
    newS8.put("Sport", "false")
    db.saveDocument(newS8)

    val newS9 = new Document
    newS9.put("Type", "Student")
    newS9.put("StudentId", "S9")
    newS9.put("StudentName", "Student9")
    newS9.put("Age", "8")
    newS9.put("LastYearPercentage", "58.40")
    newS9.put("Sport", "true")
    db.saveDocument(newS9)

    val viewDocClass = new Document
    viewDocClass.addView("Class", "getClass", "function(doc) {if(doc.Type == 'Class') { emit([doc.ClassId, doc.ClassName, doc.ClassTeacher, doc.Students], doc);}}")
    db.saveDocument(viewDocClass)

    val viewDocTeacher = new Document
    viewDocTeacher.addView("Teacher", "getTeacher", "function(doc) {if(doc.Type == 'Teacher') { emit([doc.TeacherId, doc.TeacherName, doc.Subject], doc);}}")
    db.saveDocument(viewDocTeacher)

    val viewDocStudent = new Document
    viewDocStudent.addView("Student", "getStudent", "function(doc) {if(doc.Type == 'Student') { emit([doc.StudentId, doc.StudentName, doc.Age, doc.LastYearPercentage, doc.Sport], doc);}}")
    db.saveDocument(viewDocStudent)

  }
}