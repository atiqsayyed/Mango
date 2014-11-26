package mangotest

import org.scalatest.FlatSpec
import com.mongodb.casbah.commons.MongoDBObject
import org.bson.types.ObjectId
import mango.analyser.MongoDBParser

class DemoTest extends FlatSpec{
    val mongodbParser = new MongoDBParser("localhost")
	private val db = mongodbParser.getDb("RelationDemo")
	db.dropDatabase
	
	val adminCollectionName = "admin"
    db.getCollection(adminCollectionName).drop
    val collectionAdmin = db(adminCollectionName)
    collectionAdmin += MongoDBObject(("Id", "A1"), ("Admin_Id" -> "T2"))
    collectionAdmin += MongoDBObject(("Id", "A2"), ("Admin_Id" -> "T3"))

    val classCollectionName = "class"
    db.getCollection(classCollectionName).drop
    val collectionClass = db(classCollectionName)
    collectionClass += MongoDBObject(("_id", new ObjectId), ("Teacher" -> "T1"), ("ClassTeacher", "T1"), ("Student" -> MongoDBObject(("Id" -> ("S1")))))

    
    val refCollectionName = "ref"
    db.getCollection(refCollectionName).drop
    val collectionRef = db(refCollectionName)
    collectionRef += MongoDBObject(("_id", new ObjectId), ("StudentRef" -> "student"))
    collectionRef += MongoDBObject(("_id", new ObjectId), ("StudentRef" -> "S1"))
    collectionRef += MongoDBObject(("_id", new ObjectId), ("StudentRef" ->  MongoDBObject(("Id" -> ("S1")))))

    val studentCollectionName = "student"
    db.getCollection(studentCollectionName).drop
    val collectionStudent = db(studentCollectionName)
    collectionStudent += MongoDBObject(("Id", "S1"), ("Teacher_Id" -> "T1"))
	collectionStudent += MongoDBObject(("Id", "S2"), ("Teacher_Id" -> "T3"))
	collectionStudent += MongoDBObject(("Id", "S3"), ("Teacher_Id" -> "T1"))
	collectionStudent += MongoDBObject(("Id", "S4"), ("Teacher_Id" -> "T2"))
	collectionStudent += MongoDBObject(("Id", "S5"), ("Teacher_Id" -> "T3"))
	collectionStudent += MongoDBObject(("Id", "S6"), ("Teacher_Id" -> "T2"))
	
	val teacherCollectionName = "teacher"
    db.getCollection(teacherCollectionName).drop
    val collectionTeacher = db(teacherCollectionName)
    collectionTeacher += MongoDBObject(("Id", "T1"), ("Class" -> "A"), ("Name" -> "Teacher1"))
	collectionTeacher += MongoDBObject(("Id", "T2"), ("Class" -> "B"), ("Name" -> "Teacher2"))
	collectionTeacher += MongoDBObject(("Id", "T3"), ("Class" -> "C"), ("Name" -> "Teacher3"))
	
}