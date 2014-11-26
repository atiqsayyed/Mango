package mangotest

import com.fourspaces.couchdb.Session
import mango.analyser.CouchDBParser
import mango.relation.RelationAnalyser
import mango.ui.Config

object CouchRelationTest extends App {
  private var config = Config(None, Config.dbs.head, Config.host, 5984)
  val dbSession = new Session(config.host, config.port.toInt)
  val db = dbSession.getDatabase("couchschooltest");
  val couchDBParser = new CouchDBParser(config)
  val relationAnalyser = new RelationAnalyser(couchDBParser, "couchschooltest")
  val relation = relationAnalyser.getRelations
  println("Relations =>>>" + relation)
  val collectionRlation = relationAnalyser.getCollRelations(List("Class","Teacher"))
  println("Collection Relations =>>>" + collectionRlation)
}