package mango.ui
import mango.analyser._
import scala.io._
import java.io.FileWriter
import java.io.PrintWriter
import java.io.File

object HtmlUI extends App {
  def join[A](l: Seq[A], sep: A) = {
    if (l.length > 1) {
      l.init.flatMap(Seq(_, sep)) :+ l.last
    } else {
      l
    }
  }

  private val maxFieldsPerRow = 3
  private def arrange(s: Seq[xml.NodeSeq]) = {
    val arrangedFields = join(s.grouped(maxFieldsPerRow).toSeq, Seq(<br/>)).flatten
    if (s.length > maxFieldsPerRow) {
      <br/>
      arrangedFields
    } else {
      arrangedFields
    }
  }

  def toHtml(fields: Seq[Field]): xml.NodeSeq = {
    val fieldGroups = fields.groupBy(_.name)
    def totalCount(fieldGroup:(String, Seq[Field])) = {
      fieldGroup._2.map(_.count).sum
    }
    fieldGroups.toList.sortWith(totalCount(_) > totalCount(_)).flatMap { fieldGroup =>
      val fieldsInGroup = fieldGroup._2
      val firstField = fieldsInGroup.head
      val conflicted = fieldsInGroup.length > 1
      val classStr = "field level" + firstField.level + (if (conflicted) " conflicted" else "")

      val fieldsHtml = fieldsInGroup.map { field =>

        field match {
          case o: ObjectField => {
            if (o.getChildDepth > 2) {
              <div class={ classStr }>
                <span class="fieldName">{ field.name }<span class="fieldType">Obj</span><span class="fieldCount">{ o.count }</span></span>
                { toHtml(o.value.toSeq) }
              </div>
            } else {
              <div class={ classStr }>
                <span class="fieldName">{ field.name }<span class="fieldType">Obj</span><span class="fieldCount">{ o.count }</span></span>
                { arrange(toHtml(o.value.toSeq)) }
              </div>
            }
          }
          case a: ArrayField => {
            if (a.getChildDepth > 2) {
              <div class={ classStr }>
                <span class="fieldName">{ field.name }<span class="fieldType">Arr</span><span class="fieldCount">{ a.count }</span></span>
                { toHtml(a.getMergedFields) }
              </div>
            } else {
              <div class={ classStr }>
                <span class="fieldName">{ field.name }<span class="fieldType">Arr</span><span class="fieldCount">{ a.count }</span></span>
                { arrange(toHtml(a.getMergedFields)) }
              </div>
            }
          }
          case _ => {
            <div class={ classStr }>{ field.name }<span class="fieldType">{ field.fieldType }</span><span class="fieldCount">{ field.count }</span></div>
          }
        }
      }
      if (conflicted) {
        <div class="conflictedGroup">{fieldsHtml}</div>
      } else {
        fieldsHtml
      }
    }
  }

  private val dbName = args(0)
  private val mongoDBParser = new MongoDBParser("localhost")
  private val dbAnalyser = new DBAnalyser
  private val analysis = if (args.length > 1) {
    val collectionName = args(1)
    val parsedCollection = mongoDBParser.parseCollection(dbName, collectionName)
    dbAnalyser.analyse(Seq(CollectionSchema(collectionName, parsedCollection.toSeq)))
  } else {
    val dbName = args(0)
    val parsedData = mongoDBParser.parseDB(dbName)
    dbAnalyser.analyse(parsedData)
  }

  private val styles = """
    body {color:#fcfcfc;background-color:#355664; font-family:Ubuntu, Arial, Helvetica, sans-serif;}
    h1,h3 { font-family: Ubuntu; font-weight: normal; margin: 0 0 0em;}
    h1 {font-size: 28px;}
    h3 {font-size: 22px;}
    .collection {display:inline-block; margin:10px; background:#000; padding:10px;font-size: 14px; background:#13303C; border-radius: 20px 0px;}
    .collectionName {color: #8AA63D; margin:4px 0; font-weight:bold; font-size: 18px;}
    .field {display:inline-block; padding:10px; border-right:1px solid #213E4A;font-size: 16px; border:none;border-radius:12px 0px;margin-top:10px;}
    .field.level0 {margin:10px 20px; background: #203D49;border-radius:12px 0px; padding:6px; }
    .field.level1 {margin:10px 16px; background: #13303C;}
    .field.level2 {margin:10px 12px; background: #203D49;}
    .field.level3 {margin:10px 09px; background: #13303C;}
    .field.level4 {margin:10px  5px; background: #203D49;}
    .field.level5 {margin:10px  4px; background: #13303C;}
    .field.level6 {margin:10px  2px; background: #203D49;}
    .field.level7 {margin:10px  2px; background: #13303C;}
    .field.level8 {margin:10px  2px; background: #203D49;}
    .field.conflicted {border:2px solid #f22;}
    .fieldName {font-size:16px;background:#2A4662; border-radius: 12px 0px; padding:8px; display:inline-block; margin:2px;}
    .fieldType {color:#999; font-size:14px; margin:6px}
    .fieldCount {color:#666; font-size:14px; margin:2px}
    .conflictedGroup {display:inline-block; border-radius:12px 0; background:#656; margin:2px 4px;}
    section.footer {background: #13303c; border-radius: 6px; margin:30px 20px 10px 20px; text-align:center}
    section.footer .logo {vertical-align:middle}
  """
  private val result =
    <html>
      <head>
        <style>{ styles }</style>
      </head>
      <body>
        <section id="page">
          <h1>Analysis of { dbName }</h1>
          {
            analysis.map { collectionSchema =>
              <div class="collection">
                <p class="CollectionName">[ { collectionSchema.collectionName } ]</p>
                { toHtml(collectionSchema.fields) }
              </div>
            }
          }
        </section>
        <section class="footer">
          <p>[Mango]<a href="https://bitbucket.org/hrj/mango/"> <img class="logo" width="64px" src="http://dl.dropbox.com/u/125893950/mango-logo.png"></img></a> a MongoDB Explorer</p>
        </section>
      </body>
    </html>

  // println(result.toString)
  val fw = new PrintWriter(new File(args(0) + ".html"))
  fw.write(xml.Xhtml.toXhtml(result).toString)
  fw.close()
  println("Done")
}
