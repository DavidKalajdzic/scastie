import com.olegych.scastie.api._
import org.mongodb.scala._
import org.mongodb.scala.model.Filters._
import org.mongodb.scala.model.Updates._
import org.mongodb.scala.model.Projections._
import org.mongodb.scala.model._
import play.api.libs.json._

import java.lang.System.{lineSeparator => nl}
import scala.concurrent.ExecutionContext
import scala.concurrent.Future
import com.typesafe.config.ConfigFactory
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.util.Try
import com.olegych.scastie.storage._
import scala.concurrent.ExecutionContext.Implicits.global
import org.mongodb.scala.bson.BsonDocument
import scala.concurrent.duration._

@main
def main() = {
  val mongoUri = {
    //    val config       = ConfigFactory.load().getConfig("scastie.mongodb")
    //    val user         = config.getString("user")
    //    val password     = config.getString("password")
    //    val databaseName = config.getString("database")
    //    val host         = config.getString("host")
    //    val port         = config.getInt("port")
    //    s"mongodb://$user:$password@$host:$port/$databaseName"
    "mongodb://localhost:27017"
  }

  val client: MongoClient = MongoClient(mongoUri)
  val database: MongoDatabase = client.getDatabase("snippets")
  val snippets = database.getCollection[Document]("snippets")
  val users = database.getCollection[Document]("users")

  def toBson[T](obj: T)(implicit writes: Writes[T]): Document = {
    val json = Json.toJson(obj).toString
    Document.apply(json)
  }

  def fromBson[T](obj: Document)(implicit reads: Reads[T]): Option[T] = {
    Json.parse(obj.toJson()).asOpt[T]
  }

  def printSnippets(snippetId: String): Unit = {
    println(Await.result(snippets.find(Document("simpleSnippetId" -> snippetId)).collect().head(), Duration.Inf))
  }

  def fetchSnippet(snippetId: String): Future[Seq[MongoSnippet]] = {
    snippets.find(Document("simpleSnippetId" -> snippetId)).collect().head().map(_.flatMap(fromBson[MongoSnippet]))
  }

  // We fetch all snippets, and then we modify them
  def updateResult() = ???

  /* This function is going to check for the collision
   * All user snippets will be anonymized, but first we need to check their compatibility
   *
   * Snippets will be transformed in the following way:
   * username/snippetUUID/version will become /snippetUUID/version
   *
   * This function ensures that there are no snippets with the same UUID
   */
  def checkSnippetsCollision() = {
    val snippetsNumber = Await.result(snippets.countDocuments().head(), Duration.Inf)
    println(s"There are $snippetsNumber snippets in the database")
    val allSnippetsSimpleIds = snippets
      .find()
      .projection(include("simpleSnippetId"))
      .collect()
      .head()
      .map(_.map(_.getString("simpleSnippetId")))

    val groupedSnippets = allSnippetsSimpleIds.map(_.groupBy { snippetId =>
      snippetId.split("/").toList match
        case username :: uuid :: version :: Nil => uuid
        case uuid :: Nil => uuid
        case uuid => "error"
    })

    val duplicatedSnippets = Await.result(allSnippetsSimpleIds.map(_.groupBy(x => x).filter(_._2.size > 1)), Duration.Inf)
    println(duplicatedSnippets)

    Await.result(groupedSnippets.map(mp => {
      val res = mp.getOrElse("error", Nil)
      if res.nonEmpty then
        res.map(println)
        throw new IllegalStateException()
    }), Duration.Inf)

    var count = 0L
    var progress = 0
    val result = groupedSnippets.map(_.filterNot { (key, seq) => {
      count += seq.length
      val cProgress = (count * 100) / snippetsNumber
      if cProgress > progress then
        progress = cProgress.toInt
        println(s"Progress: [$progress%/ 100%]")
      seq match
        case head :: Nil => true
        case list if list.forall(id => {
          val parts = id.split("/")
          parts.size == 3 && parts.last.toIntOption.isDefined
        }) => list.map(_.split("/".last)).distinct.size == list.size
        case list => false
    }
    })

    val fetchedResult = Await.result(result, Duration.Inf)
    println(fetchedResult.get("1msDYfKOTM2hj8aufvOL7w"))
    println(fetchedResult)

    val migrateDuplicatedSnippets = duplicatedSnippets.filterNot((key, _) => key == "old").map((key, seq) => {
      println(s"KEY $key, ${seq.size}")
      val duplicates = Future.sequence(seq.diff(seq.distinct).map { simpleSnippetId =>
        snippets.deleteOne(Document("simpleSnippetId" -> simpleSnippetId)).head().map(x => {
          println(s"Deleted snippet $simpleSnippetId ${x.getDeletedCount()} times")
          x.wasAcknowledged
        })
      })

      duplicates.map(_.forall(_ == true))
    })

    println(Await.result(Future.sequence(migrateDuplicatedSnippets).map(_.foldLeft(true)(_ && _)), Duration.Inf))
    println(s"Size of old snippets format: ${fetchedResult.getOrElse("old", Nil).size}")
    println(s"Duplications: ${duplicatedSnippets.size}")
    println(s"Final result: ${duplicatedSnippets.size == 1}")
    duplicatedSnippets.size == 1
  }


  def addUsersFromSnippets() = {
    Await.result(users.drop().head(), Duration.Inf)

    val usersFromSnippets1 = Await.result(
      snippets
        .find()
        .projection(Document("user" -> 1))
        .collect()
        .head()
        .map { results =>
          results
            .map(_ + ("acceptedPrivacyPolicy" -> false))
            .flatMap(fromBson[PolicyAcceptance])
            .toSet
        },
      Duration.Inf
    )

    val usersFromSnippets2 = Await.result(
      snippets
        .find(exists("snippetId.user.login"))
        .projection(Document("snippetId.user.login" -> 1))
        .collect()
        .head()
        .map { results =>
          results
            .map(_.toBsonDocument.getDocument("snippetId").getDocument("user").getString("login"))
            .map(x => Document("user" -> x, "acceptedPrivacyPolicy" -> false))
            .flatMap(fromBson[PolicyAcceptance])
            .toSet
        },
      Duration.Inf)

    val usersFromSnippets = usersFromSnippets1 ++ usersFromSnippets2

    val databaseCreation = database.listCollectionNames().collect().head().flatMap(colls =>
      colls.find(_ == "users") match {
        case Some(users) => Future.successful(null)
        case None => database.createCollection("users").head()
      }
    )
    Await.result(databaseCreation, Duration.Inf)
    Await.result(users.dropIndexes().head(), Duration.Inf)
    Await.result(users.createIndex(Indexes.ascending("user"), IndexOptions().unique(true)).head(), Duration.Inf)
    Await.result(users.insertMany(usersFromSnippets.map(toBson(_)).toSeq).head(), Duration.Inf)
    println(Await.result(users.countDocuments().head(), Duration.Inf))
  }


  def addIndexes() = {
    println(Await.result(snippets.listIndexes().head(), Duration.Inf))
    Await.result(snippets.dropIndexes().head(), Duration.Inf)
    Await.result(snippets.createIndex(Indexes.ascending("simpleSnippetId", "oldId"), IndexOptions().unique(true)).head(), Duration.Inf)
    Await.result(Future.sequence(Seq(
      Indexes.hashed("simpleSnippetId"),
      Indexes.hashed("oldId"),
      Indexes.hashed("user"),
      Indexes.hashed("snippetId.user.login"),
      Indexes.hashed("inputs.isShowingInUserProfile"),
      Indexes.hashed("time")
    ).map(snippets.createIndex(_).head())), Duration.Inf)
  }

  def printUsersCount() = {
    val indexes = snippets.listIndexes().collect().head()
    val usersFromSnippets = Await.result(
      users
        .countDocuments()
        .head(),
      Duration.Inf
    )
    println(s"We have :$usersFromSnippets users")
    println(Await.result(indexes, Duration.Inf))
  }

  def printUsers() = {
    println(Await.result(users.find().collect().head(), Duration.Inf))
  }

  try {
    if checkSnippetsCollision() then
      Try {
        addIndexes()
      } recover {
        case t: Throwable => t.printStackTrace()
      }
      addUsersFromSnippets()
      printUsersCount()

    else
      println("ERROR")
      println("ERROR")
      println("ERROR")
      println("ERROR")
      println("ERROR")
      println("ERROR")
      println("ERROR")
      println("ERROR")
      println("ERROR")
      println("ERROR")
  } finally {
    client.close()
  }
}
