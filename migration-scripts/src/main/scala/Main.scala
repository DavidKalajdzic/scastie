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
import org.mongodb.scala.result.{InsertOneResult, UpdateResult}

import java.util.{Objects, UUID}
import scala.concurrent.duration._

object Main {

  def main(args: Array[String]): Unit = {

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

    /**
     * This migrates the 'snippets' collection to a new one where the inputs are stored as a Folder and not as a String.
     * The result of the migration will be stored in a new collection called 'snippetsV2_some_uuid_here'.
     *
     * The migration is done in 3 steps:
     * 1. We fetch all snippet ids from the 'snippets' collection
     * 2. We fetch each snippet, we modify it, and we store it in a new collection
     * 3. We add indexes to the new collection
     *
     * If there is any error (e.g., the migration is interrupted), this program will crash. This migration can be rerun, and a new collection will be created.
     * The migration is idempotent, so it can be run multiple times without any problem.
     *
     * @note The admin can rename the 'snippetsV2_some_uuid' collection by executing the following MongoDB Shell command:
     * {{{
     * db.oldCollectionName.renameCollection("newCollectionName")
     * }}}
     * @return The name of the new collection where the result migrated snippets are stored.
     */
    def migrateSnippetsV1ToV2(snippetsV1: MongoCollection[Document]): String = {
      def transformMongoSnippet(old: MongoSnippetV1): MongoSnippetV2 = {
        old match {
          case MongoSnippetV1(simpleSnippetId, user, snippetId, oldId, inputsV1, progresses, scalaJsContent, scalaJsSourceMapContent, time) =>
            val inputsV2 = transformInputs(inputsV1)
            MongoSnippetV2(simpleSnippetId, user, snippetId, oldId, inputsV2, progresses, scalaJsContent, scalaJsSourceMapContent, time)
        }
      }

      def transformInputs(old: InputsV1): InputsV2 = {
        old match {
          case InputsV1(_isWorksheetMode, code, target, libraries, librariesFromList, sbtConfigExtra, sbtConfigSaved, sbtPluginsConfigExtra, sbtPluginsConfigSaved, isShowingInUserProfile, forked) =>
            val newCode = Folder.singleton(code)
            InputsV2(_isWorksheetMode, newCode, target, libraries, librariesFromList, sbtConfigExtra, sbtConfigSaved, sbtPluginsConfigExtra, sbtPluginsConfigSaved, isShowingInUserProfile, forked)
        }
      }

      val newCollectionName: String = "snippetsV2" + UUID.randomUUID().toString
      Await.result(database.createCollection(newCollectionName).head(), Duration.Inf)
      val snippetsV2: MongoCollection[Document] = database.getCollection(newCollectionName)
      println(s"[Migration] Successfully created a target collection named: '$newCollectionName'")

      println("[Migration] Fetching all snippet ids from snippets collection")
      val allSnippetsSimpleIds: Future[Seq[String]] = snippetsV1
        .find()
        .projection(include("simpleSnippetId"))
        .collect()
        .head()
        .map(_.map(_.getString("simpleSnippetId")))

      val allSnippetIds: Seq[String] = Await.result(allSnippetsSimpleIds, Duration.Inf)
      println(s"[Migration] ${allSnippetIds.size} snippets found, starting the migration...")

      for ((snippetId, i) <- allSnippetIds.zipWithIndex) {
        println(s"[Migration] ${i + 1}/${allSnippetIds.size} (${100 * (i + 1) / allSnippetIds.size}%)")
        val sv1: Document = Await.result(snippetsV1.find(equal("simpleSnippetId", snippetId)).toFuture(), Duration.Inf).ensuring(_.size == 1).head
        val sv2: Document = fromBson[MongoSnippetV1](sv1).map((mg1: MongoSnippetV1) => {
          val translated: MongoSnippetV2 = transformMongoSnippet(mg1)
          toBson[MongoSnippetV2](translated)
        }).get

        val result: InsertOneResult = Await.result(snippetsV2.insertOne(sv2).head(), Duration.Inf)
        assert(result.wasAcknowledged()) // checks if the write is a success
      }
      println(s"[Migration] All ${allSnippetIds.size} records have been migrated successfully")

      println(s"[Migration] Adding indexes to $newCollectionName")
      addIndexes(snippetsV2)

      println("[Migration] Migration completed successfully!")
      println(s"[Migration] the result is in collection named '$newCollectionName'")
      newCollectionName
    }

    /* This function is going to check for the collision
    * All user snippets will be anonymized, but first we need to check their compatibility
    *
    * Snippets will be transformed in the following way:
    * username/snippetUUID/version will become /snippetUUID/version
    *
    * This function ensures that there are no snippets with the same UUID
    */
    def checkSnippetsCollision(snippets: MongoCollection[Document]) = {
      val snippetsNumber = Await.result(snippets.countDocuments().head(), Duration.Inf)
      println(s"There are $snippetsNumber snippets in the database")
      val allSnippetsSimpleIds = snippets
        .find()
        .projection(include("simpleSnippetId"))
        .collect()
        .head()
        .map(_.map(_.getString("simpleSnippetId")))

      // Map of "snippetUUID" to a list whose elements are of form "username/snippetUUID/version", "/snippetUUID/version", "error"
      val groupedSnippets: Future[Map[String, Seq[String]]] = allSnippetsSimpleIds.map(_.groupBy { snippetId =>
        snippetId.split("/").toList match {

          case username :: uuid :: version :: Nil
          => uuid
          case uuid :: Nil
          => uuid
          case uuid => "error"
        }
      })

      val duplicatedSnippets = Await.result(allSnippetsSimpleIds.map(_.groupBy(x => x).filter(_._2.size > 1)), Duration.Inf)
      println(duplicatedSnippets)

      // map is used to iterate over the groupedSnippets sequence and print out any "error" values that may be present
      Await.result(groupedSnippets.map(mp => {
        val res = mp.getOrElse("error", Nil)
        if (res.nonEmpty) {
          res.map(println)
          throw new IllegalStateException()
        }
      }), Duration.Inf)

      var count = 0L
      var progress = 0
      val result = groupedSnippets.map(_.filterNot { x => {
        val (key: String, seq: Seq[String]) = x
        count += seq.length
        val cProgress = (count * 100) / snippetsNumber
        if (cProgress > progress)
          progress = cProgress.toInt
        println(s"Progress: [$progress%/ 100%]")
        seq match {
          case head :: Nil => true
          case list if list.forall(id => {
            val parts = id.split("/")
            parts.size == 3 && parts.last.toIntOption.isDefined
          }) => list.map(_.split("/".last)).distinct.size == list.size
          case list => false
        }
      }
      })

      val fetchedResult = Await.result(result, Duration.Inf)
      println(fetchedResult.get("1msDYfKOTM2hj8aufvOL7w"))
      println(fetchedResult)

      val migrateDuplicatedSnippets = duplicatedSnippets
        .filterNot(x => x._1 == "old")
        .map(x => {
          val (key, seq) = x
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
      println(s"Final result: ${duplicatedSnippets.isEmpty}")
      duplicatedSnippets.isEmpty
    }


    def addUsersFromSnippets(): Unit = {
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


    def addIndexes(snippetsCollection: MongoCollection[Document]) = {
      println(Await.result(snippetsCollection.listIndexes().head(), Duration.Inf))
      Await.result(snippetsCollection.dropIndexes().head(), Duration.Inf)
      Await.result(snippetsCollection.createIndex(Indexes.ascending("simpleSnippetId", "oldId"), IndexOptions().unique(true)).head(), Duration.Inf)
      Await.result(Future.sequence(Seq(
        Indexes.hashed("simpleSnippetId"),
        Indexes.hashed("oldId"),
        Indexes.hashed("user"),
        Indexes.hashed("snippetId.user.login"),
        Indexes.hashed("inputs.isShowingInUserProfile"),
        Indexes.hashed("time")
      ).map(snippetsCollection.createIndex(_).head())), Duration.Inf)
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
      if (checkSnippetsCollision(snippets)) {
        Try {
          addIndexes(snippets)
        } recover {
          case t: Throwable => t.printStackTrace()
        }
        addUsersFromSnippets()
        printUsersCount()

        migrateSnippetsV1ToV2(snippets)
      } else {
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
      }
    } finally {
      client.close()
    }
  }
}