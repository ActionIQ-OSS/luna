package co.actioniq.luna

import java.util.concurrent.TimeUnit

import co.actioniq.luna.dao.DbUUID
import co.actioniq.luna.dao.DAOH2Profile.api._
import co.actioniq.luna.example.{Kitty, KittyDAO}
import co.actioniq.luna.logging.NoopBackend
import org.junit.runner.RunWith
import org.specs2.mock.Mockito
import org.specs2.mutable.Specification
import org.specs2.runner.JUnitRunner

import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}

@RunWith(classOf[JUnitRunner])
class SoftDeleteSpec extends Specification with Mockito {
  "soft delete filter" should {
    "return non deleted rows" in new TestScope {
      val toDelete = Kitty(
        id = DbUUID.randomDbUUID,
        name = "larry",
        color = "white"
      )
      val toKeep = Kitty(
        id = DbUUID.randomDbUUID,
        name = "barry",
        color = "blue"
      )
      awaitResult(dao.createFuture(toDelete))
      awaitResult(dao.createFuture(toKeep))
      awaitResult(dao.softDeleteFuture(toDelete.id))
      val rows = awaitResult(dao.readFuture())
      rows.length mustEqual 1
      rows.head.id mustEqual toKeep.id

    }
  }

  trait TestScope extends SlickScope with Kitty.Provider {
    implicit val tl = new NoopBackend {}
    val dao = new KittyDAO(
      db,
      kittySlick
    )
    protected val sqlMode = sqlu"SET MODE MySQL"
    protected val createKitty = sqlu"""
      CREATE TABLE `kitty` (
       `id` binary(16) NOT NULL,
       `name` varchar(255) NOT NULL,
       `color` varchar(255) NOT NULL,
       `deleted_at` bigint default null,
        PRIMARY KEY (`id`)
      )
    """

    awaitResult(
      db.run(
        DBIO.seq(
          sqlMode,
          createKitty
        )
      )
    )

    def awaitResult[R](future: Future[R]): R = Await.result(future, Duration(6, TimeUnit.SECONDS))
  }


}
