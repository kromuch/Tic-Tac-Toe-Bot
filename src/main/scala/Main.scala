import slick.jdbc.PostgresProfile.api._

import scala.concurrent.Await
import scala.concurrent.duration._

object Main {
  def main(args: Array[String]): Unit = {
//    val db = Database.forURL(
//      "jdbc:postgresql://127.0.0.1:5432/TTTBot?user=postgres&password=mypassword"
//    )
    //init(db)
    Bot.run()
  }
  def init(db:Database):Unit = {
    Await.result(db.run(GameTable.table.schema.create),Duration.Inf)
  }
}
