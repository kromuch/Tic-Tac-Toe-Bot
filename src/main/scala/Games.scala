import MinimaxTTT.winning
import slick.jdbc.PostgresProfile.api._
import slick.lifted.Tag

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
case class Games(id: Long,
                 gameBoard: String,
                 userChar: String,
                 botChar: String,
                 botWins: Int,
                 userWins: Int,
                 totalGames: Int)

class GameTable(tag: Tag) extends Table[Games](tag, "Game") {
  val id = column[Long]("id", O.PrimaryKey)
  val gameBoard = column[String]("gameBoard")
  val userChar = column[String]("userChar")
  val botChar = column[String]("botChar")
  val botWins = column[Int]("botWins")
  val userWins = column[Int]("userWins")
  val totalGames = column[Int]("totalGames")

  def * =
    (id, gameBoard, userChar, botChar, botWins, userWins, totalGames) <> (Games.apply _ tupled, Games.unapply)
}
object GameTable {
  val table = TableQuery[GameTable]
}

class Repo(db: Database) {
  val gameTableQuery = TableQuery[GameTable]
  def startCommand(game: Games): Future[Any] = {
    if (Await
          .result(db.run(gameTableQuery.filter(_.id === game.id).result),
                  Duration.Inf)
          .isEmpty) {
      db.run(gameTableQuery returning gameTableQuery += game)
    } else {
      db.run(gameTableQuery.filter(_.id === game.id).update(game))
    }
  }
  def aiMove(id: Long): Future[Int] = {
    val oldGame: Games = Await
      .result(db.run(gameTableQuery.filter(_.id === id).result), Duration.Inf)
      .head
    val board: String = oldGame.gameBoard
    val boardSeq = ArrayBuffer(
      board(0).toString,
      board(1).toString,
      board(2).toString,
      board(3).toString,
      board(4).toString,
      board(5).toString,
      board(6).toString,
      board(7).toString,
      board(8).toString
    )
    val huPlayer = oldGame.userChar
    val aiPlayer = oldGame.botChar
    val move: ArrayBuffer[String] =
      MinimaxTTT.minimax(boardSeq, aiPlayer, huPlayer, aiPlayer)
    val boardStr = move(0) concat move(1) concat move(2) concat move(3) concat move(4) concat move(5) concat move(6) concat move(7) concat move(8)
    val avail = move.filter(a => { a != "O" && a != "X" })
    val newGame = if (winning(move,aiPlayer)) {
      Games(id,
            boardStr,
            huPlayer,
            aiPlayer,
            oldGame.botWins + 1,
            oldGame.userWins,
            oldGame.totalGames + 1)
    } else if (winning(move,huPlayer)) {
      Games(id,
            boardStr,
            huPlayer,
            aiPlayer,
            oldGame.botWins,
            oldGame.userWins + 1,
            oldGame.totalGames + 1)
    } else if (avail.isEmpty) {
      Games(id,
            boardStr,
            huPlayer,
            aiPlayer,
            oldGame.botWins,
            oldGame.userWins,
            oldGame.totalGames + 1)
    } else {
      Games(id,
            boardStr,
            huPlayer,
            aiPlayer,
            oldGame.botWins,
            oldGame.userWins,
            oldGame.totalGames)
    }
    db.run(gameTableQuery.filter(_.id === id).update(newGame))
  }
  def huMove(board: String, id: Long): Future[Int] = {
    val oldGame: Games = Await
      .result(db.run(gameTableQuery.filter(_.id === id).result), Duration.Inf)
      .head
    val huPlayer = oldGame.userChar
    val aiPlayer = oldGame.botChar
    val boardSeq = ArrayBuffer(
      board(0).toString,
      board(1).toString,
      board(2).toString,
      board(3).toString,
      board(4).toString,
      board(5).toString,
      board(6).toString,
      board(7).toString,
      board(8).toString
    )
    val availSpots = boardSeq.filter(a => a != "O" && a != "X").toList
    val newGame = if (availSpots.isEmpty) {
      if (MinimaxTTT.winning(boardSeq, huPlayer))
        Games(id,
              board,
              huPlayer,
              aiPlayer,
              oldGame.botWins,
              oldGame.userWins + 1,
              oldGame.totalGames + 1)
      else if (MinimaxTTT.winning(boardSeq, aiPlayer))
        Games(id,
              board,
              huPlayer,
              aiPlayer,
              oldGame.botWins + 1,
              oldGame.userWins,
              oldGame.totalGames + 1)
      else
        Games(id,
              board,
              huPlayer,
              aiPlayer,
              oldGame.botWins,
              oldGame.userWins,
              oldGame.totalGames + 1)

    } else {
      Games(id,
            board,
            huPlayer,
            aiPlayer,
            oldGame.botWins,
            oldGame.userWins,
            oldGame.totalGames)
    }
    db.run(gameTableQuery.filter(_.id === id).update(newGame))
  }
  def getGame(id:Long): Future[Games] = {
    db.run(gameTableQuery.filter(_.id === id).result.head)
  }
}
