import info.mukel.telegrambot4s._
import info.mukel.telegrambot4s.Implicits._
import info.mukel.telegrambot4s.api._
import info.mukel.telegrambot4s.api.declarative.Commands
import info.mukel.telegrambot4s.methods.{ParseMode, SendMessage}
import info.mukel.telegrambot4s.api.declarative.{Callbacks, InlineQueries}
import models.{ChatId, InlineKeyboardButton, InlineKeyboardMarkup, User}
import info.mukel.telegrambot4s.methods.EditMessageText
import slick.jdbc.PostgresProfile.api._

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.Await
import scala.concurrent.duration.Duration
import scala.io.Source

object Bot
    extends TelegramBot
    with Polling
    with Commands
    with InlineQueries
    with Callbacks {
  // Use 'def' or 'lazy val' for the token, using a plain 'val' may/will
  // lead to initialization order issues.
  // Fetch the token from an environment variable or untracked file.
  val db = Database.forURL(
    "jdbc:postgresql://127.0.0.1:5432/TTTBot?user=postgres&password=mypassword"
  )
  val repo = new Repo(db)
  lazy val token: String = scala.util.Properties
    .envOrNone("BOT_TOKEN")
    .getOrElse(Source.fromFile("bot.token").getLines().mkString)

  def displayBoard(board: ArrayBuffer[String]): String = {
    def getChar(s: String) = {
      if (s != "O" && s != "X") "."
      else s
    }
    getChar(board(0)) concat " " concat getChar(board(1)) concat " " concat getChar(
      board(2)) concat
      "\n" concat getChar(board(3)) concat " " concat getChar(board(4)) concat " " concat getChar(
      board(5)) concat
      "\n" concat getChar(board(6)) concat " " concat getChar(board(7)) concat " " concat getChar(
      board(8))

  }
  def makeNewKeyboard(board: ArrayBuffer[String]): InlineKeyboardMarkup = {
    def getChar(s: String) = {
      if (s != "O" && s != "X") "."
      else s

    }
    InlineKeyboardMarkup(
      Seq(
        Seq(
          InlineKeyboardButton.callbackData(getChar(board(0)), "MOVE 1 1"),
          InlineKeyboardButton.callbackData(getChar(board(1)), "MOVE 1 2"),
          InlineKeyboardButton.callbackData(getChar(board(2)), "MOVE 1 3")
        ),
        Seq(
          InlineKeyboardButton.callbackData(getChar(board(3)), "MOVE 2 1"),
          InlineKeyboardButton.callbackData(getChar(board(4)), "MOVE 2 2"),
          InlineKeyboardButton.callbackData(getChar(board(5)), "MOVE 2 3")
        ),
        Seq(
          InlineKeyboardButton.callbackData(getChar(board(6)), "MOVE 3 1"),
          InlineKeyboardButton.callbackData(getChar(board(7)), "MOVE 3 2"),
          InlineKeyboardButton.callbackData(getChar(board(8)), "MOVE 3 3")
        )
      ))
  }

  onCommand('hello) { implicit msg =>
    reply("My token is SAFE!")
  }

  onCallbackQuery { implicit cbq =>
    //SendMessage(ChatId(cbq.message.get.chat.),text="a")
    println(cbq.message.get.chat.username.get)
    if (cbq.data.isDefined) {
      val data: String = cbq.data.get
      val command = data.split(" ").head
      if (command == "MOVE") {
        val currentGame = Await.result(repo.getGame(cbq.from.id), Duration.Inf)
        val moveRow = data.split(" ")(1).toInt
        val moveCol = data.split(" ")(2).toInt
        val position = (moveRow - 1) * 3 + moveCol - 1
        if (currentGame.gameBoard(position) != 'O' && currentGame.gameBoard(
              position) != 'X') {
          val boardSeq = ArrayBuffer(
            currentGame.gameBoard(0).toString,
            currentGame.gameBoard(1).toString,
            currentGame.gameBoard(2).toString,
            currentGame.gameBoard(3).toString,
            currentGame.gameBoard(4).toString,
            currentGame.gameBoard(5).toString,
            currentGame.gameBoard(6).toString,
            currentGame.gameBoard(7).toString,
            currentGame.gameBoard(8).toString
          )
          boardSeq(position) = currentGame.userChar
          val newBoard = boardSeq(0) concat boardSeq(1) concat boardSeq(2) concat boardSeq(
            3) concat boardSeq(4) concat boardSeq(5) concat boardSeq(6) concat boardSeq(
            7) concat boardSeq(8)
          repo.huMove(newBoard, cbq.from.id).onComplete { _ =>
            {
              request(EditMessageText(ChatId(cbq.message.get.chat.id),
                cbq.message.get.messageId,
                text = displayBoard(boardSeq),
                replyMarkup = makeNewKeyboard(boardSeq)))
              val availSpots = boardSeq.filter(a => a != "O" && a != "X").toList
//              if (availSpots.isEmpty) {
                if (MinimaxTTT.winning(boardSeq, currentGame.userChar)) {
                  ackCallback("You win!", showAlert = true)
                } else if (MinimaxTTT.winning(boardSeq, currentGame.botChar)) {
                  ackCallback("You lose!", showAlert = true)
                } else if (availSpots.isEmpty) ackCallback("DRAW!", showAlert = true)
                //TODO:Start new game
               else {
                repo.aiMove(cbq.from.id).onComplete { _ =>
                  val newGame = Await.result(repo.getGame(cbq.from.id), Duration.Inf)
                  val newBoardSeq = ArrayBuffer(
                    newGame.gameBoard(0).toString,
                    newGame.gameBoard(1).toString,
                    newGame.gameBoard(2).toString,
                    newGame.gameBoard(3).toString,
                    newGame.gameBoard(4).toString,
                    newGame.gameBoard(5).toString,
                    newGame.gameBoard(6).toString,
                    newGame.gameBoard(7).toString,
                    newGame.gameBoard(8).toString
                  )
                  request(EditMessageText(ChatId(cbq.message.get.chat.id),
                                  cbq.message.get.messageId,
                                  text = displayBoard(newBoardSeq),
                                  replyMarkup = makeNewKeyboard(newBoardSeq)))
                  val availSpots = newBoardSeq.filter(a => a != "O" && a != "X").toList
                  //              if (availSpots.isEmpty) {
                  if (MinimaxTTT.winning(newBoardSeq, currentGame.userChar)) {
                    ackCallback("You win!", showAlert = true)
                  } else if (MinimaxTTT.winning(newBoardSeq, currentGame.botChar)) {
                    ackCallback("You lose!", showAlert = true)
                  } else if (availSpots.isEmpty) ackCallback("DRAW!", showAlert = true)
                  //TODO:Start new game
                }
              }
            }
          }

        } else ackCallback("Choose another field", showAlert = true)
        //ackCallback(command + moveRow + moveCol, showAlert = true) //тест команд
      }
    } else {
      ackCallback("Bad callback. Please, report @kromuch")
    }
  }
  val ikb = InlineKeyboardMarkup.singleButton(
    InlineKeyboardButton.callbackData("text", "cdb"))
  def lmgtfyBtn(query: String): InlineKeyboardMarkup = ikb
  def printt(s: String): String = {
    println(s)
    s
  }
  onCommand('help) { implicit msg =>
    reply( //TODO:ДОПИСАТИ ДОВІДКУ
      s"""Tic-Tac-Toe
         |
         |/help - list commands
         |
         |/lmgtfy2 | /btn args - clickable button
         |
         |@Bot args - Inline mode
      """.stripMargin,
      parseMode = ParseMode.Markdown
    )
  }
  onCommand('start) { implicit msg =>
    val emptyBoard = "012345678"
    val emptyUser = User(0, isBot = false, "")
    val emptyGame =
      Games(msg.from.getOrElse(emptyUser).id, emptyBoard, "X", "O", 0, 0, 0)
    repo.startCommand(emptyGame).onComplete {
      case util.Success(_) =>
        reply( //TODO:ДОПИСАТИ ДОВІДКУ
          s"""Welcome to Tic-Tac-Toe
             |
         |Available commands:
             |
         |/help - list commands
             |
      """.stripMargin,
          parseMode = ParseMode.Markdown
        ).onComplete {
          case util.Success(_) => {
            val keyboard = InlineKeyboardMarkup(
              Seq(
                Seq(
                  InlineKeyboardButton.callbackData("1 1", "MOVE 1 1"),
                  InlineKeyboardButton.callbackData("1 2", "MOVE 1 2"),
                  InlineKeyboardButton.callbackData("1 3", "MOVE 1 3")
                ),
                Seq(
                  InlineKeyboardButton.callbackData("2 1", "MOVE 2 1"),
                  InlineKeyboardButton.callbackData("2 2", "MOVE 2 2"),
                  InlineKeyboardButton.callbackData("2 3", "MOVE 2 3")
                ),
                Seq(
                  InlineKeyboardButton.callbackData("3 1", "MOVE 3 1"),
                  InlineKeyboardButton.callbackData("3 2", "MOVE 3 2"),
                  InlineKeyboardButton.callbackData("3 3", "MOVE 3 3")
                )
              ))
            val board =
              ArrayBuffer[String]("0", "1", "2", "3", "4", "5", "6", "7", "8")
            reply(displayBoard(board), replyMarkup = makeNewKeyboard(board))
          }
          case util.Failure(_) =>
            reply("Error sending message, please, restart the bot.")
        }
      case util.Failure(_) => reply("I am broken, sorry.")
    }
  }
}
