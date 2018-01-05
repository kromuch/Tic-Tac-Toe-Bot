import scala.collection.mutable.ArrayBuffer

object MinimaxTTT {
  def minimax(board: ArrayBuffer[String],
              player: String,
              huPlayer: String,
              aiPlayer: String): ArrayBuffer[String] = {
    def in(board: ArrayBuffer[String],
           player: String,depth:Int): Int = {
      val invertPlayer = {
        if (player == "X") "O"
        else "X"
      }
      val availSpots = board.filter(a => { a != "O" && a != "X" })
      if (availSpots.nonEmpty) {
        //        for (i <- availSpots.indices) {
        //          val temp = board
        //          temp(i) = player
        //          in(temp, invertPlayer, acc)
        //        }
        availSpots.foldLeft(0){(zero,value) => {
          val temp = ArrayBuffer(board(0),
            board(1),
            board(2),
            board(3),
            board(4),
            board(5),
            board(6),
            board(7),
            board(8)
          )
          temp(value.toInt) = player
          in(temp,invertPlayer,depth+1)
        }}
      }
      else if (winning(board,aiPlayer)) 10*(9-depth)
      else if (winning(board,huPlayer)) -10*(9-depth)
      else 0
//      if (winning(board, aiPlayer)) return (10+depth, 0)
//      else if (winning(board, huPlayer)) return (-10+depth, 0)
//      else if (availSpots.isEmpty) return (0, 0)
//
//      var maxScore = 0
//      var bestMove = ""
//      for (i <- availSpots.indices) {
//        val temp = ArrayBuffer(board(0),
//                               board(1),
//                               board(2),
//                               board(3),
//                               board(4),
//                               board(5),
//                               board(6),
//                               board(7),
//                               board(8))
//        temp(availSpots(i).toInt) = player
//        val score = in(temp, invertPlayer, depth + 1)
//        if (i == 0) {
//          maxScore = score._1
//          bestMove = availSpots(i)
//        } else if (player == aiPlayer)
//        {if (score._1 < maxScore) {
//          maxScore = score._1
//          bestMove = availSpots(i)
//        }} else {
//          {if (score._1 > maxScore) {
//            maxScore = score._1
//            bestMove = availSpots(i)
//          }}
//        }
//      }
//      (0, bestMove.toInt)
    }

    val availSpots = board.filter(a => { a != "O" && a != "X" })
    if (availSpots.length > 1) {
      //var moves = ArrayBuffer[String]
      var maxScore = 0
      var bestMove = ""
      for (i <- availSpots.indices) {
        val newBoard = ArrayBuffer(board(0),
          board(1),
          board(2),
          board(3),
          board(4),
          board(5),
          board(6),
          board(7),
          board(8)
        )
        newBoard(availSpots(i).toInt) = player
        val score = in(newBoard, huPlayer,0)
        //moves += (availSpots(i) concat " " concat score.toString)
        if (i == 0) {
          maxScore = score
          bestMove = availSpots(i)
        }
        else if (score > maxScore)
          {
            maxScore = score
            bestMove = availSpots(i)
          }
      }
      //bestMove = in(board, player).toString
      val temp1 = board
      temp1(bestMove.toInt) = player
      temp1
    } else {
      val temp = board
      val freeIndex = availSpots.head.toInt
      temp(freeIndex) = player
      temp
    }
//    if (winning(board, huPlayer)) return (ArrayBuffer[String](), -10)
//    else if (winning(board, aiPlayer)) return (ArrayBuffer[String](), 10)
//    else if (availSpots.isEmpty) return (ArrayBuffer[String](), 0)

  }

  def winning(board: ArrayBuffer[String], player: String): Boolean = {
    if ((board(0) == player && board(1) == player && board(2) == player) ||
        (board(3) == player && board(4) == player && board(5) == player) ||
        (board(6) == player && board(7) == player && board(8) == player) ||
        (board(0) == player && board(3) == player && board(6) == player) ||
        (board(1) == player && board(4) == player && board(7) == player) ||
        (board(2) == player && board(5) == player && board(8) == player) ||
        (board(0) == player && board(4) == player && board(8) == player) ||
        (board(2) == player && board(4) == player && board(6) == player)) {
      true
    } else {
      false
    }
  }
}
