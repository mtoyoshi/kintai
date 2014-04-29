package models

import scala.collection
import scala.collection.mutable
import java.text._
import scala.util.Try

/**
  * TimeCardを表すクラスです。
  * 各行は「from時刻-to時刻 作業名」というフォーマットを期待しており、作業名についてはセミコロンを用いてメモを付与することも可能です。
  * ※ メモはタスク名には含まれません
  * 例）
  * 10:00-11:25 HMPver.5.4:コードレビュー
  * 6:10-7:10 事務作業
  * 13:15-14:00 HMPver.5.4:会議
  */
case class TimeCard(content: String) {
  val TIME_PATTERN = """(\d{1,2}:\d{2})-(\d{1,2}:\d{2})\s(.+)""".r
  val DATE_FORMAT = new SimpleDateFormat("HH:mm")

  val taskList = mutable.ListBuffer.empty[Task]

  private def convertToTaskList(): Unit = {
    if (content == null || content.lines.size < 1) throw new RuntimeException()

    val tmpMap = new mutable.HashMap[String, mutable.ListBuffer[Double]]

    for (line <- content.lines) {
      val TIME_PATTERN(from, to, taskName) = line
      val diffTimeMs = DATE_FORMAT.parse(to).getTime() - DATE_FORMAT.parse(from).getTime()

      val taskNameWithoutMemo = taskName.takeWhile{ _ != ':' }
      tmpMap.getOrElseUpdate(taskNameWithoutMemo, mutable.ListBuffer.empty[Double]) += diffTimeMs
    }

    tmpMap foreach { record => taskList += Task(record) }
  }

  /**
    * 合計時間(hour)を算出します
    */
  def totalHour = taskList.foldLeft(0.0d){ (total, task) => total + task.time }

  def validation(): Boolean = Try(convertToTaskList()).isSuccess

}

/**
  * TimeCardの変換後はこのTask毎に勤務時間がまとめられる
  */
case class Task(name: String, time: Double)
object Task {
  val MS_TO_HOUR = 1000.0 * 60.0 * 60.0
  def apply(tpl: (String, collection.Seq[Double])): Task = {
    Task(tpl._1, tpl._2.sum / MS_TO_HOUR)
  }
}