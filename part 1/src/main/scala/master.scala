import akka.actor._
import com.typesafe.config.ConfigFactory

import scala.collection.mutable.ArrayBuffer
import scala.concurrent.ExecutionContext.Implicits.global
import scala.concurrent.duration._
import scala.util.Random


sealed trait MasterMessage

case class begin() extends MasterMessage

case class done() extends MasterMessage

case class simDone() extends MasterMessage


class MasterActor(numClients: Int, numTasks: Int) extends Actor {
  var startTime: Long = 0
  var stopTime: Long = 0
  var doneCount = 0
  var simDoneCount = 0
  var simOnce = false
  var once = false
  val friendlists = new ArrayBuffer[ArrayBuffer[Int]] //generate friendlists
  for (i <- 0 until numClients) {
    var t = new ArrayBuffer[Int]()
    friendlists += t
  }
  for (i <- 0 until numClients) {
    val randNum = Random.nextInt(10) + 1

    for (j <- 0 until randNum) {
      val k = Random.nextInt(numClients)
      if (i != k) {
        friendlists(i) += k
        friendlists(k) += i
      }
    }
  }

  for (i <- 0 until numClients) {
    friendlists(i) = friendlists(i).distinct
  }


  val masterActorList = new ArrayBuffer[ActorRef]()


  for (i <- 0 until numClients) {
    masterActorList += context.actorOf(Props(new clientActor(i, friendlists(i).toArray, self)), name = "ClientActor" + i)
  }

  def receive = {
    case done() =>
      if (doneCount == numClients - 1) {

        if (once == false) {
          println("*******************Start Simulation**************************")
          startTime = System.currentTimeMillis
          self ! begin()
          once = true
        }

      }
      else {
        doneCount += 1
      }

    case simDone() =>
      if (simDoneCount == numClients - 1) {
        if (simOnce == false) {
          //stop sim
          stopTime = System.currentTimeMillis

          println("simulation time (ms): " + (stopTime - startTime))
          context.system.shutdown()
        }
      }
      else {
        simDoneCount += 1
      }


    case begin() =>

      //masterActorList(0) ! startActivity()

      for (i <- 0 until numClients) {
        context.system.scheduler.scheduleOnce(5000 milliseconds, masterActorList(i), startActivity(numTasks))

      }


  }

}


object master {


  def main(args: Array[String]) {

    // master [# of clients] [activites per client]

    val numClients = args(0).toInt
    val numTasks = args(1).toInt

    val system = ActorSystem("Simulation", ConfigFactory.load(ConfigFactory.parseString(
      """
        akka {
        |loggers = ["akka.event.Logging$DefaultLogger"]
        |logger-startup-timeout = 500s
        |}

      """.stripMargin)))

    // create the master
    val boss = system.actorOf(
      Props(new MasterActor(numClients, numTasks)),
      name = "boss")



    //boss ! begin()


  }


}