import akka.actor._
import akka.util.Timeout
import spray.client.pipelining.{Post, sendReceive, _}
import spray.http.{HttpRequest, HttpResponse}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util._

sealed trait ClientMessage

case class bootClient() extends ClientMessage

case class setID(idVal: Int) extends ClientMessage

case class establishSession() extends ClientMessage

case class getFriendList(FriendListID: Int) extends ClientMessage

case class getPage(PageID: Int) extends ClientMessage

case class sendPost(PostId: Int) extends ClientMessage

case class getPhoto(PhotoId: Int) extends ClientMessage

case class sendFriendList(fid: Int) extends ClientMessage

case class sendPage(pid: Int) extends ClientMessage

case class sendPhoto(photoid: Int) extends ClientMessage

case class getPost(postid: Int) extends ClientMessage

case class addFriend(friendID: Int) extends ClientMessage

case class removeDuplicates() extends ClientMessage

case class startActivity(numActivites: Int) extends ClientMessage

case class printFriends() extends ClientMessage

case class sendAlbum(AlbumId: Int) extends ClientMessage

case class getAlbum(AlbumId: Int) extends ClientMessage

case class sendLike(likeid: Int, kind: String) extends ClientMessage



case class getPhotoFromPage(pageid: Int) extends ClientMessage

case class getPostFromPage(pageid: Int) extends ClientMessage

case class getPhotoFromAlbum(albumid: Int) extends ClientMessage


class clientActor(ID: Int, friendlist: Array[Int], boss: ActorRef) extends Actor {
  val system = ActorSystem("simple-spray-client")

  implicit val timeout: Timeout = 40.second

  import system.dispatcher

  val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
  var id = ID
  var sessionID = -1
  var friends = friendlist

  //initialization before simulation
  val f = Future {
    self ! sendPage(id)
    //context.system.scheduler.scheduleOnce(5000 milliseconds, self, sendPage(id))
  }

  f onComplete {
    case Success(posts) =>
      //print("success")
      val t = Future {
        self ! sendFriendList(id)
        //context.system.scheduler.scheduleOnce(5000 milliseconds, self, sendFriendList(id))
      }

      t onComplete {
        case Success(posts) =>
          //boss ! done()
          val a = Future {
            self ! sendAlbum(id)
          }

          a onComplete {
            case Success(posts) =>
              boss ! done()
            case Failure(t) =>
              println("An error has occured: " + t.getMessage)
          }
        case Failure(t) =>
          println("An error has occured: " + t.getMessage)
      }
    case Failure(f) => println("An error has occured: " + f.getMessage)
  }


  def RandomValue(length: Int, typeOfValue: Int): String = typeOfValue match { //generate random values
    case 1 => //regular random string
      Random.alphanumeric.take(length).mkString

    case 2 => //birthday
      Random.nextInt(12) + "/" + Random.nextInt(28) + "/" + Random.nextInt(99) //work on this later

    case 3 => // name
      val firstNameList = Array("John", "David", "Michael", "Chris", "Mike", "Mark", "Paul", "Daniel", "James")
      val lastNameList = Array("Smith", "jones", "Johnson", "Lee", "Brown", "Williams", "Rodriguez", "Garcia", "Lopez")

      firstNameList(Random.nextInt(9)) + " " + lastNameList(Random.nextInt(9))

    case 4 => //phone number
      "(" + Random.nextInt(10) + Random.nextInt(10) + Random.nextInt(10) + ")" + Random.nextInt(10) + Random.nextInt(10) + Random.nextInt(10) + "-" + Random.nextInt(10) + Random.nextInt(10) + Random.nextInt(10) + Random.nextInt(10)

    case 5 => //timestamp
      Random.nextInt(24) + ":" + Random.nextInt(60) + ":" + Random.nextInt(60)

    case _ =>

      return "hello"

  }


  def receive = {

    case bootClient() =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/ping"))

      response onComplete {
        case Success(response) =>
          println("client got response" + response.entity)

        case _ =>
          println("didnt get response")
        //system.shutdown()
      }

    case printFriends() =>
      println("id: " + id + " " + friends)

    case removeDuplicates() =>
      friends = friends.distinct

    case setID(idVal) =>
      id = idVal

    case startActivity(numActivites) =>
      if (numActivites > 0) {
        val rvalue = Random.nextInt(20)

        rvalue match {
          case x if (0 <= x && x < 5) =>
            self ! sendPost(friends(Random.nextInt(friends.length)))

          case x if (5 <= x && x < 10) =>
            self ! sendPhoto(friends(Random.nextInt(friends.length)))

          case x if (10 <= x && x < 12) => //pj
            //self ! sendPhoto(friends(Random.nextInt(friends.length)))
            self ! getPage(id)

          case x if (12 <= x && x < 14) => self ! getPhotoFromPage(id) // where id corresponds to the page id

          case x if (14 <= x && x < 16) =>
            //self ! sendLike(friends(Random.nextInt(friends.length)), "page")
            self ! getPostFromPage(id)

          case x if (16 <= x && x < 19) =>
            //self ! getPostFromPage(id)
            self ! sendLike(friends(Random.nextInt(friends.length)), "page")

          case 19 => self ! getPhotoFromAlbum(id)
        }

        //self ! startActivity(numActivites - 1)

        context.system.scheduler.scheduleOnce(5000 milliseconds, self, startActivity(numActivites - 1))

      }
      else {
        //send a done message to master
        boss ! simDone()
      }


    case sendLike(likeid, kind) =>
      import LikeJsonProtocol._
      import spray.json._

      val L = Like(id, likeid, kind)
      val out = L.toJson.prettyPrint
      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/like/" + likeid, out))
      response onComplete {
        case Success(response) =>
        //sessionID = response.message.entity.asString.toInt


        case _ =>
          println("didnt get response")
        //system.shutdown()

      }


    case establishSession() =>
      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/establishSession/" + id))
      response onComplete {
        case Success(response) =>
          sessionID = response.message.entity.asString.toInt

        case _ =>
          println("didnt get response")
        //system.shutdown()

      }

    case getFriendList(friendListID) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/friendlist/" + friendListID))
      import FriendListJsonProtocol._
      import spray.json._
      response onComplete {
        case Success(response) =>

          val friendJson = response.message.entity.asString
          val friendlistObj = friendJson.parseJson.convertTo[FriendList]
          // use friendlistObj for friendlist attributes
          println(friendJson)

        case _ =>
          println("didnt get response for get friendlist")
        //system.shutdown()
      }

    case getPage(pageid) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/page/" + pageid))
      import PageJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(response) =>

          val pageJson = response.message.entity.asString
          val pageObj = pageJson.parseJson.convertTo[Page]

          println(pageJson)

        case _ =>
          println("didnt get response for get page")
        //system.shutdown()
      }

    case getPhoto(photoid) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/photo/" + photoid))
      import PhotoJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(response) =>

          val photoJson = response.message.entity.asString
          val photoObj = photoJson.parseJson.convertTo[Photo]

          println(photoJson)

        case _ =>
          println("didnt get response for get photo")

        //system.shutdown()
      }

    case getPostFromPage(pageid) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/page/" + pageid))
      import PageJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(response) =>

          val pageJson = response.message.entity.asString
          val pageObj = pageJson.parseJson.convertTo[Page]
          try {
            self ! getPost(pageObj.post_list(Random.nextInt(pageObj.post_list.length)))
          } catch {
            case e: IllegalArgumentException => println("The page for the client does not have any posts")
              sender ! HttpResponse(entity = "client does not have any posts")
          }

        //println(pageJson)

        case _ =>
          println("didnt get response for get post from page")
        //system.shutdown()
      }

    case getPhotoFromPage(pageid) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/page/" + pageid))
      import PageJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(response) =>

          val pageJson = response.message.entity.asString
          val pageObj = pageJson.parseJson.convertTo[Page]
          try {
            self ! getPhoto(pageObj.photo_list(Random.nextInt(pageObj.photo_list.length)))
          } catch {
            case e: IllegalArgumentException => println("The page for the client does not have any photos")
              sender ! HttpResponse(entity = "client does not have any photos")
          }

        //println(pageJson)

        case _ =>
          println("didnt get response for get photo from page")
        //system.shutdown()
      }

    case getPhotoFromAlbum(albumid) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/album/" + albumid))
      import AlbumJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(response) =>

          val albumJson = response.message.entity.asString
          val albumObj = albumJson.parseJson.convertTo[Album]
          try {
            self ! getPhoto(albumObj.photoList(Random.nextInt(albumObj.photoList.length)))
          } catch {
            case e: IllegalArgumentException => println("The album for the client does not have any photos")
              sender ! HttpResponse(entity = "client album does not have any photos")
          }

        //println(pageJson)

        case _ =>
          println("didnt get response for get photo from page")
        //system.shutdown()
      }

    case sendPost(postid) =>
      import PostJsonProtocol._
      import spray.json._


      //dummy value for post
      val listName = "list test"
      val dummyList = Array(1, 2, 3, 4)
      val dummyComments: Array[String] = Array("hello", "world")
      val P = PostMessage(id, RandomValue(1, 5), id, postid, RandomValue(6, 1), "status", Array(), Array())
    val out = P.toJson.prettyPrint

      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/post/" + postid, out))

      response onComplete {
        case Success(response) =>

          println("client got post reply ")

        case _ =>
          println("didnt get response for send post")
        //system.shutdown()
      }

    case sendFriendList(fid) =>
      import FriendListJsonProtocol._
      import spray.json._

      //dummy value for sending friendlist
      val listName = "list test"
      val dummyList = Array(1, 2, 3, 14)
      //val dummyComments: Array[String] = Array("hello", "world")
      //val F = FriendList(100, dummyList, listName, 100)
      val F = FriendList(id, friends.toArray, RandomValue(5, 1), fid)

      val out = F.toJson.prettyPrint

      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/friendlist/" + fid, out))

      response onComplete {
        case Success(response) =>

          println("client got friendlist reply")

        //case _ =>
        case Failure(response) =>
          println("didnt get response for send friendlist" + response)
        //system.shutdown()
      }

    case sendPage(pid) =>
      import PageJsonProtocol._
      import spray.json._


      //dummy value for sending page
      val listName = "list test"
      val dummyList = Array[Int]()
      //val dummyComments: Array[String] = Array("hello", "world")
      val F = Page(pid, RandomValue(1, 2), RandomValue(10, 1), RandomValue(12, 1), "www.facebook.com/page/" + pid, RandomValue(1, 3), RandomValue(10, 1), RandomValue(10, 1), RandomValue(1, 4), 0, dummyList, Array[Int]()) //pj photolist added
    val out = F.toJson.prettyPrint

      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/page/" + pid, out))

      response onComplete {
        case Success(response) =>

          println("client got page reply")

        case Failure(response) =>
          println("didnt get response send page" + response)
        //system.shutdown()
      }

    case sendPhoto(photoid) =>
      //dummy value for photo
      import PhotoJsonProtocol._
      import spray.json._
      //case class Photo(id: Int, album_id: Int, created_time: String, height: Int, width: Int, update_time: String, link: String)
      val Ph = Photo(photoid, Random.nextInt(10), "today", 400, 500, "now", "www.facebook.com/photo" + photoid, id, photoid) //pj
    val out = Ph.toJson.prettyPrint
      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/photo/" + photoid, out))
      //println("sending photo")

      response onComplete {
        case Success(response) =>

          println("client got photo reply")

        case _ =>
          println("didnt get response for send photo")
        //system.shutdown()
      }

    case getPost(postid) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/post/" + postid))
      import PostJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(response) =>

          val postJson = response.message.entity.asString
          val postObj = postJson.parseJson.convertTo[PostMessage]

          println(postJson)

        case _ =>
          println("didnt get response for get post")
        //system.shutdown()
      }

    case getAlbum(albumid) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/album/" + albumid))
      import AlbumJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(response) =>

          val albumJson = response.message.entity.asString
          val albumObj = albumJson.parseJson.convertTo[Album]
          //println("Success")
          println(albumJson)
        case _ =>

          println("didnt get response for get album")
        //system.shutdown()
      }

    case sendAlbum(albumid) =>
      //dummy value for photo
      import AlbumJsonProtocol._
      import spray.json._
      val dummyPhotoList1: Array[Int] = Array()
      val dummyComments1: Array[String] = Array()
      val Alb = Album(albumid, true, 0, "facebook.com/album/" + albumid, id, id, RandomValue(5, 1), dummyPhotoList1, dummyPhotoList1, dummyComments1, "now", "now")

      val out = Alb.toJson.prettyPrint
      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/album/" + albumid, out))
      //println("sending album")

      response onComplete {
        case Success(response) =>
          println("client got album reply")

        //case _ =>
        case Failure(reponse) =>
          println("didnt get response for send album" + reponse)
        //system.shutdown()
      }


    case _ =>
      println("default case for client")
  }
}

object client {

  def main(args: Array[String]) {
    val system = ActorSystem()
    //var ar = Array(1,2,3)

    //val cActor = system.actorOf(Props(new clientActor(0, ar, ActorRef()), name = "cActor"))

    //    cActor ! sendPost(100)
    //    cActor ! getPage(100)
    //    cActor ! getFriendList(100)

    //cActor ! sendPage(10)

    // cActor ! sendAlbum(100)
    //cActor ! getAlbum(100)

  }
}



