import akka.actor._
import akka.util.Timeout
import spray.client.pipelining.{Post, sendReceive, _}
import spray.http.{HttpRequest, HttpResponse}

import scala.concurrent.Future
import scala.concurrent.duration._
import scala.util._

import scala.concurrent.Await
import scala.concurrent.Future
import scala.concurrent.duration._


sealed trait ClientMessage

case class bootClient() extends ClientMessage

case class setID(idVal: Int) extends ClientMessage

case class establishSession(clientid: Int) extends ClientMessage

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

case class startActivity2(numActivites: Int) extends ClientMessage

case class printFriends() extends ClientMessage

case class sendAlbum(AlbumId: Int) extends ClientMessage

case class getAlbum(AlbumId: Int) extends ClientMessage

case class sendLike(likeid: Int, kind: String) extends ClientMessage

case class getPhotoFromPage(pageid: Int) extends ClientMessage

case class getPostFromPage(pageid: Int) extends ClientMessage

case class getPhotoFromAlbum(albumid: Int) extends ClientMessage

case class sendPublicKeys(DSAKey: String, RSAKey: String) extends ClientMessage

case class getPublicKey(kid: Int) extends ClientMessage


class clientActor(ID: Int, friendlist: Array[Int], boss: ActorRef, numTasks: Int) extends Actor {
  val system = ActorSystem("simple-spray-client")
  implicit val timeout: Timeout = 100.second

  import system.dispatcher

  val pipeline: HttpRequest => Future[HttpResponse] = sendReceive
  var id = ID
  var tokenID = ""
  var friends = friendlist

  val PK = PublicKeyEncrypt
  val DSAkeypair = PK.generateKeyPair("DSA")
  val DSApublic_key = DSAkeypair._1
  val DSAprivate_key = DSAkeypair._2

  val RSAkeypair = PK.generateKeyPair("RSA")
  val RSApublic_key = RSAkeypair._1
  val RSAprivate_key = RSAkeypair._2


  //  val auth = Future {
  //    self ! sendPublicKeys(DSApublic_key, RSApublic_key)
  //  }
  //  auth onComplete {
  //    case Success(message) =>
  //      val t = Future {
  //        self ! establishSession(id)
  //      }
  //      t onComplete {
  //        case Success(m) =>
  //          self ! sendPage(id)
  //          println("finish estblish session")
  //
  //        case Failure(m) =>
  //          println("An error has occured: " + m.getMessage)
  //
  //      }
  //
  //    case Failure(message) =>
  //      println("An error has occured: " + message.getMessage)
  //  }

  //  val f1 = Future{
  //    self ! sendPublicKeys(DSApublic_key, RSApublic_key)
  //  }
  //  val r1 = Await.result(f1, 60.seconds)
  //
  //  val f2 = Future{
  //    self ! establishSession(id)
  //  }
  //  val r2 = Await.result(f2, 60.seconds)

  val f = Future {

    self ! sendPublicKeys(DSApublic_key, RSApublic_key)
    self ! sendFriendList(id)
  }
  f onComplete {
    case Success(m) =>
      boss ! done()
    case Failure(m) =>

  }


  //  val f3 = Future{
  //    self ! sendPage(id)
  //  }
  //  val r3 = Await.result(f3, 60.seconds)


  //val f = Future {


  //self ! sendFriendList(id)
  //self ! sendAlbum(id)

  //  }
  //  f onComplete {
  //    case Success(message) =>
  //      boss ! done()
  //
  //    case Failure(message) =>
  //      println("An error has occured: " + message.getMessage)
  //  }


  //  //initialization before simulation
  //  val f = Future {
  //    self ! sendPage(id)
  //    //context.system.scheduler.scheduleOnce(5000 milliseconds, self, sendPage(id))
  //  }
  //
  //  f onComplete {
  //    case Success(posts) =>
  //      //print("success")
  //      val t = Future {
  //        self ! sendFriendList(id)
  //        //context.system.scheduler.scheduleOnce(5000 milliseconds, self, sendFriendList(id))
  //      }
  //
  //      t onComplete {
  //        case Success(posts) =>
  //          //boss ! done()
  //          val a = Future {
  //            self ! sendAlbum(id)
  //          }
  //
  //          a onComplete {
  //            case Success(posts) =>
  //              boss ! done()
  //            case Failure(t) =>
  //              println("An error has occured: " + t.getMessage)
  //          }
  //        case Failure(t) =>
  //          println("An error has occured: " + t.getMessage)
  //      }
  //    case Failure(f) => println("An error has occured: " + f.getMessage)
  //  }


  def RandomValue(length: Int, typeOfValue: Int): String = typeOfValue match {
    //generate random values
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
      "hello"
  }

  def rsaJsonEncrypt(token: String, json: String, publicKey: String): String = {
    val e1 = Encrypt
    val e2 = PublicKeyEncrypt
    val keyAndText = e1.AESEncrypt(json)
    val encryptedKey = e2.encrypt(keyAndText._1, publicKey)
    val total = token + " " + keyAndText._2 + " " + encryptedKey
    println(total)
    total
  }


  def receive = {

    case bootClient() =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/ping"))

      response onComplete {
        case Success(resp) =>
          println("client got response" + resp.entity)

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

    case startActivity2(numActivites) =>
      if (numActivites > 0) {
        val rvalue = Random.nextInt(11)
        rvalue match {
          case x if 0 <= x && x < 2 =>
            self ! getPage(id)
          case x if 2 <= x && x < 7 =>
            self ! sendPost(friends(Random.nextInt(friends.length)))
          case x if 7 <= x && x < 9 =>
            self ! getPost(id)
          case x if 9 <= x && x < 11 =>
            self ! sendLike(friends(Random.nextInt(friends.length)), "page")
        }
        context.system.scheduler.scheduleOnce(5000.milliseconds, self, startActivity2(numActivites - 1))
      }
      else{
        boss ! simDone()
      }

    case startActivity(numActivites) =>
      //pj authentication testing

      if (numActivites > 0) {
        val rvalue = Random.nextInt(20)

        rvalue match {
          case x if 0 <= x && x < 5 =>
            self ! sendPost(friends(Random.nextInt(friends.length)))

          case x if 5 <= x && x < 10 =>
            self ! sendPhoto(friends(Random.nextInt(friends.length)))

          case x if 10 <= x && x < 12 =>
            self ! getPage(id)

          case x if 12 <= x && x < 14 =>
            self ! getPhotoFromPage(id) // where id corresponds to the page id

          case x if 14 <= x && x < 16 =>
            self ! getPostFromPage(id)

          case x if 16 <= x && x < 19 =>
            self ! sendLike(friends(Random.nextInt(friends.length)), "page")

          case 19 =>
            self ! getPhotoFromAlbum(id)

        }

        context.system.scheduler.scheduleOnce(5000.milliseconds, self, startActivity(numActivites - 1))

      }
      else {
        //send a done message to master
        boss ! simDone()
      }

    case sendLike(likeid, kind) =>
      import LikeJsonProtocol._
      import spray.json._

      //val L = Like(id, likeid, kind)
      //val out = L.toJson.prettyPrint
      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/like/" + likeid, tokenID + " " + id))
      response onComplete {
        case Success(r) =>
        //sessionID = response.message.entity.asString.toInt

        case _ =>
          println("didnt get response")
      }

    case establishSession(clientid) =>
      println("establishing session")
      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/establishSession/" + clientid))
      response onComplete {
        case Success(r) =>
          //sessionID = r.message.entity.asString.toInt
          val e = PublicKeyEncrypt
          val signedkeybyclient = e.sign(r.message.entity.asString, DSAprivate_key)
          import SignedclientJsonProtocol._
          import spray.json._

          val S = Signedclient(id, signedkeybyclient)
          val out = S.toJson.prettyPrint
          val response2: Future[HttpResponse] = pipeline(Post("http://localhost:8081/signedkey/" + clientid, out))
          response2 onComplete {
            case Success(verif) =>
              val R = Future {
                val Rtoken = verif.message.entity.asString
                println("token " + id + " " + Rtoken)
                tokenID = Rtoken

                var gotToken = false
                if (Rtoken != "") {
                  gotToken = true

                }

                if (gotToken) {
                  println("client got a token")
                  //self ! startActivity(numTasks)
                }
                else
                  println("client not authenticated-- alert --there's an intruder")
              }
              R onComplete {
                case Success(m) =>
                  val a = Future{
                    self ! sendPage(id)
                  }
                  a onComplete {
                   case Success(m)=>
                      //self ! getPage(id)

                   case _ =>
                      println("page not sent")
                  }

                case Failure(m) =>
                  println("bad stuff")
              }


            case _ =>
              println("didnt get response from server for authentication")

          }
        case _ =>
          println("dsa program didnt get response")
      }

    case getFriendList(friendListID) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/friendlist/" + friendListID))
      import FriendListJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(r) =>

          val friendJson = r.message.entity.asString
          val friendlistObj = friendJson.parseJson.convertTo[FriendList]
          // use friendlistObj for friendlist attributes
          println(friendJson)

        case _ =>
          println("didnt get response for get friendlist")
      }

    case getPage(pageid) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/page/" + pageid, tokenID))
      import PageJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(r) =>

          val pageJson = r.message.entity.asString.split(" ")
          //val pageObj = pageJson.parseJson.convertTo[Page]
          val aes = Encrypt
          val rsa = PublicKeyEncrypt
          val aes_key = rsa.decrypt(pageJson(1), RSAprivate_key)
          val page = aes.AESDecrypt(pageJson(0), aes_key)

          println(page)

        case _ =>
          println("didnt get response for get page")
      }

    case getPhoto(photoid) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/photo/" + photoid))
      import PhotoJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(r) =>
          val photoJson = r.message.entity.asString
          val photoObj = photoJson.parseJson.convertTo[Photo]

          println(photoJson)

        case _ =>
          println("didnt get response for get photo")
      }

    case getPostFromPage(pageid) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/page/" + pageid))
      import PageJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(r) =>

          val pageJson = r.message.entity.asString
          val pageObj = pageJson.parseJson.convertTo[Page]

          try {
            self ! getPost(pageObj.post_list(Random.nextInt(pageObj.post_list.length)))
          } catch {
            case e: IllegalArgumentException => println("The page for the client does not have any posts")
              sender ! HttpResponse(entity = "client does not have any posts")
          }

        case _ =>
          println("didnt get response for get post from page")
      }

    case getPhotoFromPage(pageid) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/page/" + pageid))
      import PageJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(r) =>
          val pageJson = r.message.entity.asString
          val pageObj = pageJson.parseJson.convertTo[Page]

          try {
            self ! getPhoto(pageObj.photo_list(Random.nextInt(pageObj.photo_list.length)))
          } catch {
            case e: IllegalArgumentException => println("The page for the client does not have any photos")
              sender ! HttpResponse(entity = "client does not have any photos")
          }

        case _ =>
          println("didnt get response for get photo from page")
      }

    case getPhotoFromAlbum(albumid) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/album/" + albumid))
      import AlbumJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(r) =>
          val albumJson = r.message.entity.asString
          val albumObj = albumJson.parseJson.convertTo[Album]

          try {
            self ! getPhoto(albumObj.photoList(Random.nextInt(albumObj.photoList.length)))
          } catch {
            case e: IllegalArgumentException => println("The album for the client does not have any photos")
              sender ! HttpResponse(entity = "client album does not have any photos")
          }

        case _ =>
          println("didnt get response for get photo from page")
      }

    //case getPublicKey(kid) =>


    case sendPost(postid) =>
      import PostJsonProtocol._
      import spray.json._

      val P = PostMessage(id, RandomValue(1, 5), id, postid, RandomValue(6, 1), "status", Array[Int](), Array[String]())
      val out = P.toJson.prettyPrint
      var friendPublicKey = ""

      val response1: Future[HttpResponse] = pipeline(Get("http://localhost:8081/getpublickey/" + postid))

      response1 onComplete {
        case Success(r) =>
          friendPublicKey = r.message.entity.asString
          //println("frind public key: " + friendPublicKey)
          val send = rsaJsonEncrypt(tokenID, out, friendPublicKey)

          val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/post/" + postid, id + " " + send))

          response onComplete {
            case Success(r) =>

              println("client got post reply ")

            case _ =>
              println("didnt get response for send post")
          }
        case Failure(r) =>
          println("didnt get friends public key")
      }
//      println("frind public key: " + friendPublicKey)
//      val send = rsaJsonEncrypt(tokenID, out, friendPublicKey)
//
//      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/post/" + postid, id + " " + send))
//
//      response onComplete {
//        case Success(r) =>
//
//          println("client got post reply ")
//
//        case _ =>
//          println("didnt get response for send post")
//      }

    case sendFriendList(fid) =>
      import FriendListJsonProtocol._
      import spray.json._

      val F = FriendList(id, friends, RandomValue(5, 1), fid)
      val out = F.toJson.prettyPrint

      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/friendlist/" + fid, out))

      response onComplete {
        case Success(r) =>
          println("client got friendlist reply")

        case Failure(r) =>
          println("didnt get response for send friendlist")
      }

    case sendPublicKeys(dsa, rsa) =>
      import PublicKeysJsonProtocol._
      import spray.json._

      val P = PublicKeys(DSApublic_key, RSApublic_key)
      val out = P.toJson.prettyPrint

      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/publickeys/" + id, out))

      response onComplete {
        case Success(r) =>
          self ! establishSession(id)
          println("client got public keys reply")

        case Failure(r) =>
          println("didnt get response for send public keys")
      }

    case sendPage(pid) =>
      import PageJsonProtocol._
      import spray.json._

      val F = Page(pid, RandomValue(1, 2), RandomValue(10, 1), RandomValue(12, 1), "www.facebook.com/page/" + pid, RandomValue(1, 3), RandomValue(10, 1), RandomValue(10, 1), RandomValue(1, 4), 0, Array[Int](), Array[Int]())
      val out = F.toJson.prettyPrint

      val send = rsaJsonEncrypt(tokenID, out, RSApublic_key)
      //println("tokenid from page" + tokenID)

      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/page/" + pid, send))

      response onComplete {
        case Success(r) =>
          //self ! getPage(id)
          boss ! done()
          println("client got page reply")

        case Failure(r) =>
          println("didnt get response send page")
      }

    case sendPhoto(photoid) =>
      import PhotoJsonProtocol._
      import spray.json._

      val Ph = Photo(photoid, Random.nextInt(10), "today", 400, 500, "now", "www.facebook.com/photo" + photoid, id, photoid)
      val out = Ph.toJson.prettyPrint
      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/photo/" + photoid, out))

      response onComplete {
        case Success(r) =>
          println("client got photo reply")

        case _ =>
          println("didnt get response for send photo")
      }

    case getPost(pid) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/post/" + pid, tokenID))
      import PostJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(r) =>
          val postJson = r.message.entity.asString
          println("here" + postJson)
          if (postJson != "no posts"){
            val postfinal = postJson.split(" ")
            try{
              val aes = Encrypt
              val rsa = PublicKeyEncrypt
              val aes_key = rsa.decrypt(postfinal(1), RSAprivate_key)
              val post = aes.AESDecrypt(postfinal(0), aes_key)
              println("this: " + post)
            }
            catch {
              case e: Exception =>
                //did not decrypt
            }


          }



        case _ =>
          println("didnt get response for get post")
      }

    case getAlbum(albumid) =>
      val response: Future[HttpResponse] = pipeline(Get("http://localhost:8081/album/" + albumid))
      import AlbumJsonProtocol._
      import spray.json._

      response onComplete {
        case Success(r) =>
          val albumJson = r.message.entity.asString
          val albumObj = albumJson.parseJson.convertTo[Album]
          println(albumJson)

        case _ =>
          println("didnt get response for get album")
      }

    case sendAlbum(albumid) =>
      import AlbumJsonProtocol._
      import spray.json._

      val Alb = Album(albumid, true, 0, "facebook.com/album/" + albumid, id, id, RandomValue(5, 1), Array[Int](), Array[Int](), Array[String](), "now", "now")
      val out = Alb.toJson.prettyPrint
      val response: Future[HttpResponse] = pipeline(Post("http://localhost:8081/album/" + albumid, out))

      response onComplete {
        case Success(r) =>
          println("client got album reply")

        case Failure(reponse) =>
          println("didnt get response for send album" + reponse)
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



