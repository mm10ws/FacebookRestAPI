import java.security.SecureRandom
import java.security.interfaces.DSAPublicKey
import java.util.Base64

import akka.actor.{Actor, _}
import akka.io.IO
import spray.can.Http
import spray.http.HttpEntity.apply
import spray.http.HttpMethods.{GET, POST}
import spray.http._
import spray.json.DefaultJsonProtocol

import scala.collection.mutable.ArrayBuffer
import scala.util.Random

sealed trait ServerMessage

case class bootServer() extends ServerMessage


case class FriendList(id: Int, list: Array[Int], listName: String, owner: Int)

object FriendListJsonProtocol extends DefaultJsonProtocol {
  implicit val FriendListFormat = jsonFormat4(FriendList.apply)
}

case class Page(id: Int, birthday: String, contact_address: String, description: String, link: String, name: String, personal_info: String, personal_interests: String, phone: String, likes: Int, post_list: Array[Int], photo_list: Array[Int])

object PageJsonProtocol extends DefaultJsonProtocol {
  implicit val PageFormat = jsonFormat12(Page.apply)
}


case class PostMessage(id: Int, create_time: String, from: Int, to: Int, message: String, post_type: String, likes: Array[Int], comments: Array[String])

object PostJsonProtocol extends DefaultJsonProtocol {
  implicit val PostFormat = jsonFormat8(PostMessage.apply)
}

case class Photo(id: Int, album_id: Int, created_time: String, height: Int, width: Int, update_time: String, link: String, from: Int, to: Int)

object PhotoJsonProtocol extends DefaultJsonProtocol {
  implicit val PhotoFormat = jsonFormat9(Photo.apply)
}

case class Album(id: Int, can_upload: Boolean, count: Int, link: String, from: Int, to: Int, name: String, photoList: Array[Int], likes: Array[Int], comments: Array[String], created_time: String, update_time: String)

object AlbumJsonProtocol extends DefaultJsonProtocol {
  implicit val AlbumFormat = jsonFormat12(Album.apply)
}

case class Like(from: Int, to: Int, kind: String)

object LikeJsonProtocol extends DefaultJsonProtocol {
  implicit val LikeFormat = jsonFormat3(Like.apply)
}

case class PublicKeys(dsa: String, rsa: String)

object PublicKeysJsonProtocol extends DefaultJsonProtocol {
  implicit val PublicKeysFormat = jsonFormat2(PublicKeys.apply)
}

case class Signedclient(id: Int, signedkeybyclient: Array[Byte])

object SignedclientJsonProtocol extends DefaultJsonProtocol {
  implicit val SignedclientFormat = jsonFormat2(Signedclient.apply)
}

class ServerActor extends Actor {
  var sessionCount: Int = 0
  var postCount: Int = 0
  var photoCount: Int = 0
  var randomMap = scala.collection.mutable.Map[Int, Int]()
  var tokenMap = scala.collection.mutable.Map[Int, String]()
  var sessionsMap = scala.collection.mutable.Map[Int, Int]()
  var clientPublicKeysMap = scala.collection.mutable.Map[Int, (String, String)]()
  var friendListMap = scala.collection.mutable.Map[Int, (Array[Int], String, Int)]()
  var pageMap = scala.collection.mutable.Map[Int, (String, String, String, String, String, String, String, String, Int, Array[Int], Array[Int])]()


  var pageMapEncrypted = scala.collection.mutable.Map[Int, String]()
  var encryptedPostMap = scala.collection.mutable.Map[Int, String]()
  var pageToPostMap = scala.collection.mutable.Map[Int, ArrayBuffer[Int]]()
  var likeToPageMap = scala.collection.mutable.Map[Int,Int]()


  var postMap = scala.collection.mutable.Map[Int, (String, Int, Int, String, String, Array[Int], Array[String])]()
  var photoMap = scala.collection.mutable.Map[Int, (Int, String, Int, Int, String, String, Int, Int)]()
  var albumMap = scala.collection.mutable.Map[Int, (Boolean, Int, String, Int, Int, String, Array[Int], Array[Int], Array[String], String, String)]()


  def areFriends(requester: Int, target: Int): Boolean = {
    //check if two clients are friends
    try {
      if (friendListMap(target)._1.contains(requester) || requester == target) {
        true
      }
      else {
        false
      }
    }
    catch {
      case _: Exception =>
        false
    }
  }

  def verifyToken(cid: Int, token: String): Boolean = {
    try {
      if (tokenMap(cid) == token) {
        true
      }
      else {
        false
      }
    }
    catch {
      case e: Exception =>
        println("there was an excpection" + e)
        false
    }
  }

  def addLikeToPage(PageNum: Int) = {
    var likes = likeToPageMap(PageNum)
    likes += 1
    likeToPageMap.remove(PageNum)
    likeToPageMap += (PageNum -> likes)
  }

  def addPostToPage(PageNum: Int, PostNum: Int) = {

    var posts = pageToPostMap(PageNum)
    posts += PostNum
    pageToPostMap.remove(PageNum)
    pageToPostMap += (PageNum -> posts)
//    val page = pageMap(PageNum)
//    val postlist = page._10.toBuffer
//    postlist += PostNum
//    val p = (page._1, page._2, page._3, page._4, page._5, page._6, page._7, page._8, page._9, postlist.toArray, page._11)
//    pageMap.remove(PageNum)
//    pageMap += (PageNum -> p)
  }

  def addPhotoToPage(PageNum: Int, PhotoNum: Int) = {
    val page = pageMap(PageNum)
    val photolist = page._11.toBuffer
    photolist += PhotoNum
    val q = (page._1, page._2, page._3, page._4, page._5, page._6, page._7, page._8, page._9, page._10, photolist.toArray)
    pageMap.remove(PageNum)
    pageMap += (PageNum -> q)
  }

  def addPhotoToAlbum(AlbumNum: Int, PhotoNum: Int) = {
    val album = albumMap(AlbumNum)
    val photolist = album._7.toBuffer
    photolist += PhotoNum
    val a = (album._1, album._2, album._3, album._4, album._5, album._6, photolist.toArray, album._8, album._9, album._10, album._11)
    albumMap.remove(AlbumNum)
    albumMap += (AlbumNum -> a)
  }

  def removeTokenFromMessage(message: String): Array[String] = {
    val a = message.split(" ")
    a
  }

  def receive = {
    case _: Http.Connected => sender ! Http.Register(self)

    case bootServer() =>
      println("hello world")

    case HttpRequest(GET, Uri.Path(path), _, _, _) if path startsWith "/getpublickey" =>
      println("server got request for getpublickey")
      val client = path.split("/").last.toInt
      val returnkey = clientPublicKeysMap(client)
      val body = HttpEntity(ContentTypes.`application/json`, returnkey._2)
      sender() ! HttpResponse(entity = body)

    //pj written method
    case HttpRequest(POST, Uri.Path(path), _, _, _) if path startsWith "/establishSession" =>
      //session establishment
      val client = path.split("/").last.toInt
      println("server got session establishment request from client: " + client)

      val r = Random.nextInt
      //random no stored in server's map for client
      randomMap += (client -> r)
      //println("Updated RandomMap"+randomMap)
      val body = HttpEntity(ContentTypes.`application/json`, r.toString) //pj
      sender() ! HttpResponse(entity = body)
    //sessionCount += 1 //increment session number for next connection //pj

    case HttpRequest(POST, Uri.Path(path), _, entity, _) if path startsWith "/signedkey" =>
      //session establishment
      import SignedclientJsonProtocol._
      import spray.json._
      val client = path.split("/").last.toInt
      val signedkeyJson = entity.asString
      val signedkeyJsonObj = signedkeyJson.parseJson.convertTo[Signedclient]
      val receivedsignedkey = signedkeyJsonObj.signedkeybyclient
      println("server received signed key from client" + client + ". The received key is :" + receivedsignedkey.toString)
      val e = PublicKeyEncrypt

      // println("random map saved on the server"+randomMap)
      //println("dsa public key on server"+clientPublicKeysMap.get(client).get._1)
      val verified = e.verify(randomMap.get(client).get.toString, clientPublicKeysMap.get(client).get._1, receivedsignedkey)
      //println("verfied  has some value"+verified.toString)
      randomMap.remove(client) //removing the random no entry as verification step is complete

      var token = ""
      if (verified) {
        //generate secure token
        val encoder = Base64.getEncoder
        val rand = new SecureRandom()
        val tokenbytes = new Array[Byte](16)
        rand.nextBytes(tokenbytes)
        token = encoder.encodeToString(tokenbytes)
        tokenMap += (client -> token)

      }

      val body = HttpEntity(ContentTypes.`application/json`, token)
      sender() ! HttpResponse(entity = body)


    case HttpRequest(POST, Uri.Path(path), _, entity, _) if path startsWith "/publickeys" =>
      //getting public keys from the client
      import PublicKeysJsonProtocol._
      import spray.json._

      val client = path.split("/").last.toInt
      val PKJson = entity.asString
      val PKObj = PKJson.parseJson.convertTo[PublicKeys]
      println("server got public keys from client: " + client)
      clientPublicKeysMap += (client ->(PKObj.dsa, PKObj.rsa))
      sender ! HttpResponse(entity = "got public keys")

    case HttpRequest(POST, Uri.Path(path), _, entity, _) if path startsWith "/like" =>
      import LikeJsonProtocol._
      import spray.json._
      val client = path.split("/").last.toInt
      val t = entity.asString.split(" ")
      //val likeObj = likeJson.parseJson.convertTo[Like]
      if(verifyToken(t(1).toInt, t(0))){
        if (areFriends(t(1).toInt, client)) {
          addLikeToPage(client)
          sender ! HttpResponse(entity = "liked page")
        }
      }

      else {
        //send the sender a not authorized message
        sender ! HttpResponse(entity = "not authorized")
      }

    case HttpRequest(GET, Uri.Path(path), _, _, _) if path startsWith "/friendlist" =>
      //get friends list
      import FriendListJsonProtocol._
      import spray.json._

      val friendlistID = path.split("/").last.toInt
      println("sever got friendlist request for: " + friendlistID)
      val listTuple = friendListMap(friendlistID)
      val output = FriendList(friendlistID, listTuple._1, listTuple._2, listTuple._3)
      val body = HttpEntity(ContentTypes.`application/json`, output.toJson.prettyPrint)
      sender() ! HttpResponse(entity = body)

    case HttpRequest(GET, Uri.Path(path), _, t, _) if path startsWith "/page" =>
      //get page
      import PageJsonProtocol._
      import spray.json._

      val p = path.split("/").last.toInt

      println("sever got page request for: " + p)
      //println("pageid(0):"+pageID(0) +"pageid(1):"+pageID(1))
      //val output = Page(pageID, pageTuple._1, pageTuple._2, pageTuple._3, pageTuple._4, pageTuple._5, pageTuple._6, pageTuple._7, pageTuple._8, pageTuple._9, pageTuple._10, pageTuple._11)
      if (verifyToken(p, t.asString)) {

        val pageTuple = pageMapEncrypted(p)
        val body = HttpEntity(ContentTypes.`application/json`, pageTuple)
        sender() ! HttpResponse(entity = body)
      }
      else {
        val body = HttpEntity(ContentTypes.`application/json`, "not authorized")
        sender() ! HttpResponse(entity = body)
      }


    case HttpRequest(POST, Uri.Path(path), _, entity, _) if path startsWith "/post" =>
      import PostJsonProtocol._
      import spray.json._
      val to = path.split("/").last.toInt
      val postJson = entity.asString
      val parts = removeTokenFromMessage(postJson)


      if (verifyToken(parts(0).toInt, parts(1))) {
        if (areFriends(parts(0).toInt, to)) {
          encryptedPostMap += (postCount -> (parts(2) + " " + parts(3)))
          addPostToPage(to, postCount)
          //println("here: " + pageToPostMap(to).mkString(" "))
          postCount += 1
          sender() ! HttpResponse(entity = "OK")
        }
        else {
          //send the sender a not authorized message
          sender ! HttpResponse(entity = "not friend")
        }
      }
      else {
        println("server did not verify")
        sender() ! HttpResponse(entity = "nope")
      }




      //      //println("this is a post " + postJson)
      //      sender ! HttpResponse(entity = "success")
      //      //val PostObj = postJson.parseJson.convertTo[PostMessage]
      //      if (areFriends(PostObj.from, PostObj.to)) {
      //        //set the post and add to page
      //        //send the sender a success message
      //        postMap += (postCount ->(PostObj.create_time, PostObj.from, PostObj.to, PostObj.message, PostObj.post_type, PostObj.likes, PostObj.comments))
      //        addPostToPage(PostObj.to, postCount)
      //        postCount += 1
      //        sender ! HttpResponse(entity = "success")
      //      }
      //      else {
      //        //send the sender a not authorized message
      //        sender ! HttpResponse(entity = "not authorized")
      //      }
      println("got post")

    case HttpRequest(POST, Uri.Path(path), _, entity, _) if path startsWith "/friendlist" =>
      import FriendListJsonProtocol._
      import spray.json._

      val FlistJson = entity.asString
      val FLObj = FlistJson.parseJson.convertTo[FriendList]
      val friendTuple = (FLObj.list, FLObj.listName, FLObj.owner)
      friendListMap += (FLObj.id -> friendTuple)
      println(FlistJson) //do stuff to add this friendlist to data structures
      println("got friendlist")
      sender() ! HttpResponse(entity = "OK")

    case HttpRequest(POST, Uri.Path(path), _, entity, _) if path startsWith "/page" =>
      import PageJsonProtocol._
      import spray.json._
      val client = path.split("/").last.toInt
      val PJson = entity.asString
      val parts = removeTokenFromMessage(PJson)

      if (verifyToken(client, parts(0))) {

        pageToPostMap += (client -> new ArrayBuffer[Int]())
        likeToPageMap += (client -> 0)
        pageMapEncrypted += (client -> (parts(1) + " " + parts(2)))
        println("server got page for" + client + " " + pageMapEncrypted(client))
        sender() ! HttpResponse(entity = "OK")
      }
      else {
        println("server did not verify")
        sender() ! HttpResponse(entity = "nope")
      }

    //val PObj = PJson.parseJson.convertTo[Page]
    //val t = (PObj.birthday, PObj.contact_address, PObj.description, PObj.link, PObj.name, PObj.personal_info, PObj.personal_interests, PObj.phone, PObj.likes, PObj.post_list, PObj.photo_list)


    case HttpRequest(POST, Uri.Path(path), _, entity, _) if path startsWith "/photo" =>
      import PhotoJsonProtocol._
      import spray.json._

      val photoJson = entity.asString
      val PhotoObj = photoJson.parseJson.convertTo[Photo]
      if (areFriends(PhotoObj.from, PhotoObj.to)) {
        photoMap += (photoCount ->(PhotoObj.album_id, PhotoObj.created_time, PhotoObj.height, PhotoObj.width, PhotoObj.update_time, PhotoObj.link, PhotoObj.from, PhotoObj.to))
        addPhotoToPage(PhotoObj.to, photoCount)
        addPhotoToAlbum(PhotoObj.from, photoCount)
        photoCount += 1
        println("got photo")
        sender ! HttpResponse(entity = "success")
      }
      else {
        //send the sender a not authorized message
        sender ! HttpResponse(entity = "not authorized")
      }

    case HttpRequest(GET, Uri.Path(path), _, entity, _) if path startsWith "/photo" =>
      //get photo information from the server
      import PhotoJsonProtocol._
      import spray.json._

      val photoID = path.split("/").last.toInt
      println("server got photo request for: " + photoID)

      try {
        val photoTuple = photoMap(photoID)
        val output = Photo(photoID, photoTuple._1, photoTuple._2, photoTuple._3, photoTuple._4, photoTuple._5, photoTuple._6, photoTuple._7, photoTuple._8)
        println("output:" + output)
        val body = HttpEntity(ContentTypes.`application/json`, output.toJson.prettyPrint)
        sender() ! HttpResponse(entity = body)
      } catch {
        case e: NoSuchElementException =>
          val body = "Photo not found"
          sender() ! HttpResponse(entity = body)
      }

    case HttpRequest(GET, Uri.Path(path), _, entity, _) if path startsWith "/post" =>
      import PostJsonProtocol._
      import spray.json._

      val tk = entity.asString

      val pID = path.split("/").last.toInt

      if (verifyToken(pID, tk)) {
        val posts = pageToPostMap(pID)
        println("in post greater" + posts.mkString(" "))
        if(posts.nonEmpty){

          val r = Random.nextInt(posts.length)
          val e = encryptedPostMap(r)
          val body = HttpEntity(ContentTypes.`application/json`, e)
          sender() ! HttpResponse(entity = body)
        }
        else{
          val body = HttpEntity(ContentTypes.`application/json`, "no posts")
          sender() ! HttpResponse(entity = body)
        }

      }
      else {
        val body = HttpEntity(ContentTypes.`application/json`, "not authorized")
        sender() ! HttpResponse(entity = body)
      }



      println("sever got post request for: " + pID)

//      try {
//        val postTuple = postMap(postID)
//        val output = PostMessage(postID, postTuple._1, postTuple._2, postTuple._3, postTuple._4, postTuple._5, postTuple._6, postTuple._7)
//        println("output:" + output)
//        val body = HttpEntity(ContentTypes.`application/json`, output.toJson.prettyPrint)
//        sender() ! HttpResponse(entity = body)
//      }
//      catch {
//        case e: NoSuchElementException =>
//          val body = "post not found"
//          sender() ! HttpResponse(entity = body)
//      }

    case HttpRequest(POST, Uri.Path(path), _, entity, _) if path startsWith "/album" =>
      import AlbumJsonProtocol._
      import spray.json._

      val albumJson = entity.asString
      val AlbumObj = albumJson.parseJson.convertTo[Album]
      val aTuple = (AlbumObj.can_upload, AlbumObj.count, AlbumObj.link, AlbumObj.from, AlbumObj.to, AlbumObj.name, AlbumObj.photoList, AlbumObj.likes, AlbumObj.comments, AlbumObj.created_time, AlbumObj.update_time)
      albumMap += (AlbumObj.id -> aTuple)
      println("got album")
      println(albumJson)
      sender() ! HttpResponse(entity = "ok")

    case HttpRequest(GET, Uri.Path(path), _, entity, _) if path startsWith "/album" =>
      //get album information from the server
      import AlbumJsonProtocol._
      import spray.json._

      val albumID = path.split("/").last.toInt
      println("sever got page request for: " + albumID)
      val albumTuple = albumMap(albumID)
      val output = Album(albumID, albumTuple._1, albumTuple._2, albumTuple._3, albumTuple._4, albumTuple._5, albumTuple._6, albumTuple._7, albumTuple._8, albumTuple._9, albumTuple._10, albumTuple._11)
      val body = HttpEntity(ContentTypes.`application/json`, output.toJson.prettyPrint)
      sender() ! HttpResponse(entity = body)


    case _ =>
      println("default case for server")
  }
}

object server {

  def main(args: Array[String]) {
    implicit val system = ActorSystem()
    val handler = system.actorOf(Props(new ServerActor()), name = "handler")
    IO(Http) ! Http.Bind(handler, interface = "127.0.0.1", port = 8081)

  }
}