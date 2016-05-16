import akka.actor.{Actor, _}
import akka.io.IO
import spray.can.Http
import spray.http.HttpEntity.apply
import spray.http.HttpMethods.{GET, POST}
import spray.http.{HttpRequest, _}
import spray.json.DefaultJsonProtocol


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


class ServerActor extends Actor {
  var sessionCount: Int = 0
  var postCount: Int = 0
  var photoCount: Int = 0

  var sessionsMap = scala.collection.mutable.Map[Int, Int]()
  var friendListMap = scala.collection.mutable.Map[Int, (Array[Int], String, Int)]()
  var pageMap = scala.collection.mutable.Map[Int, (String, String, String, String, String, String, String, String, Int, Array[Int], Array[Int])]()

  var postMap = scala.collection.mutable.Map[Int, (String, Int, Int, String, String, Array[Int], Array[String])]()

  var photoMap = scala.collection.mutable.Map[Int, (Int, String, Int, Int, String, String, Int, Int)]()
  var albumMap = scala.collection.mutable.Map[Int, (Boolean, Int, String, Int, Int, String, Array[Int], Array[Int], Array[String], String, String)]()

//  //add a value to friends list for testing
//  var dummyList: Array[Int] = Array(1, 2, 3, 4)
//  var listName = "list name"
//  var owner = 100
//  var f = (dummyList, listName, owner)
//  friendListMap += (owner -> f)
//
//  //dummy value for page
//  var pg = (listName + 1, listName + 2, listName + 3, listName + 4, listName + 5, listName + 6, listName + 7, listName + 8, owner, dummyList, Array[Int]())
//  pageMap += (owner -> pg)
//
//  //dummy value for post
//  var dummyComments: Array[String] = Array("hello", "world")
//  var pst = (listName + 1, 4, 6, listName + 2, listName + 3, dummyList, dummyComments) //pj
//  postMap += (owner -> pst)
//
//  //dummy value for photo
//  var ph = (4, listName + 1, 90, 20, listName + 2, listName + 3, 3, 4) //pj
//  photoMap += (owner -> ph)
//
//  //dummy value for album
//  var dummyPhotoList: Array[Int] = Array(100, 101, 102)
//  var dummyalbumComments: Array[String] = Array("good", "good job")
//  var alb = (true, dummyPhotoList.size, listName + 1, 100, 100, listName + 2, dummyPhotoList, dummyPhotoList, dummyalbumComments, "createdtime", "updatetime") //pj
//  albumMap += (owner -> alb)


  def areFriends(requester: Int, target: Int): Boolean = { //check if two clients are friends

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
        return false

    }


  }

  def addLikeToPage(PageNum: Int) = {
    val page = pageMap(PageNum)
    var like = page._9
    like += 1
    val p = (page._1, page._2, page._3, page._4, page._5, page._6, page._7, page._8, like, page._10, page._11)
    pageMap.remove(PageNum)
    pageMap += (PageNum -> p)
  }

  def addPostToPage(PageNum: Int, PostNum: Int) = {
    val page = pageMap(PageNum)
    val postlist = page._10.toBuffer
    postlist += PostNum
    val p = (page._1, page._2, page._3, page._4, page._5, page._6, page._7, page._8, page._9, postlist.toArray, page._11)
    pageMap.remove(PageNum)
    pageMap += (PageNum -> p)
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

  def receive = {
    case _: Http.Connected => sender ! Http.Register(self)

    case bootServer() =>
      println("hello world")

    case HttpRequest(GET, Uri.Path("/ping"), _, _, _) =>
      println("server got request")
      sender() ! HttpResponse(entity = "PONG")

    case HttpRequest(POST, Uri.Path(path), _, _, _) if path startsWith "/establishSession" =>
      //session establishment
      val client = path.split("/").last.toInt
      println("sever got session establishment request from client: " + client)
      sessionsMap += (client -> sessionCount)
      val body = HttpEntity(ContentTypes.`application/json`, sessionCount.toString)
      sender() ! HttpResponse(entity = body)

      sessionCount += 1 //increment session number for next connection



    case HttpRequest(POST, Uri.Path(path), _, entity, _) if path startsWith "/like" =>
      import LikeJsonProtocol._
      import spray.json._

      val likeJson = entity.asString
      val likeObj = likeJson.parseJson.convertTo[Like]
      if (areFriends(likeObj.from, likeObj.to)) {
        if (likeObj.kind == "page") {
          println("liked page: " + likeObj.to)
          addLikeToPage(likeObj.to)
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

    case HttpRequest(GET, Uri.Path(path), _, _, _) if path startsWith "/page" =>
      //get page
      import PageJsonProtocol._
      import spray.json._
      val pageID = path.split("/").last.toInt
      println("sever got page request for: " + pageID)
      val pageTuple = pageMap(pageID)
      val output = Page(pageID, pageTuple._1, pageTuple._2, pageTuple._3, pageTuple._4, pageTuple._5, pageTuple._6, pageTuple._7, pageTuple._8, pageTuple._9, pageTuple._10, pageTuple._11)
      val body = HttpEntity(ContentTypes.`application/json`, output.toJson.prettyPrint)
      sender() ! HttpResponse(entity = body)

    case HttpRequest(POST, Uri.Path(path), _, entity, _) if path startsWith "/post" =>
      import PostJsonProtocol._
      import spray.json._
      val postJson = entity.asString
      val PostObj = postJson.parseJson.convertTo[PostMessage]
      if (areFriends(PostObj.from, PostObj.to)) {
        //set the post and add to page
        //send the sender a success message
        postMap += (postCount ->(PostObj.create_time, PostObj.from, PostObj.to, PostObj.message, PostObj.post_type, PostObj.likes, PostObj.comments))
        addPostToPage(PostObj.to, postCount)

        postCount += 1

        //println("here: " + pageMap(PostObj.to)._10.mkString(" "))


        sender ! HttpResponse(entity = "success")
      }
      else {
        //send the sender a not authorized message
        sender ! HttpResponse(entity = "not authorized")
      }
      //println(PostObj.message) //do stuff to add this post to data structures
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
      val PJson = entity.asString
      val PObj = PJson.parseJson.convertTo[Page]
      val t = (PObj.birthday, PObj.contact_address, PObj.description, PObj.link, PObj.name, PObj.personal_info, PObj.personal_interests, PObj.phone, PObj.likes, PObj.post_list, PObj.photo_list) //pj
      pageMap += (PObj.id -> t)
      //println(PJson)
      //println("got Page")
      sender() ! HttpResponse(entity = "OK")


    case HttpRequest(POST, Uri.Path(path), _, entity, _) if path startsWith "/photo" =>
      import PhotoJsonProtocol._
      import spray.json._
      val photoJson = entity.asString
      val PhotoObj = photoJson.parseJson.convertTo[Photo]
      if (areFriends(PhotoObj.from, PhotoObj.to)) {
        //println(photoJson) //do stuff to add this post to data structures
        photoMap += (photoCount ->(PhotoObj.album_id, PhotoObj.created_time, PhotoObj.height, PhotoObj.width, PhotoObj.update_time, PhotoObj.link, PhotoObj.from, PhotoObj.to))
        addPhotoToPage(PhotoObj.to, photoCount)
        addPhotoToAlbum(PhotoObj.from, photoCount)
        photoCount += 1
        println("got photo")
        //println("here: " + pageMap(PhotoObj.to)._11.mkString(" "))
        //println("here: " + albumMap(PhotoObj.from)._7.mkString(" "))
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
          //println("Photo not found")
          var body = "Photo not found"
          sender() ! HttpResponse(entity = body)
      }


    case HttpRequest(GET, Uri.Path(path), _, _, _) if path startsWith "/post" =>
      import PostJsonProtocol._
      import spray.json._

      val postID = path.split("/").last.toInt
      println("sever got post request for: " + postID)
      try {
        val postTuple = postMap(postID)
        val output = PostMessage(postID, postTuple._1, postTuple._2, postTuple._3, postTuple._4, postTuple._5, postTuple._6, postTuple._7)
        println("output:" + output)
        val body = HttpEntity(ContentTypes.`application/json`, output.toJson.prettyPrint)
        sender() ! HttpResponse(entity = body)
      }

      catch {
        case e: NoSuchElementException =>
          //println("Photo not found")
          var body = "post not found"
          sender() ! HttpResponse(entity = body)
      }


    case HttpRequest(POST, Uri.Path(path), _, entity, _) if path startsWith "/album" =>
      import AlbumJsonProtocol._
      import spray.json._
      val albumJson = entity.asString
      val AlbumObj = albumJson.parseJson.convertTo[Album]
      val aTuple = (AlbumObj.can_upload, AlbumObj.count, AlbumObj.link, AlbumObj.from, AlbumObj.to, AlbumObj.name, AlbumObj.photoList, AlbumObj.likes, AlbumObj.comments, AlbumObj.created_time, AlbumObj.update_time)
      albumMap += (AlbumObj.id -> aTuple)

      println("got album")
      println(albumJson) //do stuff to add this post to data structures

      sender() ! HttpResponse(entity = "ok")

    case HttpRequest(GET, Uri.Path(path), _, entity, _) if path startsWith "/album" =>
      //get photo information from the server
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

    //handler ! bootServer()
  }

}