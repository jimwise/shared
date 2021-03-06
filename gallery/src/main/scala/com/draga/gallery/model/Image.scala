package com.draga.gallery.model
 
import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.util._
import Helpers._
import _root_.scala.xml._

// Following code extended from code courtesy of David Pollak, via
//   http://github.com/dpp/imagine/

class ImageCategory  extends LongKeyedMapper[ImageCategory] with IdPK {
  def getSingleton = ImageCategory
 
  object name extends MappedPoliteString(this, 128) {
    override def dbIndexed_? = true
    override def defaultValue = ""

    private def noSlashes(s: String) : List[FieldError] =
      if (s.contains("/"))
	List(FieldError(this, Text("Category name \"" + s + "\" may not contain \"/\"")))
      else
	Nil

    override def validations =
      valMinLen(1, "Category name must not be empty") _ ::
      valUnique("Category name must be unique") _ ::
      noSlashes _ ::
      super.validations
  }

  def deleteWithImages {
    ImageInfo.findAll(By(ImageInfo.category, this)).map(_.deleteWithBlob)
    this.delete_!
  }
}

object ImageCategory extends ImageCategory with LongKeyedMetaMapper[ImageCategory] {
  def choices = ImageCategory.findAll.map({ i => (i.name.toString, i.name.toString) })
  def default : Long = ImageCategory.find().map(_.id.toLong).openOr(-1) // -1 only if no cats yet
  def byName (s: String) : Box[Long] =
    ImageCategory.find(By(ImageCategory.name, s)).map(_.id.toLong)
  def byId (i: Long) : Box[String] =
    ImageCategory.findByKey(i).map(_.name.is)
}

class ImageBlob extends LongKeyedMapper[ImageBlob] with IdPK {
  def getSingleton = ImageBlob
 
  object image extends MappedBinary(this)
}
 
object ImageBlob extends ImageBlob with LongKeyedMetaMapper[ImageBlob]

class ImageInfo extends LongKeyedMapper[ImageInfo] with IdPK {
  def getSingleton = ImageInfo
 
  object date extends MappedLong(this) {
    override def defaultValue = Helpers.millis
  }
  object mimeType extends MappedPoliteString(this, 64)
  object name extends MappedPoliteString(this, 256) {
    override def dbIndexed_? = true
    override def defaultValue = ""

    private def noSlashes(s: String) : List[FieldError] =
      if (s.contains("/"))
	List(FieldError(this, Text("Image name \"" + s + "\" may not contain \"/\"")))
      else
	Nil

    override def validations =
      valMinLen(1, "Image name must not be empty") _ ::
      valUnique("Image name must be unique") _ ::
      noSlashes _ ::
      super.validations
  }

  object category extends MappedLongForeignKey(this, ImageCategory) {
    override def _toForm = 
      Full(SHtml.select(ImageCategory.choices, 
			ImageCategory.byId(this),
			{f => set(ImageCategory.byName(f).openOr(ImageCategory.default))}))
  }

  object blob extends MappedLongForeignKey(this, ImageBlob)

  def deleteWithBlob {
    this.blob.obj match {
      case Full(x) => x.delete_!
      case _ =>
    }
    this.delete_!
  }

  def url = Text("/gallery/uploaded/" + name)
}
 
object ImageInfo extends ImageInfo with LongKeyedMetaMapper[ImageInfo] {
  private object cache extends RequestMemoize[String, Box[ImageInfo]]
 
  private def findFromRequest(req: Req): Box[ImageInfo] = {
    val toFind = req.path.wholePath.last
    cache.get(toFind, find(By(name, toFind)))
  }
  
  def serveImage: LiftRules.DispatchPF = {
    case req @ Req("gallery" :: "uploaded" :: _ :: Nil, _, GetRequest) if findFromRequest(req).isDefined =>
      () => {
        val info = findFromRequest(req).open_! // open is valid here because we just tested in the guard
 
        // Test for expiration
        req.testFor304(info.date, "Expires" -> toInternetDate(millis + 30.days)) or
        // load the blob and return it
        info.blob.obj.map(blob =>
	    InMemoryResponse(blob.image, List(("Last-Modified", toInternetDate(info.date.is)),
                                              ("Expires", toInternetDate(millis + 30.days)),
                                              ("Content-Type", info.mimeType.is)), Nil, 200))
      }
  }

  def choices = ImageInfo.findAll.map({ i => (i.id.toString, i.name.toString) })
}
