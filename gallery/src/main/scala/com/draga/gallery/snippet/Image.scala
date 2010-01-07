package com.draga.gallery.snippet

import _root_.net.liftweb.mapper._
import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.js.JsCmds._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import S._
import _root_.scala.xml._

import com.draga.gallery.model._

// Following code adapted from image upload example courtesy of David Pollak, at
//   http://github.com/dpp/imagine/

class Image extends StatefulSnippet {
  // for image uploads (only!)
  object category extends RequestVar[String]("")

  def dispatch : DispatchIt = {
    case "categories" => doCategories _
    case "upload" => doUpload _
    case "showAll" => doShowAll _
    case "showOne" => doShowOne _
    case "editCategories" => doEditCategories _
    case "newCategory" => doNewCategory _
  }

  private def saveFile(fp: FileParamHolder): Unit = {
    fp.file match {
      case null =>
	S.error("No file selected")
      case x if x.length == 0 =>
	S.error("Empty file uploaded")
      case x =>
        val blob = ImageBlob.create.image(x).saveMe
	ImageInfo.create
	  .name(fp.fileName)
	  .mimeType(fp.mimeType)
	  .category(ImageCategory.byName(category))
	  .blob(blob)
	  .saveMe
	S.notice("Thanks for the upload")
    }
  }

  private def deleteImage(i: ImageInfo): Unit = {
    i.deleteWithBlob
    S.notice("Image deleted")
    S.redirectTo("/gallery/images/")
  }

  private def deleteCategory(i: ImageCategory): Unit = {
    i.deleteWithImages
    S.notice("Category deleted")
  }

  private def createCategory(s: String): Unit = {
    ImageCategory.create.name(s).saveMe
    S.notice("Category created")
  }

  def doCategories(in: NodeSeq): NodeSeq = {
    bind("c", in, 
	 "categories" ->
	   SHtml.select(ImageCategory.choices, S.param("category"),
			{c : String => S.redirectTo("/gallery/images/" + c)})
       )
  }

  def doEditCategories(in: NodeSeq): NodeSeq =
    ImageCategory.findAll.flatMap({i =>
      bind("ec", in, 
	   "name" -> i.name.toForm,
	   // XXX XXX need to validate (see below)
	   "save" -> SHtml.submit("Save Changes", {() => i.saveMe}),
	   "delete" -> SHtml.link("/gallery/categories",
				  {() => i.deleteWithImages},
				  Text("delete"))
	 )})

  def doNewCategory(in: NodeSeq): NodeSeq =
    bind("nc", in,
	 // XXX XXX need to validate (at least no /)
	 "name" -> SHtml.text("name", createCategory _),
	 "create" -> SHtml.submit("Create", {() => })
       )
 
  def doUpload(in: NodeSeq): NodeSeq = {
    val ch = ImageCategory.choices
    bind("upload", in, 
	 "category" -> SHtml.select(ch, Full(category.toString),
				    {c : String => category(c)}),
	 "file" -> SHtml.fileUpload(saveFile _))
	// XXX submit, to validate
  }

  def doShowAll(in: NodeSeq): NodeSeq = {
    def doBinding(ns: NodeSeq, i: ImageInfo): NodeSeq =
      bind(
	"showAll", ns, 
	"name" -> i.name.is,
	"link" ->
	  // {n: NodeSeq => SHtml.link("/gallery/image/" + i.id.is,
	  // 			    {() => }, doBinding(n, i))},
	  {n: NodeSeq => <a href={"/gallery/image/" + i.id.is}>{doBinding(n, i)}</a>},
	// right now, we can only view one category, so redundant, but keep for now...
	"category" -> 
	  i.category.obj.map(_.name.toString).openOr(""),
	AttrBindParam("imgUrl", i.url, "src")
      )

    // XXX XXX not quite right.  We jump right to default cat if not category
    // XXX XXX is specified, but if cat is specified and missing, an error is better (or
    // XXX XXX better yet, can we not partial match, and let it fall through? In Boot?)...
    val catName = S.param("category").openOr("")
    ImageInfo.findAll(
      By(ImageInfo.category, ImageCategory.byName(catName)),
      OrderBy(ImageInfo.name, Ascending)
    ).flatMap({i => doBinding(in, i)})
  }

  def doShowOne(in: NodeSeq): NodeSeq =
    S.param("image").flatMap(asLong) match {
      case Full(selected) =>
	ImageInfo.findByKey(selected) match {
	  case Full(i) =>
	    bind("showOne", in,
		 // "name" -> SHtml.ajaxText(i.name, {s => Noop},)
		 "name" -> i.name.toForm,
		 // "category" -> i.category.obj.map(_.name.toString).openOr("-"),
		 "category" -> SHtml.select(ImageCategory.choices, 
					    i.category.obj.map(_.name.toString), 
					    {s => i.category(ImageCategory.byName(s))}),
		 "submit" -> SHtml.submit("Save Changes", {() => i.saveMe}), // XXX validate?  S.notice?
		 "delete" -> SHtml.link("/gallery/images/", {() => deleteImage(i)}, Text("delete")),
		 AttrBindParam("imgUrl", i.url, "src")
	       )
	  case _ => 
	    S.error("No such image!")
            Text("No such image!")
	}
      case _ => 
	S.redirectTo("/gallery/images")
    }
}
