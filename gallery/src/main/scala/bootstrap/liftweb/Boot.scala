package bootstrap.liftweb

import _root_.net.liftweb.util._
import _root_.net.liftweb.common._
import _root_.net.liftweb.http._
import _root_.net.liftweb.http.provider._
import _root_.net.liftweb.sitemap._
import _root_.net.liftweb.sitemap.Loc._
import Helpers._
import _root_.net.liftweb.mapper.{DB, ConnectionManager, Schemifier, DefaultConnectionIdentifier, StandardDBVendor}
import _root_.java.sql.{Connection, DriverManager}
import _root_.com.draga.bsg.model._
import _root_.com.draga.gallery.model._


/**Site
  * A class that's instantiated early and run.  It allows the application
  * to modify lift's environment
  */
class Boot {
  def boot {
    if (!DB.jndiJdbcConnAvailable_?)
      DB.defineConnectionManager(DefaultConnectionIdentifier,
        new StandardDBVendor(Props.get("db.driver") openOr "org.h2.Driver",
			     Props.get("db.url") openOr "jdbc:h2:lift_proto.db",
			     Props.get("db.user"), Props.get("db.password")))

    // where to search snippet
    LiftRules.addToPackages("com.draga.bsg")
    LiftRules.addToPackages("com.draga.gallery")
    Schemifier.schemify(true, Log.infoF _, User,
			ImageCategory, ImageBlob, ImageInfo)

    // Build SiteMap
    val entries =
      Menu(Loc("Home", List("index"), "Home")) ::
      // Menu(Loc("View Mechs", Link(List("viewMechs"), true, "/viewMechs"), "View Mechs")) ::
      // Menu(Loc("Enter Mech", Link(List("enterMech"), true, "/enterMech"), "Enter Mech")) ::
      Menu(Loc("Gallery",  Link(List("gallery"), true, "/gallery/index"), "Gallery"),
	   Menu(Loc("Gallery - View Images", Link(List("gallery", "viewImages"), true, "/gallery/images"), "Gallery - View Images")),
	   Menu(Loc("Gallery - View Image", Link(List("gallery", "viewImage"), true, "/gallery/image"), "Gallery - View Image", Hidden)),
	   Menu(Loc("Gallery - Upload Image", Link(List("gallery", "uploadImage"), true, "/gallery/uploadImage"), "Gallery - Upload Image")),
	   Menu(Loc("Gallery - Manage Categories", Link(List("gallery", "editCategories"), true, "/gallery/categories"), "Gallery - Manage Categories"))) ::
      Nil
      // User.sitemap

    LiftRules.setSiteMap(SiteMap(entries:_*))

    LiftRules.statelessDispatchTable.append(ImageInfo.serveImage)

    /*
     * Image operations
     */
    LiftRules.rewrite.append(
      NamedPF("ImageRewrites") {
	case RewriteRequest(ParsePath("gallery" :: "images" :: category :: Nil, _, _, _), _, _) =>
	  RewriteResponse("gallery" :: "viewImages" :: Nil, Map("category" -> category))
	case RewriteRequest(ParsePath("gallery" :: "images" :: Nil, _, _, _), _, _) =>
	  RewriteResponse("gallery" :: "viewImages" :: Nil)
	case RewriteRequest(ParsePath("gallery" :: "image" :: image :: Nil, _, _, _), _, _) =>
	  RewriteResponse("gallery" :: "viewImage" :: Nil, Map("image" -> image))
	case RewriteRequest(ParsePath("gallery" :: "categories"  :: Nil, _, _, _), _, _) =>
	  RewriteResponse("gallery" :: "editCategories" :: Nil)
      }
    )

    /*
     * Show the spinny image when an Ajax call starts
     */
    LiftRules.ajaxStart =
      Full(() => LiftRules.jsArtifacts.show("ajax-loader").cmd)

    /*
     * Make the spinny image go away when it ends
     */
    LiftRules.ajaxEnd =
      Full(() => LiftRules.jsArtifacts.hide("ajax-loader").cmd)

    LiftRules.early.append(makeUtf8)

    LiftRules.loggedInTest = Full(() => User.loggedIn_?)

    S.addAround(DB.buildLoanWrapper)
  }

  /**
   * Force the request to be UTF-8
   */
  private def makeUtf8(req: HTTPRequest) {
    req.setCharacterEncoding("UTF-8")
  }

}



