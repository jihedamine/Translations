package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import views._
import models._
import collection.immutable.List
import util.Random
import com.mongodb.casbah.Imports._
import io.Source

/**
 * Manage a database of translations
 */
object Application extends Controller {


  /**
   * This result directly redirect to the application home.
   */
  val Home = Redirect(routes.Application.list())

  // -- Actions

  /**
   * Handle default path requests
   */
  def index = Action {
    Home
  }

  /**
   * Display the list of translations
   *
   */
  def list = Action {
    implicit request => Ok(html.list(Translation.all()))
  }

  val translationForm = Form(
    mapping(
      "english" -> nonEmptyText,
      "french" -> Forms.list(text)
    )((english, french) => Translation(new ObjectId, english, french))
      ((translation: Translation) => Some(translation.english, translation.french))
  )

  /**
   * Display the 'edit form' of an existing translation.
   *
   * @param id Id of the translation to edit
   */
  def edit(id: String) = Action {
    Translation.find(id) match {
      case Some(tr) => Ok(html.editForm(id, translationForm.fill(tr)))
      case _ => NotFound
    }
  }

  /**
   * Handle the 'edit form' submission 
   *
   * @param id Id of the translation to edit
   */
  def update(id: String) = Action {
    implicit request =>
      translationForm.bindFromRequest.fold(
        errors => BadRequest(html.editForm(id, errors)),
        translation => {
          Translation.update(id, translation)
          Home.flashing("success" -> "Translation has been updated to %s".format(translation.english + ": " + translation.french.mkString(" - ")))
        }
      )
  }

  /**
   * Display the 'new translation form'.
   */
  def create = Action {
    Ok(html.createForm(translationForm))
  }

  /**
   * Handle the 'new translation form' submission.
   */
  def save = Action {
    implicit request =>
      translationForm.bindFromRequest.fold(
        errors => BadRequest(html.createForm(errors)),
        translation => {
          Translation.insert(translation)
          Home.flashing("success" -> "Translation %s has been created".format(translation.english + ": " + translation.french.mkString(" - ")))
        }
      )
  }

  /**
   * Handle translation deletion.
   */
  def delete(id: String) = Action {
    Translation.remove(id)
    Home.flashing("success" -> "Translation has been deleted")
  }

  /**
   * Display the 'import translations file' page
   */
  def importTranslations = Action {
    implicit request => Ok(html.importTranslations())
  }

  /**
   * Handle Translations file upload
   */
  def upload = Action(parse.multipartFormData) {
    request =>
      request.body.file("translationsFile").map {
        translationsFile =>
          translationsFile.ref.moveTo(new java.io.File("/tmp/translationsFile"), true)
          val count = insertTranslations(Source.fromFile("/tmp/translationsFile"))
          Home.flashing("success" -> "%d translations inserted".format(count))
      }.getOrElse {
        Home.flashing("error" -> "Missing file")
      }
  }

  def insertTranslations(src: Source): Int = {
    src.getLines().map(_.split("=")
    ).filter(
      tr => (tr.size == 2) && (!Translation.exists("english", tr(0)))
    ).map(
      tr => Translation.insert(Translation(new ObjectId, tr(0), tr(1).split(",").toList))
    ).size
  }

  /**
   * Create a quizz question with propositions
   */
  def quizz = Action {
    Quizz.createQuestion match {
      case Some(tuple) => Ok(html.quizz(tuple._1, tuple._2, tuple._3))
      case _ => NotFound
    }
  }

}
