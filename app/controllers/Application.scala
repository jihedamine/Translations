package controllers

import play.api.mvc._
import play.api.data._
import play.api.data.Forms._

import org.bson.types.ObjectId

import play.api.Play.current
import se.radley.plugin.salat._
import se.radley.plugin.salat.Formats._
import com.novus.salat._

import views._
import models._

/**
 * Manage a database of computers
 */
object Application extends Controller { 

  /**
   * This result directly redirect to the application home.
   */
  val Home = Redirect(routes.Application.list())
  
  val translationForm = Form(
    mapping(
    "english" -> nonEmptyText,
    "french" -> Forms.list(text)
    ) ((english, french) => Translation(new ObjectId, english, french))
      ((translation: Translation) => Some(translation.english, translation.french))
  )
  
  // -- Actions

  /**
   * Handle default path requests, redirect to computers list
   */  
  def index = Action { Home }
  
  /**
   * Display the list of words
   *
   */
  def list() = Action { implicit request =>
    Ok(html.list(Translation.all())) }
  
  /**
   * Display the 'edit form' of a existing translation.
   *
   * @param id Id of the translation to edit
   */
  def edit(id: String) = Action {
      val translation: Translation = Translation.find(id)
      val f = translationForm.fill(translation)
      Ok(html.editForm(id, f))
  }
  
  /**
   * Handle the 'edit form' submission 
   *
   * @param id Id of the translation to edit
   */
  def update(id: String) = Action { implicit request =>
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
  def save = Action { implicit request =>
    translationForm.bindFromRequest.fold(
      errors => BadRequest(html.createForm(errors)),
      translation => {
        Translation.insert(translation)
        Home.flashing("success" -> "Translation %s has been created".format(translation.english + ": " + translation.french.mkString(" - ")))
      }
    )
  }
  
  /**
   * Handle computer deletion.
   */
  def delete(id: String) = Action {
    Translation.remove(id)
    Home.flashing("success" -> "Translation has been deleted")
  }

}
