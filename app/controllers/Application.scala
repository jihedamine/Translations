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
import scala.util.Random

/**
 * Manage a database of translations
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
    )((english, french) => Translation(new ObjectId, english, french))
      ((translation: Translation) => Some(translation.english, translation.french))
  )

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
  def list() = Action {
    implicit request =>
      Ok(html.list(Translation.all()))
  }

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

  def quizz = Action {
    import scala.collection.mutable.ListBuffer, scala.util.Random

    val translations = Translation.all()

    val quizzTranslation = pickRandomTranslation(translations)

    val wrongTranslations = translations.filterNot(_._id == quizzTranslation._id)

    val correctAnswer = pickRandomAnswer(quizzTranslation.french)

    var propositions = new ListBuffer[String]()
    propositions.append(correctAnswer)

    for(_ <- 1 to 3) {
      val wrongTranslation = pickRandomTranslation(wrongTranslations)
      val wrongAnswer = pickRandomAnswer(wrongTranslation.french.filterNot(propositions.contains(_)))
      if (! wrongAnswer.eq("")) {propositions.append(wrongAnswer)}
    }

    println(propositions.toList)
    Ok(html.quizz(quizzTranslation.english, correctAnswer, Random.shuffle(propositions.toList)))
  }

  def pickRandomTranslation(translations: List[Translation]): Translation = {
    translations(new Random().nextInt(translations.size))
  }

  def pickRandomAnswer(french: List[String]) : String = {
    if (french.size > 0) return french(new Random().nextInt(french.size))
    else return ""
  }
}
