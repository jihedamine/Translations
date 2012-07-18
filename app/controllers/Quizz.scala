package controllers

import models.Translation
import util.Random
import collection.mutable.ListBuffer

/**
 * Created with IntelliJ IDEA.
 * User: jihedamine
 * Date: 18/07/12 2:45 AM
 */

object Quizz {

  def createQuestion: Option[(String, String, List[String])] = {
    val translations = Translation.all()
    pickRandomTranslation(translations) match {
      case Some(translation) =>
        pickRandomAnswer(translation.french) match {
          case Some(answer) =>
            val propositions = ListBuffer(answer)
            val wrongTranslations = translations.filterNot(_._id == translation._id)

            while (propositions.size < 4) {
              pickRandomTranslation(wrongTranslations) flatMap {
                wrongTranslation: Translation =>
                  pickRandomAnswer(wrongTranslation.french.filterNot(propositions.contains(_))) map {
                    propositions.append(_)
                  }
              }
            }
            Some(translation.english, propositions(0), Random.shuffle(propositions.toList))

          case _ => None
        }
      case _ => None
    }
  }

  def pickRandomTranslation(translations: List[Translation]): Option[Translation] = {
    if (translations.size > 0) Some(translations(new Random().nextInt(translations.size))) else None
  }

  def pickRandomAnswer(french: List[String]): Option[String] = {
    if (french.size > 0) Some(french(new Random().nextInt(french.size))) else None
  }

}
