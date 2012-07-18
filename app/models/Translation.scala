package models

import collection.immutable.List
import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection
import com.novus.salat.dao._
import se.radley.plugin.salat._

/**
 * Created with IntelliJ IDEA.
 * User: jihedamine
 * Date: 5/22/12 11:19 AM
 */

case class Translation(_id: ObjectId, english: String, french: List[String])

object TranslationDAO extends SalatDAO[Translation, ObjectId](
  collection = MongoConnection()("translationsdb")("translations")
)

object Translation {
  def all(): List[Translation] = TranslationDAO.find(MongoDBObject.empty).toList

  def insert(translation: Translation) {
    TranslationDAO.insert(translation)
  }

  def update(id: String, translation: Translation) {
    TranslationDAO.update(
      MongoDBObject("_id" -> new ObjectId(id)),
      MongoDBObject(
      "english" -> translation.english,
      "french" -> translation.french
    ), false, false)
  }

  def remove(id: String) {
    find(id).map {
      TranslationDAO.remove(_)
    }
  }

  def find(id: String) : Option[Translation] = {
    val translations = TranslationDAO.find(ref = MongoDBObject("_id" -> new ObjectId(id))).toList
    translations.size match {
      case 1 => Some(translations(0))
      case _ => None
    }
  }

  def exists(name: String, value: String) : Boolean = {
    TranslationDAO.find(ref = MongoDBObject(name -> value)).toList.size > 0
  }

}


