package models

import com.mongodb.casbah.Imports._
import com.mongodb.casbah.MongoConnection
import com.novus.salat._
import com.novus.salat.dao._
import se.radley.plugin.salat._

/**
 * Created with IntelliJ IDEA.
 * User: jihedamine
 * Date: 5/22/12 11:19 AM
 */

import com.novus.salat.dao._

case class Translation(
                        id: ObjectId,
                        english: String,
                        french: List[String]
                        )

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
    TranslationDAO.remove(find(id))
  }

  def find(id: String) : Translation = {
    TranslationDAO.find(ref = MongoDBObject("_id" -> new ObjectId(id))).toList(0)
  }

}


