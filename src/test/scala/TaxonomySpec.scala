import org.specs2.mutable.Specification

import scala.collection.mutable.ListBuffer


class TaxonomySpec extends Specification {

  "A Taxonomy" should {
    "find node by ID" in {
      val t = makeTaxonomy
      t.findById("shows").head.children.size must equalTo(2)
      t.findById("films").head.children.size must equalTo(3)
      t.findById("music").head.children.size must equalTo(3)
      t.findById("restaurants").head.children.size must equalTo(3)
      t.findById("categories").head.children.size must equalTo(3)
      t.findById("chinese").size must equalTo(2)
      t.findById("comedy").size must equalTo(1)
      t.findById("action").size must equalTo(1)
      t.findById("jazz").size must equalTo(1)
      t.findById("pop").size must equalTo(1)
      t.findById("rock").size must equalTo(1)
      t.findById("french").size must equalTo(1)
      t.findById("italian").size must equalTo(1)
    }

    "find nodes by tag" in {
      val t = makeTaxonomy
      t.findByTag("chinese").size must equalTo(7)
    }

    "find descendants by node" in {
      val t = makeTaxonomy
      val s = t.findDescendantsByNode("music")
      t.findDescendantsByNode("music").size must equalTo(4)
    }

    "export to CSV" in {
      val t = makeTaxonomy
      val tmp = System.getProperty("java.io.tmpdir")
      val result = t.export(tmp)
      result must not empty
    }
  }

  private def makeTaxonomy: Taxon = {
    val transItCh = Translation("it_IT", "Cinese")
    val transEnCh = Translation("en_GB", "Chinese")
    val transFrCh = Translation("fr-FR", "Chinois")
    val transItRe = Translation("it_IT", "Restaurantes")
    val transEnRe = Translation("en_GB", "Restaurants")
    val transFrRe = Translation("fr_FR", "Restaurants")
    val chinTag = Tag("chinese", Seq(transItCh, transEnCh, transFrCh))
    val restTag = Tag("restaurant", Seq(transItRe, transEnRe, transFrRe))
    val filmTag = Tag("film", Seq(transItRe, transEnRe, transFrRe))

    Taxon(
      "categories",
      ListBuffer(
        Taxon(
          "shows",
          ListBuffer(
            Taxon("theatre", ListBuffer(), chinTag),
            Taxon("films", ListBuffer(
              Taxon("chinese", ListBuffer(), filmTag),
              Taxon("comedy", ListBuffer(), filmTag),
              Taxon("action", ListBuffer(), filmTag)
            ), chinTag)
          ),
          chinTag),
        Taxon(
          "music",
          ListBuffer(
            Taxon("jazz", ListBuffer(), chinTag),
            Taxon("pop", ListBuffer(), chinTag),
            Taxon("rock", ListBuffer(), chinTag)
          ),
          restTag),
        Taxon("restaurants", ListBuffer(
          Taxon("chinese", ListBuffer(), chinTag),
          Taxon("french", ListBuffer(), restTag),
          Taxon("italian", ListBuffer(), restTag)),
          restTag)
      ),
      restTag
    )
  }

}
