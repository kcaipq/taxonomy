
import java.io._
import scala.annotation.tailrec
import scala.collection.mutable
import scala.collection.mutable.{ArrayBuffer, ListBuffer}
import scala.concurrent.duration.Duration
import scala.concurrent.{Await, Future}
import scala.io.Source
import scala.util.{Failure, Success}
import scala.concurrent.ExecutionContext.Implicits.global

abstract class Node {
  protected val name: String
  protected val prefix: String
  override def toString = Seq(prefix, name).filter(_ != "").mkString(":")
}

sealed trait Taxonomy[T] extends Node {
  def findById(prefix: String): Seq[T]
  def findDescendantsByNode(node: String): Seq[T]
}

sealed trait Taggable[T] extends mutable.Traversable[T] {
  def findByTag(tag: String): Seq[T]
}

case class Translation(override val prefix: String, override val name: String) extends Tag(prefix, name, Nil)

class Tag(override val prefix: String, val name: String, val translation: Seq[Translation]) extends Node
object Tag {
  def apply(name: String, trans: Seq[Translation]) = new Tag("Tag", name, trans)
}

class Taxon(val prefix: String, val name: String, val children: mutable.ListBuffer[Taxon], val tag: Tag) extends Taxonomy[Taxon] with Taggable[Taxon] {

  override def toString = name

  /**
    * Find taxon by node id
    * @param name
    * @return
    */
  override def findById(name: String): Seq[Taxon] = {
    val results = ListBuffer[Taxon]()
    this.foreach(x => if (x.name == name) results += x)
    results.toSeq
  }

  /**
    * Find all descendants by a node, result can be a aggregated list
    * @param node
    * @return
    */
  override def findDescendantsByNode(node: String): Seq[Taxon] = {
    val t: Seq[Taxon] = findById(node)
    val fList = ListBuffer[Future[ListBuffer[Taxon]]]()
    for (s <- t) {
      val f = Future {
        val results = ListBuffer[Taxon]()
        s.foreach(x => results += x)
        results
      }
      fList += f
    }
    Await.result(waitAll(fList).map(_.flatMap(l => l.getOrElse(Nil))), Duration.Inf)
  }

  /**
    * Find all taxons which has this tag
    * @param tag
    * @return
    */
  override def findByTag(tag: String): Seq[Taxon] = {
    val results = ListBuffer[Taxon]()
    this.foreach(x => if (x.tag.name == tag) results += x)
    if (this.tag.name == tag ) this +: results
    else results
  }

  private def lift[T](futures: Seq[Future[T]]) = futures.map(_.map { Success(_) }.recover { case t => Failure(t) })

  private def waitAll[T](futures: Seq[Future[T]]) = Future.sequence(lift(futures))

  def hasChild = children.nonEmpty

  def isChild(name: String) = children.contains(name)

  def isParent(name: String) = name == this.name

  def export(path: String): String = {
    val parentChild = new ArrayBuffer[Seq[String]]
    def helper(node: Taxon): Unit = {
      def mkSeq(childName: String) = Seq(node.name, childName, node.tag) ++ node.tag.translation
      if (node.hasChild) {
        node.children.foreach { t =>
          parentChild.append(mkSeq(t.name).map(_.toString))
          helper(t)
        }
      } else parentChild.append(mkSeq("").map(_.toString))
    }
    helper(this)

    val sep = System.getProperty("line.separator")
    val content = parentChild.map(_.mkString(",")).mkString(sep)
    try {
      val pw = new PrintWriter(new File(path))
      pw.write(content)
      pw.close
      content
    } catch {
      case t: Throwable => content
    }
  }

  override def foreach[U](f: (Taxon) => U): Unit = {
    @tailrec
    def foreachHelper(nodes: Taxon*): Unit = {
      if (nodes.length != 0) {
        for (node <- nodes) f(node)
        foreachHelper(nodes.flatMap(node => node.children): _*)
      }
    }
    foreachHelper(this)
  }
}

object Taxon {

  def apply(name: String, children: mutable.ListBuffer[Taxon], tag: Tag) = new Taxon("", name, children, tag)

  def importCsv(path: String): Taxon = {
    val f = Source.fromFile(path)
    val lines = f.getLines.toSeq
    val parentChildren = lines.groupBy(x => x.split(",").head)
    val rootElems = lines.head.split(",")
    val rootChild = rootElems(1)
    val rootTaxon = Taxon(rootElems.head, ListBuffer(), Tag("RootTag", Nil))
    def make(t: Taxon): Unit = {
      val children = lines.filter(a => a.split(",").head == t.name)
      if (children.nonEmpty) {
        children.foreach { y =>
          val elems = y.split(",")
          val parent = elems(1)
          if (parent.nonEmpty) {
            val taxonChild = Taxon(parent, ListBuffer(), Tag("tag", Nil))
            t.children += taxonChild
            make(taxonChild)
          }
        }
      }
    }
    make(rootTaxon)
    rootTaxon
  }
}



