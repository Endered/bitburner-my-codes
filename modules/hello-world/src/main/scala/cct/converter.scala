package cct
import scala.scalajs.js
import cats.syntax.all.given
import scala.reflect.ClassTag

trait ConvertFromAny[T] {
  def from(data: Any): Option[T]
}

object TryConvert {
  def unapply[T](data: Any)(using c: ConvertFromAny[T]): Option[T] = c.from(data)
}

given ConvertFromAny[Double] {
  def from(data: Any): Option[Double] = data match {
    case x: Double => Some(x)
    case _         => None
  }
}

given ConvertFromAny[Int] {
  def from(data: Any): Option[Int] = data match {
    case x: Int => Some(x)
    case _      => None
  }
}

given [T: ConvertFromAny] => ConvertFromAny[Seq[T]] {
  def from(data: Any): Option[Seq[T]] = data match {
    case x: js.Array[_] => x.toSeq.traverse(summon[ConvertFromAny[T]].from)
    case _              => None
  }
}

given [T: {ConvertFromAny, ClassTag}] => ConvertFromAny[Array[T]] {
  def from(data: Any): Option[Array[T]] = data match {
    case x: js.Array[_] => x.toSeq.traverse(summon[ConvertFromAny[T]].from).map(_.toArray)
    case _              => None
  }
}

given ConvertFromAny[String] {
  def from(data: Any): Option[String] = data match {
    case x: String => Some(x)
    case _         => None
  }
}
