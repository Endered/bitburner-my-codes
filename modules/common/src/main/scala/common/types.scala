package common

import cats.kernel.Monoid
import cats.syntax.all.given
import cats.instances.map.given
import common.types.AttackElement.GrowElement
import common.types.AttackElement.WeakenElement
import common.types.AttackElement.HackElement

object types {
  opaque type HostName = String
  object HostName {
    def apply(name: String): HostName = name
    def unapply(host: HostName): String = host
  }
  extension (hostName: HostName) {
    def value: String = hostName
  }

  sealed trait AttackElement {
    def thread: Long
    def toAttack: Attack = this match {
      case HackElement(thread)   => Attack.withHack(thread)
      case GrowElement(thread)   => Attack.withGrow(thread)
      case WeakenElement(thread) => Attack.withWeaken(thread)
    }

    def withThread(newThread: Long): AttackElement = this match {
      case HackElement(_)   => HackElement(newThread)
      case GrowElement(_)   => GrowElement(newThread)
      case WeakenElement(_) => WeakenElement(newThread)
    }
  }
  object AttackElement {
    case class HackElement(override val thread: Long) extends AttackElement
    case class GrowElement(override val thread: Long) extends AttackElement
    case class WeakenElement(override val thread: Long) extends AttackElement
  }

  case class Attack(hack: Long, grow: Long, weaken: Long) {
    def toElements: Seq[AttackElement] =
      Seq(
        Option.when(hack > 0)(HackElement(hack)),
        Option.when(grow > 0)(GrowElement(grow)),
        Option.when(weaken > 0)(WeakenElement(weaken))
      ).flatten
  }
  object Attack {
    def withHack(x: Long) = Attack(x, 0, 0)
    def withGrow(x: Long) = Attack(0, x, 0)
    def withWeaken(x: Long) = Attack(0, 0, x)
  }
  given Monoid[Attack] {
    def combine(x: Attack, y: Attack): Attack =
      Attack(x.hack + y.hack, x.grow + y.grow, x.weaken + y.weaken)
    def empty: Attack = Attack(0, 0, 0)
  }

  opaque type Attackings = Map[HostName, Attack]
  object Attackings {
    def apply(value: Map[HostName, Attack]): Attackings = value

    def fromAttack(target: HostName, attack: Attack): Attackings =
      Map(target -> attack)
  }
  extension (attackings: Attackings) {
    def value: Map[HostName, Attack] = attackings
  }
  given Monoid[Attackings] = summon[Monoid[Map[HostName, Attack]]]

  case class ServerScore(moneyScore: Double, timeScore: Double, threads: Long) {
    def totalScore: Double = if (timeScore <= 0) {
      -1
    } else {
      moneyScore / timeScore
    }
  }
  given Ordering[ServerScore] {
    def compare(x: ServerScore, y: ServerScore): Int =
      x.totalScore.compare(y.totalScore)
  }
}
