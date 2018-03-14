package sjsonnew

import scala.collection.mutable

import UnbuilderState._

/**
 * Builder is an mutable structure to write JSON into.
 */
class Unbuilder[J](facade: Facade[J]) {
  private var _state: UnbuilderState = UnbuilderState.Begin
  private var contexts: List[UnbuilderContext[J]] = Nil
  private var precontext: Option[UnbuilderPrecontext[J]] = None

  /** Read `Int` value to the current context. */
  def readInt(js: J): Int = readJ[Int](js, facade.extractInt)

  /** Read `Long` value to the current context. */
  def readLong(js: J): Long = readJ[Long](js, facade.extractLong)

  /** Read `Double` value to the current context. */
  def readDouble(js: J): Double = readJ[Double](js, facade.extractDouble)

  /** Read `Float` value to the current context. */
  def readFloat(js: J): Float = readJ[Float](js, facade.extractFloat)

  /** Read `BigDecimal` value to the current context. */
  def readBigDecimal(js: J): BigDecimal = readJ[BigDecimal](js, facade.extractBigDecimal)

  /** Read `Boolean` value to the current context. */
  def readBoolean(js: J): Boolean = readJ[Boolean](js, facade.extractBoolean)

  /** Read `String` value to the current context. */
  def readString(js: J): String = readJ[String](js, facade.extractString)

  /** Check if js is null */
  def isJnull(js: J): Boolean = facade.isJnull(js)

  /** Check if js is an object */
  def isObject(js: J): Boolean = facade.isObject(js)

  /** Begin reading JSON array. Returns the size.
    * Call `nextElement` n-times, and then call `endArray`.
    */
  def beginArray(js: J): Int =
    state match {
      case Begin | InArray | InObject =>
        val context = UnbuilderContext.ArrayContext(facade.extractArray(js))
        contexts ::= context
        state = InArray
        context.elements.size
      case End => stateError(End)
    }

  def nextElement: J =
    state match {
      case InArray =>
        contexts.head match {
          case ctx: UnbuilderContext.ArrayContext[J] => ctx.next
          case x => deserializationError(s"Unexpected context: $x")
        }
      case x => stateError(x)
    }

  /** End reading JSON array. Returns the size. */
  def endArray(): Unit =
    state match {
      case InArray =>
        contexts = contexts.tail
        if (contexts.isEmpty) state = End
        else contexts.head match {
          case _: UnbuilderContext.ObjectContext[J] => state = InObject
          case _ => state = InArray
        }
      case x => stateError(x)
    }

  def state: UnbuilderState = _state
  private def state_=(newState: UnbuilderState) = _state = newState

  def isInObject: Boolean = state == InObject

  /** The unbuilder counterpart for beginPreObject.
    * This is used to filter out the type field.
    */
  def beginPreObject(js: J): Int =
    state match {
      case Begin | InArray | InObject =>
        val accessor = facade.accessFields(js)
        val context = UnbuilderContext.ObjectContext(accessor, accessor.fieldNames)
        contexts ::= context
        precontext = Some(UnbuilderPrecontext(js, mutable.ArrayBuffer.empty))
        state = InObject
        context.fieldsSize
      case End => stateError(End)
    }

  def endPreObject(): Unit =
    state match {
      case InObject =>
        contexts = contexts.tail
        if (contexts.isEmpty) state = Begin
        else contexts.head match {
          case _: UnbuilderContext.ObjectContext[J] => state = InObject
          case _ => state = InArray
        }
      case x => stateError(x)
    }

  /** Begin reading JSON object. Returns the size.
    * Call `nextField` n-times, and then call `endObject`.
    */
  def beginObject(js: J, fieldNames: Option[Vector[String]]): Int =
    state match {
      case Begin | InArray | InObject =>
        val context0 =
          precontext match {
            case Some(pre) if pre.js == js =>
              val accessor = facade.accessFields(js)
              precontext = None
              //val excludeKeys = pre.names.toSet
              UnbuilderContext.ObjectContext(accessor, accessor.fieldNames filter { k => !pre.names.contains(k) })
            case _ =>
              val accessor = facade.accessFields(js)
              UnbuilderContext.ObjectContext(accessor, accessor.fieldNames)
          }
        val context =
          fieldNames match {
            case Some(fn) => context0.withNames(fn)
            case _        => context0
          }
        contexts ::= context
        state = InObject
        context.fieldsSize
      case End => stateError(End)
    }

  /** Begin reading JSON object. Returns the size.
    * Call `nextField` n-times, and then call `endObject`.
    */
  def beginObject(js: J): Int = beginObject(js, None)

  def hasNextField: Boolean =
    state match {
      case InObject =>
        contexts.head match {
          case ctx: UnbuilderContext.ObjectContext[J] => ctx.hasNext
          case x => deserializationError(s"Unexpected context: $x")
        }
      case x => stateError(x)
    }

  def nextFieldOpt(): (String, Option[J]) =
    state match {
      case InObject =>
        contexts.head match {
          case ctx: UnbuilderContext.ObjectContext[J] => ctx.next
          case x => deserializationError(s"Unexpected context: $x")
        }
      case x => stateError(x)
    }

  def nextFieldOptWithJString(): (J, Option[J]) =
    nextFieldOpt() match {
      case (k, v) => (facade.jstring(k), v)
    }

  @deprecated("Use nextFieldOpt that returns (String, Option[J]). nextField uses JNull to encode elided fields.", "0.8.0")
  def nextField(): (String, J) =
    nextFieldOpt() match {
      case (k, Some(v)) => (k, v)
      case (k, None)    => (k, facade.jnull())
    }

  @deprecated("Use nextFieldOpt that returns (J, Option[J]). nextFieldOptWithJString uses JNull to encode elided fields.", "0.8.0")
  def nextFieldWithJString(): (J, J) =
    nextField match {
      case (k, v) => (facade.jstring(k), v)
    }

  def lookupField(name: String): Option[J] =
    state match {
      case InObject =>
        precontext match {
          case Some(pre) => pre.names += name
          case None => // do nothing
        }
        contexts.head match {
          case ctx: UnbuilderContext.ObjectContext[J] => ctx.get(name)
          case x => deserializationError(s"Unexpected context: $x")
        }
      case x => stateError(x)
    }

  def readField[A: JsonReader](name: String): A = jsonReader[A].read(lookupField(name), this)

  /** End reading JSON object. Returns the size. */
  def endObject(): Unit =
    state match {
      case InObject =>
        contexts = contexts.tail
        if (contexts.isEmpty) state = End
        else contexts.head match {
          case _: UnbuilderContext.ObjectContext[J] => state = InObject
          case _ => state = InArray
        }
      case x => stateError(x)
    }

  private def readJ[A](js: J, f: J => A): A =
    state match {
      case Begin =>
        val x = f(js)
        state = End
        x
      case InArray =>
        if (contexts.isEmpty) deserializationError("The unbuilder state is InArray, but the context is empty.")
        else f(js)
      case InObject =>
        if (contexts.isEmpty) deserializationError("The unbuilder state is InField, but the context is empty.")
        else f(js)
      case End => stateError(End)
    }

  private def stateError(x: UnbuilderState) = deserializationError(s"Unexpected builder state: $x")
}

sealed trait UnbuilderState
object UnbuilderState {
  case object Begin extends UnbuilderState
  case object End extends UnbuilderState
  case object InArray extends UnbuilderState
  case object InObject extends UnbuilderState
}

private[sjsonnew] trait UnbuilderContext[J]
private[sjsonnew] object UnbuilderContext {
  trait ObjectContext[J] extends UnbuilderContext[J] {
    def names: Seq[String]
    def fieldsSize: Int
    def withNames(newNames: Seq[String]): ObjectContext[J]
    def get(name: String): Option[J]

    //def hasNext: Boolean
    //def next: (String, Option[J])

    private val size = fieldsSize
    private var idx: Int = 0
    def hasNext: Boolean = idx < size
    def next: (String, Option[J]) = {
      val name = names(idx)
      val x = get(names(idx))
      idx = idx + 1
      (name, x)
    }
  }
  object ObjectContext {
    def apply[J](fields: Map[String, J], names: Seq[String]): ObjectContext[J] =
      ObjectContextImpl(fields, names)

    def apply[J](accessor: FieldAccessor[J], names: Seq[String]): ObjectContext[J] =
      ObjectContextAccessorImpl(accessor, names)
  }

  private case class ObjectContextAccessorImpl[J](accessor: FieldAccessor[J], names: Seq[String]) extends ObjectContext[J] {
    override def fieldsSize: Int = names.size
    override def withNames(newNames: Seq[String]): ObjectContext[J] = ObjectContext(accessor, newNames)
    override def get(name: String): Option[J] = accessor.get(name)
  }
  private case class ObjectContextImpl[J](fields: Map[String, J], names: Seq[String]) extends ObjectContext[J] {
    override def fieldsSize: Int = fields.size
    override def withNames(newNames: Seq[String]): ObjectContext[J] = this.copy(names = newNames)
    override def get(name: String): Option[J] = fields.get(name)
  }
  case class ArrayContext[J](elements: Vector[J]) extends UnbuilderContext[J] {
    private var idx: Int = 0
    def next: J = {
      val x = elements(idx)
      idx = idx + 1
      x
    }
  }
}

private[sjsonnew] case class UnbuilderPrecontext[J](js: J, names: mutable.ArrayBuffer[String])
