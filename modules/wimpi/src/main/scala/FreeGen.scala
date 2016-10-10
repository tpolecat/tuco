import java.io.File
import java.lang.reflect.{ Array => _, _ }
import scala.reflect.ClassTag
import Predef._

object FreeGen {

  // lazy val freeGenClasses = settingKey[List[Class[_]]]("classes for which free algebras should be generated")
  // lazy val freeGenDir = settingKey[File]("directory where free algebras go")
  // lazy val freeGen = taskKey[Seq[File]]("generate free algebras")
  //
  // lazy val freeGenSettings = Seq(
  //   freeGenClasses := Nil,
  //   freeGenDir := (sourceManaged in Compile).value,
  //   freeGen := new FreeGen(freeGenClasses.value, state.value.log).gen(freeGenDir.value)
  // )

  val managed = List[Class[_]](
    classOf[net.wimpi.telnetd.net.Connection],
    classOf[net.wimpi.telnetd.net.ConnectionData],
    classOf[net.wimpi.telnetd.net.ConnectionEvent],
    classOf[net.wimpi.telnetd.net.ConnectionListener],
    classOf[net.wimpi.telnetd.io.BasicTerminalIO],
    classOf[net.wimpi.telnetd.TelnetD]
  )

  val out = new File("modules/core/src/main/scala/tuco/free")

  def main(args: Array[String]): Unit = {
    new FreeGen(managed).gen(out)
    println("Done.")
  }

}

class FreeGen(managed: List[Class[_]]) {

  // These Java classes will have non-Java names in our generated code
  val ClassBoolean  = classOf[Boolean]
  val ClassByte     = classOf[Byte]
  val ClassChar     = classOf[Char]
  val ClassShort    = classOf[Short]
  val ClassInt      = classOf[Int]
  val ClassLong     = classOf[Long]
  val ClassFloat    = classOf[Float]
  val ClassDouble   = classOf[Double]
  val ClassObject   = classOf[Object]
  val ClassVoid     = Void.TYPE

  val renames: Map[Class[_], String] =
    Map(classOf[java.sql.Array] -> "SqlArray")

  def tparams(t: Type): List[String] =
    t match {
      case t: GenericArrayType  => tparams(t.getGenericComponentType)
      case t: ParameterizedType => t.getActualTypeArguments.toList.flatMap(tparams)
      case t: TypeVariable[_]   => List(t.toString)
      case _                    => Nil
    }

  def toScalaType(t: Type): String =
    t match {
      case t: GenericArrayType  => s"Array[${toScalaType(t.getGenericComponentType)}]"
      case t: ParameterizedType => s"${toScalaType(t.getRawType)}${t.getActualTypeArguments.map(toScalaType).mkString("[", ", ", "]")}"
      case t: WildcardType      => "_" // not quite right but ok
      case t: TypeVariable[_]   => t.toString
      case ClassVoid            => "Unit"
      case ClassBoolean         => "Boolean"
      case ClassByte            => "Byte"
      case ClassChar            => "Char"
      case ClassShort           => "Short"
      case ClassInt             => "Int"
      case ClassLong            => "Long"
      case ClassFloat           => "Float"
      case ClassDouble          => "Double"
      case ClassObject          => "AnyRef"
      case x: Class[_] if x.isArray => s"Array[${toScalaType(x.getComponentType)}]"
      case x: Class[_]          => renames.getOrElse(x, x.getSimpleName)
    }


  // Each constructor for our algebra maps to an underlying method, and an index is provided to
  // disambiguate in cases of overloading.
  case class Ctor(method: Method, index: Int) {

    // The method name, unchanged
    def mname: String =
      method.getName

    // The case class constructor name, capitalized and with an index when needed
    def cname: String = {
      val s = mname(0).toUpper +: mname.drop(1)
      (if (index == 0) s else s"$s$index")
    }

    // Constructor parameter type names
    def cparams: List[String] =
      method.getGenericParameterTypes.toList.map(toScalaType)

    def ctparams: String = {
      val ss = (method.getGenericParameterTypes.toList.flatMap(tparams) ++ tparams(method.getGenericReturnType)).toSet
      if (ss.isEmpty) "" else ss.mkString("[", ", ", "]")
    }

    // Constructor arguments, a .. z zipped with the right type
    def cargs: List[String] =
      "abcdefghijklmnopqrstuvwxyz".toList.zip(cparams).map {
        case (n, t) => s"$n: $t"
      }

    // Return type name
    def ret: String =
      toScalaType(method.getGenericReturnType)


    // Case class/object declaration
    def ctor(constraints: String, sname:String): String =
      ("|case " + (cparams match {
        case Nil => s"object $cname"
        case ps  => s"class  $cname$ctparams(${cargs.mkString(", ")})"
      }) + s""" extends ${sname}Op[$ret] {
        |      override def defaultTransK[M[_]: $constraints] = primitive(_.$mname($args))
        |    }""").trim.stripMargin

    // Argument list: a, b, c, ... up to the proper arity
    def args: String =
      "abcdefghijklmnopqrstuvwxyz".toList.take(cparams.length).mkString(", ")

    // Pattern to match the constructor
    def pat: String =
      cparams match {
        case Nil => s"object $cname"
        case ps  => s"class  $cname(${cargs.mkString(", ")})"
      }

    // Case clause mapping this constructor to the corresponding primitive action
    def prim(sname:String): String =
      (if (cargs.isEmpty)
        s"case $cname => primitive(_.$mname)"
      else
        s"case $cname($args) => primitive(_.$mname($args))")

    // Smart constructor
    def lifted(sname: String): String =
      if (cargs.isEmpty) {
        s"""|/**
            |   * @group Constructors (Primitives)
            |   */
            |  val $mname: ${sname}IO[$ret] =
            |    F.liftF(${cname})
         """.trim.stripMargin
      } else {
        s"""|/**
            |   * @group Constructors (Primitives)
            |   */
            |  def $mname$ctparams(${cargs.mkString(", ")}): ${sname}IO[$ret] =
            |    F.liftF(${cname}($args))
         """.trim.stripMargin
      }

  }

  // This class, plus any superclasses and interfaces, "all the way up"
  def closure(c: Class[_]): List[Class[_]] =
    (c :: (Option(c.getSuperclass).toList ++ c.getInterfaces.toList).flatMap(closure)).distinct
      .filterNot(_.getName == "java.lang.AutoCloseable") // not available in jdk1.6

  // All method for this class and any superclasses/interfaces
  def methods(c: Class[_]): List[Method] =
    closure(c).flatMap(_.getMethods.toList)
      .distinct
      .filterNot(m => (m.getModifiers & java.lang.reflect.Modifier.STATIC) != 0)
      .filterNot(_.getDeclaringClass.getName.startsWith("java.lang"))

  // Ctor values for all methods in of A plus superclasses, interfaces, etc.
  def ctors[A](implicit ev: ClassTag[A]): List[Ctor] =
    methods(ev.runtimeClass).groupBy(_.getName).toList.flatMap { case (n, ms) =>
      ms.toList.sortBy(_.getGenericParameterTypes.map(toScalaType).mkString(",")).zipWithIndex.map {
        case (m, i) => Ctor(m, i)
      }
    }.sortBy(c => (c.mname, c.index))

  // All types referenced by all methods on A, superclasses, interfaces, etc.
  def imports[A](implicit ev: ClassTag[A]): List[String] =
    (s"import ${ev.runtimeClass.getName}" :: ctors.map(_.method).flatMap { m =>
      m.getReturnType :: managed.toList.filterNot(_ == ev.runtimeClass) ::: m.getParameterTypes.toList
    }.map { t =>
      if (t.isArray) t.getComponentType else t
    }.filterNot(t => t.isPrimitive).map { c =>
      val sn = c.getSimpleName
      val an = renames.getOrElse(c, sn)
      if (sn == an) s"import ${c.getName}"
      else          s"import ${c.getPackage.getName}.{ $sn => $an }"
    }).distinct.sorted




  // The algebra module for A
  def module[A](implicit ev: ClassTag[A]): String = {
    val sname = toScalaType(ev.runtimeClass)
   s"""
    |package tuco.free
    |import tuco.util.Capture
    |
    |import scalaz.{ Catchable, Free => F, Kleisli, Monad, ~>, \\/ }
    |
    |${imports[A].mkString("\n")}
    |
    |${managed.map(_.getSimpleName).map(c => s"import ${c.toLowerCase}.${c}IO").mkString("\n")}
    |
    |/**
    | * Algebra and free monad for primitive operations over a `${ev.runtimeClass.getName}`.
    | * @group Modules
    | */
    |object ${sname.toLowerCase} {
    |
    |  /**
    |   * Sum type of primitive operations over a `${ev.runtimeClass.getName}`.
    |   * @group Algebra
    |   */
    |  sealed trait ${sname}Op[A] {
    |    protected def primitive[M[_]: Monad: Capture](f: ${sname} => A): Kleisli[M, ${sname}, A] =
    |      Kleisli((s: ${sname}) => Capture[M].apply(f(s)))
    |    def defaultTransK[M[_]: Monad: Catchable: Capture]: Kleisli[M, ${sname}, A]
    |  }
    |
    |  /**
    |   * Module of constructors for `${sname}Op`. These are rarely useful outside of the implementation;
    |   * prefer the smart constructors provided by the `${sname.toLowerCase}` module.
    |   * @group Algebra
    |   */
    |  object ${sname}Op {
    |
    |    // This algebra has a default interpreter
    |    implicit val ${sname}KleisliTrans: KleisliTrans.Aux[${sname}Op, ${sname}] =
    |      new KleisliTrans[${sname}Op] {
    |        type J = ${sname}
    |        def interpK[M[_]: Monad: Catchable: Capture]: ${sname}Op ~> Kleisli[M, ${sname}, ?] =
    |          new (${sname}Op ~> Kleisli[M, ${sname}, ?]) {
    |            def apply[A](op: ${sname}Op[A]): Kleisli[M, ${sname}, A] =
    |              op.defaultTransK[M]
    |          }
    |      }
    |
    |    // Lifting
    |    case class Lift[Op[_], A, J](j: J, action: F[Op, A], mod: KleisliTrans.Aux[Op, J]) extends ${sname}Op[A] {
    |      override def defaultTransK[M[_]: Monad: Catchable: Capture] = Kleisli(_ => mod.transK[M].apply(action).run(j))
    |    }
    |
    |    // Combinators
    |    case class Attempt[A](action: ${sname}IO[A]) extends ${sname}Op[Throwable \\/ A] {
    |      override def defaultTransK[M[_]: Monad: Catchable: Capture] =
    |        Predef.implicitly[Catchable[Kleisli[M, ${sname}, ?]]].attempt(action.transK[M])
    |    }
    |    case class Pure[A](a: () => A) extends ${sname}Op[A] {
    |      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(_ => a())
    |    }
    |    case class Raw[A](f: ${sname} => A) extends ${sname}Op[A] {
    |      override def defaultTransK[M[_]: Monad: Catchable: Capture] = primitive(f)
    |    }
    |
    |    // Primitive Operations
    |    ${ctors[A].map(_.ctor("Monad: Catchable: Capture", sname)).mkString("\n    ")}
    |
    |  }
    |  import ${sname}Op._ // We use these immediately
    |
    |  /**
    |   * Free monad over a free functor of [[${sname}Op]]; abstractly, a computation that consumes
    |   * a `${ev.runtimeClass.getName}` and produces a value of type `A`.
    |   * @group Algebra
    |   */
    |  type ${sname}IO[A] = F[${sname}Op, A]
    |
    |  /**
    |   * Catchable instance for [[${sname}IO]].
    |   * @group Typeclass Instances
    |   */
    |  implicit val Catchable${sname}IO: Catchable[${sname}IO] =
    |    new Catchable[${sname}IO] {
    |      def attempt[A](f: ${sname}IO[A]): ${sname}IO[Throwable \\/ A] = ${sname.toLowerCase}.attempt(f)
    |      def fail[A](err: Throwable): ${sname}IO[A] = ${sname.toLowerCase}.delay(throw err)
    |    }
    |
    |  /**
    |   * Capture instance for [[${sname}IO]].
    |   * @group Typeclass Instances
    |   */
    |  implicit val Capture${sname}IO: Capture[${sname}IO] =
    |    new Capture[${sname}IO] {
    |      def apply[A](a: => A): ${sname}IO[A] = ${sname.toLowerCase}.delay(a)
    |    }
    |
    |  /**
    |   * Lift a different type of program that has a default Kleisli interpreter.
    |   * @group Constructors (Lifting)
    |   */
    |  def lift[Op[_], A, J](j: J, action: F[Op, A])(implicit mod: KleisliTrans.Aux[Op, J]): ${sname}IO[A] =
    |    F.liftF(Lift(j, action, mod))
    |
    |  /**
    |   * Lift a ${sname}IO[A] into an exception-capturing ${sname}IO[Throwable \\/ A].
    |   * @group Constructors (Lifting)
    |   */
    |  def attempt[A](a: ${sname}IO[A]): ${sname}IO[Throwable \\/ A] =
    |    F.liftF[${sname}Op, Throwable \\/ A](Attempt(a))
    |
    |  /**
    |   * Non-strict unit for capturing effects.
    |   * @group Constructors (Lifting)
    |   */
    |  def delay[A](a: => A): ${sname}IO[A] =
    |    F.liftF(Pure(a _))
    |
    |  /**
    |   * Backdoor for arbitrary computations on the underlying ${sname}.
    |   * @group Constructors (Lifting)
    |   */
    |  def raw[A](f: ${sname} => A): ${sname}IO[A] =
    |    F.liftF(Raw(f))
    |
    |  ${ctors[A].map(_.lifted(sname)).mkString("\n\n  ")}
    |
    | /**
    |  * Natural transformation from `${sname}Op` to `Kleisli` for the given `M`, consuming a `${ev.runtimeClass.getName}`.
    |  * @group Algebra
    |  */
    |  def interpK[M[_]: Monad: Catchable: Capture]: ${sname}Op ~> Kleisli[M, ${sname}, ?] =
    |   ${sname}Op.${sname}KleisliTrans.interpK
    |
    | /**
    |  * Natural transformation from `${sname}IO` to `Kleisli` for the given `M`, consuming a `${ev.runtimeClass.getName}`.
    |  * @group Algebra
    |  */
    |  def transK[M[_]: Monad: Catchable: Capture]: ${sname}IO ~> Kleisli[M, ${sname}, ?] =
    |   ${sname}Op.${sname}KleisliTrans.transK
    |
    | /**
    |  * Natural transformation from `${sname}IO` to `M`, given a `${ev.runtimeClass.getName}`.
    |  * @group Algebra
    |  */
    | def trans[M[_]: Monad: Catchable: Capture](c: $sname): ${sname}IO ~> M =
    |   ${sname}Op.${sname}KleisliTrans.trans[M](c)
    |
    |  /**
    |   * Syntax for `${sname}IO`.
    |   * @group Algebra
    |   */
    |  implicit class ${sname}IOOps[A](ma: ${sname}IO[A]) {
    |    def transK[M[_]: Monad: Catchable: Capture]: Kleisli[M, ${sname}, A] =
    |      ${sname}Op.${sname}KleisliTrans.transK[M].apply(ma)
    |  }
    |
    |}
    |""".trim.stripMargin
  }

  def gen(base: File): Seq[java.io.File] = {
    import java.io._
    println("Generating free algebras into " + base)
    managed.map { c =>
      base.mkdirs
      val mod  = module(ClassTag(c))
      val file = new File(base, s"${c.getSimpleName.toLowerCase}.scala")
      val pw = new PrintWriter(file)
      pw.println(mod)
      pw.close()
      println(s"${c.getName} -> ${file.getName}")
      file
    }
  }

}
