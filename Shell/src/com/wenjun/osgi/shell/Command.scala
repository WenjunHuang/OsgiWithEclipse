package com.wenjun.osgi.shell

import org.osgi.framework.{Bundle, BundleContext, BundleException, Constants}
import org.osgi.service.packageadmin.PackageAdmin

import java.io.File
import java.net.URI
import java.nio.file.{Path, Paths}

enum Command:
  case Help(cmdAlias: Option[String])
  case Install(file: File)
  case Start(bundleId: Long)
  case Stop(bundleId: Long)
  case Uninstall(bundleId: Long)
  case Update(bundleId: Long)
  case StartLevel(level: Option[Int])
  case BundleLevel(level: Option[Int], bundleId: Long)
  case Refresh(bundleId: Long)
  case Resolve(bundleId: Long)
  case Bundles()
  case Error(error: String)
end Command

import com.wenjun.osgi.shell.CommandExecute.*

trait CommandExecute[T <: Command]:
  def execute(args: List[String], context: BundleContext): Either[Throwable, Unit] =
    builder(args) match
      case Some(cmd) =>
        try Right(executer(cmd, context))
        catch case e: Throwable => Left(e)
      case None =>
        Left(new Exception(s"""error command line please use
                              |$help
                              |""".stripMargin))
  def help: String

  protected def builder: Builder[T]
  protected def executer: Executer[T]

object CommandExecute:
  type Builder[T <: Command] = Function1[List[String], Option[T]]
  type Executer[T] = Function2[T, BundleContext, Unit]

import com.wenjun.osgi.shell.Command.*

given CommandExecute[Help] with
  protected def builder: Builder[Help] =
    case Nil       => Some(Help(None))
    case tail :: _ => Some(Help(Some(tail)))

  protected def executer: Executer[Help] = (help: Help, context: BundleContext) =>
    help.cmdAlias match
      case Some(cmdAlias) =>
        commandAlias.get(cmdAlias).foreach { help => println(help.help) }
      case None =>
        commandAlias.foreach { (key, help) => println(help.help) }

  val help: String = "help - display commands"

given CommandExecute[Install] with
  protected def builder: Builder[Install] =
    case head :: _ =>
      try Some(Install(new File(head)))
      catch
        case e =>
          e.printStackTrace()
          None
    case _ => None

  protected def executer: Executer[Install] = (install: Install, context: BundleContext) =>
    val bundle = context.installBundle(install.file.toURI.toString)
    println(s"Bundle: ${bundle.getBundleId}")

  def help: String = "install <url> - Install the bundle jar at the given url."

given CommandExecute[Start] with
  protected def builder: Builder[Start] =
    case head :: _ if head.toIntOption.nonEmpty =>
      Some(Start(head.toInt))
    case _ => None

  protected def executer = (start: Start, context: BundleContext) =>
    context.getBundle(start.bundleId) match
      case bundle: Bundle =>
        bundle.start()
      case null => println(s"bundle ${start.bundleId} not found")

  def help: String = "start <id> - Start the bundle with the given bundle id."

given CommandExecute[Stop] with
  protected def builder: Builder[Stop] =
    case head :: _ if head.toLongOption.nonEmpty => Some(Stop(head.toLong))
    case _                                       => None

  protected def executer: Executer[Stop] = (stop: Stop, context: BundleContext) =>
    context.getBundle(stop.bundleId) match
      case bundle: Bundle =>
        bundle.stop()
      case null => throw new Exception(s"Bundle ${stop.bundleId} not found")

  override def help: String = "stop <id> - Stop the bundle with the given bundle id."

//given CommandExecute[Uninstall] with
//  override def help: String = "uninstall <id> - Uninstall the bundle with the given bundle id."

given CommandExecute[Update] with
  protected def builder: Builder[Update] =
    case head :: _ if head.toLongOption.nonEmpty => Some(Update(head.toLong))
    case _                                       => None

  protected def executer: Executer[Update] = (update: Update, context: BundleContext) =>
    context.getBundle(update.bundleId) match
      case bundle: Bundle =>
        bundle.update()

  val help: String = "update <id> - Update the bundle with the given bundle id."

////given CommandExecute[StartLevel] with
////  override def help: String = "startlevel [<level>] - Get or set the framework startlevel. "
////
////given CommandExecute[BundleLevel] with
////  override def help: String = "bundlelevel [-i] [<level>] <id> - Get or set (initial) bundle startlevel."
////
////given CommandExecute[Refresh] with
////  override def help: String = "refresh [<id> ...] - refresh bundles."
////
//given CommandExecute[Resolve] with
//  protected def builder: Builder[Resolve] =
//      case head :: _ if head.toLongOption.nonEmpty => Some(Resolve(head.toLong))
//  extension (resolve: Resolve)
//    def execute(context: BundleContext): Unit =
//      context.getService(context.getServiceReference(classOf[PackageAdmin])) match
//        case packageAdmin: PackageAdmin =>
//          packageAdmin.resolveBundles(Array(context.getBundle(resolve.bundleId)))
//          ()
//        case _ =>
//          ()
//  override def help: String = "resolve [<id> ...] - resolve bundles."

given CommandExecute[Bundles] with
  protected def builder: Builder[Bundles] =
    case _ => Some(Bundles())

  protected def executer: Executer[Bundles] = (bundles: Bundles, context: BundleContext) =>
    val bundles = context.getBundles
    println("ID    State    Name")
    bundles.foreach(bundle =>
      printBundle(
        bundle.getBundleId,
        stateString(bundle.getState),
        bundle.getHeaders().get(Constants.BUNDLE_NAME),
        bundle.getLocation,
        bundle.getSymbolicName
      )
    )

  val help: String = "bundles - Print information about the currently installed bundles"

  private def stateString(state: Int): String =
    state match
      case Bundle.INSTALLED => "INSTALLED"
      case Bundle.RESOLVED  => "RESOLVED"
      case Bundle.STARTING  => "STARTING"
      case Bundle.ACTIVE    => "ACTIVE"
      case Bundle.STOPPING  => "STOPPING"
      case _                => "UNKNOWN"

  private def printBundle(id: Long, state: String, name: String, location: String, symbolicName: String): Unit =
    println(f"[$id%4d] [$state] $name     Location: $location      Symbolic Name: $symbolicName")

private val commandAlias = Map[String, CommandExecute[?]](
  "help" -> summon[CommandExecute[Help]],
  "install" -> summon[CommandExecute[Install]],
  "bundles" -> summon[CommandExecute[Bundles]],
  "start" -> summon[CommandExecute[Start]],
  "stop" -> summon[CommandExecute[Stop]],
//  "resolve" -> summon[CommandExecute[Resolve]],
  "update" -> summon[CommandExecute[Update]]
)

def executeCommandLine(commandLine: String, context: BundleContext): Either[Throwable, Unit] =
  commandLine.split(' ').toList match
    case alias :: tails =>
      commandAlias.get(alias) match
        case Some(commandExecute) =>
          commandExecute.execute(tails, context)
        case None =>
          Left(new Exception(s"unknown command ${alias}"))
    case _ =>
      Left(new Exception("command line format error"))
