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

trait CommandExecute[T]:
  extension (args: List[String]) def toCommand: Option[T]
  extension (cmd: T) def execute(context: BundleContext): Unit
  def help: String

import com.wenjun.osgi.shell.Command.*

given CommandExecute[Help] with
  extension (args: List[String])
    def toCommand: Option[Help] = args match
      case Nil       => Some(Help(None))
      case tail :: _ => Some(Help(Some(tail)))

  extension (help: Help)
    def execute(context: BundleContext): Unit =
      help.cmdAlias match
        case Some(cmdAlias) =>
          commandAlias.get(cmdAlias).foreach { help => println(help.help) }
        case None =>
          commandAlias.foreach { (key, help) => println(help.help) }

  def help: String = "help - display commands"

given CommandExecute[Install] with
  extension (args: List[String])
    def toCommand: Option[Install] = args match
      case head :: _ =>
        try Some(Install(new File(head)))
        catch
          case e =>
            e.printStackTrace()
            None
      case _ => None

  extension (install: Install)
    def execute(context: BundleContext): Unit =
      val bundle = context.installBundle(install.file.toURI.toString)
      println(s"Bundle: ${bundle.getBundleId}")

  def help: String = "install <url> - Install the bundle jar at the given url."

given CommandExecute[Start] with
  extension (args: List[String])
    def toCommand: Option[Start] = args match
      case head :: _ if head.toIntOption.nonEmpty =>
        Some(Start(head.toInt))
      case _ => None

  extension (start: Start)
    def execute(context: BundleContext): Unit =
      context.getBundle(start.bundleId) match
        case bundle: Bundle =>
          bundle.start()
        case null => println(s"bundle ${start.bundleId} not found")

  def help: String = "start <id> - Start the bundle with the given bundle id."

given CommandExecute[Stop] with
  extension (args: List[String])
    def toCommand: Option[Stop] = args match
      case head :: _ if head.toLongOption.nonEmpty =>
        Some(Stop(head.toLong))
      case _ => None

  extension (stop: Stop)
    def execute(context: BundleContext): Unit =
      context.getBundle(stop.bundleId) match
        case bundle: Bundle =>
          bundle.stop()
        case _ => throw new Exception(s"Bundle ${stop.bundleId} not found")

  override def help: String = "stop <id> - Stop the bundle with the given bundle id."

//given CommandExecute[Uninstall] with
//  override def help: String = "uninstall <id> - Uninstall the bundle with the given bundle id."

given CommandExecute[Update] with
  extension (args: List[String])
    def toCommand: Option[Update] = args match
      case head :: _ if head.toLongOption.nonEmpty => Some(Update(head.toLong))
      case _ => None

  extension (update: Update)
    def execute(context: BundleContext): Unit =
      context.getBundle(update.bundleId) match
        case bundle: Bundle =>
          bundle.update()

  override def help: String = "update <id> - Update the bundle with the given bundle id."

//given CommandExecute[StartLevel] with
//  override def help: String = "startlevel [<level>] - Get or set the framework startlevel. "
//
//given CommandExecute[BundleLevel] with
//  override def help: String = "bundlelevel [-i] [<level>] <id> - Get or set (initial) bundle startlevel."
//
//given CommandExecute[Refresh] with
//  override def help: String = "refresh [<id> ...] - refresh bundles."
//
given CommandExecute[Resolve] with
  extension (args: List[String])
    def toCommand: Option[Resolve] = args match
      case head :: _ if head.toLongOption.nonEmpty => Some(Resolve(head.toLong))
  extension (resolve: Resolve)
    def execute(context: BundleContext): Unit =
      context.getService(context.getServiceReference(classOf[PackageAdmin])) match
        case packageAdmin: PackageAdmin =>
          packageAdmin.resolveBundles(Array(context.getBundle(resolve.bundleId)))
          ()
        case _ =>
          ()
  override def help: String = "resolve [<id> ...] - resolve bundles."

given CommandExecute[Bundles] with
  extension (args: List[String])
    def toCommand: Option[Bundles] = args match
      case _ => Some(Bundles())

  extension (b: Bundles)
    def execute(context: BundleContext): Unit =
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

  override def help: String = "bundles - Print information about the currently installed bundles"

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
  "resolve" -> summon[CommandExecute[Resolve]],
  "update" -> summon[CommandExecute[Update]]
)

private def executeArgs[T](args: List[String], context: BundleContext)(using
  CommandExecute[T]
): Either[Throwable, Unit] =
  args.toCommand match
    case Some(cmd) =>
      try Right(cmd.execute(context))
      catch case e: Throwable => Left(e)
    case None =>
      Left(new Exception(s"""error command line please use
                            |${summon[CommandExecute[T]].help}
                            |""".stripMargin))

def executeCommandLine(commandLine: String, context: BundleContext): Either[Throwable, Unit] =
  commandLine.split(' ').toList match
    case alias :: tails =>
      alias match
        case "help"    => executeArgs[Help](tails, context)
        case "bundles" => executeArgs[Bundles](tails, context)
        case "install" => executeArgs[Install](tails, context)
        case "start"   => executeArgs[Start](tails, context)
        case "stop"    => executeArgs[Stop](tails, context)
        case "update"  => executeArgs[Update](tails, context)
        case _         => Left(new Exception(s"unknown command ${alias}"))
    case _ =>
      Left(new Exception("command line formate error"))
