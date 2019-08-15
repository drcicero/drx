package platform

import javax.management.{InstanceNotFoundException, MBeanException, ObjectName, ReflectionException}
import java.lang.management.ManagementFactory

object GCHelper {
  def getHisto1: String = { /* slow */
    val pid = ProcessHandle.current.pid
    val cmd = Array("/bin/sh", "-c", s"jmap -histo:live $pid | grep drx.[^$$]*$$")
    new String(Runtime.getRuntime.exec(cmd).getInputStream.readAllBytes())
  }
  def getHisto2: String = { /* slow */
    val pid = ProcessHandle.current.pid
    val cmd = Array("/bin/sh", "-c", s"jcmd GC.class_stats $pid | grep drx.[^$$]*$$")
    new String(Runtime.getRuntime.exec(cmd).getInputStream.readAllBytes().toString)
  }
  def getHisto3: String =
    invokeNoStringArgumentsCommand("gcClassHistogram")
      .split("\n").filter(x => x.contains("drx.") && !x.contains("$")).mkString("\n")
  def parseHisto(str: String): (Long, Map[String, Int]) = {
    val lines = str.strip() split "\n"
    val usedMemory = lines(0).toLong
    val instanceCount = lines drop 1 map { x: String =>
      val Array(_, amount, bytes, classname) = x split "  +"
      classname.strip() -> amount.strip().toInt }
    (usedMemory, instanceCount.toMap)
  }

  // from https://github.com/dustinmarx/javautilities/blob/master/dustin/utilities/diagnostics/VirtualMachineDiagnostics.java
  private val DIAGNOSTIC_COMMAND_MBEAN_OBJECT_NAME = "com.sun.management:type=DiagnosticCommand"
  private lazy val server = ManagementFactory.getPlatformMBeanServer
  private lazy val objectName = new ObjectName(DIAGNOSTIC_COMMAND_MBEAN_OBJECT_NAME)
  //case badObjectNameEx: MalformedObjectNameException =>
  //  throw new RuntimeException("Unable to create an ObjectName and so unable to create instance of VirtualMachineDiagnostics")

  private def invokeNoStringArgumentsCommand(operationName: String): String =
    try
      server.invoke(
        objectName,
        operationName,
        Array[AnyRef](null),
        Array[String](classOf[Array[String]].getName)
      ).asInstanceOf[String]
    catch {
      case exception@(_: InstanceNotFoundException | _: ReflectionException | _: MBeanException) =>
        "ERROR: Unable to access '" + operationName+ "' - " + exception
    }
}
