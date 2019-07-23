package platform

// from https://github.com/dustinmarx/javautilities/blob/master/dustin/utilities/diagnostics/VirtualMachineDiagnostics.java

import javax.management.{InstanceNotFoundException, MBeanException, ObjectName, ReflectionException}
import java.lang.management.ManagementFactory

object GCHelper {
  private val DIAGNOSTIC_COMMAND_MBEAN_OBJECT_NAME = "com.sun.management:type=DiagnosticCommand"
  private val server = ManagementFactory.getPlatformMBeanServer
  private val objectName = new ObjectName(DIAGNOSTIC_COMMAND_MBEAN_OBJECT_NAME)
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

  def getHisto1: Array[Byte] = {
    val pid = ProcessHandle.current.pid
    val cmd = Array("/bin/sh", "-c", s"jmap -histo:live $pid | grep drx.graph.[^$$]*$$")
    Runtime.getRuntime.exec(cmd).getInputStream.readAllBytes()
  }
  def getHisto2: Array[Byte]= {
    val pid = ProcessHandle.current.pid
    val cmd = Array("/bin/sh", "-c", s"cjmd GC.class_stats $pid | grep drx.graph.[^$$]*$$")
    Runtime.getRuntime.exec(cmd).getInputStream.readAllBytes()
  }
  def getHisto3: String = {
    invokeNoStringArgumentsCommand("gcClassHistogram") split "\n" filter
      (x => x.contains("drx.graph") && !x.contains("$")) mkString "\n"
  }

}
