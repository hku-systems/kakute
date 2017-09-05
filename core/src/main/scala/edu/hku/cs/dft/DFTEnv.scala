package edu.hku.cs.dft

import edu.hku.cs.dft.datamodel.{GraphDumper, GraphManager}
import edu.hku.cs.dft.SampleMode.SampleMode
import edu.hku.cs.dft.TrackingMode.TrackingMode
import edu.hku.cs.dft.debug.DebugReplay
import edu.hku.cs.dft.network._
import edu.hku.cs.dft.optimization.RuleLocalControl
import edu.hku.cs.dft.tracker.ShuffleOpt.ShuffleOpt
import edu.hku.cs.dft.tracker.TrackingTaint.TrackingTaint
import edu.hku.cs.dft.tracker._
import edu.hku.cs.dft.tracker.TrackingType.TrackingType

/**
  * Created by jianyu on 3/6/17.
  */

class ConfEnumeration extends Enumeration {
  class ConfVal(val string: String) extends Val(string)
  def ConfValue(string: String): ConfVal = new ConfVal(string)
}


object TrackingMode extends ConfEnumeration {
  type TrackingMode = Value
  val RuleTracking = ConfValue("rule")
  val FullTracking = ConfValue("full")
  val SecurityTracking = ConfValue("security")
  val Debug = ConfValue("debug")
  val Off = ConfValue("off")
}

object SampleMode extends ConfEnumeration {
  type SampleMode = Value

  val Sample = ConfValue("sample")
  val Machine = ConfValue("machine")
  val Off = ConfValue("off")
}

case class GlobalCheckerConf(host: String, port: Int)

// sampleMode, sampleNum
// partition scheme

// Driver -> IftConf (policies) -> Executor (DFTEnv)

case class IftConf(trackingType: TrackingType,
                   trackingTaint: TrackingTaint, shuffleOpt: ShuffleOpt,
                   trackingPolicy: TrackingPolicy,
                   globalCheckerConf: GlobalCheckerConf = null)

class DFTEnv {
  var isServer: Boolean = false
  var phosphorRunner: PhosphorRunner = _
  var localChecker: Boolean = _
  var GlobalChecker: Boolean = _
  var trackingPolicy: TrackingPolicy = _
}

class DFTEnvOld(val argumentHandle: ArgumentHandle) {
  argumentHandle.init()

  val serverPort: Int = argumentHandle.parseArgs(DefaultArgument.CONF_PORT) match {
    case s: String => if (s.length > 0) s.toInt else DefaultArgument.port
    case _ => DefaultArgument.port
  }

  val serverHost: String = argumentHandle.parseArgs(DefaultArgument.CONF_HOST) match {
    case s: String => s
    case _ => DefaultArgument.host
  }

  var trackingMode: TrackingMode = {
    argumentHandle.parseArgs(DefaultArgument.CONF_TRACKING) match {
      case s: String => TrackingMode.withName(s)
      case _ => DefaultArgument.trackingMode
    }
  }

  val trackingOn: Boolean = {
    trackingMode match {
      case TrackingMode.Off => false
      case _ => true
    }
  }

  val trackingType: TrackingType = {
    argumentHandle.parseArgs(DefaultArgument.CONF_TYPE) match {
      case s: String => TrackingType.withName(s)
      case _ => TrackingType.KeyValues
    }
  }

  val sampleMode: SampleMode = {
    argumentHandle.parseArgs(DefaultArgument.CONF_SAMPLE) match {
      case s: String => SampleMode.withName(s)
      case _ => DefaultArgument.sampleMode
    }
  }

  //use percentage sampling
  val sampleNum: Int = {
    val stringInt = argumentHandle.parseArgs(DefaultArgument.CONF_SAMPLE_INT)
    if (stringInt != null) {
      Integer.valueOf(stringInt)
    } else {
      DefaultArgument.sampleInt
    }
  }

  var isServer: Boolean = argumentHandle.parseArgs(DefaultArgument.CONF_MODE) match {
    case "server" => true
    case "worker" => false
    case _ => false
  }

  val partitionSchemeOutput: String = {
    argumentHandle.parseArgs(DefaultArgument.CONF_PARTITION_OUTPUT) match {
      case s: String => s
      case _ => DefaultArgument.partitionPath
    }
  }

  val phosphorEnv: PhosphorEnv = {
    var java = argumentHandle.parseArgs(DefaultArgument.CONF_PHOSPHOR_JAVA)
    if (java == null) {
      java = "phosphor/bin/"
    }
    var jar = argumentHandle.parseArgs(DefaultArgument.CONF_PHOSPHOR_JAR)
    if (jar == null) {
      jar = "phosphor/phosphor.jar"
    }
    var cache = argumentHandle.parseArgs(DefaultArgument.CONF_PHOSPHOR_CACHE)
    if (cache == null) {
      cache = "phosphor/cache"
    }
    PhosphorEnv(java, jar, cache)
  }

  val graphDumpPath: String = argumentHandle.parseArgs(DefaultArgument.CONF_DUMP_PATH) match {
    case s: String => s
    case _ => DefaultArgument.dumpGraph
  }

  val trackingTaint: TrackingTaint = argumentHandle.parseArgs(DefaultArgument.CONF_TAINT) match {
    case s: String => TrackingTaint.withName(s)
    case _ => DefaultArgument.trackingTaint
  }
  
  val generateScheme: Boolean = argumentHandle.parseArgs(DefaultArgument.CONF_SCHEME) match {
    case "true" => true
    case _ => false
  }

  var shuffleOpt: ShuffleOpt = argumentHandle.parseArgs(DefaultArgument.CONF_SHUFFLE) match {
    case s: String => ShuffleOpt.withName(s)
    case _ => ShuffleOpt.WithoutOpt
  }

}

case class PhosphorEnv(phosphorJava: String, phosphorJar: String, cache: String)

object DFTEnv {
  var currentIft: DFTEnv = _

  def on(): Boolean = currentIft != null

  def ift(): DFTEnv = if (currentIft != null) currentIft else throw new Exception("DFT Environment not set")

  def executor(iftConf: IftConf): Unit = {
    if (iftConf != null) {
      currentIft = new DFTEnv()
      currentIft.trackingPolicy = iftConf.trackingPolicy
    }
  }

  def server(iftConf: IftConf): Unit = {
    currentIft = new DFTEnv
    currentIft.isServer = true
  }

  def worker(): Unit = {
    val argumentHandle = new ConfFileHandle(DefaultArgument.confFile)
    currentIft = new DFTEnv
    var java = argumentHandle.parseArgs(DefaultArgument.CONF_PHOSPHOR_JAVA)
    if (java == null) {
      java = "phosphor/bin/"
    }
    var jar = argumentHandle.parseArgs(DefaultArgument.CONF_PHOSPHOR_JAR)
    if (jar == null) {
      jar = "phosphor/phosphor.jar"
    }
    var cache = argumentHandle.parseArgs(DefaultArgument.CONF_PHOSPHOR_CACHE)
    if (cache == null) {
      cache = "phosphor/cache"
    }
    PhosphorEnv(java, jar, cache)
    currentIft.phosphorRunner = new PhosphorRunner(cache, jar, java, TrackingTaint.IntTaint)
  }

  def stop_all(): Boolean = {
    true
  }
}

/*// TODO: currently only one DFTEnv is supported, which means that if there are multiple context
// TODO: then the system will be broken, but this is a wire case
// TODO: we also need to rebind another port when failure happens
object DFTEnv {

  type DFTLogging = org.apache.spark.internal.Logging

  private var _confPath: String = DefaultArgument.confFile

  private var _dftEnv: DFTEnv = _

  val commandlineConf: ArgumentHandle = new CommandlineHandle

  var trackingPolicy: TrackingPolicy = _

  var graphManager: GraphManager = _

  var localControl: RuleLocalControl = _

  var phosphorRunner: PhosphorRunner = _

  var networkEnv: EndpointRegister = _

  val shuffleTag: Int = 1 << 31

  // set the conf path
  def pathInit(path: String): Unit = _confPath = path

  // init the common variables and objects

  def dftEnv(): DFTEnv = {
    if (_dftEnv == null) throw new Exception("DFT Environment not set")
    _dftEnv
  }

  def init(any: Any): Unit = {
    if (_dftEnv.isServer) {
      networkEnv = new NettyServer(new EndpointDispatcher, _dftEnv)
    } else {
      networkEnv = new NettyClient(new EndpointDispatcher, _dftEnv)
    }
  }

  // TODO: we may need to check if the tracking engine could be working
  // TODO: if that is not working, we will reject every tracking requests
  def worker_init(): Unit = {
    _dftEnv = new DFTEnv(new ConfFileHandle(_confPath))
    phosphorRunner = new PhosphorRunner(DFTEnv.dftEnv().phosphorEnv.cache,
      DFTEnv.dftEnv().phosphorEnv.phosphorJar,
      DFTEnv.dftEnv().phosphorEnv.phosphorJava,
      DFTEnv.dftEnv().trackingTaint)
    // when tracking is on, then
  }

  def server_init(any: Any): Unit = {
    val start = any match {
      case t: TrackingMode => t match {
        case TrackingMode.Debug => true
        case TrackingMode.RuleTracking => true
        case _ => false
      }
      case _ => false
    }

    if (!start) {
      _dftEnv = new DFTEnv(new CommandlineHandle)
      _dftEnv.trackingMode = any.asInstanceOf[TrackingMode]
      return
    }

    _dftEnv = new DFTEnv(new ConfFileHandle(_confPath))
    _dftEnv.trackingMode = any.asInstanceOf[TrackingMode]
    networkEnv = new NettyServer(new EndpointDispatcher, _dftEnv)
    new Thread(new Runnable {
      override def run():Unit = networkEnv.run()
    }).start()

    // sleep 1s to make sure the network is on
    Thread.sleep(1000)
    _dftEnv.isServer = true
    graphManager = new GraphManager
    networkEnv.register(graphManager)
  }

  def client_init(iftConf: IftConf): Unit = {

    _dftEnv = new DFTEnv(commandlineConf)

    if (iftConf != null) {
      trackingPolicy = iftConf.trackingPolicy
      if (trackingPolicy.localSubmodule) {
        networkEnv = new NettyClient(new EndpointDispatcher, _dftEnv)
        new Thread(new Runnable {
          override def run(): Unit = networkEnv.run()
        }).start()
        Thread.sleep(1000)
        localControl = new RuleLocalControl
        networkEnv.register(localControl)
      }
    }
    // sleep 1s to make sure the network is on
    _dftEnv.isServer = false
  }

  def stop_all(trackingMode: TrackingMode): Unit = {
    networkEnv.stop()
    if (DFTEnv.dftEnv().isServer) {
      if (trackingMode == TrackingMode.Debug && graphManager.debugInfo != null) {
        graphManager.debugInfo.writeToFile(DebugReplay.debugTraceFile)
      }
      val graphDumper = new GraphDumper(DFTEnv.dftEnv().graphDumpPath)
      graphDumper.open()
      graphDumper.dumpGraph(graphManager)
      graphDumper.close()
    }
  }
}*/
