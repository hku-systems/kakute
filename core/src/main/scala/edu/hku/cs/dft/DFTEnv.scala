package edu.hku.cs.dft

import edu.hku.cs.dft.datamodel.{GraphDumper, GraphManager}
import edu.hku.cs.dft.SampleMode.SampleMode
import edu.hku.cs.dft.TrackingMode.TrackingMode
import edu.hku.cs.dft.network.{EndpointDispatcher, EndpointRegister, NettyClient, NettyServer}
import edu.hku.cs.dft.optimization.RuleLocalControl
import edu.hku.cs.dft.tracker.{PhosphorRunner, TrackingPolicy, TrackingType}
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
  val MixTracking = ConfValue("mix")

}

object SampleMode extends ConfEnumeration {
  type SampleMode = Value

  val Sample = ConfValue("sample")
  val Machine = ConfValue("machine")
  val Off = ConfValue("off")
}

class DFTEnv(val argumentHandle: ArgumentHandle) {
  argumentHandle.init()
  val serverPort: Int = argumentHandle.parseArgs(DefaultArgument.CONF_PORT) match {
    case s: String => if (s.length > 0) s.toInt else DefaultArgument.port
    case _ => DefaultArgument.port
  }
  val serverHost: String = argumentHandle.parseArgs(DefaultArgument.CONF_HOST) match {
    case s: String => s
    case _ => DefaultArgument.host
  }
  val trackingMode: TrackingMode = {
    argumentHandle.parseArgs(DefaultArgument.CONF_TRACKING) match {
      case TrackingMode.FullTracking.string => TrackingMode.FullTracking
      case TrackingMode.RuleTracking.string => TrackingMode.RuleTracking
      case TrackingMode.MixTracking.string => TrackingMode.MixTracking
      case _ => DefaultArgument.trackingMode
    }
  }

  val trackingType: TrackingType = {
    argumentHandle.parseArgs(DefaultArgument.CONF_TYPE) match {
      case TrackingType.Key.string => TrackingType.Key
      case TrackingType.Values.string => TrackingType.Values
      case TrackingType.KeyValues.string => TrackingType.KeyValues
      case _ => TrackingType.KeyValues
    }
  }

  val sampleMode: SampleMode = {
    argumentHandle.parseArgs(DefaultArgument.CONF_SAMPLE) match {
      case SampleMode.Sample.string => SampleMode.Sample
      case SampleMode.Machine.string => SampleMode.Machine
      case SampleMode.Off.string => SampleMode.Off
      case _ => DefaultArgument.sampleMode
    }
  }

  val auto_taint_input: Boolean = {
    argumentHandle.parseArgs(DefaultArgument.CONF_INPUT_TAINT) match {
      case "true" => true
      case _ => false
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
    argumentHandle.parseArgs("partition_output") match {
      case s: String => s
      case _ => DefaultArgument.partitionPath
    }
  }

  val trackingOn: Boolean = {
    argumentHandle.parseArgs(DefaultArgument.CONF_DFT) match {
      case "on" => true
      case _ => false
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
  val graphDumpPath: String = argumentHandle.parseArgs("graph_dump_path") match {
    case s: String => s
    case _ => "phosphor/graph.dump"
  }
}

case class PhosphorEnv(phosphorJava: String, phosphorJar: String, cache: String)

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
      DFTEnv.dftEnv().phosphorEnv.phosphorJava)
  }

  def server_init(any: Any): Unit = {
    _dftEnv = new DFTEnv(new ConfFileHandle(_confPath))
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

  def client_init(any: Any): Unit = {

    _dftEnv = new DFTEnv(commandlineConf)

    trackingPolicy = new TrackingPolicy(DFTEnv.dftEnv().trackingType,
      DFTEnv.dftEnv().trackingMode,
      DFTEnv.dftEnv().auto_taint_input,
      DFTEnv.dftEnv().trackingOn)

    if (_dftEnv.trackingOn) {
      networkEnv = new NettyClient(new EndpointDispatcher, _dftEnv)
      new Thread(new Runnable {
        override def run(): Unit = networkEnv.run()
      }).start()
      Thread.sleep(1000)
      localControl = new RuleLocalControl
      networkEnv.register(localControl)
    }
    // sleep 1s to make sure the network is on
    _dftEnv.isServer = false
  }

  def stop_all(): Unit = {
    networkEnv.stop()
    if (DFTEnv.dftEnv().isServer) {
      val graphDumper = new GraphDumper(DFTEnv.dftEnv().graphDumpPath)
      graphDumper.open()
      graphDumper.dumpGraph(graphManager)
      graphDumper.close()
    }
  }
}
