package edu.hku.cs

import edu.hku.cs.DataModel.GraphManager
import edu.hku.cs.Optimization.RuleLocalControl
import edu.hku.cs.SampleMode.SampleMode
import edu.hku.cs.TaintTracking.PhosphorRunner
import edu.hku.cs.TrackingMode.TrackingMode
import edu.hku.cs.network.{EndpointDispatcher, EndpointRegister, NettyClient, NettyServer}

/**
  * Created by jianyu on 3/6/17.
  */

object TrackingMode extends Enumeration {
  type TrackingMode = Value
  val RuleTracking, FullTracking, MixTracking = Value
}

object SampleMode extends Enumeration {
  type SampleMode = Value
  val Sample, Machine, SampleAndMachine, Off = Value
}

class DFTEnv(argumentHandle: ArgumentHandle) {
  argumentHandle.init()
  val serverPort: Int = argumentHandle.parseArgs("port") match {
    case s: String => s.toInt
    case _ => DefaultArgument.port
  }
  val serverHost: String = argumentHandle.parseArgs("host") match {
    case s: String => s
    case _ => DefaultArgument.host
  }
  val trackingMode: TrackingMode = {
    argumentHandle.parseArgs("tracking") match {
      case "full" => TrackingMode.FullTracking
      case "rule" => TrackingMode.RuleTracking
      case "mix" => TrackingMode.MixTracking
      case _ => DefaultArgument.trackingMode
    }
  }
  val sampleMode: SampleMode = {
    argumentHandle.parseArgs("sample") match {
      case "sample" => SampleMode.Sample
      case "machine" => SampleMode.Machine
      case "sample&machine" => SampleMode.SampleAndMachine
      case "off" => SampleMode.Off
      case _ => DefaultArgument.sampleMode
    }
  }
  var _isServer: Boolean = argumentHandle.parseArgs("mode") match {
    case "server" => true
    case "worker" => false
    case _ => false
  }
}

object DFTEnv {

  var dFTEnv: DFTEnv = _

  var graphManager: GraphManager = _

  var localControl: RuleLocalControl = _

  var phosphorRunner: PhosphorRunner = new PhosphorRunner("/home/jianyu/spark-dft-cache",
    "/home/jianyu/phosphor/Phosphor-0.0.3-SNAPSHOT.jar",
    "/home/jianyu/phosphor/Phosphor/target/jre-inst-int")

  phosphorRunner.setTracking(true)

  var networkEnv: EndpointRegister = _

  def dftEnv(): DFTEnv = {
    if (dFTEnv == null) throw new Exception("DFT Environment not set")
    dFTEnv
  }

  def init(any: Any): Unit = {
    dFTEnv = new DFTEnv(new ConfFileHandle("/Users/max/dft.conf"))
    if (dFTEnv._isServer) {
      networkEnv = new NettyServer(new EndpointDispatcher, dFTEnv)
    } else {
      networkEnv = new NettyClient(new EndpointDispatcher, dFTEnv)
    }
  }

  def server_init(any: Any): Unit = {
    dFTEnv = new DFTEnv(new ConfFileHandle("/home/jianyu/dft.conf"))
    networkEnv = new NettyServer(new EndpointDispatcher, dFTEnv)
    dFTEnv._isServer = true
    graphManager = new GraphManager
    networkEnv.register(graphManager)
  }

  def client_init(any: Any): Unit = {
    dFTEnv = new DFTEnv(new ConfFileHandle("/home/jianyu/dft.conf"))
    networkEnv = new NettyClient(new EndpointDispatcher, dFTEnv)
    dFTEnv._isServer = false
    localControl = new RuleLocalControl
    networkEnv.register(localControl)
  }


}