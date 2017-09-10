package edu.hku.cs.dft.checker

import edu.hku.cs.dft.DFTEnv
import edu.hku.cs.dft.debug.DebugTracer
import edu.hku.cs.dft.network.Message
import edu.hku.cs.dft.tracker.TrackingTaint.TrackingTaint
import edu.hku.cs.dft.tracker._

/**
  * Created by max on 9/9/2017.
  */
class DebugChecker extends IFTChecker {
  override val taint: TrackingTaint = TrackingTaint.ObjTaint
  override val tapConf: TapConf = new TapConf {
    override val tap_exception: Option[PartialFunction[Exception, (Unit) => Unit]] = {
      case e:Exception =>
        DebugTracer.backTrace()
    }
  }
  override val localChecker: LocalChecker = new LocalChecker {
    override def instrument(): Unit = {}

    override def onRegister(): Unit = {}

    override def receiveAndReply(message: Message): Message = {
      null
    }

    override val id: String = "debug"
  }
  override val globalChecker: GlobalChecker = new GlobalChecker {

    override def stop(): Unit = {}

    override def onRegister(): Unit = {}

    override def receiveAndReply(message: Message): Message = {
      null
    }

    override val id: String = "debug"

  }

  override val across_machine: Boolean = true

}
