package edu.hku.cs

import edu.hku.cs.SampleMode.SampleMode
import edu.hku.cs.TrackingMode.TrackingMode

/**
  * Created by jianyu on 3/6/17.
  */

object TrackingMode extends Enumeration {
  type TrackingMode = Value
  val RuleTracking, FullTracking, MixTracking = Value
}

object SampleMode extends Enumeration {
  type SampleMode = Value
  val RuleTracking, FullTracking, MixTracking = Value
}

class DFTEnv(
            val serverHost: String,
            val serverPort: Int,
            val trackingMode: TrackingMode,
            val sampleMode: SampleMode) {

}
