package edu.hku.cs.TaintTracking

import edu.hku.cs.TaintTracking.TrackingType.TrackingType
import edu.hku.cs.{DFTEnv, TrackingMode}
import edu.hku.cs.TrackingMode.TrackingMode


/**
  * Created by jianyu on 3/7/17.
  */

/**
  * A [[TrackingPolicy]] is to determined how to add / remove tag to data,
  * And how to handle tag propagation between cluster
  *
  * A [[TrackingPolicy]] determines following behaviours:
  * 1) How to add / remove tag in a new operator
  * 2) How to add tags to newly emitted key-value pair
  * 3) How to add tags to the input files(like HDFS?)
  * 4) How to deal with tag propagation between machines
  * 5) Which tags to be added when propagating
  *
* */

/**
  * In the system, values, keys or both of them could be tracked
  * It also accepts optional rules to add tags
  * For example, people could add tags to only first or second key
  * of a key-value tuple
  *
* */

object TrackingType extends Enumeration {
  type TrackingType = Value
  val Values, Keys, KeyValues, Optional = Value
}

class TrackingPolicy(trackType: TrackingType, trackingMode: TrackingMode) {

  /*
  * Default Setting
  * */
  val typeInfering: Boolean = {
    trackingMode match {
      case TrackingMode.FullTracking => false
      case TrackingMode.RuleTracking => true
      case _ => true
    }
  }

  val clear_tags_per_ops: Boolean = {
    trackingMode match {
      case TrackingMode.RuleTracking => true
      case TrackingMode.FullTracking => false
      case _ => false
    }
  }
  val add_tags_per_ops: Boolean = {
    trackingMode match {
      case TrackingMode.RuleTracking => true
      case TrackingMode.FullTracking => false
      case _ => true
    }
  }
  val add_tags_emitted: Boolean = {
    trackingMode match {
      case TrackingMode.RuleTracking => false //TODO
      case TrackingMode.FullTracking => true //TODO
      case _ => true
    }
  }
  val add_tags_input_files: Boolean = {
    trackingMode match {
      case TrackingMode.RuleTracking => false
      case TrackingMode.FullTracking => true
      case _ => false
    }
  }
  val propagation_across_machines: Boolean = {
    trackingMode match {
      case TrackingMode.RuleTracking => false
      case TrackingMode.FullTracking => true
      case _ => false
    }
  }
  var tracking_type: TrackingType = trackType

}