package edu.hku.cs.dft.tracker

import edu.hku.cs.dft.{ConfEnumeration, TrackingMode}
import edu.hku.cs.dft.TrackingMode.TrackingMode
import edu.hku.cs.dft.tracker.TrackingType.TrackingType


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

object TrackingType extends ConfEnumeration {
  type TrackingType = Value
  val Values = ConfValue("value")
  val Key = ConfValue("key")
  val KeyValues = ConfValue("key-value")
}

class TrackingPolicy(trackType: TrackingType, trackingMode: TrackingMode,
                     _add_tags_input: Boolean, trackingOn: Boolean = false) {

  /*
  * Default Setting
  * */
  val typeInfering: Boolean = {
    val o = trackingMode match {
      case TrackingMode.FullTracking => false
      case TrackingMode.RuleTracking => true
      case _ => false
    }
    o && trackingOn
  }

  val clear_tags_per_ops: Boolean = {
    val o = trackingMode match {
      case TrackingMode.RuleTracking => true
      case TrackingMode.FullTracking => false
      case _ => false
    }
    o && trackingOn
  }
  val add_tags_per_ops: Boolean = {
    val o = trackingMode match {
      case TrackingMode.RuleTracking => true
      case TrackingMode.FullTracking => false
      case _ => false
    }
    o && trackingOn
  }
  val add_tags_emitted: Boolean = {
    val o = trackingMode match {
      case TrackingMode.RuleTracking => false //TODO
      case TrackingMode.FullTracking => true //TODO
      case _ => true
    }
    o && trackingOn
  }
  val add_tags_input_files: Boolean = _add_tags_input && trackingOn
  val propagation_across_machines: Boolean = {
    val o = trackingMode match {
      case TrackingMode.RuleTracking => false
      case TrackingMode.FullTracking => true
      case _ => false
    }
    o && trackingOn
  }
  var tracking_type: TrackingType = trackType

}