package edu.hku.cs.dft.tracker

import edu.hku.cs.dft.{ConfEnumeration, TrackingMode}
import edu.hku.cs.dft.TrackingMode.TrackingMode
import edu.hku.cs.dft.tracker.TrackingTaint.TrackingTaint
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
  val KeyValuesArray = ConfValue("key-value-array")
}

object TrackingTaint extends ConfEnumeration {
  type TrackingTaint = Value
  val IntTaint = ConfValue("int")
  val ObjTaint = ConfValue("obj")
  val SelectiveIntTaint = ConfValue("int-selective")
  val SelectiveObjTaint = ConfValue("obj-selective")
  val ObjImplicitTaint = ConfValue("obj-implicit")
}

object ShuffleOpt extends ConfEnumeration {
  type ShuffleOpt = Value
  val CombinedTag = ConfValue("ctag")
  val CacheTag = ConfValue("cache")
  val CCTag = ConfValue("cctag")
  val WithoutOpt = ConfValue("no-opt")
}

class TrackingPolicy(val typeInfering: Boolean,
                     val clear_tags_per_ops: Boolean,
                     val add_tags_per_ops: Boolean,
                     val add_tags_emitted: Boolean,
                     val add_tags_input_files: Boolean,
                     val propagation_across_machines: Boolean,
                     val localSubmodule: Boolean,
                     val collectPerOp: Boolean) extends Serializable{
  def this(trackingPolicyOld: TrackingPolicyOld) =
    this (trackingPolicyOld.typeInfering, trackingPolicyOld.clear_tags_per_ops,
      trackingPolicyOld.add_tags_per_ops, trackingPolicyOld.add_tags_emitted,
      trackingPolicyOld.add_tags_input_files, trackingPolicyOld.propagation_across_machines,
      trackingPolicyOld.localSubmodule, trackingPolicyOld.collectPerOp)
}

class TrackingPolicyOld(trackType: TrackingType, trackingMode: TrackingMode,
                     trackingOn: Boolean = false,
                     taint: TrackingTaint = TrackingTaint.IntTaint) {

  /*
  * Default Setting
  * */
  val typeInfering: Boolean = {
    val o = trackingMode match {
      case TrackingMode.FullTracking => false
      case TrackingMode.RuleTracking | TrackingMode.Debug => true
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
  val add_tags_input_files: Boolean = {
    val o = trackingMode match {
      case TrackingMode.SecurityTracking | TrackingMode.RuleTracking => true
      case _ => false
    }
    o && trackingOn
  }
  val propagation_across_machines: Boolean = {
    val o = trackingMode match {
      case TrackingMode.RuleTracking => false
      case TrackingMode.FullTracking | TrackingMode.SecurityTracking | TrackingMode.Debug => true
      case _ => false
    }
    o && trackingOn
  }

  val localSubmodule: Boolean = {
    val o = trackingMode match {
      case TrackingMode.RuleTracking | TrackingMode.Debug => true
      case _ => false
    }
    o && trackingOn
  }

  val collectPerOp: Boolean = {
    val o = trackingMode match {
      case TrackingMode.Debug => true
      case _ => false
    }
    o && trackingOn
  }

  var tracking_type: TrackingType = trackType

  val trackingTaint: TrackingTaint = taint

}