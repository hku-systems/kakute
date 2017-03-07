package edu.hku.cs.TaintTracking


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

class TrackingPolicy {

  /*
  * Default Setting
  * */
  var _clear_tags_per_ops = false
  var _add_tags_per_ops = true
  var _add_tags_emitted = true
  var _add_tags_input_files = false
  var _tags_propagation_machines = false
  var _tracking_type = TrackingType.KeyValues

}

object TrackingPolicy {
  var _trackingPolicy: TrackingPolicy = new TrackingPolicy

  private def setPolicy(trackingPolicy: TrackingPolicy) {
      if (trackingPolicy == null) throw Exception
      _trackingPolicy = new TrackingPolicy
  }

  def currentOrDefaultPolicy(): TrackingPolicy = _trackingPolicy
}
