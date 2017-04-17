package edu.hku.cs.dft.tracker

import edu.hku.cs.dft.tracker.TrackingTaint.TrackingTaint

/**
  * Created by jianyu on 3/3/17.
  */
/**
  A [[PhosphorRunner]] build the environment to run the program in
 */

class PhosphorRunner(cacheDir: String, phospherJar: String, targetHome: String, var trackingTaint: TrackingTaint) {

  private val _agent = "-javaagent:" + phospherJar +
    (if (cacheDir != null) "=cacheDir=" + cacheDir + "," else "")

  private val _bootclasspath = "-Xbootclasspath/a:" + phospherJar

  private val _ignore = "checkTaint=true"

  private val _ignoreInt = "checkTaintIgnoreAll="

  def jreInst(): String = if (trackingTaint == TrackingTaint.IntTaint) "jre-inst-int" else "jre-inst-obj"

  def agent(checkTaint: Boolean = false, ignoreTaintAll: Int = 0): String = _agent +
    (if (checkTaint) _ignore + "," + _ignoreInt + ignoreTaintAll else "")

  def bootclasspath(): String = _bootclasspath

  def java(): String = targetHome + s"/${jreInst()}" + "/bin/java"

  def setTrackingTaint(taint: TrackingTaint): Unit = trackingTaint = taint

}
