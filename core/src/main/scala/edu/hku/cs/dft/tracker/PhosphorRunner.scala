package edu.hku.cs.dft.TaintTracking

/**
  * Created by jianyu on 3/3/17.
  */
/**
  A [[PhosphorRunner]] build the environment to run the program in
 */

class PhosphorRunner(cacheDir: String, phospherJar: String, javaHome: String) {

  private var _tracking = false

  private val _agent = "-javaagent:" + phospherJar +
    (if (cacheDir != null) "=cacheDir=" + cacheDir else "")

  private val _bootclasspath = "-Xbootclasspath/a:" + phospherJar

  private val _javaBin = javaHome + "/bin/java"

  def agent(): String = _agent

  def bootclasspath(): String = _bootclasspath

  def java(): String = _javaBin

  def tracking(): Boolean = _tracking

  def setTracking(tracking: Boolean) {
    _tracking = tracking
  }
}
