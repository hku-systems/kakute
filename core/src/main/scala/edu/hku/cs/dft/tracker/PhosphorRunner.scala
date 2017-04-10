package edu.hku.cs.dft.tracker

/**
  * Created by jianyu on 3/3/17.
  */
/**
  A [[PhosphorRunner]] build the environment to run the program in
 */

class PhosphorRunner(cacheDir: String, phospherJar: String, javaHome: String) {

  private val _agent = "-javaagent:" + phospherJar +
    (if (cacheDir != null) "=cacheDir=" + cacheDir + "," else "")

  private val _bootclasspath = "-Xbootclasspath/a:" + phospherJar

  private val _javaBin = javaHome + "/bin/java"

  private val _ignore = "checkTaint=true"

  private val _ignoreInt = "checkTaintIgnoreAll="

  def agent(checkTaint: Boolean = false, ignoreTaintAll: Int = 0): String = _agent +
    (if (checkTaint) _ignore + "," + _ignoreInt + ignoreTaintAll else "")

  def bootclasspath(): String = _bootclasspath

  def java(): String = _javaBin

}
