package edu.hku.cs.dft.tracker

import edu.hku.cs.dft.network.Message

/**
  * Created by jianyu on 9/7/17.
  */
trait GlobalChecker extends Serializable {

  val id: String
  /* define the behavior of global checker */
  def receive(message:Message): Message

}
