package edu.hku.cs.dft.tracker

import edu.hku.cs.dft.network.Message

/**
  * Created by jianyu on 9/7/17.
  */
trait LocalChecker extends Serializable {

  val id: String
  /* define the behavior of receiving a message from the [[GlobalChecker]] */
  def receive(message: Message): Message

  /* instrument some functions (I/O funcs) */
  def instrument(): Unit
}
