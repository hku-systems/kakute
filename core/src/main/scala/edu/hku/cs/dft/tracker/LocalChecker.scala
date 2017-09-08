package edu.hku.cs.dft.tracker

import edu.hku.cs.dft.network.{Message, NettyEndpoint}

/**
  * Created by jianyu on 9/7/17.
  */
trait LocalChecker extends NettyEndpoint with Serializable{

  /* instrument some functions (I/O funcs) */
  def instrument(): Unit
}
