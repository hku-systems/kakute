package edu.hku.cs.network

import edu.hku.cs.DFTEnv
import io.netty.bootstrap.{Bootstrap, ServerBootstrap}
import io.netty.channel.{ChannelInitializer, SimpleChannelInboundHandler}
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.{NioServerSocketChannel, NioSocketChannel}
import io.netty.handler.codec.serialization.{ClassResolvers, ObjectDecoder, ObjectEncoder}

/**
  * Created by jianyu on 3/8/17.
  */
trait EndPoint {
  val id: String
  // def onReceive(message: Message): Unit
  def receiveAndReply(message: Message): Message
  def send(obj: Message): Unit
}

trait EndpointRegister {
  def register(endPoint: EndPoint)
}

trait NettyHandle {
  def sendMsg(obj: Any): Unit
}

abstract class NettyEndpoint extends EndPoint {

  private var nettyHandle: NettyHandle = _

  override def send(obj: Message): Unit = {
    nettyHandle.sendMsg(BoxMessage(this.id, obj))
  }

  def setHandle(_nettyHandle: NettyHandle): Unit = {
    nettyHandle = _nettyHandle
  }
}

sealed trait Message

case class BoxMessage(endpointId: String, message: Message)

case class onStart(int: Int, string: String, double: Double) extends Message

case class onStarted(int: Int) extends Message

case class onEnded(int: Int) extends Message

case class EndpointError(string: String) extends Message

