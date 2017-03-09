package edu.hku.cs.network

import edu.hku.cs.{DFTEnv, SampleMode, TrackingMode}
import io.netty.bootstrap.Bootstrap
import io.netty.channel.ChannelInitializer
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioSocketChannel
import io.netty.handler.codec.serialization.{ClassResolvers, ObjectDecoder, ObjectEncoder}

/**
  * Created by jianyu on 3/8/17.
  */
class NettyClient(endpointDispatcher: EndpointDispatcher, dFTEnv: DFTEnv) extends EndpointRegister {

  private var nettyHandle: NettyHandle = _

  override def register(endPoint: EndPoint): Unit = {
    endPoint.asInstanceOf[NettyEndpoint].setHandle(nettyHandle)
    endpointDispatcher.registerEndpoint(endPoint)
    super.register(endPoint)
  }

  def run(): Unit = {
    val eventLoopGroup = new NioEventLoopGroup()
    try {
      val boostrap = new Bootstrap()
      boostrap.group(eventLoopGroup)
        .channel(classOf[NioSocketChannel])
        .handler(new ChannelInitializer[SocketChannel] {
          override def initChannel(c: SocketChannel) {
            c.pipeline().addLast(new ObjectEncoder)
            c.pipeline().addLast(new ObjectDecoder(ClassResolvers.cacheDisabled(null)))
            nettyHandle = new NettyClientHandler(endpointDispatcher.onReceive)
            c.pipeline().addLast(nettyHandle.asInstanceOf[NettyClientHandler])
          }
        })

      val f = boostrap.connect(dFTEnv.serverHost, dFTEnv.serverPort).sync()
      f.channel().closeFuture().sync()
    } finally {
      eventLoopGroup.shutdownGracefully()
    }
  }

}

/*
class RuleEndpoint extends NettyEndpoint {
  val id: String = "Rule"

  def receiveAndReply(message: Message): Message = {
    println(message)
    message match {
      case _: onStart => onStarted(1)
      case _: onStarted => onEnded(1)
      case _ => null
    }
  }
}
*/

object NettyClient{
  def main(args: Array[String]): Unit = {
    DFTEnv.init()
    val nettyClient = new NettyClient(new EndpointDispatcher, DFTEnv.dFTEnv)
    new Thread(new Runnable {
      override def run() {
        nettyClient.run()
      }
    }).start()
    Thread.sleep(1000)
//    val rule = new RuleEndpoint
//    nettyClient.register(rule)
//    rule.send(onStart(1, "aa", 1.5))
  }
}