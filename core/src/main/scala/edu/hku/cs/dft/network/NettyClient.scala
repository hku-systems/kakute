package edu.hku.cs.dft.network

import java.io.ObjectOutputStream
import java.net.{InetAddress, Socket}

import edu.hku.cs.dft.DFTEnv
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

  private var eventLoopGroup: NioEventLoopGroup = _

  override def stop(): Unit = eventLoopGroup.shutdownGracefully()

  override def register(endPoint: EndPoint): Unit = {
    endPoint.asInstanceOf[NettyEndpoint].setHandle(nettyHandle)
    endpointDispatcher.registerEndpoint(endPoint)
    super.register(endPoint)
  }

  def run(): Unit = {
    eventLoopGroup = new NioEventLoopGroup()
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

class TestClass extends Serializable {
  private val r = 1
  private val m = 2
  val a: Array[Int] = new Array[Int](3)
  def add(): Int = r + m
  class InClass extends Serializable {
    val arr: Array[_] = new Array[Int](3)
  }
  val inClass: InClass = new InClass
}

object NettyClient{
  def main(args: Array[String]): Unit = {
/*    DFTEnv.init()
    val nettyClient = new NettyClient(new EndpointDispatcher, DFTEnv.dftEnv())
    new Thread(new Runnable {
      override def run() {
        nettyClient.run()
      }
    }).start()
    Thread.sleep(1000)*/

    val client = new Socket(InetAddress.getByName("localhost"), 9999)
    val out = new ObjectOutputStream(client.getOutputStream)
    out.writeObject(new TestClass)

    //    val rule = new RuleEndpoint
//    nettyClient.register(rule)
//    rule.send(onStart(1, "aa", 1.5))
  }
}