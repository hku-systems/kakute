package edu.hku.cs.dft.network

import edu.hku.cs.dft.DFTEnv
import io.netty.bootstrap.ServerBootstrap
import io.netty.channel.ChannelInitializer
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel.socket.SocketChannel
import io.netty.channel.socket.nio.NioServerSocketChannel
import io.netty.handler.codec.serialization.{ClassResolvers, ObjectDecoder, ObjectEncoder}

/**
  * Created by jianyu on 3/8/17.
  */
class NettyServer(endpointDispatcher: EndpointDispatcher, dFTEnv: DFTEnv) extends EndpointRegister {

  var nettyHandle: NettyHandle = _

  private var bossGroup: NioEventLoopGroup = _

  private var workerGroup: NioEventLoopGroup = _

  override def stop(): Unit = {
    bossGroup.shutdownGracefully()
    workerGroup.shutdownGracefully()
  }

  override def register(endPoint: EndPoint): Unit = {
    endPoint.asInstanceOf[NettyEndpoint].setHandle(nettyHandle)
    endpointDispatcher.registerEndpoint(endPoint)
    super.register(endPoint)
  }

  def run() {
    // create it in new thread ?
    bossGroup = new NioEventLoopGroup()
    workerGroup = new NioEventLoopGroup()
    try {
      val bootstrapServer = new ServerBootstrap()
      bootstrapServer.group(bossGroup, workerGroup)
        .channel(classOf[NioServerSocketChannel])
        .childHandler(new ChannelInitializer[SocketChannel] {
          override def initChannel(ch: SocketChannel): Unit = {
            ch.pipeline().addLast(new ObjectDecoder(ClassResolvers.cacheDisabled(null)))
            ch.pipeline().addLast(new ObjectEncoder)
            nettyHandle = new NettyServerHandler(endpointDispatcher.onReceive)
            ch.pipeline().addLast(nettyHandle.asInstanceOf[NettyServerHandler])
          }
        })

      val f = bootstrapServer.bind(dFTEnv.serverHost, dFTEnv.serverPort).sync()
      f.channel().closeFuture().sync()
    } finally {
      workerGroup.shutdownGracefully()
      bossGroup.shutdownGracefully()
    }
  }
}

object NettyServer {
  def main(args: Array[String]): Unit = {
    DFTEnv.init()
    val nettyServer = new NettyServer(new EndpointDispatcher, DFTEnv.dFTEnv)
    new Thread(new Runnable {
      override def run(): Unit = {
        nettyServer.run()
      }
    }).start()
//    nettyServer.register(new RuleEndpoint)
  }
}