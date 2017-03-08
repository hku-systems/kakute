/*
package edu.hku.cs.Optimization

import java.nio.channels.SocketChannel

import io.netty.bootstrap.ServerBootstrap
import io.netty.buffer.ByteBuf
import io.netty.channel.nio.NioEventLoopGroup
import io.netty.channel._

/**
  * Created by jianyu on 3/8/17.
  */
class NettyHandle {

}

class DiscardServerHandler extends ChannelInboundHandlerAdapter {
  override def channelRead(ctx: ChannelHandlerContext, msg: scala.Any) = {
    msg.asInstanceOf[ByteBuf].release()
  }

  override def exceptionCaught(ctx: ChannelHandlerContext, cause: Throwable) = {
    cause.printStackTrace()
    ctx.close()
  }

  override def channelActive(ctx: ChannelHandlerContext): Unit = super.channelActive(ctx)

  override def channelUnregistered(ctx: ChannelHandlerContext): Unit = super.channelUnregistered(ctx)

  override def channelInactive(ctx: ChannelHandlerContext): Unit = super.channelInactive(ctx)

  override def channelWritabilityChanged(ctx: ChannelHandlerContext): Unit = super.channelWritabilityChanged(ctx)

  override def userEventTriggered(ctx: ChannelHandlerContext, evt: scala.Any): Unit = super.userEventTriggered(ctx, evt)

  override def channelRegistered(ctx: ChannelHandlerContext): Unit = super.channelRegistered(ctx)

  override def channelReadComplete(ctx: ChannelHandlerContext): Unit = super.channelReadComplete(ctx)

  override def handlerRemoved(ctx: ChannelHandlerContext): Unit = super.handlerRemoved(ctx)

  override def handlerAdded(ctx: ChannelHandlerContext): Unit = super.handlerAdded(ctx)
}

class DiscardServer {
  def run() {
    val bossGroup = new NioEventLoopGroup()
    val workerGroup = new NioEventLoopGroup()
    try {
      val bootstrapServer = new ServerBootstrap()
      bootstrapServer.group(bossGroup, workerGroup)
        .channel(classOf[DiscardServerHandler])
        .childHandler(new ChannelInitializer[SocketChannel] {
          override def initChannel(ch: SocketChannel): Unit = {
            ch.asInstanceOf[Channel].pipeline().addLast(new DiscardServerHandler)
          }
        })
        .option(ChannelOption.SO_BACKLOG, 128)
        .childOption(ChannelOption.SO_KEEPALIVE, true)

      val f = bootstrapServer.bind(1111).sync()

      f.channel().closeFuture().sync()
    } finally {
      workerGroup.shutdownGracefully()
      bossGroup.shutdownGracefully()
    }
  }
}

object DiscardServer {
  def main(arg: Array[String]): Unit = {
    new DiscardServer().run()
  }
}*/
