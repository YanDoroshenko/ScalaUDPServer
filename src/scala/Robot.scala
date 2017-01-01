package scala

import java.io.{FileInputStream, FileOutputStream}
import java.net.{DatagramPacket, DatagramSocket, InetAddress}

/**
  * Created by Yan Doroshenko (yandoroshenko@protonmail.com) on 31.12.16.
  */
object Robot {

  val socket = new DatagramSocket(4545)
  var id: (Byte, Byte, Byte, Byte) = _
  val address = InetAddress.getByName("localhost")
  var length: Int = 10

  def handshake = {
    val id = Array[Byte](0, 0, 0, 0)
    val seq = Array[Byte](0, 0)
    val confirmation = Array[Byte](0, 0)
    val flag: Byte = 2
    val data = Array[Byte](1)
    val packetData = id ++ seq ++ confirmation ++ Array(flag) ++ data
    val packet = new DatagramPacket(packetData, packetData.length, address, 4445)
    socket.send(packet)
    socket.receive(packet)
    val response = packet.getData
    Robot.id = (response(0), response(1), response(2), response(3))

  }

  def send = {
    val fis = new FileInputStream("/home/yan/logo.png")
    val data = Stream.continually(fis.read).takeWhile(-1 !=).map(_.toByte).toArray
    length = data.length
    println(length)
    socket.send(new DatagramPacket(data, data.length, address, 4445))
    val packet = new DatagramPacket(data, 100)
    socket.receive(packet)
    val received: String = new String(packet.getData(), 0, packet.getLength())
    received
  }

  def receive = {

  }

  def main(args: Array[String]): Unit = {
    new TestServer().start()
    send
  }

  class TestServer extends Thread {
    private var socket = new DatagramSocket(4445)
    private var running = false
    private val buf: Array[Byte] = Array.fill(46894) {
      0
    }

    override def run() {
      running = true
      while (running) {
        var packet = new DatagramPacket(buf, buf.length)
        socket.receive(packet)
        val address = packet.getAddress
        val port = packet.getPort
        packet = new DatagramPacket(buf, buf.length, address, port)
        val received = packet.getData
        /*received.foreach(print)
        println()*/
        val fos = new FileOutputStream("newlogo.png")
        fos.write(received)
        fos.close()
        println("Done")
        socket.send(packet)
      }
      println("End")
      socket.close()
    }
  }

  class TestClient {
    private var socket = new DatagramSocket()
    private var address = InetAddress.getByName("localhost")
    private var buf: Array[Byte] = _

    def sendEcho(msg: String): String = {
      buf = msg.getBytes
      var packet = new DatagramPacket(buf, buf.length, address, 4445)
      socket.send(packet)
      packet = new DatagramPacket(buf, buf.length)
      socket.receive(packet)
      val received: String = new String(packet.getData(), 0, packet.getLength())
      println(received)
      received
    }

    def close() {
      socket.close()
    }
  }

}