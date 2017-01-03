package scala

import java.io.FileInputStream
import java.net.{DatagramPacket, DatagramSocket, InetAddress}

/**
  * Created by Yan Doroshenko (yandoroshenko@protonmail.com) on 31.12.16.
  */
object Robot {

  val socket = new DatagramSocket(4545)
  var id: (Byte, Byte, Byte, Byte) = _
  val address = InetAddress.getByAddress(Array(192, 168, 56, 101).map(_.toByte))
  var length: Int = 10

  def handshake = {
    println("Handshake")
    val id = Array[Byte](0, 0, 0, 0)
    val seq = Array[Byte](0, 0)
    val confirmation = Array[Byte](0, 0)
    val flag: Byte = 2
    val data = Array[Byte](1)
    val packetData = id ++ seq ++ confirmation ++ Array(flag) ++ data
    val outPacket = new DatagramPacket(packetData, packetData.length, address, 4000)
    socket.send(outPacket)
    println("Packet sent")
    val inPacket = new DatagramPacket(packetData, packetData.length)
    println("Incoming packet created")
    socket.receive(inPacket)
    println("Packet recieved")
    val response = inPacket.getData
    println("Response got")
    Robot.id = (response(0), response(1), response(2), response(3))
    println(Robot.id)
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
    /*new TestServer().start()
    send*/
    handshake
  }
}