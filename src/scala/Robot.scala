package scala

import java.io.{FileInputStream, FileOutputStream}
import java.net.{DatagramPacket, DatagramSocket, InetAddress}

/**
  * Created by Yan Doroshenko (yandoroshenko@protonmail.com) on 31.12.16.
  */
object Robot {

  val socket = new DatagramSocket(4545)
  var id: Array[Byte] = _
  val address = InetAddress.getByAddress(Array(192, 168, 56, 101).map(_.toByte))
  var length: Int = 10

  def handshake = {
    println("Handshake")
    var id = Array[Byte](0, 0, 0, 0)
    var seq = Array[Byte](0, 0)
    var confirmation = Array[Byte](0, 0)
    var flag: Byte = 4
    var data = Array[Byte](1)
    val packetData = id ++ seq ++ confirmation ++ Array(flag) ++ data
    val outPacket = new DatagramPacket(packetData, packetData.length, address, 4000)
    socket.send(outPacket)
    val inPacket = new DatagramPacket(packetData, packetData.length)
    socket.receive(inPacket)
    println("Received handshake")
    val response = inPacket.getData.map(_ & 0xff).map(_.toByte)
    id = response.take(4)
    seq = Array(response(4), response(5))
    confirmation = Array(response(6), response(7))
    flag = response(8)
    data = response.drop(9)
    println(id.mkString("id: [", ",", "]"))
    println(seq.mkString("seq: [", ",", "]"))
    println(confirmation.mkString("conf: [", ",", "]"))
    println("flag: " + flag)
    println(data.mkString("data" + data.length + ": [", ",", "]"))
    Robot.id = id
    checkHandshake(id, seq, confirmation, flag, data)
  }

  def checkHandshake(id: Array[Byte], seq: Array[Byte], confirmation: Array[Byte], flag: Byte, data: Array[Byte]) = {
    println("Checking handshake")
    if (id.sum == 0) {
      System.err.println("Id is zero")
      terminate
    }
    else if (seq.sum != 0) {
      System.err.println("Seq is non-zero")
      terminate
    }
    else if (confirmation.sum != 0) {
      System.err.println("Confirmation is non-zero")
      terminate
    }
    else if (flag != 4) {
      System.err.println("Wrong flag")
      terminate
    }
    else if (data.size != 1 || data(0) > 2) {
      System.err.println("Wrong data")
      terminate
    }
  }

  def terminate = {
    println("Closing connection")
    val id = Robot.id
    val seq = Array[Byte](0, 0)
    val confirmation = Array[Byte](0, 0)
    val flag: Byte = 0
    val packetData = id ++ seq ++ confirmation ++ Array(flag)
    val outPacket = new DatagramPacket(packetData, packetData.length, address, 4000)
    socket.send(outPacket)
    socket.close()
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
    val fos = new FileOutputStream("image.png")
    while (!socket.isClosed) {
      var id = Array[Byte](0, 0, 0, 0)
      var seq = Array[Byte](0, 0)
      var confirmation = Array[Byte](0, 0)
      var flag: Byte = 0
      var data: Array[Byte] = Array.fill(255)(0)
      val packetData = id ++ seq ++ confirmation ++ Array(flag) ++ data
      val inPacket = new DatagramPacket(packetData, packetData.length)
      socket.receive(inPacket)
      val response = inPacket.getData.map(_ & 0xff).map(_.toByte)
      id = response.take(4)
      seq = Array(response(4), response(5))
      confirmation = Array(response(6), response(7))
      flag = response(8)
      data = response.drop(9)
      println(id.mkString("id: [", ",", "]"))
      println(seq.mkString("seq: [", ",", "]"))
      println(confirmation.mkString("conf: [", ",", "]"))
      println("flag: " + flag)
      println(data.mkString("data (" + data.length + "): [", ",", "]"))
      if (!data.forall(_ == 0)) {
        fos.write(data)
        id = Robot.id
        seq = Array(0, 255).map(_.toByte)
        confirmation = Array(1, 255).map(_.toByte)
        flag = 0
        data = Array(0).map(_.toByte)
        val confirm = id ++ seq ++ confirmation ++ Array(flag) ++ data
        val outPacket = new DatagramPacket(confirm, confirm.length, address, 4000)
      }
      if (flag == 1)
        terminate
      if (flag == 2)
        socket.close()
    }
    fos.close()
  }

  def main(args: Array[String]): Unit = {
    /*new TestServer().start()
    send*/
    handshake
    receive
    terminate
  }
}