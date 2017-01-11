package scala

import java.io.{FileInputStream, FileOutputStream}
import java.net.{DatagramPacket, DatagramSocket, InetAddress}

import scala.collection.mutable.ListBuffer

/**
  * Created by Yan Doroshenko (yandoroshenko@protonmail.com) on 31.12.16.
  */
object Robot {

  val socket = new DatagramSocket(4545)
  var id: Array[Byte] = _
  val address = InetAddress.getByAddress(Array(127, 0, 0, 1).map(_.toByte))
  val arr = ListBuffer[Int]()
  var length: Int = 10
  var toDownloadSeq = 0
  var count = 0

  val buffer: collection.mutable.Map[Int, Packet] = collection.mutable.Map[Int, Packet]()

  class Packet(private val frame: Array[Byte]) {
    val id = frame take 4
    val seq = frame slice(4, 6)
    var conf = frame slice(6, 8)
    val flag = frame(8)
    val data = frame drop 9

    def getFrame = id ++ seq ++ conf ++ Array(flag)

    def seqInt = (seq(0).toInt & 0xff) * 256 + (seq(1).toInt & 0xff)
  }

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
    println("-------HANDSHAKE-------")
    println(id.mkString("id: [", ",", "]"))
    println(seq.mkString("seq: [", ",", "]"))
    println(confirmation.mkString("conf: [", ",", "]"))
    println("flag: " + flag)
    println(data.mkString("data" + data.length + ": [", ",", "]"))
    println("-------HANDSHAKE-------")
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
    println("Terminating connection")
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
    val fos = new FileOutputStream("small.png")
    while (!socket.isClosed) {
      var id = Array[Byte](0, 0, 0, 0)
      var seq = Array[Byte](0, 0)
      var confirmation = Array[Byte](0, 0)
      var flag: Byte = 0
      var data: Array[Byte] = Array.fill(255)(0)
      val packetData = id ++ seq ++ confirmation ++ Array(flag) ++ data
      val inPacket = new DatagramPacket(packetData, packetData.length)
      while (!socket.isClosed) {
        if (toDownloadSeq > 65536)
          toDownloadSeq -= 65536
        socket.receive(inPacket)
        val packet = new Packet(inPacket.getData)
        println("Got packet " + packet.seqInt)
        if (packet.flag == 1)
          terminate
        else if (packet.flag == 2) {
          val fin = new DatagramPacket(Robot.id ++ Array(0, 0).map(_.toByte) ++ packet.seq ++ Array(2.toByte), 9, address, 4000)
          socket.send(fin)
          socket.close()
        }
        else if (inPacket.getLength != 264) {
          println("End detected")
          val data = inPacket.getData.drop(9)
          val seqInt = packet.seqInt + inPacket.getLength - 9
          if (!arr.contains(seqInt)) {
            fos.write(data take (inPacket.getLength - 9))
            arr += packet.seqInt
            fos.flush()
          }
          packet.conf = Array(seqInt / 256, seqInt % 256).map(_.toByte)
          val confirmation = new DatagramPacket(Robot.id ++ packet.seq ++ packet.conf ++ Array(0.toByte), 9, address, 4000)
          socket.send(confirmation)
        }
        else if (packet.seqInt == toDownloadSeq) {
          println("Writing packet " + packet.seqInt + "(" + count + ")")
          if (!arr.contains(packet.seqInt)) {
            fos.write(packet.data)
            arr += packet.seqInt
            fos.flush()
          }
          count += 255
          toDownloadSeq += 255
          while (buffer.contains(toDownloadSeq)) {
            val next = buffer(toDownloadSeq)
            println("Writing packet " + next.seqInt + "(" + count + ") from buffer")
            if (!arr.contains(next.seqInt)) {
              fos.write(next.data)
              arr += next.seqInt
              fos.flush()
            }
            count += 255
            toDownloadSeq += 255
            buffer -= next.seqInt
          }
          println(toDownloadSeq + "(" + toDownloadSeq / 256 + "," + toDownloadSeq % 256 + ") is now needed")
          packet.conf = Array(toDownloadSeq / 256, toDownloadSeq % 256).map(_.toByte)
          println("Sending confirmation " + ((packet.conf(0).toInt & 0xff) * 256 + (packet.conf(1).toInt & 0xff)))
          val confirmation = new DatagramPacket(packet.getFrame, packet.getFrame.length, address, 4000)
          socket.send(confirmation)

        }
        else if (packet.seqInt > toDownloadSeq && !buffer.contains(toDownloadSeq)) {
          println("Adding packet with seq " + packet.seqInt + " to buffer, " + toDownloadSeq + " needed")
          buffer += (packet.seqInt -> packet)
          packet.conf = Array(toDownloadSeq / 256, toDownloadSeq % 256).map(_.toByte)
          val confirmation = new DatagramPacket(packet.getFrame, packet.getFrame.length, address, 4000)
          socket.send(confirmation)
        }
        else if (toDownloadSeq - packet.seqInt == 2040) {
          packet.conf = Array(toDownloadSeq / 256, toDownloadSeq % 256).map(_.toByte)
          println("Sending confirmation " + ((packet.conf(0).toInt & 0xff) * 256 + (packet.conf(1).toInt & 0xff)) + " AGAIN")
          val confirmation = new DatagramPacket(packet.getFrame, packet.getFrame.length, address, 4000)
          socket.send(confirmation)
        }
      }
    }
    println("Closing file stream")
    fos.close()
    println(arr.mkString("Written to file: [", ",", "]"))
    println("Connection id was " + Robot.id.mkString("[", ",", "]"))
  }

  def main(args: Array[String]): Unit = {
    /*new TestServer().start()
    send*/
    handshake
    receive
  }
}