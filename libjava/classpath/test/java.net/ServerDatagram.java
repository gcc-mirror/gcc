/* Server Datagram Socket for testing */

import java.io.*;
import java.net.*;

public class ServerDatagram implements Runnable
{

private DatagramSocket s;

public static void
main(String[] argv) throws IOException
{
  ServerDatagram sd = new ServerDatagram(37900);
  sd.run();
}

public
ServerDatagram(int port) throws SocketException
{
  s = new DatagramSocket(port);
  System.out.println("Server datagram socket created");
}

public void 
run()
{
  try
    {
      byte[] buf = new byte[65535];

      DatagramPacket p = new DatagramPacket(buf, buf.length);

      p.setLength(buf.length);

      s.receive(p);
      System.out.println("ServerDatagram: received " + p.getLength() +
         " bytes from " + p.getAddress().getHostName() + ":" +
         p.getPort());

      if (p.getLength() != 65332)
        throw new IOException("Incorrect data size");
      System.out.println("PASSED max values test");
    }
  catch (IOException e)
    {
      System.out.print("FAILED: ServerDatagram caught an exception: " + e);
    }
}

}

