/* Mulitcast Server Socket for testing */

import java.io.*;
import java.net.*;

public class MulticastServer 
{

private MulticastSocket s;

public static void
main(String[] argv) throws IOException
{
  MulticastServer ms = new MulticastServer(3333);
  ms.run();
}

public
MulticastServer(int port) throws IOException
{
  s = new MulticastSocket(port);
  System.out.println("Server multicast socket created");
}

public void 
run()
{
  try
    {
      byte[] buf = new byte[255];

      DatagramPacket p = new DatagramPacket(buf, buf.length);
      InetAddress addr = InetAddress.getByName("234.0.0.1");

      p.setLength(buf.length);

      System.out.println("Joining multicast group");
      s.joinGroup(addr);
      System.out.print("Receiving ...");
      s.receive(p);
      System.out.println("");
      s.leaveGroup(addr);
      System.out.println("ServerDatagram: received " + p.getLength() +
         " bytes from " + p.getAddress().getHostName() + ":" +
         p.getPort());
      System.out.println("Data: " + new String(p.getData()));

      System.out.println("PASSED multicast server test");
    }
  catch (IOException e)
    {
      System.out.println("FAILED: MulticastServer caught an exception: " + e);
    }
}

}

