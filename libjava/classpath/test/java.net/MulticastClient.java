/* Test Multicast Sockets */

import java.net.*;
import java.io.*;

public class MulticastClient
{

public static void
main(String[] argv) throws IOException
{
  System.out.println("Starting multicast tests");
  System.out.println("NOTE: You need to do an 'ifconfig <interface> " +
                     "multicast' or this will fail on linux");

  byte[] buf = { 'H', 'e', 'l', 'l', 'o', ',', ' ', 'W', 'o','r','l','d' };

  /* Simple Send */
  System.out.println("Test 1: Multicast send/receive test");
  try
    {
      InetAddress addr = InetAddress.getByName("234.0.0.1");

      MulticastSocket s = new MulticastSocket();
      DatagramPacket p = new DatagramPacket(buf, buf.length, addr, 3333);

      s.joinGroup(addr);
      s.send(p);
      s.close();
    }
  catch(IOException e)
    {
      System.out.println("FAILED: simple multicast send: " + e);
    }

  /* Options */
  System.out.println("Test 2: Multicast socket options");
  try
    {
      InetAddress addr;
      MulticastSocket s = new MulticastSocket();

      System.out.println("TTL = " + s.getTTL());
      System.out.println("Setting TTT to 121");
      s.setTTL((byte)12);
      System.out.println("TTL = " + s.getTTL());

      InetAddress oaddr = s.getInterface();
      System.out.println("Multicast Interface = " + oaddr);
      System.out.println("Setting interface to localhost");
      addr = InetAddress.getByName("198.211.138.177");
      s.setInterface(addr);
      System.out.println("Multicast Interface = " + s.getInterface());
      System.out.println("Setting interface to " + oaddr);
      s.setInterface(oaddr);
      System.out.println("Multicast Interface = " + s.getInterface());
    }
  catch(IOException e)
    {
      System.out.println("FAILED: multicast options: " + e);
    }
}

}

