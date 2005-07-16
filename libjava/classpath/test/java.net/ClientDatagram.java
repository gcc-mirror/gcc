/* Class to test Datagrams from a client perspective */

import java.io.*;
import java.net.*;

public class ClientDatagram
{

public static void
main(String[] argv) throws IOException
{
  System.out.println("Starting datagram tests");

  byte[] buf = new byte[2048];
  DatagramPacket p = new DatagramPacket(buf, buf.length);
  InetAddress addr = InetAddress.getByName("localhost");

  /* Execute the daytime UDP service on localhost.  You may need to
     enable this in inetd to make the test work */
  System.out.println("Test 1: Simple daytime test");
  try
    {
      
      DatagramSocket s = new DatagramSocket();

      System.out.println("Socket bound to " + s.getLocalAddress() +
                         ":" + s.getLocalPort());

      byte[] sbuf = { 'H', 'I' };
      DatagramPacket spack = new DatagramPacket(sbuf, sbuf.length, addr, 13);

      s.send(spack);
      s.receive(p);

      System.out.println("Received " + p.getLength() + " bytes from " +
                         p.getAddress() + ":" + p.getPort());
      
      byte[] tmp = new byte[p.getLength()];
      for (int i = 0; i < p.getLength(); i++)
        tmp[i] = buf[i];

      System.out.print("Data: " + new String(tmp));

      s.close();
      System.out.println("PASSED simple datagram test");
    }
  catch(Exception e)
    {
      System.out.println("FAILED simple datagram test: " + e);
    }

  System.out.println("Test 2: Specific host/port binding");
  try
    {
      DatagramSocket s = new DatagramSocket(8765, addr);
      if (s.getLocalPort() != 8765) 
        throw new IOException("Bound to wrong port: " + s.getLocalPort());

      if (!s.getLocalAddress().equals(addr))
        throw new IOException("Bound to wrong host:" + s.getLocalAddress());

      s.close();
      System.out.println("PASSED specific host/port binding test");
    }
  catch (Exception e)
    {
      System.out.println("FAILED specific host/port binding: " + e);
    }
 
  System.out.println("Test 3: Socket Options test");
  try
    {
      DatagramSocket s = new DatagramSocket();
      System.out.println("SO_TIMEOUT = " + s.getSoTimeout());       
      System.out.println("Setting SO_TIMEOUT to 170");
      s.setSoTimeout(170);
      System.out.println("SO_TIMEOUT = " + s.getSoTimeout());       
      System.out.println("Setting SO_TIMEOUT to 0");
      s.setSoTimeout(0);
      System.out.println("SO_TIMEOUT = " + s.getSoTimeout());       
      s.close();
    }
  catch(Exception e)
    {
      System.out.println("WARNING: Problem with SO_TIMEOUT test: " + e.getMessage());
      System.out.println("This is ok on Linux");
    }

  System.out.println("Test 4: Max values test");
  try
    {
//      ServerDatagram sd = new ServerDatagram(37900);
//      sd.run();

      DatagramSocket s = new DatagramSocket();
      byte[] sbuf = new byte[65332];
      DatagramPacket spack = new DatagramPacket(sbuf, sbuf.length, 
                                                addr, 37900);

      s.send(spack);
      s.close();
    }
  catch (Exception e)
    {
      System.out.println("FAILED max values test: " + e);
    }

  System.out.println("Datagram testing complete");
}

}

