/* A class to test my client TCP socket implementation */

import java.net.*;
import java.io.*;

public class ClientSocket extends Object
{
public static void
main(String[] argv) throws IOException
{
  System.out.println("Starting client stream socket test");

  /* Simple connection and read test */
  System.out.println("Test 1: Connection to daytime port on local host");
  try
    {
      InetAddress addr = InetAddress.getByName("127.0.0.1");

      Socket s = new Socket(addr, 13);

      InputStream is = s.getInputStream();
      BufferedReader br = new BufferedReader(new InputStreamReader(is));

      for (String str = br.readLine(); ; str = br.readLine())
       {
         if (str == null)
           break;
         System.out.println(str);
       }
      s.close();
      System.out.println("PASSED: daytime test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: daytime test: " + e);
    }
   
  /* Simple connection refused test */
  System.out.println("Test 2: Connection refused test");
  try
    {
      InetAddress addr = InetAddress.getByName("127.0.0.1");

      Socket s = new Socket(addr, 47);
      s.close();

      System.out.print("WARNING: Cannot perform connection refused test");
      System.out.println(" because someone is listening on localhost:47");
    }
  catch(IOException e)
    {
      System.out.println("PASSED: connection refused test: " + e.getMessage());
    }
   
  /* Socket attributes test */
  System.out.println("Test 3: Connection attributes");  
  try
    {
      Socket s = new Socket("www.netscape.com", 80);

      String laddr = s.getLocalAddress().getHostName();
      int lport = s.getLocalPort();
      String raddr = s.getInetAddress().getHostName();
      int rport = s.getPort();

      System.out.println("Local Address is: " + laddr);
      System.out.println("Local Port is: " + lport);
      System.out.println("Remote Address is: " + raddr);
      System.out.println("Remote Port is: " + rport);
      System.out.println("Socket.toString is: " + s);

      if ( (laddr == null) ||
          ((lport < 0) || (lport > 65535)) ||
           (raddr.indexOf("netscape.com") == -1) ||
           (rport != 80))
        System.out.println("FAILED: connection attribute test");
      else
        System.out.println("PASSED: connection attribute test");

      s.close();
    }
  catch(IOException e)
    {
      System.out.println("FAILED: connection attributes test: " + e.getMessage());
    }
   
  /* Socket options test */
  System.out.println("Test 4: Socket options");
  Socket s = new Socket("127.0.0.1", 23);

  try
    {
      // SO_TIMEOUT
      System.out.println("SO_TIMEOUT = " + s.getSoTimeout());
      System.out.println("Setting SO_TIMEOUT to 142");
      s.setSoTimeout(142);
      System.out.println("SO_TIMEOUT = " + s.getSoTimeout());
      System.out.println("Setting SO_TIMEOUT to 0");
      s.setSoTimeout(0);
      System.out.println("SO_TIMEOUT = " + s.getSoTimeout());
    }
  catch (IOException e)
    {
      System.out.println("WARNING: SO_TIMEOUT problem: " + e.getMessage());
      System.out.println("This is ok on Linux");
    }
  try
    {
      // Try TCP_NODELAY
      System.out.println("TCP_NODELAY = " + s.getTcpNoDelay());
      System.out.println("Setting TCP_NODELAY to true");
      s.setTcpNoDelay(true);
      System.out.println("TCP_NODELAY = " + s.getTcpNoDelay());
      System.out.println("Setting TCP_NODELAY to false");
      s.setTcpNoDelay(false);
      System.out.println("TCP_NODELAY = " + s.getTcpNoDelay());

      // Try SO_LINGER
      System.out.println("SO_LINGER = " + s.getSoLinger());
      System.out.println("Setting SO_LINGER to 100");
      s.setSoLinger(true, 100);
      System.out.println("SO_LINGER = " + s.getSoLinger());
      System.out.println("Setting SO_LINGER to off");
      s.setSoLinger(false, 0);
      System.out.println("SO_LINGER = " + s.getSoLinger());

      System.out.println("PASSED: socket options test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: socket options test: " + e.getMessage());
    }
  s.close();

  /* Simple read/write test */
  System.out.println("Test 5: Simple read/write test");
  try
    {
      System.out.println("Downloading the Transmeta homepage");
      s = new Socket("www.transmeta.com", 80);
      
      BufferedReader in = new BufferedReader(new 
                          InputStreamReader(s.getInputStream()));
      PrintWriter out = new PrintWriter(new 
                          OutputStreamWriter(s.getOutputStream()));

      out.print("GET /\r\n");
      out.flush();
      
      for (String str = in.readLine(); ; str = in.readLine())
       {
         if (str == null)
           break;
         System.out.println(str);
       }

      s.close();
      System.out.println("PASSED: simple read/write test");
    }
  catch(IOException e)
    {
      System.out.println("FAILED: simple read/write test: " + e.getMessage());
    }

  /* Connect to our server socket */
  System.out.println("Test 6: Connect to ServerSocket");
  try
    {
       s = new Socket("localhost", 9999);

       PrintWriter out = new PrintWriter(new
                             OutputStreamWriter(s.getOutputStream()));

       out.println("Hello, there server socket");
       out.print("I'm dun");
       out.flush();
       s.close();
       System.out.println("PASSED: connect to server socket");
    }
  catch(Exception e)
    {
      System.out.println("FAILED: connect to server socket: " + e);
    }

  System.out.println("Client stream socket test complete");
}

}

