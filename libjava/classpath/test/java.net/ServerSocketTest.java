/* Class to test server sockets */

import java.io.*;
import java.net.*;

public class ServerSocketTest extends ServerSocket
{

public
ServerSocketTest(int port) throws IOException
{
  super(port);
}

public static void
main(String[] argv)
{
  System.out.println("Starting up server socket");

  try {
  ServerSocketTest ss = new ServerSocketTest(9999);

  System.out.println("Created server socket bound to port " +
                     ss.getLocalPort() + " on local address " +
                     ss.getInetAddress());

  SubSocket s = new SubSocket();
  ss.implAccept(s);
//  Socket s = ss.accept();

  System.out.println("Got a connection from " + s.getInetAddress() +
                     " on port " + s.getPort());

  BufferedReader br = new BufferedReader(new 
                          InputStreamReader(s.getInputStream()));

  for (String str = br.readLine(); ; str = br.readLine())
    {
      if (str == null)
        break;
      System.out.println(str);
    }
  s.close();
  ss.close();
  System.out.println("PASSED: server socket test");
  }
  catch (Exception e) {
  System.out.println("FAILED: server socket test: " + e);
  }
}

}

