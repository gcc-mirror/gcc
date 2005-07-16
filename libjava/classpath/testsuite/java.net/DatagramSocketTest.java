import java.net.*;

public class DatagramSocketTest
{
  public static void main(String args[])
    {
      try {
	DatagramSocket socket = new DatagramSocket();

	InetAddress local = socket.getLocalAddress();

	int port = socket.getLocalPort();

	socket.setSoTimeout(socket.getSoTimeout());

	socket.close();

	System.out.println("PASSED: new DatagramSocket()");
      } catch (Exception e) {
	System.out.println("FAILED: " + e);
      }
    }
}
