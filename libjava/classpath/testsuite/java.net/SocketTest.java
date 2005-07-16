import java.net.*;

public class SocketTest
{
  public static void main(String args[])
    {
      try {
	Socket socket = new Socket("www.hungry.com", 80);

	InetAddress remote = socket.getInetAddress();
	InetAddress local = socket.getLocalAddress();

        int rport = socket.getPort();
	int lport = socket.getLocalPort();

	socket.setSoTimeout(socket.getSoTimeout());
	socket.setTcpNoDelay(socket.getTcpNoDelay());
	int linger = socket.getSoLinger();
	if (-1 != linger)
		socket.setSoLinger(true, linger);
	else
		socket.setSoLinger(false, 0);

	String socketString = socket.toString();
	if (null == socketString)
		throw new Exception("toString() failed");

	socket.close();
	System.out.println("PASSED: new Socket()" + socketString);
      } catch (Exception e) {
	System.out.println("FAILED: " + e);
      }
    }
}
