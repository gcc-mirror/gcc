import java.net.*;
import java.io.*;

/*
 * Start one thread for receiving a packet, wait for it to set up,
 * send a packet to it, and wait until it completes.  Compare the
 * packet to make sure it came thru without errors.
 */

public class SocketSendReceiveTest
  implements Runnable
{
  public static final int port = 4000 + (int)(java.lang.Math.random() * 2000);
  public static final String message = "hello";
  public static int count = 0;
  public static String received;

  void send()
    throws Exception
    {
      InetAddress local = InetAddress.getLocalHost();
      Socket sender = new Socket(local, port);
      byte []message_bytes = message.getBytes();

      DataOutputStream out = new DataOutputStream(sender.getOutputStream());
      out.write(message_bytes, 0, message_bytes.length);
      out.flush();
      sender.close();
    }
  void receive()
    throws Exception
    {
      ServerSocket socket = new ServerSocket(port);

      synchronized(this) {
	  notifyAll();
      }

      Socket connection = socket.accept();
      DataInputStream in = new DataInputStream(connection.getInputStream());

      byte[] buffer = new byte[100];

      int length = in.read(buffer);

      connection.close();
      socket.close();

      received = new String(buffer, 0, length);

      count++;
      if ( message.length() != received.length() )
	throw new Exception("Receved "+ received.length()+
			    " bytes but sent "+message.length() + " bytes");

      if ( ! message.equals(received) )
	throw new Exception("Receved \""+ received+
			    "\" but sent \""+message + "\"");
    }

  public void run()
    {
      String name = Thread.currentThread().getName();
      if (name.equals("timer")) {
	try {
	  Thread.sleep(10000);
	} catch (InterruptedException e){}
	System.out.println("FAILED: timer triggered");
	System.exit(0);
      }
      try {
	receive();
      } catch (Exception e) {
	System.out.println("FAILED: receiver (port "+port + "): " + e);
	System.exit(0);
      }
    }
  public static void main(String args[])
    {
      try {
	SocketSendReceiveTest sender = new SocketSendReceiveTest();
	SocketSendReceiveTest receiver = new SocketSendReceiveTest();
	Thread receiver_thread = new Thread(receiver);

	/* Make sure the test terminates even if it hangs on network */
	SocketSendReceiveTest timer = new SocketSendReceiveTest();
	Thread timer_thread = new Thread(timer, "timer");
	timer_thread.start();

	synchronized(receiver) {
	  receiver_thread.start();
	  receiver.wait();
	}
	try {
	  sender.send();
	} catch (Exception e) {
	  System.out.println("FAILED: receiver (port "+port + "): " + e);
	  System.exit(0);
	}
	receiver_thread.join();

	if (0 == count)
	  throw new Exception("Nothing received");

	System.out.println("PASSED: Socket send/receive count="+count+
			   " message="+received);
	System.exit(0);
      } catch (Exception e) {
	System.out.println("FAILED: " + e);
	System.exit(0);
      }
    }
}
