import java.net.*;

/*
 * Start one thread for receiving a packet, wait for it to set up,
 * send a packet to it, and wait until it completes.  Compare the
 * packet to make sure it came thru without errors.
 */

public class DatagramSocketSendReceiveTest
  implements Runnable
{
  public static final int port = 4000 + (int)(java.lang.Math.random() * 1000);
  public static final String message = "hello";
  public static int count = 0;
  public static String received;

  void send()
    throws Exception
    {
      DatagramSocket sender = new DatagramSocket();
      InetAddress local = sender.getLocalAddress();
      byte []message_bytes = message.getBytes();

      DatagramPacket packet = new DatagramPacket(message_bytes,
						 message_bytes.length,
						 local, port);

      sender.send(packet);
      sender.close();
    }
  void receive()
    throws Exception
    {
      DatagramSocket socket = new DatagramSocket(port);
      socket.setSoTimeout(10);

      byte[] buffer = new byte[100];
      DatagramPacket packet = new DatagramPacket(buffer, buffer.length);

      synchronized(this) {
	notifyAll();
      }

      socket.receive(packet);
      socket.close();

      received = new String(buffer, 0, packet.getLength());

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
	System.out.println("FAILED: receiver: " + e);
	System.exit(0);
      }
    }
  public static void main(String args[])
    {
      try {
	DatagramSocketSendReceiveTest sender =
	  new DatagramSocketSendReceiveTest();
	DatagramSocketSendReceiveTest receiver =
	  new DatagramSocketSendReceiveTest();
	Thread receiver_thread = new Thread(receiver);

	/* Make sure the test terminates even if it hangs on network */
	DatagramSocketSendReceiveTest timer = new DatagramSocketSendReceiveTest();
	Thread timer_thread = new Thread(timer, "timer");
	timer_thread.start();

	synchronized(receiver) {
	  receiver_thread.start();
	  receiver.wait();
	}
	try {
	  sender.send();
	} catch (Exception e) {
	  System.out.println("FAILED: sender: " + e);
	  System.exit(0);
	}
	receiver_thread.join();
	  
	if (0 == count)
	  throw new Exception("Nothing received");

	System.out.println("PASSED: DatagramSocket send/receive count="+count+
			   " message="+received);
	System.exit(0);
      } catch (Exception e) {
	System.out.println("FAILED: " + e);
	System.exit(0);
      }
    }
}
