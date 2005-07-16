// Class to identify socket option constants.

import java.io.*;
import java.net.*;

public class GetSocketOptionInfo extends Socket implements SocketImplFactory
{

public SocketImpl 
createSocketImpl()
{
  return(new PlainSocketImpl());
}

public static void
main(String[] argv) throws IOException
{
  Socket.setSocketImplFactory(new GetSocketOptionInfo());

  Socket s = new Socket();

  System.err.println("Setting TCP_NODELAY on");
  s.setTcpNoDelay(true);
  System.err.println("Setting TCP_NODELAY off");
  s.setTcpNoDelay(false);

  System.err.println("Setting SO_LINGER on");
  s.setSoLinger(true, 10);
  System.err.println("Setting SO_LINGER off");
  s.setSoLinger(false, 1);

  System.err.println("Setting SO_TIMEOUT to 15");
  s.setSoTimeout(15);
  System.err.println("Setting SO_TIMEOUT to 0");
  s.setSoTimeout(0);
}

}

