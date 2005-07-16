
import java.io.*;

public class OOSNoCallDefault implements Serializable
{
  int x;
  String s;
  boolean b;
  
  OOSNoCallDefault()
  {}

  OOSNoCallDefault( int X, String S, boolean B )
  {
    x = X;
    s = S;
    b = B;
  }

  public boolean equals( Object o )
  {
    OOSNoCallDefault oo = (OOSNoCallDefault)o;
    return oo.x == x
      && oo.b == b
      && oo.s.equals( s );
  }
  
  private void writeObject( ObjectOutputStream oos ) throws IOException
  {
    oos.writeInt( x );
    oos.writeObject( s );
    oos.writeBoolean( b );
  }

  private void readObject( ObjectInputStream ois )
    throws ClassNotFoundException, IOException
  {
    x = ois.readInt();
    s = (String)ois.readObject();
    b = ois.readBoolean();
  }
}
