
import java.io.*;

public class OOSCallDefault implements Serializable
{
  int x;
  double y;
  transient String s;

  OOSCallDefault( int X, double Y, String S )
  {
    x = X;
    y = Y;
    s = S;
  }

  public boolean equals( Object o )
  {
    OOSCallDefault oo = (OOSCallDefault)o;
    return oo.x == x
      && oo.y == y
      && oo.s.equals( s );
  }
  
  private void writeObject( ObjectOutputStream oos ) throws IOException
  {
    oos.writeObject( s );
    oos.defaultWriteObject();
    oos.writeObject( s );
  }

  private void readObject( ObjectInputStream ois )
    throws ClassNotFoundException, IOException
  {
    ois.readObject();
    ois.defaultReadObject();
    s = (String)ois.readObject();
  }
}
