
import java.io.*;

public class OOSExtern extends OOSNoCallDefault implements Externalizable
{
  public OOSExtern()
  {}

  OOSExtern( int X, String S, boolean B )
  {
    super( X, S, B );
  }

  public void writeExternal( ObjectOutput oo ) throws IOException
  {
    oo.writeInt( super.x );
    oo.writeObject( super.s );
    oo.writeBoolean( super.b );
  }
  
  public void readExternal( ObjectInput oi )
    throws ClassNotFoundException, IOException
  {
    super.x = oi.readInt();
    super.s = (String)oi.readObject();
    super.b = oi.readBoolean();
  }

  public boolean equals( Object o )
  {
    OOSExtern e = (OOSExtern)o;
    return e.x == super.x
      && e.s.equals( super.s )
      && e.b == super.b;
  }
  
}
