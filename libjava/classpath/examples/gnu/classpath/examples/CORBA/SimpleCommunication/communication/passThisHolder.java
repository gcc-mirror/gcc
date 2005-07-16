

package gnu.classpath.examples.CORBA.SimpleCommunication.communication;

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;

public final class passThisHolder
  implements Streamable
{
  public passThis value;

  public passThisHolder()
  {
  }

  public passThisHolder(passThis initialValue)
  {
    value = initialValue;
  }

  public void _read(InputStream i)
  {
    value = passThisHelper.read(i);
  }

  public org.omg.CORBA.TypeCode _type()
  {
    return passThisHelper.type();
  }

  public void _write(OutputStream o)
  {
    passThisHelper.write(o, value);
  }
}
