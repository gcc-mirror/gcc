

package gnu.classpath.examples.CORBA.SimpleCommunication.communication;

import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;

public final class StructureToPassHolder
  implements Streamable
{
  public StructureToPass value;

  public StructureToPassHolder()
  {
  }

  public StructureToPassHolder(StructureToPass initialValue)
  {
    value = initialValue;
  }

  public void _read(InputStream i)
  {
    value = StructureToPassHelper.read(i);
  }

  public org.omg.CORBA.TypeCode _type()
  {
    return StructureToPassHelper.type();
  }

  public void _write(OutputStream o)
  {
    StructureToPassHelper.write(o, value);
  }
}
