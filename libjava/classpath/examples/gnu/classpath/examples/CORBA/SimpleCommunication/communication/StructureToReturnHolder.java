

package gnu.classpath.examples.CORBA.SimpleCommunication.communication;

import org.omg.CORBA.TypeCode;
import org.omg.CORBA.portable.InputStream;
import org.omg.CORBA.portable.OutputStream;
import org.omg.CORBA.portable.Streamable;

/**
 * The holder for the structure, returned from the server.
 */
public final class StructureToReturnHolder
  implements Streamable
{
  /**
   * The enclosed structure.
   */
  public StructureToReturn value = null;

  /**
   * Create the empty holder.
   */
  public StructureToReturnHolder()
  {
  }

  /**
   * Crate the holder with the defined initial value.
   */
  public StructureToReturnHolder(StructureToReturn initialValue)
  {
    value = initialValue;
  }

  /**
   * Read the value from the CDR stream.
   */
  public void _read(InputStream in)
  {
    value = StructureToReturnHelper.read(in);
  }

  /**
   * Get the typecode of this structure.
   */
  public TypeCode _type()
  {
    return StructureToReturnHelper.type();
  }

  /**
   * Write the value from the CDR stream.
   * @param out
   */
  public void _write(OutputStream out)
  {
    StructureToReturnHelper.write(out, value);
  }
}
