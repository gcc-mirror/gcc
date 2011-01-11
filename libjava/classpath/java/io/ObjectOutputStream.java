/* ObjectOutputStream.java -- Class used to write serialized objects
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2004, 2005, 2006, 2008
   Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package java.io;

import gnu.java.io.ObjectIdentityMap2Int;
import gnu.java.lang.reflect.TypeSignature;
import gnu.java.security.action.SetAccessibleAction;

import java.lang.reflect.Array;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;


/**
 * An <code>ObjectOutputStream</code> can be used to write objects
 * as well as primitive data in a platform-independent manner to an
 * <code>OutputStream</code>.
 *
 * The data produced by an <code>ObjectOutputStream</code> can be read
 * and reconstituted by an <code>ObjectInputStream</code>.
 *
 * <code>writeObject (Object)</code> is used to write Objects, the
 * <code>write&lt;type&gt;</code> methods are used to write primitive
 * data (as in <code>DataOutputStream</code>). Strings can be written
 * as objects or as primitive data.
 *
 * Not all objects can be written out using an
 * <code>ObjectOutputStream</code>.  Only those objects that are an
 * instance of <code>java.io.Serializable</code> can be written.
 *
 * Using default serialization, information about the class of an
 * object is written, all of the non-transient, non-static fields of
 * the object are written, if any of these fields are objects, they are
 * written out in the same manner.
 *
 * An object is only written out the first time it is encountered.  If
 * the object is encountered later, a reference to it is written to
 * the underlying stream.  Thus writing circular object graphs
 * does not present a problem, nor are relationships between objects
 * in a graph lost.
 *
 * Example usage:
 * <pre>
 * Hashtable map = new Hashtable ();
 * map.put ("one", new Integer (1));
 * map.put ("two", new Integer (2));
 *
 * ObjectOutputStream oos =
 * new ObjectOutputStream (new FileOutputStream ("numbers"));
 * oos.writeObject (map);
 * oos.close ();
 *
 * ObjectInputStream ois =
 * new ObjectInputStream (new FileInputStream ("numbers"));
 * Hashtable newmap = (Hashtable)ois.readObject ();
 *
 * System.out.println (newmap);
 * </pre>
 *
 * The default serialization can be overriden in two ways.
 *
 * By defining a method <code>private void
 * writeObject (ObjectOutputStream)</code>, a class can dictate exactly
 * how information about itself is written.
 * <code>defaultWriteObject ()</code> may be called from this method to
 * carry out default serialization.  This method is not
 * responsible for dealing with fields of super-classes or subclasses.
 *
 * By implementing <code>java.io.Externalizable</code>.  This gives
 * the class complete control over the way it is written to the
 * stream.  If this approach is used the burden of writing superclass
 * and subclass data is transfered to the class implementing
 * <code>java.io.Externalizable</code>.
 *
 * @see java.io.DataOutputStream
 * @see java.io.Externalizable
 * @see java.io.ObjectInputStream
 * @see java.io.Serializable
 * @author Tom Tromey (tromey@redhat.com)
 * @author Jeroen Frijters (jeroen@frijters.net)
 * @author Guilhem Lavaux (guilhem@kaffe.org)
 * @author Michael Koch (konqueror@gmx.de)
 * @author Andrew John Hughes (gnu_andrew@member.fsf.org)
 */
public class ObjectOutputStream extends OutputStream
  implements ObjectOutput, ObjectStreamConstants
{
  /**
   * Creates a new <code>ObjectOutputStream</code> that will do all of
   * its writing onto <code>out</code>.  This method also initializes
   * the stream by writing the header information (stream magic number
   * and stream version).
   *
   * @exception IOException Writing stream header to underlying
   * stream cannot be completed.
   *
   * @see #writeStreamHeader()
   */
  public ObjectOutputStream (OutputStream out) throws IOException
  {
    realOutput = new DataOutputStream(out);
    blockData = new byte[ BUFFER_SIZE ];
    blockDataCount = 0;
    blockDataOutput = new DataOutputStream(this);
    setBlockDataMode(true);
    replacementEnabled = false;
    isSerializing = false;
    nextOID = baseWireHandle;
    OIDLookupTable = new ObjectIdentityMap2Int();
    protocolVersion = defaultProtocolVersion;
    useSubclassMethod = false;
    writeStreamHeader();

    if (DEBUG)
      {
        String val = System.getProperty("gcj.dumpobjects");
        if (val != null && !val.equals(""))
          dump = true;
      }
  }

  /**
   * Writes a representation of <code>obj</code> to the underlying
   * output stream by writing out information about its class, then
   * writing out each of the objects non-transient, non-static
   * fields.  If any of these fields are other objects,
   * they are written out in the same manner.
   *
   * This method can be overriden by a class by implementing
   * <code>private void writeObject (ObjectOutputStream)</code>.
   *
   * If an exception is thrown from this method, the stream is left in
   * an undefined state.
   *
   * @param obj the object to serialize.
   * @exception NotSerializableException An attempt was made to
   * serialize an <code>Object</code> that is not serializable.
   *
   * @exception InvalidClassException Somebody tried to serialize
   * an object which is wrongly formatted.
   *
   * @exception IOException Exception from underlying
   * <code>OutputStream</code>.
   * @see #writeUnshared(Object)
   */
  public final void writeObject(Object obj) throws IOException
  {
    writeObject(obj, true);
  }

  /**
   * Writes an object to the stream in the same manner as
   * {@link #writeObject(Object)}, but without the use of
   * references.  As a result, the object is always written
   * to the stream in full.  Likewise, if an object is written
   * by this method and is then later written again by
   * {@link #writeObject(Object)}, both calls will write out
   * the object in full, as the later call to
   * {@link #writeObject(Object)} will know nothing of the
   * earlier use of {@link #writeUnshared(Object)}.
   *
   * @param obj the object to serialize.
   * @throws NotSerializableException if the object being
   *                                  serialized does not implement
   *                                  {@link Serializable}.
   * @throws InvalidClassException if a problem occurs with
   *                               the class of the object being
   *                               serialized.
   * @throws IOException if an I/O error occurs on the underlying
   *                     <code>OutputStream</code>.
   * @since 1.4
   * @see #writeObject(Object)
   */
  public void writeUnshared(Object obj)
    throws IOException
  {
    writeObject(obj, false);
  }

  /**
   * Writes a representation of <code>obj</code> to the underlying
   * output stream by writing out information about its class, then
   * writing out each of the objects non-transient, non-static
   * fields.  If any of these fields are other objects,
   * they are written out in the same manner.
   *
   * This method can be overriden by a class by implementing
   * <code>private void writeObject (ObjectOutputStream)</code>.
   *
   * If an exception is thrown from this method, the stream is left in
   * an undefined state.
   *
   * @param obj the object to serialize.
   * @param shared true if the serialized object should be
   *               shared with later calls.
   * @exception NotSerializableException An attempt was made to
   * serialize an <code>Object</code> that is not serializable.
   *
   * @exception InvalidClassException Somebody tried to serialize
   * an object which is wrongly formatted.
   *
   * @exception IOException Exception from underlying
   * <code>OutputStream</code>.
   * @see #writeUnshared(Object)
   */
  private final void writeObject(Object obj, boolean shared)
    throws IOException
  {
    if (useSubclassMethod)
      {
        if (dump)
          dumpElementln ("WRITE OVERRIDE: " + obj);

        writeObjectOverride(obj);
        return;
      }

    if (dump)
      dumpElementln ("WRITE: ", obj);

    depth += 2;

    boolean was_serializing = isSerializing;
    boolean old_mode = setBlockDataMode(false);
    try
      {
        isSerializing = true;
        boolean replaceDone = false;
        Object replacedObject = null;

        while (true)
          {
            if (obj == null)
              {
                realOutput.writeByte(TC_NULL);
                break;
              }

            int handle = findHandle(obj);
            if (handle >= 0 && shared)
              {
                realOutput.writeByte(TC_REFERENCE);
                realOutput.writeInt(handle);
                break;
              }

            if (obj instanceof Class)
              {
                Class cl = (Class)obj;
                ObjectStreamClass osc = ObjectStreamClass.lookupForClassObject(cl);
                realOutput.writeByte(TC_CLASS);
                if (!osc.isProxyClass)
                  {
                    writeObject (osc);
                  }
                else
                  {System.err.println("1");
                    realOutput.writeByte(TC_PROXYCLASSDESC);
                    Class[] intfs = cl.getInterfaces();
                    realOutput.writeInt(intfs.length);
                    for (int i = 0; i < intfs.length; i++)
                      realOutput.writeUTF(intfs[i].getName());

                    boolean oldmode = setBlockDataMode(true);
                    annotateProxyClass(cl);
                    setBlockDataMode(oldmode);
                    realOutput.writeByte(TC_ENDBLOCKDATA);

                    writeObject(osc.getSuper());
                  }
                if (shared)
                  assignNewHandle(obj);
                break;
              }

            if (obj instanceof ObjectStreamClass)
              {
                writeClassDescriptor((ObjectStreamClass) obj);
                break;
              }

            Class clazz = obj.getClass();
            ObjectStreamClass osc = ObjectStreamClass.lookupForClassObject(clazz);
            if (osc == null)
              throw new NotSerializableException(clazz.getName());

            if (osc.isEnum())
              {
                /* TC_ENUM classDesc newHandle enumConstantName */
                realOutput.writeByte(TC_ENUM);
                writeObject(osc);
                if (shared)
                  assignNewHandle(obj);
                writeObject(((Enum) obj).name());
                break;
              }

            if ((replacementEnabled || obj instanceof Serializable)
                && ! replaceDone)
              {
                replacedObject = obj;

                if (obj instanceof Serializable)
                  {
                    try
                      {
                        Method m = osc.writeReplaceMethod;
                        if (m != null)
                            obj = m.invoke(obj, new Object[0]);
                      }
                    catch (IllegalAccessException ignore)
                      {
                      }
                    catch (InvocationTargetException ignore)
                      {
                      }
                  }

                if (replacementEnabled)
                  obj = replaceObject(obj);

                replaceDone = true;
                continue;
              }

            if (obj instanceof String)
              {
                String s = (String)obj;
                long l = realOutput.getUTFlength(s, 0, 0);
                if (l <= 65535)
                  {
                    realOutput.writeByte(TC_STRING);
                    if (shared)
                      assignNewHandle(obj);
                    realOutput.writeUTFShort(s, (int)l);
                  }
                else
                  {
                    realOutput.writeByte(TC_LONGSTRING);
                    if (shared)
                      assignNewHandle(obj);
                    realOutput.writeUTFLong(s, l);
                  }
                break;
              }

            if (clazz.isArray ())
              {
                realOutput.writeByte(TC_ARRAY);
                writeObject(osc);
                if (shared)
                  assignNewHandle(obj);
                writeArraySizeAndElements(obj, clazz.getComponentType());
                break;
              }

            realOutput.writeByte(TC_OBJECT);
            writeObject(osc);

            if (shared)
              if (replaceDone)
                assignNewHandle(replacedObject);
              else
                assignNewHandle(obj);

            if (obj instanceof Externalizable)
              {
                if (protocolVersion == PROTOCOL_VERSION_2)
                  setBlockDataMode(true);

                ((Externalizable)obj).writeExternal(this);

                if (protocolVersion == PROTOCOL_VERSION_2)
                  {
                    setBlockDataMode(false);
                    realOutput.writeByte(TC_ENDBLOCKDATA);
                  }

                break;
              }

            if (obj instanceof Serializable)
              {
                Object prevObject = this.currentObject;
                ObjectStreamClass prevObjectStreamClass = this.currentObjectStreamClass;
                currentObject = obj;
                ObjectStreamClass[] hierarchy = osc.hierarchy();

                for (int i = 0; i < hierarchy.length; i++)
                  {
                    currentObjectStreamClass = hierarchy[i];

                    fieldsAlreadyWritten = false;
                    if (currentObjectStreamClass.hasWriteMethod())
                      {
                        if (dump)
                          dumpElementln ("WRITE METHOD CALLED FOR: ", obj);
                        setBlockDataMode(true);
                        callWriteMethod(obj, currentObjectStreamClass);
                        setBlockDataMode(false);
                        realOutput.writeByte(TC_ENDBLOCKDATA);
                        if (dump)
                          dumpElementln ("WRITE ENDBLOCKDATA FOR: ", obj);
                      }
                    else
                      {
                        if (dump)
                          dumpElementln ("WRITE FIELDS CALLED FOR: ", obj);
                        writeFields(obj, currentObjectStreamClass);
                      }
                  }

                this.currentObject = prevObject;
                this.currentObjectStreamClass = prevObjectStreamClass;
                currentPutField = null;
                break;
              }

            throw new NotSerializableException(clazz.getName()
                                               + " in "
                                               + obj.getClass());
          } // end pseudo-loop
      }
    catch (ObjectStreamException ose)
      {
        // Rethrow these are fatal.
        throw ose;
      }
    catch (IOException e)
      {
        realOutput.writeByte(TC_EXCEPTION);
        reset(true);

        setBlockDataMode(false);
        try
          {
            if (DEBUG)
              {
                e.printStackTrace(System.out);
              }
            writeObject(e);
          }
        catch (IOException ioe)
          {
            StreamCorruptedException ex =
              new StreamCorruptedException
              (ioe + " thrown while exception was being written to stream.");
            if (DEBUG)
              {
                ex.printStackTrace(System.out);
              }
            throw ex;
          }

        reset (true);

      }
    finally
      {
        isSerializing = was_serializing;
        setBlockDataMode(old_mode);
        depth -= 2;

        if (dump)
          dumpElementln ("END: ", obj);
      }
  }

  protected void writeClassDescriptor(ObjectStreamClass osc) throws IOException
  {
    if (osc.isProxyClass)
      {
        realOutput.writeByte(TC_PROXYCLASSDESC);
        Class[] intfs = osc.forClass().getInterfaces();
        realOutput.writeInt(intfs.length);
        for (int i = 0; i < intfs.length; i++)
          realOutput.writeUTF(intfs[i].getName());

        assignNewHandle(osc);

        boolean oldmode = setBlockDataMode(true);
        annotateProxyClass(osc.forClass());
        setBlockDataMode(oldmode);
        realOutput.writeByte(TC_ENDBLOCKDATA);
      }
    else
      {
        realOutput.writeByte(TC_CLASSDESC);
        realOutput.writeUTF(osc.getName());
        if (osc.isEnum())
          realOutput.writeLong(0L);
        else
          realOutput.writeLong(osc.getSerialVersionUID());
        assignNewHandle(osc);

        int flags = osc.getFlags();

        if (protocolVersion == PROTOCOL_VERSION_2
            && osc.isExternalizable())
        flags |= SC_BLOCK_DATA;

        realOutput.writeByte(flags);

        ObjectStreamField[] fields = osc.fields;

        if (fields == ObjectStreamClass.INVALID_FIELDS)
          throw new InvalidClassException
                  (osc.getName(), "serialPersistentFields is invalid");

        realOutput.writeShort(fields.length);

        ObjectStreamField field;
        for (int i = 0; i < fields.length; i++)
          {
            field = fields[i];
            realOutput.writeByte(field.getTypeCode ());
            realOutput.writeUTF(field.getName ());

            if (! field.isPrimitive())
              writeObject(field.getTypeString());
          }

        boolean oldmode = setBlockDataMode(true);
        annotateClass(osc.forClass());
        setBlockDataMode(oldmode);
        realOutput.writeByte(TC_ENDBLOCKDATA);
      }

    if (osc.isSerializable() || osc.isExternalizable())
      writeObject(osc.getSuper());
    else
      writeObject(null);
  }

  /**
   * Writes the current objects non-transient, non-static fields from
   * the current class to the underlying output stream.
   *
   * This method is intended to be called from within a object's
   * <code>private void writeObject (ObjectOutputStream)</code>
   * method.
   *
   * @exception NotActiveException This method was called from a
   * context other than from the current object's and current class's
   * <code>private void writeObject (ObjectOutputStream)</code>
   * method.
   *
   * @exception IOException Exception from underlying
   * <code>OutputStream</code>.
   */
  public void defaultWriteObject()
    throws IOException, NotActiveException
  {
    markFieldsWritten();
    writeFields(currentObject, currentObjectStreamClass);
  }


  private void markFieldsWritten() throws IOException
  {
    if (currentObject == null || currentObjectStreamClass == null)
      throw new NotActiveException
        ("defaultWriteObject called by non-active class and/or object");

    if (fieldsAlreadyWritten)
      throw new IOException
        ("Only one of writeFields and defaultWriteObject may be called, and it may only be called once");

    fieldsAlreadyWritten = true;
  }

  /**
   * Resets stream to state equivalent to the state just after it was
   * constructed.
   *
   * Causes all objects previously written to the stream to be
   * forgotten.  A notification of this reset is also written to the
   * underlying stream.
   *
   * @exception IOException Exception from underlying
   * <code>OutputStream</code> or reset called while serialization is
   * in progress.
   */
  public void reset() throws IOException
  {
    reset(false);
  }


  private void reset(boolean internal) throws IOException
  {
    if (!internal)
      {
        if (isSerializing)
          throw new IOException("Reset called while serialization in progress");

        realOutput.writeByte(TC_RESET);
      }

    clearHandles();
  }


  /**
   * Informs this <code>ObjectOutputStream</code> to write data
   * according to the specified protocol.  There are currently two
   * different protocols, specified by <code>PROTOCOL_VERSION_1</code>
   * and <code>PROTOCOL_VERSION_2</code>.  This implementation writes
   * data using <code>PROTOCOL_VERSION_2</code> by default, as is done
   * since the JDK 1.2.
   * <p>
   * For an explanation of the differences between the two protocols
   * see the Java Object Serialization Specification.
   * </p>
   *
   * @param version the version to use.
   *
   * @throws IllegalArgumentException if <code>version</code> is not a valid
   * protocol.
   * @throws IllegalStateException if called after the first the first object
   * was serialized.
   * @throws IOException if an I/O error occurs.
   *
   * @see ObjectStreamConstants#PROTOCOL_VERSION_1
   * @see ObjectStreamConstants#PROTOCOL_VERSION_2
   *
   * @since 1.2
   */
  public void useProtocolVersion(int version) throws IOException
  {
    if (version != PROTOCOL_VERSION_1 && version != PROTOCOL_VERSION_2)
      throw new IllegalArgumentException("Invalid protocol version requested.");

    if (nextOID != baseWireHandle)
      throw new IllegalStateException("Protocol version cannot be changed "
                                      + "after serialization started.");

    protocolVersion = version;
  }

  /**
   * An empty hook that allows subclasses to write extra information
   * about classes to the stream.  This method is called the first
   * time each class is seen, and after all of the standard
   * information about the class has been written.
   *
   * @exception IOException Exception from underlying
   * <code>OutputStream</code>.
   *
   * @see ObjectInputStream#resolveClass(java.io.ObjectStreamClass)
   */
  protected void annotateClass(Class<?> cl) throws IOException
  {
  }

  protected void annotateProxyClass(Class<?> cl) throws IOException
  {
  }

  /**
   * Allows subclasses to replace objects that are written to the
   * stream with other objects to be written in their place.  This
   * method is called the first time each object is encountered
   * (modulo reseting of the stream).
   *
   * This method must be enabled before it will be called in the
   * serialization process.
   *
   * @exception IOException Exception from underlying
   * <code>OutputStream</code>.
   *
   * @see #enableReplaceObject(boolean)
   */
  protected Object replaceObject(Object obj) throws IOException
  {
    return obj;
  }


  /**
   * If <code>enable</code> is <code>true</code> and this object is
   * trusted, then <code>replaceObject (Object)</code> will be called
   * in subsequent calls to <code>writeObject (Object)</code>.
   * Otherwise, <code>replaceObject (Object)</code> will not be called.
   *
   * @exception SecurityException This class is not trusted.
   */
  protected boolean enableReplaceObject(boolean enable)
    throws SecurityException
  {
    if (enable)
      {
        SecurityManager sm = System.getSecurityManager();
        if (sm != null)
          sm.checkPermission(new SerializablePermission("enableSubstitution"));
      }

    boolean old_val = replacementEnabled;
    replacementEnabled = enable;
    return old_val;
  }


  /**
   * Writes stream magic and stream version information to the
   * underlying stream.
   *
   * @exception IOException Exception from underlying
   * <code>OutputStream</code>.
   */
  protected void writeStreamHeader() throws IOException
  {
    realOutput.writeShort(STREAM_MAGIC);
    realOutput.writeShort(STREAM_VERSION);
  }

  /**
   * Protected constructor that allows subclasses to override
   * serialization.  This constructor should be called by subclasses
   * that wish to override <code>writeObject (Object)</code>.  This
   * method does a security check <i>NOTE: currently not
   * implemented</i>, then sets a flag that informs
   * <code>writeObject (Object)</code> to call the subclasses
   * <code>writeObjectOverride (Object)</code> method.
   *
   * @see #writeObjectOverride(Object)
   */
  protected ObjectOutputStream() throws IOException, SecurityException
  {
    SecurityManager sec_man = System.getSecurityManager ();
    if (sec_man != null)
      sec_man.checkPermission(SUBCLASS_IMPLEMENTATION_PERMISSION);
    useSubclassMethod = true;
  }


  /**
   * This method allows subclasses to override the default
   * serialization mechanism provided by
   * <code>ObjectOutputStream</code>.  To make this method be used for
   * writing objects, subclasses must invoke the 0-argument
   * constructor on this class from there constructor.
   *
   * @see #ObjectOutputStream()
   *
   * @exception NotActiveException Subclass has arranged for this
   * method to be called, but did not implement this method.
   */
  protected void writeObjectOverride(Object obj) throws NotActiveException,
    IOException
  {
    throw new NotActiveException
      ("Subclass of ObjectOutputStream must implement writeObjectOverride");
  }


  /**
   * @see DataOutputStream#write(int)
   */
  public void write (int data) throws IOException
  {
    if (writeDataAsBlocks)
      {
        if (blockDataCount == BUFFER_SIZE)
          drain();

        blockData[ blockDataCount++ ] = (byte)data;
      }
    else
      realOutput.write(data);
  }


  /**
   * @see DataOutputStream#write(byte[])
   */
  public void write(byte[] b) throws IOException
  {
    write(b, 0, b.length);
  }


  /**
   * @see DataOutputStream#write(byte[],int,int)
   */
  public void write(byte[] b, int off, int len) throws IOException
  {
    if (writeDataAsBlocks)
      {
        if (len < 0)
          throw new IndexOutOfBoundsException();

        if (blockDataCount + len < BUFFER_SIZE)
          {
            System.arraycopy(b, off, blockData, blockDataCount, len);
            blockDataCount += len;
          }
        else
          {
            drain();
            writeBlockDataHeader(len);
            realOutput.write(b, off, len);
          }
      }
    else
      realOutput.write(b, off, len);
  }


  /**
   * @see DataOutputStream#flush()
   */
  public void flush () throws IOException
  {
    drain();
    realOutput.flush();
  }


  /**
   * Causes the block-data buffer to be written to the underlying
   * stream, but does not flush underlying stream.
   *
   * @exception IOException Exception from underlying
   * <code>OutputStream</code>.
   */
  protected void drain() throws IOException
  {
    if (blockDataCount == 0)
      return;

    if (writeDataAsBlocks)
      writeBlockDataHeader(blockDataCount);
    realOutput.write(blockData, 0, blockDataCount);
    blockDataCount = 0;
  }


  /**
   * @see java.io.DataOutputStream#close ()
   */
  public void close() throws IOException
  {
    flush();
    realOutput.close();
  }


  /**
   * @see java.io.DataOutputStream#writeBoolean (boolean)
   */
  public void writeBoolean(boolean data) throws IOException
  {
    blockDataOutput.writeBoolean(data);
  }


  /**
   * @see java.io.DataOutputStream#writeByte (int)
   */
  public void writeByte(int data) throws IOException
  {
    blockDataOutput.writeByte(data);
  }


  /**
   * @see java.io.DataOutputStream#writeShort (int)
   */
  public void writeShort (int data) throws IOException
  {
    blockDataOutput.writeShort(data);
  }


  /**
   * @see java.io.DataOutputStream#writeChar (int)
   */
  public void writeChar(int data) throws IOException
  {
    blockDataOutput.writeChar(data);
  }


  /**
   * @see java.io.DataOutputStream#writeInt (int)
   */
  public void writeInt(int data) throws IOException
  {
    blockDataOutput.writeInt(data);
  }


  /**
   * @see java.io.DataOutputStream#writeLong (long)
   */
  public void writeLong(long data) throws IOException
  {
    blockDataOutput.writeLong(data);
  }


  /**
   * @see java.io.DataOutputStream#writeFloat (float)
   */
  public void writeFloat(float data) throws IOException
  {
    blockDataOutput.writeFloat(data);
  }


  /**
   * @see java.io.DataOutputStream#writeDouble (double)
   */
  public void writeDouble(double data) throws IOException
  {
    blockDataOutput.writeDouble(data);
  }


  /**
   * @see java.io.DataOutputStream#writeBytes (java.lang.String)
   */
  public void writeBytes(String data) throws IOException
  {
    blockDataOutput.writeBytes(data);
  }


  /**
   * @see java.io.DataOutputStream#writeChars (java.lang.String)
   */
  public void writeChars(String data) throws IOException
  {
    dataOutput.writeChars(data);
  }


  /**
   * @see java.io.DataOutputStream#writeUTF (java.lang.String)
   */
  public void writeUTF(String data) throws IOException
  {
    dataOutput.writeUTF(data);
  }


  /**
   * This class allows a class to specify exactly which fields should
   * be written, and what values should be written for these fields.
   *
   * XXX: finish up comments
   */
  public abstract static class PutField
  {
    public abstract void put (String name, boolean value);
    public abstract void put (String name, byte value);
    public abstract void put (String name, char value);
    public abstract void put (String name, double value);
    public abstract void put (String name, float value);
    public abstract void put (String name, int value);
    public abstract void put (String name, long value);
    public abstract void put (String name, short value);
    public abstract void put (String name, Object value);

    /**
     * @deprecated
     */
    public abstract void write (ObjectOutput out) throws IOException;
  }

  public PutField putFields() throws IOException
  {
    if (currentPutField != null)
      return currentPutField;

    currentPutField = new PutField()
      {
        private byte[] prim_field_data
          = new byte[currentObjectStreamClass.primFieldSize];
        private Object[] objs
          = new Object[currentObjectStreamClass.objectFieldCount];

        private ObjectStreamField getField (String name)
        {
          ObjectStreamField field
            = currentObjectStreamClass.getField(name);

          if (field == null)
            throw new IllegalArgumentException("no such serializable field " + name);

          return field;
        }

        public void put(String name, boolean value)
        {
          ObjectStreamField field = getField(name);

          checkType(field, 'Z');
          prim_field_data[field.getOffset ()] = (byte)(value ? 1 : 0);
        }

        public void put(String name, byte value)
        {
          ObjectStreamField field = getField(name);

          checkType(field, 'B');
          prim_field_data[field.getOffset()] = value;
        }

        public void put(String name, char value)
        {
          ObjectStreamField field = getField(name);

          checkType(field, 'C');
          int off = field.getOffset();
          prim_field_data[off++] = (byte)(value >>> 8);
          prim_field_data[off] = (byte)value;
        }

        public void put(String name, double value)
        {
          ObjectStreamField field = getField (name);

          checkType(field, 'D');
          int off = field.getOffset();
          long l_value = Double.doubleToLongBits (value);
          prim_field_data[off++] = (byte)(l_value >>> 52);
          prim_field_data[off++] = (byte)(l_value >>> 48);
          prim_field_data[off++] = (byte)(l_value >>> 40);
          prim_field_data[off++] = (byte)(l_value >>> 32);
          prim_field_data[off++] = (byte)(l_value >>> 24);
          prim_field_data[off++] = (byte)(l_value >>> 16);
          prim_field_data[off++] = (byte)(l_value >>> 8);
          prim_field_data[off] = (byte)l_value;
        }

        public void put(String name, float value)
        {
          ObjectStreamField field = getField(name);

          checkType(field, 'F');
          int off = field.getOffset();
          int i_value = Float.floatToIntBits(value);
          prim_field_data[off++] = (byte)(i_value >>> 24);
          prim_field_data[off++] = (byte)(i_value >>> 16);
          prim_field_data[off++] = (byte)(i_value >>> 8);
          prim_field_data[off] = (byte)i_value;
        }

        public void put(String name, int value)
        {
          ObjectStreamField field = getField(name);
          checkType(field, 'I');
          int off = field.getOffset();
          prim_field_data[off++] = (byte)(value >>> 24);
          prim_field_data[off++] = (byte)(value >>> 16);
          prim_field_data[off++] = (byte)(value >>> 8);
          prim_field_data[off] = (byte)value;
        }

        public void put(String name, long value)
        {
          ObjectStreamField field = getField(name);
          checkType(field, 'J');
          int off = field.getOffset();
          prim_field_data[off++] = (byte)(value >>> 52);
          prim_field_data[off++] = (byte)(value >>> 48);
          prim_field_data[off++] = (byte)(value >>> 40);
          prim_field_data[off++] = (byte)(value >>> 32);
          prim_field_data[off++] = (byte)(value >>> 24);
          prim_field_data[off++] = (byte)(value >>> 16);
          prim_field_data[off++] = (byte)(value >>> 8);
          prim_field_data[off] = (byte)value;
        }

        public void put(String name, short value)
        {
          ObjectStreamField field = getField(name);
          checkType(field, 'S');
          int off = field.getOffset();
          prim_field_data[off++] = (byte)(value >>> 8);
          prim_field_data[off] = (byte)value;
        }

        public void put(String name, Object value)
        {
          ObjectStreamField field = getField(name);

          if (value != null &&
              ! field.getType().isAssignableFrom(value.getClass ()))
            throw new IllegalArgumentException("Class " + value.getClass() +
                                               " cannot be cast to " + field.getType());
          objs[field.getOffset()] = value;
        }

        public void write(ObjectOutput out) throws IOException
        {
          // Apparently Block data is not used with PutField as per
          // empirical evidence against JDK 1.2.  Also see Mauve test
          // java.io.ObjectInputOutput.Test.GetPutField.
          boolean oldmode = setBlockDataMode(false);
          out.write(prim_field_data);
          for (int i = 0; i < objs.length; ++ i)
            out.writeObject(objs[i]);
          setBlockDataMode(oldmode);
        }

        private void checkType(ObjectStreamField field, char type)
          throws IllegalArgumentException
        {
          if (TypeSignature.getEncodingOfClass(field.getType()).charAt(0)
              != type)
            throw new IllegalArgumentException();
        }
      };
    // end PutFieldImpl

    return currentPutField;
  }


  public void writeFields() throws IOException
  {
    if (currentPutField == null)
      throw new NotActiveException("writeFields can only be called after putFields has been called");

    markFieldsWritten();
    currentPutField.write(this);
  }


  // write out the block-data buffer, picking the correct header
  // depending on the size of the buffer
  private void writeBlockDataHeader(int size) throws IOException
  {
    if (size < 256)
      {
        realOutput.writeByte(TC_BLOCKDATA);
        realOutput.write(size);
      }
    else
      {
        realOutput.writeByte(TC_BLOCKDATALONG);
        realOutput.writeInt(size);
      }
  }


  // lookup the handle for OBJ, return null if OBJ doesn't have a
  // handle yet
  private int findHandle(Object obj)
  {
    return OIDLookupTable.get(obj);
  }


  // assigns the next availible handle to OBJ
  private int assignNewHandle(Object obj)
  {
    OIDLookupTable.put(obj, nextOID);
    return nextOID++;
  }


  // resets mapping from objects to handles
  private void clearHandles()
  {
    nextOID = baseWireHandle;
    OIDLookupTable.clear();
  }


  // write out array size followed by each element of the array
  private void writeArraySizeAndElements(Object array, Class clazz)
    throws IOException
  {
    int length = Array.getLength(array);

    if (clazz.isPrimitive())
      {
        if (clazz == Boolean.TYPE)
          {
            boolean[] cast_array = (boolean[])array;
            realOutput.writeInt (length);
            for (int i = 0; i < length; i++)
              realOutput.writeBoolean(cast_array[i]);
            return;
          }
        if (clazz == Byte.TYPE)
          {
            byte[] cast_array = (byte[])array;
            realOutput.writeInt(length);
            realOutput.write(cast_array, 0, length);
            return;
          }
        if (clazz == Character.TYPE)
          {
            char[] cast_array = (char[])array;
            realOutput.writeInt(length);
            for (int i = 0; i < length; i++)
              realOutput.writeChar(cast_array[i]);
            return;
          }
        if (clazz == Double.TYPE)
          {
            double[] cast_array = (double[])array;
            realOutput.writeInt(length);
            for (int i = 0; i < length; i++)
              realOutput.writeDouble(cast_array[i]);
            return;
          }
        if (clazz == Float.TYPE)
          {
            float[] cast_array = (float[])array;
            realOutput.writeInt(length);
            for (int i = 0; i < length; i++)
              realOutput.writeFloat(cast_array[i]);
            return;
          }
        if (clazz == Integer.TYPE)
          {
            int[] cast_array = (int[])array;
            realOutput.writeInt(length);
            for (int i = 0; i < length; i++)
              realOutput.writeInt(cast_array[i]);
            return;
          }
        if (clazz == Long.TYPE)
          {
            long[] cast_array = (long[])array;
            realOutput.writeInt (length);
            for (int i = 0; i < length; i++)
              realOutput.writeLong(cast_array[i]);
            return;
          }
        if (clazz == Short.TYPE)
          {
            short[] cast_array = (short[])array;
            realOutput.writeInt (length);
            for (int i = 0; i < length; i++)
              realOutput.writeShort(cast_array[i]);
            return;
          }
      }
    else
      {
        Object[] cast_array = (Object[])array;
        realOutput.writeInt(length);
        for (int i = 0; i < length; i++)
          writeObject(cast_array[i]);
      }
  }


/* GCJ LOCAL */
  // writes out FIELDS of OBJECT for the specified ObjectStreamClass.
  // FIELDS are already supposed already to be in canonical order, but
  // under some circumstances (to do with Proxies) this isn't the
  // case, so we call ensureFieldsSet().
  private void writeFields(Object obj, ObjectStreamClass osc)
    throws IOException
  {
    osc.ensureFieldsSet(osc.forClass());
/* END GCJ LOCAL */

    ObjectStreamField[] fields = osc.fields;
    boolean oldmode = setBlockDataMode(false);

    try
      {
        writeFields(obj,fields);
      }
    catch (IllegalArgumentException _)
      {
        InvalidClassException e = new InvalidClassException
          ("writing fields of class " + osc.forClass().getName());
        e.initCause(_);
        throw e;
      }
    catch (IOException e)
      {
        throw e;
      }
    catch (Exception _)
      {
        IOException e = new IOException("Unexpected exception " + _);
        e.initCause(_);
        throw(e);
      }

    setBlockDataMode(oldmode);
  }


  /**
   * Helper function for writeFields(Object,ObjectStreamClass): write
   * fields from given fields array.  Pass exception on.
   *
   * @param obj the object to be written
   *
   * @param fields the fields of obj to be written.
   */
  private void writeFields(Object obj, ObjectStreamField[] fields)
    throws
      IllegalArgumentException, IllegalAccessException, IOException
  {
    for (int i = 0; i < fields.length; i++)
      {
        ObjectStreamField osf = fields[i];
        Field field = osf.field;

        if (DEBUG && dump)
          dumpElementln ("WRITE FIELD: " + osf.getName() + " type=" + osf.getType());

        switch (osf.getTypeCode())
          {
          case 'Z': realOutput.writeBoolean(field.getBoolean(obj)); break;
          case 'B': realOutput.writeByte   (field.getByte   (obj)); break;
          case 'S': realOutput.writeShort  (field.getShort  (obj)); break;
          case 'C': realOutput.writeChar   (field.getChar   (obj)); break;
          case 'I': realOutput.writeInt    (field.getInt    (obj)); break;
          case 'F': realOutput.writeFloat  (field.getFloat  (obj)); break;
          case 'J': realOutput.writeLong   (field.getLong   (obj)); break;
          case 'D': realOutput.writeDouble (field.getDouble (obj)); break;
          case 'L':
          case '[':            writeObject (field.get       (obj)); break;
          default:
            throw new IOException("Unexpected type code " + osf.getTypeCode());
          }
      }
  }


  // Toggles writing primitive data to block-data buffer.
  // Package-private to avoid a trampoline constructor.
  boolean setBlockDataMode(boolean on) throws IOException
  {
    if (on == writeDataAsBlocks)
      return on;

    drain();
    boolean oldmode = writeDataAsBlocks;
    writeDataAsBlocks = on;

    if (on)
      dataOutput = blockDataOutput;
    else
      dataOutput = realOutput;

    return oldmode;
  }


  private void callWriteMethod(Object obj, ObjectStreamClass osc)
    throws IOException
  {
    currentPutField = null;
    try
      {
        Object args[] = {this};
        osc.writeObjectMethod.invoke(obj, args);
      }
    catch (InvocationTargetException x)
      {
        /* Rethrow if possible. */
        Throwable exception = x.getTargetException();
        if (exception instanceof RuntimeException)
          throw (RuntimeException) exception;
        if (exception instanceof IOException)
          throw (IOException) exception;

        IOException ioe
          = new IOException("Exception thrown from writeObject() on " +
                            osc.forClass().getName() + ": " +
                            exception.getClass().getName());
        ioe.initCause(exception);
        throw ioe;
      }
    catch (Exception x)
      {
        IOException ioe
          = new IOException("Failure invoking writeObject() on " +
                            osc.forClass().getName() + ": " +
                            x.getClass().getName());
        ioe.initCause(x);
        throw ioe;
      }
  }

  private void dumpElementln (String msg, Object obj)
  {
    try
      {
        for (int i = 0; i < depth; i++)
          System.out.print (" ");
        System.out.print (Thread.currentThread() + ": ");
        System.out.print (msg);
        if (java.lang.reflect.Proxy.isProxyClass(obj.getClass()))
          System.out.print (obj.getClass());
        else
          System.out.print (obj);
      }
    catch (Exception _)
      {
      }
    finally
      {
        System.out.println ();
      }
  }

  private void dumpElementln (String msg)
  {
    for (int i = 0; i < depth; i++)
      System.out.print (" ");
    System.out.print (Thread.currentThread() + ": ");
    System.out.println(msg);
  }

  // this value comes from 1.2 spec, but is used in 1.1 as well
  private static final int BUFFER_SIZE = 1024;

  private static int defaultProtocolVersion = PROTOCOL_VERSION_2;

  private DataOutputStream dataOutput;
  private boolean writeDataAsBlocks;
  private DataOutputStream realOutput;
  private DataOutputStream blockDataOutput;
  private byte[] blockData;
  private int blockDataCount;
  private Object currentObject;
  // Package-private to avoid a trampoline.
  ObjectStreamClass currentObjectStreamClass;
  private PutField currentPutField;
  private boolean fieldsAlreadyWritten;
  private boolean replacementEnabled;
  private boolean isSerializing;
  private int nextOID;
  private ObjectIdentityMap2Int OIDLookupTable;
  private int protocolVersion;
  private boolean useSubclassMethod;
  private SetAccessibleAction setAccessible = new SetAccessibleAction();

  // The nesting depth for debugging output
  private int depth = 0;

  // Set if we're generating debugging dumps
  private boolean dump = false;

  private static final boolean DEBUG = false;
}
