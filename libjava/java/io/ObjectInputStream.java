/* ObjectInputStream.java -- Class used to read serialized objects
   Copyright (C) 1998, 1999, 2000, 2001 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

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

import gnu.classpath.Configuration;

import java.lang.reflect.Array;
import java.lang.reflect.Modifier;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Vector;

import gnu.java.io.ObjectIdentityWrapper;
import gnu.java.lang.reflect.TypeSignature;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;



public class ObjectInputStream extends InputStream
  implements ObjectInput, ObjectStreamConstants
{
  /**
     Creates a new <code>ObjectInputStream</code> that will do all of
     its reading from <code>in</code>.  This method also checks
     the stream by reading the header information (stream magic number
     and stream version).

     @exception IOException Reading stream header from underlying
     stream cannot be completed.

     @exception StreamCorruptedException An invalid stream magic
     number or stream version was read from the stream.

     @see readStreamHeader ()
  */
  public ObjectInputStream (InputStream in)
    throws IOException, StreamCorruptedException
  {
    if (Configuration.DEBUG)
      {
	String val = System.getProperty("gcj.dumpobjects");
	if (dump == false && val != null && !val.equals(""))
	  {
	    dump = true;
	    System.out.println ("Serialization debugging enabled");
	  }
	else if (dump == true && (val == null || val.equals("")))
	  {
	    dump = false;
	    System.out.println ("Serialization debugging disabled");
	  }
      }

    this.resolveEnabled = false;
    this.isDeserializing = false;
    this.blockDataPosition = 0;
    this.blockDataBytes = 0;
    this.blockData = new byte[BUFFER_SIZE];
    this.blockDataInput = new DataInputStream (this);
    this.realInputStream = new DataInputStream (in);
    this.nextOID = baseWireHandle;
    this.objectLookupTable = new Hashtable ();
    this.validators = new Vector ();
    setBlockDataMode (true);
    readStreamHeader ();
  }


  /**
     Returns the next deserialized object read from the underlying stream.

     This method can be overriden by a class by implementing
     <code>private void readObject (ObjectInputStream)</code>.

     If an exception is thrown from this method, the stream is left in
     an undefined state.

     @exception ClassNotFoundException The class that an object being
     read in belongs to cannot be found.

     @exception IOException Exception from underlying
     <code>InputStream</code>.
  */
  public final Object readObject () throws ClassNotFoundException, IOException
  {
    if (this.useSubclassMethod)
      return readObjectOverride ();

    boolean was_deserializing;

    Object ret_val;
    was_deserializing = this.isDeserializing;

    if (! was_deserializing)
      setBlockDataMode (false);

    this.isDeserializing = true;

    byte marker = this.realInputStream.readByte ();
    dumpElement ("MARKER: 0x" + Integer.toHexString(marker) + " ");

    switch (marker)
    {
      case TC_BLOCKDATA:
      case TC_BLOCKDATALONG:
	if (marker == TC_BLOCKDATALONG) 
	  dumpElementln ("BLOCKDATALONG");
	else
	  dumpElementln ("BLOCKDATA");
	readNextBlock (marker);
	throw new StreamCorruptedException ("Unexpected blockData");

      case TC_NULL:
	dumpElementln ("NULL");
	ret_val = null;
	break;

      case TC_REFERENCE:
      {
	dumpElement ("REFERENCE ");
	Integer oid = new Integer (this.realInputStream.readInt ());
	dumpElementln (Integer.toHexString(oid.intValue()));
	ret_val = ((ObjectIdentityWrapper)
		   this.objectLookupTable.get (oid)).object;
	break;
      }

      case TC_CLASS:
      {
	dumpElementln ("CLASS");
	ObjectStreamClass osc = (ObjectStreamClass)readObject ();
	Class clazz = osc.forClass ();
	assignNewHandle (clazz);
	ret_val = clazz;
	break;
      }

      case TC_CLASSDESC:
      {
	dumpElement ("CLASSDESC NAME=");
	String name = this.realInputStream.readUTF ();
	dumpElement (name + "; UID=");
	long uid = this.realInputStream.readLong ();
	dumpElement (Long.toHexString(uid) + "; FLAGS=");
	byte flags = this.realInputStream.readByte ();
	dumpElement (Integer.toHexString(flags) + "; FIELD COUNT=");
	short field_count = this.realInputStream.readShort ();
	dumpElementln (Short.toString(field_count));
	ObjectStreamField[] fields = new ObjectStreamField[field_count];

	ObjectStreamClass osc = new ObjectStreamClass (name, uid,
						       flags, fields);
	assignNewHandle (osc);

	for (int i=0; i < field_count; i++)
	{
	  dumpElement ("  TYPE CODE=");
	  char type_code = (char)this.realInputStream.readByte ();
	  dumpElement (type_code + "; FIELD NAME=");
	  String field_name = this.realInputStream.readUTF ();
	  dumpElementln (field_name);
	  String class_name;

	  if (type_code == 'L' || type_code == '[')
	    class_name = (String)readObject ();
	  else
	    class_name = String.valueOf (type_code);

	  fields[i] =
	    new ObjectStreamField (field_name,
				   TypeSignature.getClassForEncoding
				   (class_name));
	}

	Class cl = resolveClass (osc);
	osc.setClass (cl);
	setBlockDataMode (false);

	if (this.realInputStream.readByte () != TC_ENDBLOCKDATA)
	  throw new IOException ("Data annotated to class was not consumed.");
	dumpElementln ("ENDBLOCKDATA ");

	osc.setSuperclass ((ObjectStreamClass)readObject ());
	ret_val = osc;
	break;
      }

      case TC_STRING:
      {
	dumpElement ("STRING=");
	String s = this.realInputStream.readUTF ();
	dumpElementln (s);
	ret_val = processResolution (s, assignNewHandle (s));
	break;
      }

      case TC_ARRAY:
      {
	dumpElementln ("ARRAY");
	ObjectStreamClass osc = (ObjectStreamClass)readObject ();
	Class componentType = osc.forClass ().getComponentType ();
	dumpElement ("ARRAY LENGTH=");
	int length = this.realInputStream.readInt ();
	dumpElementln (length + "; COMPONENT TYPE=" + componentType);
	Object array = Array.newInstance (componentType, length);
	int handle = assignNewHandle (array);
	readArrayElements (array, componentType);
	for (int i=0, len=Array.getLength(array); i < len; i++)
	  dumpElementln ("  ELEMENT[" + i + "]=" + Array.get(array, i).toString());
	ret_val = processResolution (array, handle);
	break;
      }

      case TC_OBJECT:
      {
	dumpElementln ("OBJECT");
	ObjectStreamClass osc = (ObjectStreamClass)readObject ();
	Class clazz = osc.forClass ();

	if (!Serializable.class.isAssignableFrom (clazz))
	  throw new NotSerializableException (clazz + " is not Serializable, and thus cannot be deserialized.");

	if (Externalizable.class.isAssignableFrom (clazz))
	{
	  Externalizable obj = null;

	  try
	  {
	    obj = (Externalizable)clazz.newInstance ();
	  }
	  catch (InstantiationException e)
	  {
	    throw new ClassNotFoundException ("Instance of " + clazz
					      + " could not be created");
	  }
	  catch (IllegalAccessException e)
	  {
	    throw new ClassNotFoundException ("Instance of " + clazz
					      + " could not be created because class or zero-argument constructor is not accessible");
	  }
	  catch (NoSuchMethodError e)
	  {
	    throw new ClassNotFoundException ("Instance of " + clazz
					      + " could not be created because zero-argument constructor is not defined");
	  }

	  int handle = assignNewHandle (obj);

	  boolean read_from_blocks = ((osc.getFlags () & SC_BLOCK_DATA) != 0);

	  if (read_from_blocks)
	    setBlockDataMode (true);

	  obj.readExternal (this);

	  if (read_from_blocks)
	    setBlockDataMode (false);

	  ret_val = processResolution (obj, handle);
	  break;
	} // end if (Externalizable.class.isAssignableFrom (clazz))

	// find the first non-serializable, non-abstract
	// class in clazz's inheritance hierarchy
	Class first_nonserial = clazz.getSuperclass ();
	while (Serializable.class.isAssignableFrom (first_nonserial)
	       || Modifier.isAbstract (first_nonserial.getModifiers ()))
	  first_nonserial = first_nonserial.getSuperclass ();

//	DEBUGln ("Using " + first_nonserial
//		 + " as starting point for constructing " + clazz);

	Object obj = null;
	obj = newObject (clazz, first_nonserial);

	if (obj == null)
	  throw new ClassNotFoundException ("Instance of " + clazz +
					    " could not be created");

	int handle = assignNewHandle (obj);
	this.currentObject = obj;
	ObjectStreamClass[] hierarchy =
	  ObjectStreamClass.getObjectStreamClasses (clazz);

//	DEBUGln ("Got class hierarchy of depth " + hierarchy.length);

	boolean has_read;
	for (int i=0; i < hierarchy.length; i++)
	{
	  this.currentObjectStreamClass = hierarchy[i];

	  dumpElementln ("Reading fields of "
		   + this.currentObjectStreamClass.getName ());

	  has_read = true;

	  try
	  {
	    this.currentObjectStreamClass.forClass ().
	      getDeclaredMethod ("readObject", readObjectParams);
	  }
	  catch (NoSuchMethodException e)
	  {
	    has_read = false;
	  }

	  // XXX: should initialize fields in classes in the hierarchy
	  // that aren't in the stream
	  // should skip over classes in the stream that aren't in the
	  // real classes hierarchy
	  readFields (obj, this.currentObjectStreamClass.fields,
		      has_read, this.currentObjectStreamClass);

	  if (has_read)
	  {
	    dumpElement ("ENDBLOCKDATA? ");
	    try
	      {
		// FIXME: XXX: This try block is to catch EOF which is
		// thrown for some objects.  That indicates a bug in the logic.
	        if (this.realInputStream.readByte () != TC_ENDBLOCKDATA)
		  throw new IOException ("No end of block data seen for class with readObject (ObjectInputStream) method.");
	        dumpElementln ("yes");
	      }
	    catch (EOFException e)
	      {
	        dumpElementln ("no, got EOFException");
	      }
	    catch (IOException e)
	      {
	        dumpElementln ("no, got IOException");
	      }
	  }
	}

	this.currentObject = null;
	this.currentObjectStreamClass = null;
	ret_val = processResolution (obj, handle);
	break;
      }

      case TC_RESET:
	dumpElementln ("RESET");
	clearHandles ();
	ret_val = readObject ();
	break;

      case TC_EXCEPTION:
      {
	dumpElement ("EXCEPTION=");
	Exception e = (Exception)readObject ();
	dumpElementln (e.toString());
	clearHandles ();
	throw new WriteAbortedException ("Exception thrown during writing of stream", e);
      }

      default:
	throw new IOException ("Unknown marker on stream");
    }

    this.isDeserializing = was_deserializing;

    if (! was_deserializing)
    {
      setBlockDataMode (true);

      if (validators.size () > 0)
	invokeValidators ();
    }

    return ret_val;
  }


  /**
     Reads the current objects non-transient, non-static fields from
     the current class from the underlying output stream.

     This method is intended to be called from within a object's
     <code>private void readObject (ObjectInputStream)</code>
     method.

     @exception ClassNotFoundException The class that an object being
     read in belongs to cannot be found.

     @exception NotActiveException This method was called from a
     context other than from the current object's and current class's
     <code>private void readObject (ObjectInputStream)</code>
     method.

     @exception IOException Exception from underlying
     <code>OutputStream</code>.
  */
  public void defaultReadObject ()
    throws ClassNotFoundException, IOException, NotActiveException
  {
    if (this.currentObject == null || this.currentObjectStreamClass == null)
      throw new NotActiveException ("defaultReadObject called by non-active class and/or object");

    if (fieldsAlreadyRead)
      throw new NotActiveException ("defaultReadObject called but fields already read from stream (by defaultReadObject or readFields)");

    readFields (this.currentObject,
		this.currentObjectStreamClass.fields,
		false, this.currentObjectStreamClass);

    fieldsAlreadyRead = true;
  }


  /**
     Registers a <code>ObjectInputValidation</code> to be carried out
     on the object graph currently being deserialized before it is
     returned to the original caller of <code>readObject ()</code>.
     The order of validation for multiple
     <code>ObjectInputValidation</code>s can be controled using
     <code>priority</code>.  Validators with higher priorities are
     called first.

     @see java.io.ObjectInputValidation

     @exception InvalidObjectException <code>validator</code> is
     <code>null</code>

     @exception NotActiveException an attempt was made to add a
     validator outside of the <code>readObject</code> method of the
     object currently being deserialized
  */
  public void registerValidation (ObjectInputValidation validator,
				  int priority)
    throws InvalidObjectException, NotActiveException
  {
    if (this.currentObject == null || this.currentObjectStreamClass == null)
      throw new NotActiveException ("registerValidation called by non-active class and/or object");

    if (validator == null)
      throw new InvalidObjectException ("attempt to add a null ObjectInputValidation object");

    this.validators.addElement (new ValidatorAndPriority (validator,
							  priority));
  }


  /**
     Called when a class is being deserialized.  This is a hook to
     allow subclasses to read in information written by the
     <code>annotateClass (Class)</code> method of an
     <code>ObjectOutputStream</code>.

     This implementation looks up the active call stack for a
     <code>ClassLoader</code>; if a <code>ClassLoader</code> is found,
     it is used to load the class associated with <code>osc</code>,
     otherwise, the default system <code>ClassLoader</code> is used.

     @exception IOException Exception from underlying
     <code>OutputStream</code>.

     @see java.io.ObjectOutputStream#annotateClass (java.lang.Class)
  */
  protected Class resolveClass (ObjectStreamClass osc)
    throws ClassNotFoundException, IOException
  {
    SecurityManager sm = System.getSecurityManager ();

    // FIXME: currentClassLoader doesn't yet do anything useful. We need
    // to call forName() with the classloader of the class which called 
    // readObject(). See SecurityManager.getClassContext().
    ClassLoader cl = currentClassLoader (sm);

    return Class.forName (osc.getName (), true, cl);
  }

  /**
     Allows subclasses to resolve objects that are read from the
     stream with other objects to be returned in their place.  This
     method is called the first time each object is encountered.

     This method must be enabled before it will be called in the
     serialization process.

     @exception IOException Exception from underlying
     <code>OutputStream</code>.

     @see enableResolveObject (boolean)
  */
  protected Object resolveObject (Object obj) throws IOException
  {
    return obj;
  }


  /**
     If <code>enable</code> is <code>true</code> and this object is
     trusted, then <code>resolveObject (Object)</code> will be called
     in subsequent calls to <code>readObject (Object)</code>.
     Otherwise, <code>resolveObject (Object)</code> will not be called.

     @exception SecurityException This class is not trusted.
  */
  protected boolean enableResolveObject (boolean enable)
    throws SecurityException
  {
    if (enable)
      {
	SecurityManager sm = System.getSecurityManager ();
	if (sm != null)
	  sm.checkPermission (new SerializablePermission ("enableSubtitution"));
      }

    boolean old_val = this.resolveEnabled;
    this.resolveEnabled = enable;
    return old_val;
  }


  /**
     Reads stream magic and stream version information from the
     underlying stream.

     @exception IOException Exception from underlying stream.

     @exception StreamCorruptedException An invalid stream magic
     number or stream version was read from the stream.
  */
  protected void readStreamHeader ()
    throws IOException, StreamCorruptedException
  {
    dumpElement ("STREAM MAGIC ");
    if (this.realInputStream.readShort () != STREAM_MAGIC)
      throw new StreamCorruptedException ("Invalid stream magic number");

    dumpElementln ("STREAM VERSION ");
    if (this.realInputStream.readShort () != STREAM_VERSION)
      throw new StreamCorruptedException ("Invalid stream version number");
  }


  public int read () throws IOException
  {
    if (this.readDataFromBlock)
    {
      if (this.blockDataPosition >= this.blockDataBytes)
	readNextBlock ();
      return (this.blockData[this.blockDataPosition++] & 0xff);
    }
    else
      return this.realInputStream.read ();
  }

  public int read (byte[] data, int offset, int length) throws IOException
  {
    if (this.readDataFromBlock)
    {
      if (this.blockDataPosition + length > this.blockDataBytes)
	readNextBlock ();

      System.arraycopy (this.blockData, this.blockDataPosition,
			data, offset, length);
      blockDataPosition += length;	

      return length;
    }
    else
      return this.realInputStream.read (data, offset, length);
  }

  public int available () throws IOException
  {
    if (this.readDataFromBlock)
    {
      if (this.blockDataPosition >= this.blockDataBytes)
	readNextBlock ();

      return this.blockDataBytes - this.blockDataPosition;
    }
    else
      return this.realInputStream.available ();
  }

  public void close () throws IOException
  {
    this.realInputStream.close ();
  }

  public boolean readBoolean () throws IOException
  {
    return this.dataInputStream.readBoolean ();
  }

  public byte readByte () throws IOException
  {
    return this.dataInputStream.readByte ();
  }

  public int readUnsignedByte () throws IOException
  {
    return this.dataInputStream.readUnsignedByte ();
  }

  public short readShort () throws IOException
  {
    return this.dataInputStream.readShort ();
  }

  public int readUnsignedShort () throws IOException
  {
    return this.dataInputStream.readUnsignedShort ();
  }

  public char readChar () throws IOException
  {
    return this.dataInputStream.readChar ();
  }

  public int readInt () throws IOException
  {
    return this.dataInputStream.readInt ();
  }

  public long readLong () throws IOException
  {
    return this.dataInputStream.readLong ();
  }

  public float readFloat () throws IOException
  {
    return this.dataInputStream.readFloat ();
  }

  public double readDouble () throws IOException
  {
    return this.dataInputStream.readDouble ();
  }

  public void readFully (byte data[]) throws IOException
  {
    this.dataInputStream.readFully (data);
  }

  public void readFully (byte data[], int offset, int size)
    throws IOException
  {
    this.dataInputStream.readFully (data, offset, size);
  }

  public int skipBytes (int len) throws IOException
  {
    return this.dataInputStream.skipBytes (len);
  }

  /**
     @deprecated
     @see java.io.DataInputStream#readLine ()
  */
  public String readLine () throws IOException
  {
    return this.dataInputStream.readLine ();
  }

  public String readUTF () throws IOException
  {
    return this.dataInputStream.readUTF ();
  }


  /**
     This class allows a class to specify exactly which fields should
     be read, and what values should be read for these fields.

     XXX: finish up comments
  */
  public static abstract class GetField
  {
    public abstract ObjectStreamClass getObjectStreamClass ();

    public abstract boolean defaulted (String name)
      throws IOException, IllegalArgumentException;

    public abstract boolean get (String name, boolean defvalue)
      throws IOException, IllegalArgumentException;

    public abstract char get (String name, char defvalue)
      throws IOException, IllegalArgumentException;

    public abstract byte get (String name, byte defvalue)
      throws IOException, IllegalArgumentException;

    public abstract short get (String name, short defvalue)
      throws IOException, IllegalArgumentException;

    public abstract int get (String name, int defvalue)
      throws IOException, IllegalArgumentException;

    public abstract long get (String name, long defvalue)
      throws IOException, IllegalArgumentException;

    public abstract float get (String name, float defvalue)
      throws IOException, IllegalArgumentException;

    public abstract double get (String name, double defvalue)
      throws IOException, IllegalArgumentException;

    public abstract Object get (String name, Object defvalue)
      throws IOException, IllegalArgumentException;
  }

  public GetField readFields ()
    throws IOException, ClassNotFoundException, NotActiveException
  {
    if (this.currentObject == null || this.currentObjectStreamClass == null)
      throw new NotActiveException ("readFields called by non-active class and/or object");

    if (fieldsAlreadyRead)
      throw new NotActiveException ("readFields called but fields already read from stream (by defaultReadObject or readFields)");

    final ObjectStreamClass clazz = this.currentObjectStreamClass;
    final byte[] prim_field_data = new byte[clazz.primFieldSize];
    final Object[] objs = new Object[clazz.objectFieldCount];

    // Apparently Block data is not used with GetField as per
    // empirical evidence against JDK 1.2.  Also see Mauve test
    // java.io.ObjectInputOutput.Test.GetPutField.
    setBlockDataMode (false);
    readFully (prim_field_data);
    for (int i = 0; i < objs.length; ++ i)
      objs[i] = readObject ();
    setBlockDataMode (true);

    return new GetField ()
    {
      public ObjectStreamClass getObjectStreamClass ()
      {
	return clazz;
      }

      public boolean defaulted (String name)
	throws IOException, IllegalArgumentException
      {
	return clazz.getField (name) == null;
      }

      public boolean get (String name, boolean defvalue)
	throws IOException, IllegalArgumentException
      {
	ObjectStreamField field = getField (name, Boolean.TYPE);

	if (field == null)
	  return defvalue;

	return prim_field_data[field.getOffset ()] == 0 ? false : true;
      }

      public char get (String name, char defvalue)
	throws IOException, IllegalArgumentException
      {
	ObjectStreamField field = getField (name, Character.TYPE);

	if (field == null)
	  return defvalue;

	int off = field.getOffset ();

	return (char)(((prim_field_data[off++] & 0xFF) << 8)
		      | (prim_field_data[off] & 0xFF));
      }

      public byte get (String name, byte defvalue)
	throws IOException, IllegalArgumentException
      {
	ObjectStreamField field = getField (name, Byte.TYPE);

	if (field == null)
	  return defvalue;

	return prim_field_data[field.getOffset ()];
      }

      public short get (String name, short defvalue)
	throws IOException, IllegalArgumentException
      {
	ObjectStreamField field = getField (name, Short.TYPE);

	if (field == null)
	  return defvalue;

	int off = field.getOffset ();

	return (short)(((prim_field_data[off++] & 0xFF) << 8)
		       | (prim_field_data[off] & 0xFF));
      }

      public int get (String name, int defvalue)
	throws IOException, IllegalArgumentException
      {
	ObjectStreamField field = getField (name, Integer.TYPE);

	if (field == null)
	  return defvalue;

	int off = field.getOffset ();

	return ((prim_field_data[off++] & 0xFF) << 24)
	  | ((prim_field_data[off++] & 0xFF) << 16)
	  | ((prim_field_data[off++] & 0xFF) << 8)
	  | (prim_field_data[off] & 0xFF);
      }

      public long get (String name, long defvalue)
	throws IOException, IllegalArgumentException
      {
	ObjectStreamField field = getField (name, Long.TYPE);

	if (field == null)
	  return defvalue;

	int off = field.getOffset ();

	return (long)(((prim_field_data[off++] & 0xFF) << 56)
		      | ((prim_field_data[off++] & 0xFF) << 48)
		      | ((prim_field_data[off++] & 0xFF) << 40)
		      | ((prim_field_data[off++] & 0xFF) << 32)
		      | ((prim_field_data[off++] & 0xFF) << 24)
		      | ((prim_field_data[off++] & 0xFF) << 16)
		      | ((prim_field_data[off++] & 0xFF) << 8)
		      | (prim_field_data[off] & 0xFF));
      }

      public float get (String name, float defvalue)
	throws IOException, IllegalArgumentException
      {
	ObjectStreamField field = getField (name, Float.TYPE);

	if (field == null)
	  return defvalue;

	int off = field.getOffset ();

	return Float.intBitsToFloat (((prim_field_data[off++] & 0xFF) << 24)
				    | ((prim_field_data[off++] & 0xFF) << 16)
				    | ((prim_field_data[off++] & 0xFF) << 8)
				    | (prim_field_data[off] & 0xFF));
      }

      public double get (String name, double defvalue)
	throws IOException, IllegalArgumentException
      {
	ObjectStreamField field = getField (name, Double.TYPE);

	if (field == null)
	  return defvalue;

	int off = field.getOffset ();

	return Double.longBitsToDouble (
	  (long)(((prim_field_data[off++] & 0xFF) << 56)
		 | ((prim_field_data[off++] & 0xFF) << 48)
		 | ((prim_field_data[off++] & 0xFF) << 40)
		 | ((prim_field_data[off++] & 0xFF) << 32)
		 | ((prim_field_data[off++] & 0xFF) << 24)
		 | ((prim_field_data[off++] & 0xFF) << 16)
		 | ((prim_field_data[off++] & 0xFF) << 8)
		 | (prim_field_data[off] & 0xFF)));
      }

      public Object get (String name, Object defvalue)
	throws IOException, IllegalArgumentException
      {
	ObjectStreamField field =
	  getField (name, defvalue == null ? null : defvalue.getClass ());

	if (field == null)
	  return defvalue;

	return objs[field.getOffset ()];
      }

      private ObjectStreamField getField (String name, Class type)
	throws IllegalArgumentException
      {
	ObjectStreamField field = clazz.getField (name);

	if (field == null)
	  return null;

	Class field_type = field.getType ();

	if (type == field_type ||
	    (type == null && ! field_type.isPrimitive ()))
	  return field;

	throw new IllegalArgumentException ("Field requested is of type "
					    + field_type.getName ()
					    + ", but requested type was "
					    + (type == null ?
					       "Object" : type.getName ()));
      }
    };

  }


  /**
     Protected constructor that allows subclasses to override
     deserialization.  This constructor should be called by subclasses
     that wish to override <code>readObject (Object)</code>.  This
     method does a security check <i>NOTE: currently not
     implemented</i>, then sets a flag that informs
     <code>readObject (Object)</code> to call the subclasses
     <code>readObjectOverride (Object)</code> method.

     @see readObjectOverride (Object)
  */
  protected ObjectInputStream ()
    throws IOException, SecurityException
  {
    SecurityManager sec_man = System.getSecurityManager ();
    if (sec_man != null)
      sec_man.checkPermission (SUBCLASS_IMPLEMENTATION_PERMISSION);
    this.useSubclassMethod = true;
  }


  /**
     This method allows subclasses to override the default
     de serialization mechanism provided by
     <code>ObjectInputStream</code>.  To make this method be used for
     writing objects, subclasses must invoke the 0-argument
     constructor on this class from there constructor.

     @see ObjectInputStream ()
  */
  protected Object readObjectOverride ()
    throws ClassNotFoundException, IOException, OptionalDataException
  {
    throw new IOException ("Subclass of ObjectInputStream must implement readObjectOverride");
  }


  // assigns the next availible handle to OBJ
  private int assignNewHandle (Object obj)
  {
    this.objectLookupTable.put (new Integer (this.nextOID),
			     new ObjectIdentityWrapper (obj));

//    try
//    {
//      DEBUG ("Assigning handle " + this.nextOID);
//      DEBUGln (" to " + obj);
//    }
//    catch (Throwable t) {}

    return this.nextOID++;
  }


  private Object processResolution (Object obj, int handle)
    throws IOException
  {
    if (obj instanceof Serializable)
      {
        Method m = null; 
	try
	{
	  Class classArgs[] = {};
	  m = obj.getClass ().getDeclaredMethod ("readResolve", classArgs);
	  // m can't be null by definition since an exception would
	  // have been thrown so a check for null is not needed.
	  obj = m.invoke (obj, new Object[] {});	
	}
	catch (NoSuchMethodException ignore)
	{
	}
	catch (IllegalAccessException ignore)
	{
	}
	catch (InvocationTargetException ignore)
	{
	}
      }

    if (this.resolveEnabled)
      obj = resolveObject (obj);

    this.objectLookupTable.put (new Integer (handle),
				new ObjectIdentityWrapper (obj));

    return obj;
  }


  private void clearHandles ()
  {
    this.objectLookupTable.clear ();
    this.nextOID = baseWireHandle;
  }


  private void readNextBlock () throws IOException
  {
//  DEBUGln ("In readNextBlock ");
    readNextBlock (this.realInputStream.readByte ());
  }


  private void readNextBlock (byte marker) throws IOException
  {
    if (marker == TC_BLOCKDATA)
    {
      dumpElement ("BLOCK DATA SIZE=");
      this.blockDataBytes = this.realInputStream.readUnsignedByte ();
      dumpElementln (Integer.toString(this.blockDataBytes));
    }
    else if (marker == TC_BLOCKDATALONG)
    {
      dumpElement ("BLOCK DATA LONG SIZE=");
      this.blockDataBytes = this.realInputStream.readInt ();
      dumpElementln (Integer.toString(this.blockDataBytes));
    }
    else
    {
      throw new EOFException ("Attempt to read primitive data, but no data block is active.");
    }

    if (this.blockData.length < this.blockDataBytes)
      this.blockData = new byte[this.blockDataBytes];

    this.realInputStream.readFully (this.blockData, 0, this.blockDataBytes);
    this.blockDataPosition = 0;
  }


  private void readArrayElements (Object array, Class clazz)
    throws ClassNotFoundException, IOException
  {
    if (clazz.isPrimitive ())
    {
      if (clazz == Boolean.TYPE)
      {
	boolean[] cast_array = (boolean[])array;
	for (int i=0; i < cast_array.length; i++)
	  cast_array[i] = this.realInputStream.readBoolean ();
	return;
      }
      if (clazz == Byte.TYPE)
      {
	byte[] cast_array = (byte[])array;
	for (int i=0; i < cast_array.length; i++)
	  cast_array[i] = this.realInputStream.readByte ();
	return;
      }
      if (clazz == Character.TYPE)
      {
	char[] cast_array = (char[])array;
	for (int i=0; i < cast_array.length; i++)
	  cast_array[i] = this.realInputStream.readChar ();
	return;
      }
      if (clazz == Double.TYPE)
      {
	double[] cast_array = (double[])array;
	for (int i=0; i < cast_array.length; i++)
	  cast_array[i] = this.realInputStream.readDouble ();
	return;
      }
      if (clazz == Float.TYPE)
      {
	float[] cast_array = (float[])array;
	for (int i=0; i < cast_array.length; i++)
	  cast_array[i] = this.realInputStream.readFloat ();
	return;
      }
      if (clazz == Integer.TYPE)
      {
	int[] cast_array = (int[])array;
	for (int i=0; i < cast_array.length; i++)
	  cast_array[i] = this.realInputStream.readInt ();
	return;
      }
      if (clazz == Long.TYPE)
      {
	long[] cast_array = (long[])array;
	for (int i=0; i < cast_array.length; i++)
	  cast_array[i] = this.realInputStream.readLong ();
	return;
      }
      if (clazz == Short.TYPE)
      {
	short[] cast_array = (short[])array;
	for (int i=0; i < cast_array.length; i++)
	  cast_array[i] = this.realInputStream.readShort ();
	return;
      }
    }
    else
    {
      Object[] cast_array = (Object[])array;
      for (int i=0; i < cast_array.length; i++)
 	  cast_array[i] = readObject ();
    }
  }


  private void readFields (Object obj, ObjectStreamField[] stream_fields,
			   boolean call_read_method,
			   ObjectStreamClass stream_osc)
    throws ClassNotFoundException, IOException
  {
//  DEBUGln ("In readFields");
    if (call_read_method)
    {
//    DEBUGln ("  call_read_method is true");
      fieldsAlreadyRead = false;
      setBlockDataMode (true);
      callReadMethod (obj, stream_osc.forClass ());
      setBlockDataMode (false);
      return;
    }

    ObjectStreamField[] real_fields =
      ObjectStreamClass.lookup (stream_osc.forClass ()).fields;

    boolean default_initialize, set_value;
    String field_name = null;
    Class type = null;
    ObjectStreamField stream_field = null;
    ObjectStreamField real_field = null;
    int stream_idx = 0;
    int real_idx = 0;

    while (stream_idx < stream_fields.length
	   && real_idx < real_fields.length)
    {
      default_initialize = false;
      set_value = true;

      if (stream_idx == stream_fields.length)
	default_initialize = true;
      else
      {
	stream_field = stream_fields[stream_idx];
	type = stream_field.getType ();
      }

      if (real_idx == real_fields.length)
	set_value = false;
      else
      {
	real_field = real_fields[real_idx];
	type = real_field.getType ();
	field_name = real_field.getName ();
      }

      if (set_value && !default_initialize)
      {
	int comp_val =
	  real_field.compareTo (stream_field);

	if (comp_val < 0)
	{
	  default_initialize = true;
	  real_idx++;
	}
	else if (comp_val > 0)
	{
	  set_value = false;
	  stream_idx++;
	}
	else
	{
	  real_idx++;
	  stream_idx++;
	}
      }

      if (type == Boolean.TYPE)
      {
	boolean value =
	  default_initialize ? false : this.realInputStream.readBoolean ();
	if (!default_initialize && set_value)
	  dumpElementln ("  " + field_name + ": " + value);
	if (set_value)
	  setBooleanField (obj, field_name, value);
      }
      else if (type == Byte.TYPE)
      {
	byte value =
	  default_initialize ? 0 : this.realInputStream.readByte ();
	if (!default_initialize && set_value)
	  dumpElementln ("  " + field_name + ": " + value);
	if (set_value)
	  setByteField (obj, field_name, value);
      }
      else if (type == Character.TYPE)
      {
	char value =
	  default_initialize ? (char)0 : this.realInputStream.readChar ();
	if (!default_initialize && set_value)
	  dumpElementln ("  " + field_name + ": " + value);
	if (set_value)
	  setCharField (obj, field_name, value);
      }
      else if (type == Double.TYPE)
      {
	double value =
	  default_initialize ? 0 : this.realInputStream.readDouble ();
	if (!default_initialize && set_value)
	  dumpElementln ("  " + field_name + ": " + value);
	if (set_value)
	  setDoubleField (obj, field_name, value);
      }
      else if (type == Float.TYPE)
      {
	float value =
	  default_initialize ? 0 : this.realInputStream.readFloat ();
	if (!default_initialize && set_value)
	  dumpElementln ("  " + field_name + ": " + value);
	if (set_value)
	  setFloatField (obj, field_name, value);
      }
      else if (type == Integer.TYPE)
      {
	int value =
	  default_initialize ? 0 : this.realInputStream.readInt ();
	if (!default_initialize && set_value)
	  dumpElementln ("  " + field_name + ": " + value);
	if (set_value)
	  setIntField (obj, field_name, value);
      }
      else if (type == Long.TYPE)
      {
	long value =
	  default_initialize ? 0 : this.realInputStream.readLong ();
	if (!default_initialize && set_value)
	  dumpElementln ("  " + field_name + ": " + value);
	if (set_value)
	  setLongField (obj, field_name, value);
      }
      else if (type == Short.TYPE)
      {
	short value =
	  default_initialize ? (short)0 : this.realInputStream.readShort ();
	if (!default_initialize && set_value)
	  dumpElementln ("  " + field_name + ": " + value);
	if (set_value)
	  setShortField (obj, field_name, value);
      }
      else
      {
	Object value =
	  default_initialize ? null : readObject ();
	if (set_value)
	  setObjectField (obj, field_name,
			  real_field.getTypeString (), value);
      }
    }
  }


  // Toggles writing primitive data to block-data buffer.
  private void setBlockDataMode (boolean on)
  {
//    DEBUGln ("Setting block data mode to " + on);

    this.readDataFromBlock = on;

    if (on)
      this.dataInputStream = this.blockDataInput;
    else
      this.dataInputStream = this.realInputStream;
  }


  // returns a new instance of REAL_CLASS that has been constructed
  // only to the level of CONSTRUCTOR_CLASS (a super class of REAL_CLASS)
  private Object newObject (Class real_class, Class constructor_class)
  {
    try
    {
      Object obj = allocateObject (real_class);
      callConstructor (constructor_class, obj);
      return obj;
    }
    catch (InstantiationException e)
    {
      return null;
    }
  }


  // runs all registered ObjectInputValidations in prioritized order
  // on OBJ
  private void invokeValidators () throws InvalidObjectException
  {
    Object[] validators = new Object[this.validators.size ()];
    this.validators.copyInto (validators);
    Arrays.sort (validators);

    try
    {
      for (int i=0; i < validators.length; i++)
	((ObjectInputValidation)validators[i]).validateObject ();
    }
    finally
    {
      this.validators.removeAllElements ();
    }
  }


  // this native method is used to get access to the protected method
  // of the same name in SecurityManger
  private static ClassLoader currentClassLoader (SecurityManager sm)
  {
    // FIXME: This is too simple.
    return ClassLoader.getSystemClassLoader ();
  }

  private static native Field getField (Class klass, String name)
    throws java.lang.NoSuchFieldException;

  private static native Method getMethod (Class klass, String name, Class args[])
    throws java.lang.NoSuchMethodException;

  private void callReadMethod (Object obj, Class klass) throws IOException
  {
    try
      {
	Class classArgs[] = {ObjectInputStream.class};
	Method m = getMethod (klass, "readObject", classArgs);
	if (m == null)
	  return;
	Object args[] = {this};
	m.invoke (obj, args);
      }
    catch (InvocationTargetException x)
      {
        /* Rethrow if possible. */
	Throwable exception = x.getTargetException();
	if (exception instanceof RuntimeException)
	  throw (RuntimeException) exception;
	if (exception instanceof IOException)
	  throw (IOException) exception;

	throw new IOException ("Exception thrown from readObject() on " +
			       klass + ": " + exception.getClass().getName());
      }
    catch (Exception x)
      {
	throw new IOException ("Failure invoking readObject() on " +
			       klass + ": " + x.getClass().getName());
      }
  }
    
  private native Object allocateObject (Class clazz)
    throws InstantiationException;

  private native void callConstructor (Class clazz, Object obj);

  private void setBooleanField (Object obj, String field_name,
				boolean val)
  {
    try
      {
	Class klass = obj.getClass ();
	Field f = getField (klass, field_name);
	f.setBoolean (obj, val);
      }
    catch (Exception _)
      {
      }    
  }

  private void setByteField (Object obj, String field_name,
				byte val)
  {
    try
      {
	Class klass = obj.getClass ();
	Field f = getField (klass, field_name);
	f.setByte (obj, val);
      }
    catch (Exception _)
      {
      }    
  }

  private void setCharField (Object obj, String field_name,
			     char val)
  {
    try
      {
	Class klass = obj.getClass ();
	Field f = getField (klass, field_name);
	f.setChar (obj, val);
      }
    catch (Exception _)
      {
      }    
  }

  private void setDoubleField (Object obj, String field_name,
			       double val)
  {
    try
      {
	Class klass = obj.getClass ();
	Field f = getField (klass, field_name);
	f.setDouble (obj, val);
      }
    catch (Exception _)
      {
      }    
  }

  private void setFloatField (Object obj, String field_name,
			      float val)
  {
    try
      {
	Class klass = obj.getClass ();
	Field f = getField (klass, field_name);
	f.setFloat (obj, val);
      }
    catch (Exception _)
      {
      }    
  }

  private void setIntField (Object obj, String field_name,
			      int val)
  {
    try
      {
	Class klass = obj.getClass ();
	Field f = getField (klass, field_name);
	f.setInt (obj, val);
      }
    catch (Exception _)
      {
      }    
  }


  private void setLongField (Object obj, String field_name,
			      long val)
  {
    try
      {
	Class klass = obj.getClass ();
	Field f = getField (klass, field_name);
	f.setLong (obj, val);
      }
    catch (Exception _)
      {
      }    
  }


  private void setShortField (Object obj, String field_name,
			      short val)
  {
    try
      {
	Class klass = obj.getClass ();
	Field f = getField (klass, field_name);
	f.setShort (obj, val);
      }
    catch (Exception _)
      {
      }    
  }


  private void setObjectField (Object obj, String field_name, String type_code,
			       Object val)
  {
    try
      {
	Class klass = obj.getClass ();
	Field f = getField (klass, field_name);
	// FIXME: We should check the type_code here
	f.set (obj, val);
      }
    catch (Exception _)
      {
      }    
  }

  private static final int BUFFER_SIZE = 1024;
  private static final Class[] readObjectParams = { ObjectInputStream.class };

  private DataInputStream realInputStream;
  private DataInputStream dataInputStream;
  private DataInputStream blockDataInput;
  private int blockDataPosition;
  private int blockDataBytes;
  private byte[] blockData;
  private boolean useSubclassMethod;
  private int nextOID;
  private boolean resolveEnabled;
  private Hashtable objectLookupTable;
  private Object currentObject;
  private ObjectStreamClass currentObjectStreamClass;
  private boolean readDataFromBlock;
  private boolean isDeserializing;
  private boolean fieldsAlreadyRead;
  private Vector validators;

  private static boolean dump;  

  private void dumpElement (String msg)
  {
    if (Configuration.DEBUG && dump)  
      System.out.print(msg);
  }
  
  private void dumpElementln (String msg)
  {
    if (Configuration.DEBUG && dump)
      System.out.println(msg);
  }
}


// used to keep a prioritized list of object validators
class ValidatorAndPriority implements Comparable
{
  int priority;
  ObjectInputValidation validator;

  ValidatorAndPriority (ObjectInputValidation validator, int priority)
  {
    this.priority = priority;
    this.validator = validator;
  }

  public int compareTo (Object o)
  {
    ValidatorAndPriority vap = (ValidatorAndPriority)o;
    return this.priority - vap.priority;
  }
}
