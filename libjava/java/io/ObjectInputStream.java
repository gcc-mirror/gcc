/* ObjectInputStream.java -- Class used to read serialized objects
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003 Free Software Foundation, Inc.

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

import java.lang.reflect.Array;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.security.PrivilegedAction;
import java.security.AccessController;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Vector;


import gnu.java.io.ObjectIdentityWrapper;
import gnu.java.lang.reflect.TypeSignature;
import java.lang.reflect.Field;
import java.lang.reflect.Method;
import java.lang.reflect.InvocationTargetException;

import gnu.classpath.Configuration;

public class ObjectInputStream extends InputStream
  implements ObjectInput, ObjectStreamConstants
{
  /**
   * Creates a new <code>ObjectInputStream</code> that will do all of
   * its reading from <code>in</code>.  This method also checks
   * the stream by reading the header information (stream magic number
   * and stream version).
   *
   * @exception IOException Reading stream header from underlying
   * stream cannot be completed.
   *
   * @exception StreamCorruptedException An invalid stream magic
   * number or stream version was read from the stream.
   *
   * @see #readStreamHeader()
   */
  public ObjectInputStream(InputStream in)
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
    this.blockDataInput = new DataInputStream(this);
    this.realInputStream = new DataInputStream(in);
    this.nextOID = baseWireHandle;
    this.objectLookupTable = new Hashtable();
    this.validators = new Vector();
    this.classLookupTable = new Hashtable();
    setBlockDataMode(true);
    readStreamHeader();
  }


  /**
   * Returns the next deserialized object read from the underlying stream.
   *
   * This method can be overriden by a class by implementing
   * <code>private void readObject (ObjectInputStream)</code>.
   *
   * If an exception is thrown from this method, the stream is left in
   * an undefined state.
   *
   * @exception ClassNotFoundException The class that an object being
   * read in belongs to cannot be found.
   *
   * @exception IOException Exception from underlying
   * <code>InputStream</code>.
   */
  public final Object readObject() throws ClassNotFoundException, IOException
  {
    if (this.useSubclassMethod)
      return readObjectOverride();

    boolean was_deserializing;

    Object ret_val;
    was_deserializing = this.isDeserializing;

    boolean is_consumed = false;
    boolean old_mode = setBlockDataMode(false);

    this.isDeserializing = true;

    byte marker = this.realInputStream.readByte();
    dumpElement("MARKER: 0x" + Integer.toHexString(marker) + " ");

    try
      {
	switch (marker)
	  {
	  case TC_ENDBLOCKDATA:
	    {
	      ret_val = null;
	      is_consumed = true;
	      break;
	    }

	  case TC_BLOCKDATA:
	  case TC_BLOCKDATALONG:
	    {
	      if (marker == TC_BLOCKDATALONG)
		dumpElementln("BLOCKDATALONG");
	      else
		dumpElementln("BLOCKDATA");
	      readNextBlock(marker);
	      throw new StreamCorruptedException("Unexpected blockData");
	    }

	  case TC_NULL:
	    {
	      dumpElementln("NULL");
	      ret_val = null;
	      break;
	    }

	  case TC_REFERENCE:
	    {
	      dumpElement("REFERENCE ");
	      Integer oid = new Integer(this.realInputStream.readInt());
	      dumpElementln(Integer.toHexString(oid.intValue()));
	      ret_val = ((ObjectIdentityWrapper)
			 this.objectLookupTable.get(oid)).object;
	      break;
	    }

	  case TC_CLASS:
	    {
	      dumpElementln("CLASS");
	      ObjectStreamClass osc = (ObjectStreamClass)readObject();
	      Class clazz = osc.forClass();
	      assignNewHandle(clazz);
	      ret_val = clazz;
	      break;
	    }

	  case TC_PROXYCLASSDESC:
	    {
	      dumpElementln("PROXYCLASS");
	      int n_intf = this.realInputStream.readInt();
	      String[] intfs = new String[n_intf];
	      for (int i = 0; i < n_intf; i++)
		{
		  intfs[i] = this.realInputStream.readUTF();
		  System.out.println(intfs[i]);
		}
	      
	      boolean oldmode = setBlockDataMode(true);
	      Class cl = resolveProxyClass(intfs);
	      setBlockDataMode(oldmode);
	      
	      ObjectStreamClass osc = lookupClass(cl);
	      assignNewHandle(osc);
	      
	      if (!is_consumed)
		{
		  byte b = this.realInputStream.readByte();
		  if (b != TC_ENDBLOCKDATA)
		    throw new IOException("Data annotated to class was not consumed." + b);
		}
	      else
		is_consumed = false;
	      ObjectStreamClass superosc = (ObjectStreamClass)readObject();
	      osc.setSuperclass(superosc);
	      ret_val = osc;
	      break;
	    }

	  case TC_CLASSDESC:
	    {
	      ObjectStreamClass osc = readClassDescriptor();
	      
	      if (!is_consumed)
		{
		  byte b = this.realInputStream.readByte();
		  if (b != TC_ENDBLOCKDATA)
		    throw new IOException("Data annotated to class was not consumed." + b);
		}
	      else
		is_consumed = false;
	      
	      osc.setSuperclass ((ObjectStreamClass)readObject());
	      ret_val = osc;
	      break;
	    }

	  case TC_STRING:
	  case TC_LONGSTRING:
	    {
	      dumpElement("STRING=");
	      String s = this.realInputStream.readUTF();
	      dumpElementln(s);
	      ret_val = processResolution(s, assignNewHandle(s));
	      break;
	    }

	  case TC_ARRAY:
	    {
	      dumpElementln("ARRAY");
	      ObjectStreamClass osc = (ObjectStreamClass)readObject();
	      Class componentType = osc.forClass().getComponentType();
	      dumpElement("ARRAY LENGTH=");
	      int length = this.realInputStream.readInt();
	      dumpElementln (length + "; COMPONENT TYPE=" + componentType);
	      Object array = Array.newInstance(componentType, length);
	      int handle = assignNewHandle(array);
	      readArrayElements(array, componentType);
	      for (int i = 0, len = Array.getLength(array); i < len; i++)
		dumpElementln("  ELEMENT[" + i + "]=" + Array.get(array, i));
	      ret_val = processResolution(array, handle);
	      break;
	    }

	  case TC_OBJECT:
	    {
	      dumpElementln("OBJECT");
	      ObjectStreamClass osc = (ObjectStreamClass)readObject();
	      Class clazz = osc.forClass();
	      
	      if (!Serializable.class.isAssignableFrom(clazz))
		throw new NotSerializableException
		  (clazz + " is not Serializable, and thus cannot be deserialized.");
	      
	      if (Externalizable.class.isAssignableFrom(clazz))
		{
		  Externalizable obj = null;
		  
		  try
		    {
		      obj = (Externalizable)clazz.newInstance();
		    }
		  catch (InstantiationException e)
		    {
		      throw new ClassNotFoundException
			("Instance of " + clazz + " could not be created");
		    }
		  catch (IllegalAccessException e)
		    {
		      throw new ClassNotFoundException
			("Instance of " + clazz + " could not be created because class or "
			 + "zero-argument constructor is not accessible");
		    }
		  catch (NoSuchMethodError e)
		    {
		      throw new ClassNotFoundException
			("Instance of " + clazz
			 + " could not be created because zero-argument constructor is not defined");
		    }
		  
		  int handle = assignNewHandle(obj);
		  
		  boolean read_from_blocks = ((osc.getFlags() & SC_BLOCK_DATA) != 0);
		  
		  boolean oldmode = this.readDataFromBlock;
		  if (read_from_blocks)
		    setBlockDataMode(true);
		  
		  obj.readExternal(this);
		  
		  if (read_from_blocks)
		    setBlockDataMode(oldmode);
		  
		  ret_val = processResolution(obj, handle);
		  break;
		} // end if (Externalizable.class.isAssignableFrom (clazz))
	      
	      // find the first non-serializable, non-abstract
	      // class in clazz's inheritance hierarchy
	      Class first_nonserial = clazz.getSuperclass();
	      while (Serializable.class.isAssignableFrom(first_nonserial)
		     || Modifier.isAbstract(first_nonserial.getModifiers()))
		first_nonserial = first_nonserial.getSuperclass();
	      
	      Object obj = null;
	      obj = newObject(clazz, first_nonserial);
	      
	      if (obj == null)
		throw new ClassNotFoundException
		  ("Instance of " + clazz + " could not be created");
	      
	      int handle = assignNewHandle(obj);
	      this.currentObject = obj;
	      ObjectStreamClass[] hierarchy =
		inputGetObjectStreamClasses(clazz);
	      
	      for (int i = 0; i < hierarchy.length; i++)
		{
		  this.currentObjectStreamClass = hierarchy[i];
		  
		  dumpElementln("Reading fields of " + this.currentObjectStreamClass.getName ());

		  // XXX: should initialize fields in classes in the hierarchy
		  // that aren't in the stream
		  // should skip over classes in the stream that aren't in the
		  // real classes hierarchy
		  
		  if (this.currentObjectStreamClass.hasReadMethod())
		    {
		      fieldsAlreadyRead = false;
		      boolean oldmode = setBlockDataMode(true);
		      callReadMethod(obj, this.currentObjectStreamClass);
		      setBlockDataMode(oldmode);
		      dumpElement("ENDBLOCKDATA? ");
		      try
			{
			  // FIXME: XXX: This try block is to catch EOF which is
			  // thrown for some objects.  That indicates a bug in the logic.
			  if (this.realInputStream.readByte() != TC_ENDBLOCKDATA)
			    throw new IOException
			      ("No end of block data seen for class with readObject (ObjectInputStream) method.");
			  dumpElementln("yes");
			}
		      catch (EOFException e)
			{
			  dumpElementln("no, got EOFException");
			}
		      catch (IOException e)
			{
			  dumpElementln("no, got IOException");
			}
		    }
		  else
		    {
		      readFields(obj, currentObjectStreamClass);
		    }
		}

	      this.currentObject = null;
	      this.currentObjectStreamClass = null;
	      ret_val = processResolution(obj, handle);
	      break;
	    }

	  case TC_RESET:
	    dumpElementln("RESET");
	    clearHandles();
	    ret_val = readObject();
	    break;

	  case TC_EXCEPTION:
	    {
	      dumpElement("EXCEPTION=");
	      Exception e = (Exception)readObject();
	      dumpElementln(e.toString());
	      clearHandles();
	      throw new WriteAbortedException("Exception thrown during writing of stream", e);
	    }

	  default:
	    throw new IOException("Unknown marker on stream: " + marker);
	  }
      }
    finally
      {
	setBlockDataMode(old_mode);
	
	this.isDeserializing = was_deserializing;
	
	if (! was_deserializing)
	  {
	    if (validators.size() > 0)
	      invokeValidators();
	  }
      }
    
    return ret_val;
  }

  /**
   * This method reads a class descriptor from the real input stream
   * and use these data to create a new instance of ObjectStreamClass.
   * Fields are sorted and ordered for the real read which occurs for
   * each instance of the described class. Be aware that if you call that
   * method you must ensure that the stream is synchronized, in the other
   * case it may be completely desynchronized.
   *
   * @return A new instance of ObjectStreamClass containing the freshly
   * created descriptor.
   * @throws ClassNotFoundException if the required class to build the
   * descriptor has not been found in the system.
   * @throws IOException An input/output error occured.
   * @throws InvalidClassException If there was a compatibility problem
   * between the class present in the system and the serialized class.
   */
  protected ObjectStreamClass readClassDescriptor()
    throws ClassNotFoundException, IOException
  {
    dumpElement("CLASSDESC NAME=");
    String name = this.realInputStream.readUTF();
    dumpElement(name + "; UID=");
    long uid = this.realInputStream.readLong ();
    dumpElement(Long.toHexString(uid) + "; FLAGS=");
    byte flags = this.realInputStream.readByte ();
    dumpElement(Integer.toHexString(flags) + "; FIELD COUNT=");
    short field_count = this.realInputStream.readShort();
    dumpElementln(Short.toString(field_count));
    ObjectStreamField[] fields = new ObjectStreamField[field_count];
    ObjectStreamClass osc = new ObjectStreamClass(name, uid,
						  flags, fields);
    assignNewHandle(osc);
	      
    for (int i = 0; i < field_count; i++)
      {
	dumpElement("  TYPE CODE=");
	char type_code = (char)this.realInputStream.readByte();
	dumpElement(type_code + "; FIELD NAME=");
	String field_name = this.realInputStream.readUTF();
	dumpElementln(field_name);
	String class_name;
		  
	// If the type code is an array or an object we must
	// decode a String here. In the other case we convert
	// the type code and pass it to ObjectStreamField.
	// Type codes are decoded by gnu.java.lang.reflect.TypeSignature.
	if (type_code == 'L' || type_code == '[')
	  class_name = (String)readObject();
	else
	  class_name = String.valueOf(type_code);
		  
	fields[i] =
	  new ObjectStreamField(field_name, class_name, currentLoader());
      }
	      
    /* Now that fields have been read we may resolve the class
     * (and read annotation if needed). */
    Class clazz = resolveClass(osc);
    
    for (int i = 0; i < field_count; i++)
      {
	Field f;
	
	try
	  {
	    f = clazz.getDeclaredField(fields[i].getName());
	    if (f != null && !f.getType().equals(fields[i].getType()))
	      throw new InvalidClassException
		("invalid field type for " + fields[i].getName() + " in class "
		 + name + " (requested was \"" + fields[i].getType()
		 + " and found \"" + f.getType() + "\")"); 
	  }
	catch (NoSuchFieldException _)
	  {
	  }
      }

    boolean oldmode = setBlockDataMode(true);
    osc.setClass(clazz, lookupClass(clazz.getSuperclass()));
    classLookupTable.put(clazz, osc);
    setBlockDataMode(oldmode);

    return osc;
  }

  /**
   * Reads the current objects non-transient, non-static fields from
   * the current class from the underlying output stream.
   *
   * This method is intended to be called from within a object's
   * <code>private void readObject (ObjectInputStream)</code>
   * method.
   *
   * @exception ClassNotFoundException The class that an object being
   * read in belongs to cannot be found.
   *
   * @exception NotActiveException This method was called from a
   * context other than from the current object's and current class's
   * <code>private void readObject (ObjectInputStream)</code>
   * method.
   *
   * @exception IOException Exception from underlying
   * <code>OutputStream</code>.
   */
  public void defaultReadObject()
    throws ClassNotFoundException, IOException, NotActiveException
  {
    if (this.currentObject == null || this.currentObjectStreamClass == null)
      throw new NotActiveException("defaultReadObject called by non-active"
				   + " class and/or object");

    if (fieldsAlreadyRead)
      throw new NotActiveException("defaultReadObject called but fields "
				   + "already read from stream (by "
				   + "defaultReadObject or readFields)");

    boolean oldmode = setBlockDataMode(false);
    readFields(this.currentObject, this.currentObjectStreamClass);
    setBlockDataMode(oldmode);

    fieldsAlreadyRead = true;
  }


  /**
   * Registers a <code>ObjectInputValidation</code> to be carried out
   * on the object graph currently being deserialized before it is
   * returned to the original caller of <code>readObject ()</code>.
   * The order of validation for multiple
   * <code>ObjectInputValidation</code>s can be controled using
   * <code>priority</code>.  Validators with higher priorities are
   * called first.
   *
   * @see java.io.ObjectInputValidation
   *
   * @exception InvalidObjectException <code>validator</code> is
   * <code>null</code>
   *
   * @exception NotActiveException an attempt was made to add a
   * validator outside of the <code>readObject</code> method of the
   * object currently being deserialized
   */
  public void registerValidation(ObjectInputValidation validator,
				 int priority)
    throws InvalidObjectException, NotActiveException
  {
    if (this.currentObject == null || this.currentObjectStreamClass == null)
      throw new NotActiveException("registerValidation called by non-active "
				   + "class and/or object");

    if (validator == null)
      throw new InvalidObjectException("attempt to add a null "
				       + "ObjectInputValidation object");

    this.validators.addElement(new ValidatorAndPriority (validator,
							 priority));
  }


  /**
   * Called when a class is being deserialized.  This is a hook to
   * allow subclasses to read in information written by the
   * <code>annotateClass (Class)</code> method of an
   * <code>ObjectOutputStream</code>.
   *
   * This implementation looks up the active call stack for a
   * <code>ClassLoader</code>; if a <code>ClassLoader</code> is found,
   * it is used to load the class associated with <code>osc</code>,
   * otherwise, the default system <code>ClassLoader</code> is used.
   *
   * @exception IOException Exception from underlying
   * <code>OutputStream</code>.
   *
   * @see java.io.ObjectOutputStream#annotateClass (java.lang.Class)
   */
  protected Class resolveClass(ObjectStreamClass osc)
    throws ClassNotFoundException, IOException
  {
    return Class.forName(osc.getName(), true, currentLoader());
  }

  /**
   * This method invokes the method currentClassLoader for the
   * current security manager (or build an empty one if it is not
   * present).
   *
   * @return The most recent non-system ClassLoader on the execution stack.
   * @see java.lang.SecurityManager#currentClassLoader()
   */
  private ClassLoader currentLoader()
  {
    SecurityManager sm = System.getSecurityManager();
    if (sm == null)
      sm = new SecurityManager () {};
    
    return currentClassLoader(sm);
  }

  /**
   * Lookup a class stored in the local hashtable. If it is not
   * use the global lookup function in ObjectStreamClass to build
   * the ObjectStreamClass. This method is requested according to
   * the behaviour detected in the JDK by Kaffe's team.
   *
   * @param clazz Class to lookup in the hash table or for which
   * we must build a descriptor.
   * @return A valid instance of ObjectStreamClass corresponding
   * to the specified class.
   */
  private ObjectStreamClass lookupClass(Class clazz)
  {
    ObjectStreamClass oclazz;

    oclazz = (ObjectStreamClass)classLookupTable.get(clazz);
    if (oclazz == null)
      return ObjectStreamClass.lookup(clazz);
    else
      return oclazz;
  }

  /**
   * Reconstruct class hierarchy the same way
   * {@link java.io.ObjectStreamClass.getObjectStreamClasses(java.lang.Class)} does
   * but using lookupClass instead of ObjectStreamClass.lookup. This
   * dup is necessary localize the lookup table. Hopefully some future
   * rewritings will be able to prevent this.
   *
   * @param clazz This is the class for which we want the hierarchy.
   *
   * @return An array of valid {@link java.io.ObjectStreamClass} instances which
   * represent the class hierarchy for clazz.
   */
  private ObjectStreamClass[] inputGetObjectStreamClasses(Class clazz)
  {
    ObjectStreamClass osc = lookupClass(clazz);

    if (osc == null)
      return new ObjectStreamClass[0];
    else
      {
        Vector oscs = new Vector();

        while (osc != null)
          {
            oscs.addElement(osc);
            osc = osc.getSuper();
	  }

        int count = oscs.size();
	ObjectStreamClass[] sorted_oscs = new ObjectStreamClass[count];

        for (int i = count - 1; i >= 0; i--)
          sorted_oscs[count - i - 1] = (ObjectStreamClass) oscs.elementAt(i);

        return sorted_oscs;
      }
  }

  /**
   * Allows subclasses to resolve objects that are read from the
   * stream with other objects to be returned in their place.  This
   * method is called the first time each object is encountered.
   *
   * This method must be enabled before it will be called in the
   * serialization process.
   *
   * @exception IOException Exception from underlying
   * <code>OutputStream</code>.
   *
   * @see #enableResolveObject(boolean)
   */
  protected Object resolveObject(Object obj) throws IOException
  {
    return obj;
  }


  protected Class resolveProxyClass(String[] intfs)
    throws IOException, ClassNotFoundException
  {
    SecurityManager sm = System.getSecurityManager();
    
    if (sm == null)
      sm = new SecurityManager() {};
    
    ClassLoader cl = currentClassLoader(sm);
    
    Class[] clss = new Class[intfs.length];
    if(cl == null)
      {
	for (int i = 0; i < intfs.length; i++)
	  clss[i] = Class.forName(intfs[i]);
	cl = ClassLoader.getSystemClassLoader();
      }
    else
      for (int i = 0; i < intfs.length; i++)
	clss[i] = cl.loadClass(intfs[i]);
    try 
      {
	return Proxy.getProxyClass(cl, clss);
      } 
    catch (IllegalArgumentException e) 
      {
	throw new ClassNotFoundException(null, e);
      }
  }
  
  /**
   * If <code>enable</code> is <code>true</code> and this object is
   * trusted, then <code>resolveObject (Object)</code> will be called
   * in subsequent calls to <code>readObject (Object)</code>.
   * Otherwise, <code>resolveObject (Object)</code> will not be called.
   *
   * @exception SecurityException This class is not trusted.
   */
  protected boolean enableResolveObject (boolean enable)
    throws SecurityException
  {
    if (enable)
      {
	SecurityManager sm = System.getSecurityManager();
	if (sm != null)
	  sm.checkPermission(new SerializablePermission("enableSubstitution"));
      }

    boolean old_val = this.resolveEnabled;
    this.resolveEnabled = enable;
    return old_val;
  }

  /**
   * Reads stream magic and stream version information from the
   * underlying stream.
   *
   * @exception IOException Exception from underlying stream.
   *
   * @exception StreamCorruptedException An invalid stream magic
   * number or stream version was read from the stream.
   */
  protected void readStreamHeader()
    throws IOException, StreamCorruptedException
  {
    dumpElement("STREAM MAGIC ");
    if (this.realInputStream.readShort() != STREAM_MAGIC)
      throw new StreamCorruptedException("Invalid stream magic number");

    dumpElementln("STREAM VERSION ");
    if (this.realInputStream.readShort() != STREAM_VERSION)
      throw new StreamCorruptedException("Invalid stream version number");
  }

  public int read() throws IOException
  {
    if (this.readDataFromBlock)
      {
	if (this.blockDataPosition >= this.blockDataBytes)
	  readNextBlock();
	return (this.blockData[this.blockDataPosition++] & 0xff);
      }
    else
      return this.realInputStream.read();
  }

  public int read(byte[] data, int offset, int length) throws IOException
  {
    if (this.readDataFromBlock)
      {
	if (this.blockDataPosition + length > this.blockDataBytes)
	  {
	    int remain = this.blockDataBytes - this.blockDataPosition;
	    if (remain != 0)
	      {
		System.arraycopy(this.blockData, this.blockDataPosition,
				 data, offset, remain);
		offset += remain;
		length -= remain;
	      }
	    readNextBlock ();
	  }

	System.arraycopy(this.blockData, this.blockDataPosition,
			 data, offset, length);
	this.blockDataPosition += length;

	return length;
      }
    else
      return this.realInputStream.read(data, offset, length);
  }

  public int available() throws IOException
  {
    if (this.readDataFromBlock)
      {
	if (this.blockDataPosition >= this.blockDataBytes)
	  readNextBlock ();

	return this.blockDataBytes - this.blockDataPosition;
      }
    else
      return this.realInputStream.available();
  }

  public void close() throws IOException
  {
    this.realInputStream.close();
  }

  public boolean readBoolean() throws IOException
  {
    boolean switchmode = true;
    boolean oldmode = this.readDataFromBlock;
    if (!oldmode || this.blockDataBytes - this.blockDataPosition >= 1)
      switchmode = false;
    if (switchmode)
      oldmode = setBlockDataMode (true);
    boolean value = this.dataInputStream.readBoolean ();
    if (switchmode)
      setBlockDataMode (oldmode);
    return value;
  }

  public byte readByte() throws IOException
  {
    boolean switchmode = true;
    boolean oldmode = this.readDataFromBlock;
    if (!oldmode || this.blockDataBytes - this.blockDataPosition >= 1)
      switchmode = false;
    if (switchmode)
      oldmode = setBlockDataMode(true);
    byte value = this.dataInputStream.readByte();
    if (switchmode)
      setBlockDataMode(oldmode);
    return value;
  }

  public int readUnsignedByte() throws IOException
  {
    boolean switchmode = true;
    boolean oldmode = this.readDataFromBlock;
    if (!oldmode || this.blockDataBytes - this.blockDataPosition >= 1)
      switchmode = false;
    if (switchmode)
      oldmode = setBlockDataMode(true);
    int value = this.dataInputStream.readUnsignedByte();
    if (switchmode)
      setBlockDataMode(oldmode);
    return value;
  }

  public short readShort() throws IOException
  {
    boolean switchmode = true;
    boolean oldmode = this.readDataFromBlock;
    if (!oldmode || this.blockDataBytes - this.blockDataPosition >= 2)
      switchmode = false;
    if (switchmode)
      oldmode = setBlockDataMode(true);
    short value = this.dataInputStream.readShort();
    if (switchmode)
      setBlockDataMode(oldmode);
    return value;
  }

  public int readUnsignedShort() throws IOException
  {
    boolean switchmode = true;
    boolean oldmode = this.readDataFromBlock;
    if (!oldmode || this.blockDataBytes - this.blockDataPosition >= 2)
      switchmode = false;
    if (switchmode)
      oldmode = setBlockDataMode(true);
    int value = this.dataInputStream.readUnsignedShort();
    if (switchmode)
      setBlockDataMode(oldmode);
    return value;
  }

  public char readChar() throws IOException
  {
    boolean switchmode = true;
    boolean oldmode = this.readDataFromBlock;
    if (!oldmode || this.blockDataBytes - this.blockDataPosition >= 2)
      switchmode = false;
    if (switchmode)
      oldmode = setBlockDataMode(true);
    char value = this.dataInputStream.readChar();
    if (switchmode)
      setBlockDataMode(oldmode);
    return value;
  }

  public int readInt() throws IOException
  {
    boolean switchmode = true;
    boolean oldmode = this.readDataFromBlock;
    if (!oldmode || this.blockDataBytes - this.blockDataPosition >= 4)
      switchmode = false;
    if (switchmode)
      oldmode = setBlockDataMode(true);
    int value = this.dataInputStream.readInt();
    if (switchmode)
      setBlockDataMode(oldmode);
    return value;
  }

  public long readLong() throws IOException
  {
    boolean switchmode = true;
    boolean oldmode = this.readDataFromBlock;
    if (!oldmode || this.blockDataBytes - this.blockDataPosition >= 8)
      switchmode = false;
    if (switchmode)
      oldmode = setBlockDataMode(true);
    long value = this.dataInputStream.readLong();
    if (switchmode)
      setBlockDataMode(oldmode);
    return value;
  }

  public float readFloat() throws IOException
  {
    boolean switchmode = true;
    boolean oldmode = this.readDataFromBlock;
    if (!oldmode || this.blockDataBytes - this.blockDataPosition >= 4)
      switchmode = false;
    if (switchmode)
      oldmode = setBlockDataMode(true);
    float value = this.dataInputStream.readFloat();
    if (switchmode)
      setBlockDataMode(oldmode);
    return value;
  }

  public double readDouble() throws IOException
  {
    boolean switchmode = true;
    boolean oldmode = this.readDataFromBlock;
    if (!oldmode || this.blockDataBytes - this.blockDataPosition >= 8)
      switchmode = false;
    if (switchmode)
      oldmode = setBlockDataMode(true);
    double value = this.dataInputStream.readDouble();
    if (switchmode)
      setBlockDataMode(oldmode);
    return value;
  }

  public void readFully(byte data[]) throws IOException
  {
    this.dataInputStream.readFully(data);
  }

  public void readFully(byte data[], int offset, int size)
    throws IOException
  {
    this.dataInputStream.readFully(data, offset, size);
  }

  public int skipBytes(int len) throws IOException
  {
    return this.dataInputStream.skipBytes(len);
  }

  /**
   * @deprecated
   * @see java.io.DataInputStream#readLine ()
   */
  public String readLine() throws IOException
  {
    return this.dataInputStream.readLine();
  }

  public String readUTF() throws IOException
  {
    return this.dataInputStream.readUTF();
  }

  /**
   * This class allows a class to specify exactly which fields should
   * be read, and what values should be read for these fields.
   *
   * XXX: finish up comments
   */
  public static abstract class GetField
  {
    public abstract ObjectStreamClass getObjectStreamClass();

    public abstract boolean defaulted(String name)
      throws IOException, IllegalArgumentException;

    public abstract boolean get(String name, boolean defvalue)
      throws IOException, IllegalArgumentException;

    public abstract char get(String name, char defvalue)
      throws IOException, IllegalArgumentException;

    public abstract byte get(String name, byte defvalue)
      throws IOException, IllegalArgumentException;

    public abstract short get(String name, short defvalue)
      throws IOException, IllegalArgumentException;

    public abstract int get(String name, int defvalue)
      throws IOException, IllegalArgumentException;

    public abstract long get(String name, long defvalue)
      throws IOException, IllegalArgumentException;

    public abstract float get(String name, float defvalue)
      throws IOException, IllegalArgumentException;

    public abstract double get(String name, double defvalue)
      throws IOException, IllegalArgumentException;

    public abstract Object get(String name, Object defvalue)
      throws IOException, IllegalArgumentException;
  }

  /**
   * This method should be called by a method called 'readObject' in the
   * deserializing class (if present). It cannot (and should not)be called
   * outside of it. Its goal is to read all fields in the real input stream
   * and keep them accessible through the {@link #GetField} class. Calling
   * this method will not alterate the deserializing object.
   *
   * @return A valid freshly created 'GetField' instance to get access to
   * the deserialized stream.
   * @throws IOException An input/output exception occured. 
   * @throws ClassNotFoundException 
   * @throws NotActiveException
   */
  public GetField readFields()
    throws IOException, ClassNotFoundException, NotActiveException
  {
    if (this.currentObject == null || this.currentObjectStreamClass == null)
      throw new NotActiveException("readFields called by non-active class and/or object");

    if (prereadFields != null)
      return prereadFields;

    if (fieldsAlreadyRead)
      throw new NotActiveException("readFields called but fields already read from"
				   + " stream (by defaultReadObject or readFields)");

    final ObjectStreamClass clazz = this.currentObjectStreamClass;
    final byte[] prim_field_data = new byte[clazz.primFieldSize];
    final Object[] objs = new Object[clazz.objectFieldCount];

    // Apparently Block data is not used with GetField as per
    // empirical evidence against JDK 1.2.  Also see Mauve test
    // java.io.ObjectInputOutput.Test.GetPutField.
    boolean oldmode = setBlockDataMode(false);
    readFully(prim_field_data);
    for (int i = 0; i < objs.length; ++ i)
      objs[i] = readObject();
    setBlockDataMode(oldmode);

    prereadFields = new GetField()
      {
	public ObjectStreamClass getObjectStreamClass()
	{
	  return clazz;
	}

	public boolean defaulted(String name)
	  throws IOException, IllegalArgumentException
	{
	  ObjectStreamField f = clazz.getField(name);
	  
	  /* First if we have a serialized field use the descriptor */
	  if (f != null)
	    {
	      /* It is in serialPersistentFields but setClass tells us
	       * it should not be set. This value is defaulted.
	       */
	      if (f.isPersistent() && !f.isToSet())
		return true;
	      
	      return false;
	    }

	  /* This is not a serialized field. There should be
	   * a default value only if the field really exists.
	   */
	  try
	    {
	      return (clazz.forClass().getDeclaredField (name) != null);
	    }
	  catch (NoSuchFieldException e)
	    {
	      throw new IllegalArgumentException(e.getMessage());
	    }
	}

	public boolean get(String name, boolean defvalue)
	  throws IOException, IllegalArgumentException
	{
	  ObjectStreamField field = getField(name, Boolean.TYPE);

	  if (field == null)
	    return defvalue;

	  return prim_field_data[field.getOffset()] == 0 ? false : true;
	}

	public char get(String name, char defvalue)
	  throws IOException, IllegalArgumentException
	{
	  ObjectStreamField field = getField(name, Character.TYPE);

	  if (field == null)
	    return defvalue;

	  int off = field.getOffset();

	  return (char)(((prim_field_data[off++] & 0xFF) << 8)
			| (prim_field_data[off] & 0xFF));
	}

	public byte get(String name, byte defvalue)
	  throws IOException, IllegalArgumentException
	{
	  ObjectStreamField field = getField(name, Byte.TYPE);

	  if (field == null)
	    return defvalue;

	  return prim_field_data[field.getOffset()];
	}

	public short get(String name, short defvalue)
	  throws IOException, IllegalArgumentException
	{
	  ObjectStreamField field = getField(name, Short.TYPE);

	  if (field == null)
	    return defvalue;

	  int off = field.getOffset();

	  return (short)(((prim_field_data[off++] & 0xFF) << 8)
			 | (prim_field_data[off] & 0xFF));
	}

	public int get(String name, int defvalue)
	  throws IOException, IllegalArgumentException
	{
	  ObjectStreamField field = getField(name, Integer.TYPE);

	  if (field == null)
	    return defvalue;

	  int off = field.getOffset();

	  return ((prim_field_data[off++] & 0xFF) << 24)
	    | ((prim_field_data[off++] & 0xFF) << 16)
	    | ((prim_field_data[off++] & 0xFF) << 8)
	    | (prim_field_data[off] & 0xFF);
	}

	public long get(String name, long defvalue)
	  throws IOException, IllegalArgumentException
	{
	  ObjectStreamField field = getField(name, Long.TYPE);

	  if (field == null)
	    return defvalue;

	  int off = field.getOffset();

	  return (long)(((prim_field_data[off++] & 0xFF) << 56)
			| ((prim_field_data[off++] & 0xFF) << 48)
			| ((prim_field_data[off++] & 0xFF) << 40)
			| ((prim_field_data[off++] & 0xFF) << 32)
			| ((prim_field_data[off++] & 0xFF) << 24)
			| ((prim_field_data[off++] & 0xFF) << 16)
			| ((prim_field_data[off++] & 0xFF) << 8)
			| (prim_field_data[off] & 0xFF));
	}

	public float get(String name, float defvalue)
	  throws IOException, IllegalArgumentException
	{
	  ObjectStreamField field = getField(name, Float.TYPE);

	  if (field == null)
	    return defvalue;

	  int off = field.getOffset();

	  return Float.intBitsToFloat(((prim_field_data[off++] & 0xFF) << 24)
				      | ((prim_field_data[off++] & 0xFF) << 16)
				      | ((prim_field_data[off++] & 0xFF) << 8)
				      | (prim_field_data[off] & 0xFF));
	}

	public double get(String name, double defvalue)
	  throws IOException, IllegalArgumentException
	{
	  ObjectStreamField field = getField(name, Double.TYPE);

	  if (field == null)
	    return defvalue;

	  int off = field.getOffset();

	  return Double.longBitsToDouble
	    ( (long) (((prim_field_data[off++] & 0xFF) << 56)
		      | ((prim_field_data[off++] & 0xFF) << 48)
		      | ((prim_field_data[off++] & 0xFF) << 40)
		      | ((prim_field_data[off++] & 0xFF) << 32)
		      | ((prim_field_data[off++] & 0xFF) << 24)
		      | ((prim_field_data[off++] & 0xFF) << 16)
		      | ((prim_field_data[off++] & 0xFF) << 8)
		      | (prim_field_data[off] & 0xFF)));
	}

	public Object get(String name, Object defvalue)
	  throws IOException, IllegalArgumentException
	{
	  ObjectStreamField field =
	    getField(name, defvalue == null ? null : defvalue.getClass ());

	  if (field == null)
	    return defvalue;

	  return objs[field.getOffset()];
	}

	private ObjectStreamField getField(String name, Class type)
	  throws IllegalArgumentException
	{
	  ObjectStreamField field = clazz.getField(name);
	  boolean illegal = false;

	  try
	    {
	      try
		{
		  Class field_type = field.getType();
		  
		  if (type == field_type ||
		      (type == null && !field_type.isPrimitive()))
		    {
		      /* See defaulted */
		      return field;
		    }
	 
		  illegal = true;
		  throw new IllegalArgumentException
		    ("Field requested is of type "
		     + field_type.getName()
		     + ", but requested type was "
		     + (type == null ?  "Object" : type.getName()));
		}
	      catch (NullPointerException _)
		{
		  /* Here we catch NullPointerException, because it may
		     only come from the call 'field.getType()'. If field
		     is null, we have to return null and classpath ethic
		     say we must try to avoid 'if (xxx == null)'.
		  */
		}
	      catch (IllegalArgumentException e)
		{
		  throw e;
		}
	      
	      return null;
	    }
	  finally
	    {
	      /* If this is an unassigned field we should return
	       * the default value.
	       */
	      if (!illegal && field != null && !field.isToSet() && field.isPersistent())
		return null;

	      /* We do not want to modify transient fields. They should
	       * be left to 0.
	       */
	      try
		{
		  Field f = clazz.forClass().getDeclaredField(name);
		  if (Modifier.isTransient(f.getModifiers()))
		    throw new IllegalArgumentException
		      ("no such field (non transient) " + name);
		  if (field == null && f.getType() != type)
		    throw new IllegalArgumentException
		      ("Invalid requested type for field " + name);
		}
	      catch (NoSuchFieldException e)
		{
		  if (field == null)
		    throw new IllegalArgumentException(e.getMessage());
		}
	       
	    }
	}
      };

    fieldsAlreadyRead = true;
    return prereadFields;
  }

  /**
   * Protected constructor that allows subclasses to override
   * deserialization.  This constructor should be called by subclasses
   * that wish to override <code>readObject (Object)</code>.  This
   * method does a security check <i>NOTE: currently not
   * implemented</i>, then sets a flag that informs
   * <code>readObject (Object)</code> to call the subclasses
   * <code>readObjectOverride (Object)</code> method.
   *
   * @see #readObjectOverride()
   */
  protected ObjectInputStream()
    throws IOException, SecurityException
  {
    SecurityManager sec_man = System.getSecurityManager();
    if (sec_man != null)
      sec_man.checkPermission(SUBCLASS_IMPLEMENTATION_PERMISSION);
    this.useSubclassMethod = true;
  }

  /**
   * This method allows subclasses to override the default
   * de serialization mechanism provided by
   * <code>ObjectInputStream</code>.  To make this method be used for
   * writing objects, subclasses must invoke the 0-argument
   * constructor on this class from their constructor.
   *
   * @see #ObjectInputStream()
   */
  protected Object readObjectOverride()
    throws ClassNotFoundException, IOException, OptionalDataException
  {
    throw new IOException("Subclass of ObjectInputStream must implement readObjectOverride");
  }

  /**
   * Assigns the next available handle to <code>obj</code>.
   *
   * @param obj The object for which we want a new handle.
   * @return A valid handle for the specified object.
   */
  private int assignNewHandle(Object obj)
  {
    this.objectLookupTable.put(new Integer(this.nextOID),
			       new ObjectIdentityWrapper(obj));
    return this.nextOID++;
  }

  private Object processResolution(Object obj, int handle)
    throws IOException
  {
    if (obj instanceof Serializable)
      {
        Method m = null; 
	try
	  {
	    Class classArgs[] = {};
	    m = getMethod(obj.getClass(), "readResolve", classArgs);
	    obj = m.invoke(obj, new Object[] {});	
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
      obj = resolveObject(obj);

    this.objectLookupTable.put(new Integer(handle),
			       new ObjectIdentityWrapper(obj));

    return obj;
  }

  private void clearHandles()
  {
    this.objectLookupTable.clear();
    this.nextOID = baseWireHandle;
  }

  private void readNextBlock() throws IOException
  {
    readNextBlock(this.realInputStream.readByte());
  }

  private void readNextBlock(byte marker) throws IOException
  {
    if (marker == TC_BLOCKDATA)
      {
	dumpElement("BLOCK DATA SIZE=");
	this.blockDataBytes = this.realInputStream.readUnsignedByte();
	dumpElementln (Integer.toString(this.blockDataBytes));
      }
    else if (marker == TC_BLOCKDATALONG)
      {
	dumpElement("BLOCK DATA LONG SIZE=");
	this.blockDataBytes = this.realInputStream.readInt();
	dumpElementln (Integer.toString(this.blockDataBytes));
      }
    else
      {
	throw new EOFException("Attempt to read primitive data, but no data block is active.");
      }

    if (this.blockData.length < this.blockDataBytes)
      this.blockData = new byte[this.blockDataBytes];

    this.realInputStream.readFully (this.blockData, 0, this.blockDataBytes);
    this.blockDataPosition = 0;
  }

  private void readArrayElements (Object array, Class clazz)
    throws ClassNotFoundException, IOException
  {
    if (clazz.isPrimitive())
      {
	if (clazz == Boolean.TYPE)
	  {
	    boolean[] cast_array = (boolean[])array;
	    for (int i=0; i < cast_array.length; i++)
	      cast_array[i] = this.realInputStream.readBoolean();
	    return;
	  }
	if (clazz == Byte.TYPE)
	  {
	    byte[] cast_array = (byte[])array;
	    for (int i=0; i < cast_array.length; i++)
	      cast_array[i] = this.realInputStream.readByte();
	    return;
	  }
	if (clazz == Character.TYPE)
	  {
	    char[] cast_array = (char[])array;
	    for (int i=0; i < cast_array.length; i++)
	      cast_array[i] = this.realInputStream.readChar();
	    return;
	  }
	if (clazz == Double.TYPE)
	  {
	    double[] cast_array = (double[])array;
	    for (int i=0; i < cast_array.length; i++)
	      cast_array[i] = this.realInputStream.readDouble();
	    return;
	  }
	if (clazz == Float.TYPE)
	  {
	    float[] cast_array = (float[])array;
	    for (int i=0; i < cast_array.length; i++)
	      cast_array[i] = this.realInputStream.readFloat();
	    return;
	  }
	if (clazz == Integer.TYPE)
	  {
	    int[] cast_array = (int[])array;
	    for (int i=0; i < cast_array.length; i++)
	      cast_array[i] = this.realInputStream.readInt();
	    return;
	  }
	if (clazz == Long.TYPE)
	  {
	    long[] cast_array = (long[])array;
	    for (int i=0; i < cast_array.length; i++)
	      cast_array[i] = this.realInputStream.readLong();
	    return;
	  }
	if (clazz == Short.TYPE)
	  {
	    short[] cast_array = (short[])array;
	    for (int i=0; i < cast_array.length; i++)
	      cast_array[i] = this.realInputStream.readShort();
	    return;
	  }
      }
    else
      {
	Object[] cast_array = (Object[])array;
	for (int i=0; i < cast_array.length; i++)
 	  cast_array[i] = readObject();
      }
  }

  private void readFields (Object obj, ObjectStreamClass stream_osc)
    throws ClassNotFoundException, IOException
  {
    ObjectStreamField[] stream_fields = stream_osc.fields;
    ObjectStreamField[] real_fields =
      lookupClass(stream_osc.forClass()).fields;

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
	    type = stream_field.getType();
	  }

	if (real_idx == real_fields.length)
	  set_value = false;
	else
	  {
	    real_field = real_fields[real_idx];
	    type = real_field.getType();
	    field_name = real_field.getName();
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

	if (stream_field.getOffset() < 0)
	  {
	    default_initialize = true;
	    set_value = false;
	  }
	
	if (!stream_field.isToSet()) 
	  set_value = false;

	try
	  {
	    if (type == Boolean.TYPE)
	      {
		boolean value =
		  default_initialize ? false : this.realInputStream.readBoolean();
		if (!default_initialize && set_value)
		  dumpElementln("  " + field_name + ": " + value);
		if (set_value)
		  setBooleanField(obj, stream_osc.forClass(), field_name, value);
	      }
	    else if (type == Byte.TYPE)
	      {
		byte value =
		  default_initialize ? 0 : this.realInputStream.readByte();
		if (!default_initialize && set_value)
		  dumpElementln("  " + field_name + ": " + value);
		if (set_value)
		  setByteField(obj, stream_osc.forClass(), field_name, value);
	      }
	    else if (type == Character.TYPE)
	      {
		char value =
		  default_initialize ? (char)0 : this.realInputStream.readChar();
		if (!default_initialize && set_value)
		  dumpElementln("  " + field_name + ": " + value);
		if (set_value)
		  setCharField(obj, stream_osc.forClass(), field_name, value);
	      }
	    else if (type == Double.TYPE)
	      {
		double value =
		  default_initialize ? 0 : this.realInputStream.readDouble();
		if (!default_initialize && set_value)
		  dumpElementln("  " + field_name + ": " + value);
		if (set_value)
		  setDoubleField(obj, stream_osc.forClass(), field_name, value);
	      }
	    else if (type == Float.TYPE)
	      {
		float value =
		  default_initialize ? 0 : this.realInputStream.readFloat();
		if (!default_initialize && set_value)
		  dumpElementln("  " + field_name + ": " + value);
		if (set_value)
		  setFloatField(obj, stream_osc.forClass(), field_name, value);
	      }
	    else if (type == Integer.TYPE)
	      {
		int value =
		  default_initialize ? 0 : this.realInputStream.readInt();
		if (!default_initialize && set_value)
		  dumpElementln("  " + field_name + ": " + value);
		if (set_value)
		  setIntField(obj, stream_osc.forClass(), field_name, value);
	      }
	    else if (type == Long.TYPE)
	      {
		long value =
		  default_initialize ? 0 : this.realInputStream.readLong();
		if (!default_initialize && set_value)
		  dumpElementln("  " + field_name + ": " + value);
		if (set_value)
		  setLongField(obj, stream_osc.forClass(), field_name, value);
	      }
	    else if (type == Short.TYPE)
	      {
		short value =
		  default_initialize ? (short)0 : this.realInputStream.readShort();
		if (!default_initialize && set_value)
		  dumpElementln("  " + field_name + ": " + value);
		if (set_value)
		  setShortField(obj, stream_osc.forClass(), field_name, value);
	      }
	    else
	      {
		Object value =
		  default_initialize ? null : readObject();
		if (set_value)
		  setObjectField(obj, stream_osc.forClass(), field_name,
				  real_field.getTypeString(), value);
	      }
	  }
	catch (NoSuchFieldError e)
	  {
	    dumpElementln("XXXX " + field_name + " does not exist.");
	  }
      }
  }

  // Toggles writing primitive data to block-data buffer.
  private boolean setBlockDataMode (boolean on)
  {
    boolean oldmode = this.readDataFromBlock;
    this.readDataFromBlock = on;

    if (on)
      this.dataInputStream = this.blockDataInput;
    else
      this.dataInputStream = this.realInputStream;
    return oldmode;
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
  private void invokeValidators() throws InvalidObjectException
  {
    Object[] validators = new Object[this.validators.size()];
    this.validators.copyInto (validators);
    Arrays.sort (validators);

    try
      {
	for (int i=0; i < validators.length; i++)
	  ((ObjectInputValidation)validators[i]).validateObject();
      }
    finally
      {
	this.validators.removeAllElements();
      }
  }

  /**
   * This native method is used to get access to the protected method
   * of the same name in SecurityManger.
   *
   * @param sm SecurityManager instance which should be called.
   * @return The current class loader in the calling stack.
   */
  private static ClassLoader currentClassLoader (SecurityManager sm)
  {
    // FIXME: This is too simple.
    return ClassLoader.getSystemClassLoader ();
  }

  /**
   * This method tries to access a precise field called in the
   * specified class. Before accessing the field, it tries to
   * gain control on this field. If the field is either declared as 
   * not persistent or transient then it returns null
   * immediately.
   *
   * @param klass Class to get the field from.
   * @param name Name of the field to access.
   * @return Field instance representing the requested field.
   * @throws NoSuchFieldException if the field does not exist.
   */
  private Field getField(Class klass, String name)
    throws java.lang.NoSuchFieldException
  {
    final Field f = klass.getDeclaredField(name);
    ObjectStreamField sf = lookupClass(klass).getField(name);
    
    AccessController.doPrivileged(new PrivilegedAction()
      {
	public Object run()
	{
	  f.setAccessible(true);
	  return null;
	}
      });

    /* We do not want to modify transient fields. They should
     * be left to 0.
     * N.B.: Not valid if the field is in serialPersistentFields. 
     */
    if (Modifier.isTransient(f.getModifiers()) && !sf.isPersistent())
      return null;
   
    return f;
  }

  private static Method getMethod (Class klass, String name, Class args[])
    throws java.lang.NoSuchMethodException
  {
    final Method m = klass.getDeclaredMethod(name, args);
    AccessController.doPrivileged(new PrivilegedAction()
      {
	public Object run()
	{
	  m.setAccessible(true);
	  return null;
	}
      });
    return m;
  }

  private void callReadMethod (Object obj, ObjectStreamClass osc) throws IOException
  {
    Class klass = osc.forClass();
    try
      {
	Class classArgs[] = {ObjectInputStream.class};
	Method m = getMethod (klass, "readObject", classArgs);
	Object args[] = {this};
	m.invoke(obj, args);
      }
    catch (NoSuchMethodException nsme)
      {
	// Nothing.
      }
    catch (InvocationTargetException x)
      {
        /* Rethrow if possible. */
	Throwable exception = x.getTargetException();
	if (exception instanceof RuntimeException)
	  throw (RuntimeException) exception;
	if (exception instanceof IOException)
	  throw (IOException) exception;

	throw new IOException("Exception thrown from readObject() on " +
			       klass + ": " + exception.getClass().getName());
      }
    catch (Exception x)
      {
	throw new IOException("Failure invoking readObject() on " +
			       klass + ": " + x.getClass().getName());
      }

    // Invalidate fields which has been read through readFields.
    prereadFields = null;
  }
    
  private native Object allocateObject (Class clazz)
    throws InstantiationException;

  private native void callConstructor (Class clazz, Object obj);

  /**
   * This method writes a "boolean" value <code>val</code> in the specified field
   * of the instance <code>obj</code> of the type <code>klass</code>.
   *
   * @param obj Instance to setup.
   * @param klass Class type of the specified instance.
   * @param field_name Name of the field in the specified class type.
   * @param val The boolean value to write into the field.
   * @throws InvalidClassException if the specified field has not the required type.
   * @throws IOException if there is no field of that name in the specified class.
   */
  private void setBooleanField(Object obj, Class klass, String field_name,
				boolean val) throws IOException, InvalidClassException
  {
    try
      {
	Field f = getField(klass, field_name);
	f.setBoolean(obj, val);
      }
    catch (IllegalArgumentException _)
      {
	throw new InvalidClassException("incompatible field type for " + klass.getName() + "." + field_name);
      }
    catch (Exception _)
      {
      }    
  }

  /**
   * This method writes a "byte" value <code>val</code> in the specified field
   * of the instance <code>obj</code> of the type <code>klass</code>.
   *
   * @param obj Instance to setup.
   * @param klass Class type of the specified instance.
   * @param field_name Name of the field in the specified class type.
   * @param val The byte value to write into the field.
   * @throws InvalidClassException if the specified field has not the required type.
   * @throws IOException if there is no field of that name in the specified class.
   */
  private void setByteField(Object obj, Class klass, String field_name,
			     byte val) throws IOException, InvalidClassException
  {
    try
      {
	Field f = getField(klass, field_name);
	f.setByte(obj, val);
      }
    catch (IllegalArgumentException _)
      {
	throw new InvalidClassException("incompatible field type for " + klass.getName() + "." + field_name);
      }
    catch (Exception _)
      {
      }    
  }

  /**
   * This method writes a "character" value <code>val</code> in the specified field
   * of the instance <code>obj</code> of the type <code>klass</code>.
   *
   * @param obj Instance to setup.
   * @param klass Class type of the specified instance.
   * @param field_name Name of the field in the specified class type.
   * @param val The character value to write into the field.
   * @throws InvalidClassException if the specified field has not the required type.
   * @throws IOException if there is no field of that name in the specified class.
   */
  private void setCharField(Object obj, Class klass, String field_name,
			     char val) throws IOException, InvalidClassException
  {
    try
      {
	Field f = getField(klass, field_name);
	f.setChar(obj, val);
      }
    catch (IllegalArgumentException _)
      {
	throw new InvalidClassException("incompatible field type for " + klass.getName() + "." + field_name);
      }
    catch (Exception _)
      {
      }    
  }

  /**
   * This method writes a "double" value <code>val</code> in the specified field
   * of the instance <code>obj</code> of the type <code>klass</code>.
   *
   * @param obj Instance to setup.
   * @param klass Class type of the specified instance.
   * @param field_name Name of the field in the specified class type.
   * @param val The double value to write into the field.
   * @throws InvalidClassException if the specified field has not the required type.
   * @throws IOException if there is no field of that name in the specified class.
   */
  private void setDoubleField(Object obj, Class klass, String field_name,
			       double val) throws IOException, InvalidClassException
  {
    try
      {
	Field f = getField(klass, field_name);
	f.setDouble(obj, val);
      }
    catch (IllegalArgumentException _)
      {
	throw new InvalidClassException("incompatible field type for " + klass.getName() + "." + field_name);
      }
    catch (Exception _)
      {
      }    
  }

  /**
   * This method writes a "float" value <code>val</code> in the specified field
   * of the instance <code>obj</code> of the type <code>klass</code>.
   *
   * @param obj Instance to setup.
   * @param klass Class type of the specified instance.
   * @param field_name Name of the field in the specified class type.
   * @param val The float value to write into the field.
   * @throws InvalidClassException if the specified field has not the required type.
   * @throws IOException if there is no field of that name in the specified class.
   */
  private void setFloatField(Object obj, Class klass, String field_name,
			      float val) throws IOException, InvalidClassException
  {
    try
      {
	Field f = getField(klass, field_name);
	f.setFloat(obj, val);
      }
    catch (IllegalArgumentException _)
      {
	throw new InvalidClassException("incompatible field type for " + klass.getName() + "." + field_name);
      }
    catch (Exception _)
      {
      }    
  }

  /**
   * This method writes an "integer" value <code>val</code> in the specified field
   * of the instance <code>obj</code> of the type <code>klass</code>.
   *
   * @param obj Instance to setup.
   * @param klass Class type of the specified instance.
   * @param field_name Name of the field in the specified class type.
   * @param val The integer value to write into the field.
   * @throws InvalidClassException if the specified field has not the required type.
   * @throws IOException if there is no field of that name in the specified class.
   */
  private void setIntField(Object obj, Class klass, String field_name,
			    int val) throws IOException, InvalidClassException
  {
    try
      {
	Field f = getField(klass, field_name);
	f.setInt(obj, val);
      }
    catch (IllegalArgumentException _)
      {
	throw new InvalidClassException("incompatible field type for " + klass.getName() + "." + field_name);
      }
    catch (Exception _)
      {
      }    
  }

  /**
   * This method writes the long value <code>val</code> in the specified field
   * of the instance <code>obj</code> of the type <code>klass</code>.
   *
   * @param obj Instance to setup.
   * @param klass Class type of the specified instance.
   * @param field_name Name of the field in the specified class type.
   * @param val The long value to write into the field.
   * @throws InvalidClassException if the specified field has not the required type.
   * @throws IOException if there is no field of that name in the specified class.
   */
  private void setLongField(Object obj, Class klass, String field_name,
			     long val) throws IOException, InvalidClassException
  {
    try
      {
	Field f = getField(klass, field_name);
	f.setLong(obj, val);
      }
    catch (IllegalArgumentException _)
      {
	throw new InvalidClassException("incompatible field type for " + klass.getName() + "." + field_name);
      }
    catch (Exception _)
      {
      }    
  }

  /**
   * This method writes a "short" value <code>val</code> in the specified field
   * of the instance <code>obj</code> of the type <code>klass</code>.
   *
   * @param obj Instance to setup.
   * @param klass Class type of the specified instance.
   * @param field_name Name of the field in the specified class type.
   * @param val The short value to write into the field.
   * @throws InvalidClassException if the specified field has not the required type.
   * @throws IOException if there is no field of that name in the specified class.
   */
  private void setShortField(Object obj, Class klass, String field_name,
			      short val) throws IOException, InvalidClassException
  {
    try
      {
	Field f = getField(klass, field_name);
	f.setShort(obj, val);
      }
    catch (IllegalArgumentException _)
      {
	throw new InvalidClassException("incompatible field type for " + klass.getName() + "." + field_name);
      }
    catch (Exception _)
      {
      }    
  }

  /**
   * This method writes an "object" value <code>val</code> in the specified field
   * of the instance <code>obj</code> of the type <code>klass</code>.
   *
   * @param obj Instance to setup.
   * @param klass Class type of the specified instance.
   * @param field_name Name of the field in the specified class type.
   * @param val The "object" value to write into the field.
   * @throws InvalidClassException if the specified field has not the required type.
   * @throws IOException if there is no field of that name in the specified class.
   */
  private void setObjectField(Object obj, Class klass, String field_name,
			       String type_code, Object val) throws IOException, InvalidClassException
  {
    try
      {
 	Field f = getField(klass, field_name);
	ObjectStreamField of = new ObjectStreamField(field_name, f.getType());
	
	if (of.getTypeString() == null ||
	    !of.getTypeString().equals(type_code))
          throw new InvalidClassException("incompatible field type for " + klass.getName() + "." + field_name);
 	f.set(obj, val);
      }
    catch (InvalidClassException e)
      {
	throw e;
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
  private Hashtable classLookupTable;
  private GetField prereadFields;

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

  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
	System.loadLibrary ("javaio");
      }
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
