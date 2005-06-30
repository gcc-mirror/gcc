/* ObjectInputStream.java -- Class used to read serialized objects
   Copyright (C) 1998, 1999, 2000, 2001, 2002, 2003, 2005
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

import gnu.classpath.Configuration;
import gnu.java.io.ObjectIdentityWrapper;

import java.lang.reflect.Array;
import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.InvocationTargetException;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.Arrays;
import java.util.Hashtable;
import java.util.Vector;

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

    depth += 2;

    if(dump) dumpElement("MARKER: 0x" + Integer.toHexString(marker) + " ");

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
		{ if(dump) dumpElementln("BLOCKDATALONG"); }
	      else
		{ if(dump) dumpElementln("BLOCKDATA"); }
	      readNextBlock(marker);
	      throw new StreamCorruptedException("Unexpected blockData");
	    }

	  case TC_NULL:
	    {
	      if(dump) dumpElementln("NULL");
	      ret_val = null;
	      break;
	    }

	  case TC_REFERENCE:
	    {
	      if(dump) dumpElement("REFERENCE ");
	      Integer oid = new Integer(this.realInputStream.readInt());
	      if(dump) dumpElementln(Integer.toHexString(oid.intValue()));
	      ret_val = ((ObjectIdentityWrapper)
			 this.objectLookupTable.get(oid)).object;
	      break;
	    }

	  case TC_CLASS:
	    {
	      if(dump) dumpElementln("CLASS");
	      ObjectStreamClass osc = (ObjectStreamClass)readObject();
	      Class clazz = osc.forClass();
	      assignNewHandle(clazz);
	      ret_val = clazz;
	      break;
	    }

	  case TC_PROXYCLASSDESC:
	    {
	      if(dump) dumpElementln("PROXYCLASS");
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
	      if(dump) dumpElement("STRING=");
	      String s = this.realInputStream.readUTF();
	      if(dump) dumpElementln(s);
	      ret_val = processResolution(null, s, assignNewHandle(s));
	      break;
	    }

	  case TC_ARRAY:
	    {
	      if(dump) dumpElementln("ARRAY");
	      ObjectStreamClass osc = (ObjectStreamClass)readObject();
	      Class componentType = osc.forClass().getComponentType();
	      if(dump) dumpElement("ARRAY LENGTH=");
	      int length = this.realInputStream.readInt();
	      if(dump) dumpElementln (length + "; COMPONENT TYPE=" + componentType);
	      Object array = Array.newInstance(componentType, length);
	      int handle = assignNewHandle(array);
	      readArrayElements(array, componentType);
	      if(dump)
	        for (int i = 0, len = Array.getLength(array); i < len; i++)
		  dumpElementln("  ELEMENT[" + i + "]=" + Array.get(array, i));
	      ret_val = processResolution(null, array, handle);
	      break;
	    }

	  case TC_OBJECT:
	    {
	      if(dump) dumpElementln("OBJECT");
	      ObjectStreamClass osc = (ObjectStreamClass)readObject();
	      Class clazz = osc.forClass();
	      
	      if (!osc.realClassIsSerializable)
		throw new NotSerializableException
		  (clazz + " is not Serializable, and thus cannot be deserialized.");
	      
	      if (osc.realClassIsExternalizable)
		{
		  Externalizable obj = osc.newInstance();
		  
		  int handle = assignNewHandle(obj);
		  
		  boolean read_from_blocks = ((osc.getFlags() & SC_BLOCK_DATA) != 0);
		  
		  boolean oldmode = this.readDataFromBlock;
		  if (read_from_blocks)
		    setBlockDataMode(true);
		  
		  obj.readExternal(this);
		  
		  if (read_from_blocks)
                    {
		      setBlockDataMode(oldmode);
                      if (!oldmode)
			if (this.realInputStream.readByte() != TC_ENDBLOCKDATA)
			    throw new IOException("No end of block data seen for class with readExternal (ObjectInputStream) method.");
                    }
		  
		  ret_val = processResolution(osc, obj, handle);
		  break;
		} // end if (osc.realClassIsExternalizable)

	      Object obj = newObject(clazz, osc.firstNonSerializableParentConstructor);
	      
	      int handle = assignNewHandle(obj);
	      Object prevObject = this.currentObject;
	      ObjectStreamClass prevObjectStreamClass = this.currentObjectStreamClass;
	      
	      this.currentObject = obj;
	      ObjectStreamClass[] hierarchy =
		inputGetObjectStreamClasses(clazz);
	      
	      for (int i = 0; i < hierarchy.length; i++)
		{
		  this.currentObjectStreamClass = hierarchy[i];
		  
		  if(dump) dumpElementln("Reading fields of " + this.currentObjectStreamClass.getName ());

		  // XXX: should initialize fields in classes in the hierarchy
		  // that aren't in the stream
		  // should skip over classes in the stream that aren't in the
		  // real classes hierarchy
		  
		  Method readObjectMethod = this.currentObjectStreamClass.readObjectMethod;
		  if (readObjectMethod != null)
		    {
		      fieldsAlreadyRead = false;
		      boolean oldmode = setBlockDataMode(true);
		      callReadMethod(readObjectMethod, this.currentObjectStreamClass.forClass(), obj);
		      setBlockDataMode(oldmode);
		    }
		  else
		    {
		      readFields(obj, currentObjectStreamClass);
		    }

		  if (this.currentObjectStreamClass.hasWriteMethod())
		    {
		      if(dump) dumpElement("ENDBLOCKDATA? ");
		      try
			{
			  // FIXME: XXX: This try block is to
			  // catch EOF which is thrown for some
			  // objects.  That indicates a bug in
			  // the logic.

			  if (this.realInputStream.readByte() != TC_ENDBLOCKDATA)
			    throw new IOException
			      ("No end of block data seen for class with readObject (ObjectInputStream) method.");
			  if(dump) dumpElementln("yes");
			}
// 		      catch (EOFException e)
// 			{
// 			  if(dump) dumpElementln("no, got EOFException");
// 			}
		      catch (IOException e)
			{
			  if(dump) dumpElementln("no, got IOException");
			}
		    }
		}

	      this.currentObject = prevObject;
	      this.currentObjectStreamClass = prevObjectStreamClass;
	      ret_val = processResolution(osc, obj, handle);
		  
	      break;
	    }

	  case TC_RESET:
	    if(dump) dumpElementln("RESET");
	    clearHandles();
	    ret_val = readObject();
	    break;

	  case TC_EXCEPTION:
	    {
	      if(dump) dumpElement("EXCEPTION=");
	      Exception e = (Exception)readObject();
	      if(dump) dumpElementln(e.toString());
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
	
	depth -= 2;
	
	if (! was_deserializing)
	  {
	    if (validators.size() > 0)
	      invokeValidators();
	  }
      }
    
    return ret_val;
  }

  /**
   * This method makes a partial check of types for the fields
   * contained given in arguments. It checks primitive types of
   * fields1 against non primitive types of fields2. This method 
   * assumes the two lists has already been sorted according to 
   * the Java specification.
   *
   * @param name Name of the class owning the given fields.
   * @param fields1 First list to check.
   * @param fields2 Second list to check.
   * @throws InvalidClassException if a field in fields1, which has a primitive type, is a present
   * in the non primitive part in fields2.
   */
  private void checkTypeConsistency(String name, ObjectStreamField[] fields1, ObjectStreamField[] fields2)
    throws InvalidClassException
  {
    int nonPrimitive = 0;
    
    for (nonPrimitive = 0; 
	 nonPrimitive < fields1.length
	   && fields1[nonPrimitive].isPrimitive(); nonPrimitive++)
      {
      }

    if (nonPrimitive == fields1.length)
      return;
    
    int i = 0;
    ObjectStreamField f1;
    ObjectStreamField f2;
    
    while (i < fields2.length
	   && nonPrimitive < fields1.length)
      {
	f1 = fields1[nonPrimitive];
	f2 = fields2[i];
	
	if (!f2.isPrimitive())
	  break;

	int compVal = f1.getName().compareTo (f2.getName());

	if (compVal < 0)
	  {
	    nonPrimitive++;
	  }
	else if (compVal > 0)
	  {
	    i++;
	  }
	else
	  {
	    throw new InvalidClassException
	      ("invalid field type for " + f2.getName() +
	       " in class " + name);
	  }
      }
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
    if(dump) dumpElement("CLASSDESC NAME=");
    String name = this.realInputStream.readUTF();
    if(dump) dumpElement(name + "; UID=");
    long uid = this.realInputStream.readLong ();
    if(dump) dumpElement(Long.toHexString(uid) + "; FLAGS=");
    byte flags = this.realInputStream.readByte ();
    if(dump) dumpElement(Integer.toHexString(flags) + "; FIELD COUNT=");
    short field_count = this.realInputStream.readShort();
    if(dump) dumpElementln(Short.toString(field_count));
    ObjectStreamField[] fields = new ObjectStreamField[field_count];
    ObjectStreamClass osc = new ObjectStreamClass(name, uid,
						  flags, fields);
    assignNewHandle(osc);

    if (callersClassLoader == null)
      callersClassLoader = currentLoader();
	      
    for (int i = 0; i < field_count; i++)
      {
	if(dump) dumpElement("  TYPE CODE=");
	char type_code = (char)this.realInputStream.readByte();
	if(dump) dumpElement(type_code + "; FIELD NAME=");
	String field_name = this.realInputStream.readUTF();
	if(dump) dumpElementln(field_name);
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
	  new ObjectStreamField(field_name, class_name, callersClassLoader);
      }
	      
    /* Now that fields have been read we may resolve the class
     * (and read annotation if needed). */
    Class clazz;
    try
      {
	clazz = resolveClass(osc);
      }
    catch (ClassNotFoundException cnfe)
      {
	// Maybe it was an primitive class?
	if (name.equals("void"))
	  clazz = Void.TYPE;
	else if (name.equals("boolean"))
	  clazz = Boolean.TYPE;
	else if (name.equals("byte"))
	  clazz = Byte.TYPE;
	else if (name.equals("short"))
	  clazz = Short.TYPE;
	else if (name.equals("char"))
	  clazz = Character.TYPE;
	else if (name.equals("int"))
	  clazz = Integer.TYPE;
	else if (name.equals("long"))
	  clazz = Long.TYPE;
	else if (name.equals("float"))
	  clazz = Float.TYPE;
	else if (name.equals("double"))
	  clazz = Double.TYPE;
	else
	  throw cnfe;
      }

    boolean oldmode = setBlockDataMode(true);
    osc.setClass(clazz, lookupClass(clazz.getSuperclass()));
    classLookupTable.put(clazz, osc);
    setBlockDataMode(oldmode);

    // find the first non-serializable, non-abstract
    // class in clazz's inheritance hierarchy
    Class first_nonserial = clazz.getSuperclass();
    // Maybe it is a primitive class, those don't have a super class,
    // or Object itself.  Otherwise we can keep getting the superclass
    // till we hit the Object class, or some other non-serializable class.

    if (first_nonserial == null)
      first_nonserial = clazz;
    else
      while (Serializable.class.isAssignableFrom(first_nonserial)
	     || Modifier.isAbstract(first_nonserial.getModifiers()))
	first_nonserial = first_nonserial.getSuperclass();

    final Class local_constructor_class = first_nonserial;

    osc.firstNonSerializableParentConstructor =
        (Constructor)AccessController.doPrivileged(new PrivilegedAction()
          {
            public Object run()
            {
              try
                {
                  Constructor c = local_constructor_class.
                                    getDeclaredConstructor(new Class[0]);
                  if (Modifier.isPrivate(c.getModifiers()))
                    return null;
                  return c;
                }
              catch (NoSuchMethodException e)
                {
                  // error will be reported later, in newObject()
                  return null;
                }
            }
          });

    osc.realClassIsSerializable = Serializable.class.isAssignableFrom(clazz);
    osc.realClassIsExternalizable = Externalizable.class.isAssignableFrom(clazz);

    ObjectStreamField[] stream_fields = osc.fields;
    ObjectStreamField[] real_fields = ObjectStreamClass.lookupForClassObject(clazz).fields;
    ObjectStreamField[] fieldmapping = new ObjectStreamField[2 * Math.max(stream_fields.length, real_fields.length)];

    int stream_idx = 0;
    int real_idx = 0;
    int map_idx = 0;

    /*
     * Check that there is no type inconsistencies between the lists.
     * A special checking must be done for the two groups: primitive types and
     * not primitive types. 
     */
    checkTypeConsistency(name, real_fields, stream_fields);
    checkTypeConsistency(name, stream_fields, real_fields);

    
    while (stream_idx < stream_fields.length
	   || real_idx < real_fields.length)
      {
	ObjectStreamField stream_field = null;
	ObjectStreamField real_field = null;

	if (stream_idx == stream_fields.length)
	  {
	    real_field = real_fields[real_idx++];
	  }
	else if (real_idx == real_fields.length)
	  {
	    stream_field = stream_fields[stream_idx++];
	  }
	else
	  {
	    int comp_val =
	      real_fields[real_idx].compareTo (stream_fields[stream_idx]);

	    if (comp_val < 0)
	      {
		real_field = real_fields[real_idx++];
	      }
	    else if (comp_val > 0)
	      {
		stream_field = stream_fields[stream_idx++];
	      }
	    else
	      {
		stream_field = stream_fields[stream_idx++];
		real_field = real_fields[real_idx++];
		if (stream_field.getType() != real_field.getType())
		  throw new InvalidClassException
		    ("invalid field type for " + real_field.getName() +
		     " in class " + name);
	      }
	  }

	/* If some of stream_fields does not correspond to any of real_fields,
	 * or the opposite, then fieldmapping will go short.
	 */
	if (map_idx == fieldmapping.length)
	  {
	    ObjectStreamField[] newfieldmapping =
	      new ObjectStreamField[fieldmapping.length + 2];
	    System.arraycopy(fieldmapping, 0,
			     newfieldmapping, 0, fieldmapping.length);
	    fieldmapping = newfieldmapping;
	  }
	fieldmapping[map_idx++] = stream_field;
	fieldmapping[map_idx++] = real_field;
      }
    osc.fieldMapping = fieldmapping;

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
    if (callersClassLoader == null)
      {
	callersClassLoader = currentLoader ();
	if (Configuration.DEBUG && dump)
	  {
	    dumpElementln ("CallersClassLoader = " + callersClassLoader);
	  }
      }

    return Class.forName(osc.getName(), true, callersClassLoader);
  }

  /**
   * Returns the most recent user defined ClassLoader on the execution stack
   * or null if none is found.
   */
  // GCJ LOCAL: native method.
  private native ClassLoader currentLoader();

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
    if (clazz == null)
      return null;

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
    ClassLoader cl = currentLoader();
    
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
    if(dump) dumpElement("STREAM MAGIC ");
    if (this.realInputStream.readShort() != STREAM_MAGIC)
      throw new StreamCorruptedException("Invalid stream magic number");

    if(dump) dumpElementln("STREAM VERSION ");
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
  public abstract static class GetField
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
   * this method will not alter the deserializing object.
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

	  return (long)(((prim_field_data[off++] & 0xFFL) << 56)
			| ((prim_field_data[off++] & 0xFFL) << 48)
			| ((prim_field_data[off++] & 0xFFL) << 40)
			| ((prim_field_data[off++] & 0xFFL) << 32)
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
	    ( (long) (((prim_field_data[off++] & 0xFFL) << 56)
		      | ((prim_field_data[off++] & 0xFFL) << 48)
		      | ((prim_field_data[off++] & 0xFFL) << 40)
		      | ((prim_field_data[off++] & 0xFFL) << 32)
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

  private Object processResolution(ObjectStreamClass osc, Object obj, int handle)
    throws IOException
  {
    if (osc != null && obj instanceof Serializable)
      {
	try
	  {
	    Method m = osc.readResolveMethod; 
	    if(m != null)
	    {
		obj = m.invoke(obj, new Object[] {});
	    }
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
	if(dump) dumpElement("BLOCK DATA SIZE=");
	this.blockDataBytes = this.realInputStream.readUnsignedByte();
	if(dump) dumpElementln (Integer.toString(this.blockDataBytes));
      }
    else if (marker == TC_BLOCKDATALONG)
      {
	if(dump) dumpElement("BLOCK DATA LONG SIZE=");
	this.blockDataBytes = this.realInputStream.readInt();
	if(dump) dumpElementln (Integer.toString(this.blockDataBytes));
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
    ObjectStreamField[] fields = stream_osc.fieldMapping;

    for (int i = 0; i < fields.length; i += 2)
      {
	ObjectStreamField stream_field = fields[i];
	ObjectStreamField real_field = fields[i + 1];
	boolean read_value = (stream_field != null && stream_field.getOffset() >= 0 && stream_field.isToSet());
	boolean set_value = (real_field != null && real_field.isToSet());
	String field_name;
	char type;

	if (stream_field != null)
	  {
	    field_name = stream_field.getName();
	    type = stream_field.getTypeCode();
	  }
	else
	  {
	    field_name = real_field.getName();
	    type = real_field.getTypeCode();
	  }
	
	switch(type)
	  {
	  case 'Z':
	    {
	      boolean value =
		read_value ? this.realInputStream.readBoolean() : false;
	      if (dump && read_value && set_value)
		dumpElementln("  " + field_name + ": " + value);
	      if (set_value)
		real_field.setBooleanField(obj, value);
	      break;
	    }
	  case 'B':
	    {
	      byte value =
		read_value ? this.realInputStream.readByte() : 0;
	      if (dump && read_value && set_value)
		dumpElementln("  " + field_name + ": " + value);
	      if (set_value)
		real_field.setByteField(obj, value);
	      break;
	    }
	  case 'C':
	    {
	      char value =
		read_value ? this.realInputStream.readChar(): 0;
	      if (dump && read_value && set_value)
		dumpElementln("  " + field_name + ": " + value);
	      if (set_value)
		real_field.setCharField(obj, value);
	      break;
	    }
	  case 'D':
	    {
	      double value =
		read_value ? this.realInputStream.readDouble() : 0;
	      if (dump && read_value && set_value)
		dumpElementln("  " + field_name + ": " + value);
	      if (set_value)
		real_field.setDoubleField(obj, value);
	      break;
	    }
	  case 'F':
	    {
	      float value =
		read_value ? this.realInputStream.readFloat() : 0;
	      if (dump && read_value && set_value)
		dumpElementln("  " + field_name + ": " + value);
	      if (set_value)
		real_field.setFloatField(obj, value);
	      break;
	    }
	  case 'I':
	    {
	      int value =
		read_value ? this.realInputStream.readInt() : 0;
	      if (dump && read_value && set_value)
		dumpElementln("  " + field_name + ": " + value);
	      if (set_value)
		real_field.setIntField(obj, value);
	      break;
	    }
	  case 'J':
	    {
	      long value =
		read_value ? this.realInputStream.readLong() : 0;
	      if (dump && read_value && set_value)
		dumpElementln("  " + field_name + ": " + value);
	      if (set_value)
		real_field.setLongField(obj, value);
	      break;
	    }
	  case 'S':
	    {
	      short value =
		read_value ? this.realInputStream.readShort() : 0;
	      if (dump && read_value && set_value)
		dumpElementln("  " + field_name + ": " + value);
	      if (set_value)
		real_field.setShortField(obj, value);
	      break;
	    }
	  case 'L':
	  case '[':
	    {
	      Object value =
		read_value ? readObject() : null;
	      if (set_value)
		real_field.setObjectField(obj, value);
	      break;
	    }
	  default:
	    throw new InternalError("Invalid type code: " + type);
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
  private Object newObject (Class real_class, Constructor constructor)
    throws ClassNotFoundException, IOException
  {
    if (constructor == null)
        throw new InvalidClassException("Missing accessible no-arg base class constructor for " + real_class.getName()); 
    try
      {
	return allocateObject(real_class, constructor.getDeclaringClass(), constructor);
      }
    catch (InstantiationException e)
      {
        throw new ClassNotFoundException
		("Instance of " + real_class + " could not be created");
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

  private void callReadMethod (Method readObject, Class klass, Object obj)
    throws ClassNotFoundException, IOException
  {
    try
      {
	readObject.invoke(obj, new Object[] { this });
      }
    catch (InvocationTargetException x)
      {
        /* Rethrow if possible. */
	Throwable exception = x.getTargetException();
	if (exception instanceof RuntimeException)
	  throw (RuntimeException) exception;
	if (exception instanceof IOException)
	  throw (IOException) exception;
        if (exception instanceof ClassNotFoundException)
          throw (ClassNotFoundException) exception;

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
    
  private native Object allocateObject(Class clazz, Class constr_clazz, Constructor constructor)
    throws InstantiationException;

  private static final int BUFFER_SIZE = 1024;

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

  private ClassLoader callersClassLoader;
  private static boolean dump;

  // The nesting depth for debugging output
  private int depth = 0;

  private void dumpElement (String msg)
  {
    System.out.print(msg);
  }
  
  private void dumpElementln (String msg)
  {
    System.out.println(msg);
    for (int i = 0; i < depth; i++)
      System.out.print (" ");
    System.out.print (Thread.currentThread() + ": ");
  }

  static
  {
    if (Configuration.INIT_LOAD_LIBRARY)
      {
	System.loadLibrary ("javaio");
      }
  }

  // used to keep a prioritized list of object validators
  private static final class ValidatorAndPriority implements Comparable
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
}

