/* ObjectStreamClass.java -- Class used to write class information
   about serialized objects.
   Copyright (C) 1998, 1999, 2000, 2001, 2003  Free Software Foundation, Inc.

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

import java.lang.reflect.Constructor;
import java.lang.reflect.Field;
import java.lang.reflect.Member;
import java.lang.reflect.Method;
import java.lang.reflect.Modifier;
import java.lang.reflect.Proxy;
import java.security.DigestOutputStream;
import java.security.MessageDigest;
import java.security.NoSuchAlgorithmException;
import java.security.Security;
import java.util.Arrays;
import java.util.Comparator;
import java.util.Hashtable;
import java.util.Vector;
import gnu.java.io.NullOutputStream;
import gnu.java.lang.reflect.TypeSignature;
import gnu.java.security.provider.Gnu;


public class ObjectStreamClass implements Serializable
{
  /**
   * Returns the <code>ObjectStreamClass</code> for <code>cl</code>.
   * If <code>cl</code> is null, or is not <code>Serializable</code>,
   * null is returned.  <code>ObjectStreamClass</code>'s are memorized;
   * later calls to this method with the same class will return the
   * same <code>ObjectStreamClass</code> object and no recalculation
   * will be done.
   *
   * @see java.io.Serializable
   */
  public static ObjectStreamClass lookup(Class cl)
  {
    if (cl == null)
      return null;
    if (! (Serializable.class).isAssignableFrom(cl))
      return null;

    return lookupForClassObject(cl);
  }

  /**
   * This lookup for internal use by ObjectOutputStream.  Suppose
   * we have a java.lang.Class object C for class A, though A is not
   * serializable, but it's okay to serialize C.
   */
  static ObjectStreamClass lookupForClassObject(Class cl)
  {
    if (cl == null)
      return null;

    ObjectStreamClass osc = (ObjectStreamClass) classLookupTable.get(cl);

    if (osc != null)
      return osc;
    else
      {
	osc = new ObjectStreamClass(cl);
	classLookupTable.put(cl, osc);
	return osc;
      }
  }


  /**
   * Returns the name of the class that this
   * <code>ObjectStreamClass</code> represents.
   */
  public String getName()
  {
    return name;
  }


  /**
   * Returns the class that this <code>ObjectStreamClass</code>
   * represents.  Null could be returned if this
   * <code>ObjectStreamClass</code> was read from an
   * <code>ObjectInputStream</code> and the class it represents cannot
   * be found or loaded.
   *
   * @see java.io.ObjectInputStream
   */
  public Class forClass()
  {
    return clazz;
  }


  /**
   * Returns the serial version stream-unique identifier for the class
   * represented by this <code>ObjectStreamClass</code>.  This SUID is
   * either defined by the class as <code>static final long
   * serialVersionUID</code> or is calculated as specified in
   * Javasoft's "Object Serialization Specification" XXX: add reference
   */
  public long getSerialVersionUID()
  {
    return uid;
  }


  // Returns the serializable (non-static and non-transient) Fields
  // of the class represented by this ObjectStreamClass.  The Fields
  // are sorted by name.
  // XXX doc
  public ObjectStreamField[] getFields()
  {
    ObjectStreamField[] copy = new ObjectStreamField[ fields.length ];
    System.arraycopy(fields, 0, copy, 0, fields.length);
    return copy;
  }


  // XXX doc
  // Can't do binary search since fields is sorted by name and
  // primitiveness.
  public ObjectStreamField getField (String name)
  {
    for (int i = 0; i < fields.length; i++)
      if (fields[i].getName().equals(name))
	return fields[i];
    return null;
  }


  /**
   * Returns a textual representation of this
   * <code>ObjectStreamClass</code> object including the name of the
   * class it represents as well as that class's serial version
   * stream-unique identifier.
   *
   * @see #getSerialVersionUID()
   * @see #getName()
   */
  public String toString()
  {
    return "java.io.ObjectStreamClass< " + name + ", " + uid + " >";
  }


  // Returns true iff the class that this ObjectStreamClass represents
  // has the following method:
  //
  // private void writeObject (ObjectOutputStream)
  //
  // This method is used by the class to override default
  // serialization behavior.
  boolean hasWriteMethod()
  {
    return (flags & ObjectStreamConstants.SC_WRITE_METHOD) != 0;
  }


  // Returns true iff the class that this ObjectStreamClass represents
  // has the following method:
  //
  // private void readObject (ObjectOutputStream)
  //
  // This method is used by the class to override default
  // serialization behavior.
  boolean hasReadMethod()
  {
    try
      {
	Class[] readObjectParams = { ObjectInputStream.class };
	forClass().getDeclaredMethod("readObject", readObjectParams);
	return true;
      }
    catch (NoSuchMethodException e)
      {
	return false;
      }
  }


  // Returns true iff the class that this ObjectStreamClass represents
  // implements Serializable but does *not* implement Externalizable.
  boolean isSerializable()
  {
    return (flags & ObjectStreamConstants.SC_SERIALIZABLE) != 0;
  }


  // Returns true iff the class that this ObjectStreamClass represents
  // implements Externalizable.
  boolean isExternalizable()
  {
    return (flags & ObjectStreamConstants.SC_EXTERNALIZABLE) != 0;
  }


  // Returns the <code>ObjectStreamClass</code> that represents the
  // class that is the superclass of the class this
  // <code>ObjectStreamClass</code> represents.  If the superclass is
  // not Serializable, null is returned.
  ObjectStreamClass getSuper()
  {
    return superClass;
  }


  // returns an array of ObjectStreamClasses that represent the super
  // classes of CLAZZ and CLAZZ itself in order from most super to
  // CLAZZ.  ObjectStreamClass[0] is the highest superclass of CLAZZ
  // that is serializable.
  static ObjectStreamClass[] getObjectStreamClasses(Class clazz)
  {
    ObjectStreamClass osc = ObjectStreamClass.lookup(clazz);

    if (osc == null)
      return new ObjectStreamClass[0];
    else
      {
	Vector oscs = new Vector();

	while (osc != null)
	  {
	    oscs.addElement (osc);
	    osc = osc.getSuper();
	  }

	int count = oscs.size();
	ObjectStreamClass[] sorted_oscs = new ObjectStreamClass[ count ];

	for (int i = count - 1; i >= 0; i--)
	  sorted_oscs[ count - i - 1 ] = (ObjectStreamClass) oscs.elementAt(i);

	return sorted_oscs;
      }
  }


  // Returns an integer that consists of bit-flags that indicate
  // properties of the class represented by this ObjectStreamClass.
  // The bit-flags that could be present are those defined in
  // ObjectStreamConstants that begin with `SC_'
  int getFlags()
  {
    return flags;
  }


  ObjectStreamClass(String name, long uid, byte flags,
		    ObjectStreamField[] fields)
  {
    this.name = name;
    this.uid = uid;
    this.flags = flags;
    this.fields = fields;
  }

  /**
   * This method builds the internal description corresponding to a Java Class.
   * As the constructor only assign a name to the current ObjectStreamClass instance,
   * that method sets the serial UID, chose the fields which will be serialized,
   * and compute the position of the fields in the serialized stream.
   *
   * @param cl The Java class which is used as a reference for building the descriptor.
   * @param superClass The descriptor of the super class for this class descriptor.
   * @throws InvalidClassException if an incompatibility between computed UID and
   * already set UID is found.
   */
  void setClass(Class cl, ObjectStreamClass superClass) throws InvalidClassException
  {
    this.clazz = cl;

    long class_uid = getClassUID(cl);
    if (uid == 0)
      uid = class_uid;
    else
      {
	// Check that the actual UID of the resolved class matches the UID from 
	// the stream.    
	if (uid != class_uid)
	  {
	    String msg = cl + 
	      ": Local class not compatible: stream serialVersionUID="
	      + uid + ", local serialVersionUID=" + class_uid;
	    throw new InvalidClassException (msg);
	  }
      }

    isProxyClass = clazz != null && Proxy.isProxyClass(clazz);
    this.superClass = superClass;
    calculateOffsets();
    
    try
      {
	ObjectStreamField[] exportedFields = getSerialPersistentFields (clazz);  

	if (exportedFields == null)
	  return;

	ObjectStreamField[] newFieldList = new ObjectStreamField[exportedFields.length + fields.length];
	int i, j, k;

	/* We now check the import fields against the exported fields.
	 * There should not be contradiction (e.g. int x and String x)
	 * but extra virtual fields can be added to the class.
	 */

	Arrays.sort(exportedFields);

	i = 0; j = 0; k = 0;
	while (i < fields.length && j < exportedFields.length)
	  {
	    int comp = fields[i].getName().compareTo(exportedFields[j].getName());

	    if (comp < 0)
	      {
		newFieldList[k] = fields[i];
		fields[i].setPersistent(false);
		fields[i].setToSet(false);
		i++;
	      }
	    else if (comp > 0)
	      {
		/* field not found in imported fields. We add it
		 * in the list of supported fields.
		 */
		newFieldList[k] = exportedFields[j];
		newFieldList[k].setPersistent(true);
		newFieldList[k].setToSet(false);
		j++;
	      }
	    else
	      {
		if (!fields[i].getType().equals(exportedFields[j].getType()))
		  throw new InvalidClassException
		    ("serialPersistentFields must be compatible with" +
		     " imported fields (about " + fields[i].getName() + ")");
		newFieldList[k] = fields[i];
		fields[i].setPersistent(true);
		i++;
		j++;
	      }
	    k++;
	  }

	if (i < fields.length)
	  for (;i<fields.length;i++,k++)
	    {
	      fields[i].setPersistent(false);
	      fields[i].setToSet(false);
	      newFieldList[k] = fields[i];
	    }
	else
	  if (j < exportedFields.length)
	    for (;j<exportedFields.length;j++,k++)
	      {
		exportedFields[j].setPersistent(true);
		exportedFields[j].setToSet(false);
		newFieldList[k] = exportedFields[j];
	      }
	
	fields = new ObjectStreamField[k];
	System.arraycopy(newFieldList, 0, fields, 0, k);
      }
    catch (NoSuchFieldException ignore)
      {
	return;
      }
    catch (IllegalAccessException ignore)
      {
	return;
      }
  }

  void setSuperclass (ObjectStreamClass osc)
  {
    superClass = osc;
  }

  void calculateOffsets()
  {
    int i;
    ObjectStreamField field;
    primFieldSize = 0;
    int fcount = fields.length;
    for (i = 0; i < fcount; ++ i)
      {
	field = fields[i];

	if (! field.isPrimitive())
	  break;

	field.setOffset(primFieldSize);
	switch (field.getTypeCode())
	  {
	  case 'B':
	  case 'Z':
	    ++ primFieldSize;
	    break;
	  case 'C':
	  case 'S':
	    primFieldSize += 2;
	    break;
	  case 'I':
	  case 'F':
	    primFieldSize += 4;
	    break;
	  case 'D':
	  case 'J':
	    primFieldSize += 8;
	    break;
	  }
      }

    for (objectFieldCount = 0; i < fcount; ++ i)
      fields[i].setOffset(objectFieldCount++);
  }


  private ObjectStreamClass(Class cl)
  {
    uid = 0;
    flags = 0;
    isProxyClass = Proxy.isProxyClass(cl);

    clazz = cl;
    name = cl.getName();
    setFlags(cl);
    setFields(cl);
    // to those class nonserializable, its uid field is 0
    if ( (Serializable.class).isAssignableFrom(cl) && !isProxyClass)
      uid = getClassUID(cl);
    superClass = lookup(cl.getSuperclass());
  }


  // Sets bits in flags according to features of CL.
  private void setFlags(Class cl)
  {
    if ((java.io.Externalizable.class).isAssignableFrom(cl))
      flags |= ObjectStreamConstants.SC_EXTERNALIZABLE;
    else if ((java.io.Serializable.class).isAssignableFrom(cl))
      // only set this bit if CL is NOT Externalizable
      flags |= ObjectStreamConstants.SC_SERIALIZABLE;

    try
      {
	Method writeMethod = cl.getDeclaredMethod("writeObject",
						  writeMethodArgTypes);
	int modifiers = writeMethod.getModifiers();

	if (writeMethod.getReturnType() == Void.TYPE
	    && Modifier.isPrivate(modifiers)
	    && !Modifier.isStatic(modifiers))
	  flags |= ObjectStreamConstants.SC_WRITE_METHOD;
      }
    catch(NoSuchMethodException oh_well)
      {
      }
  }


  // Sets fields to be a sorted array of the serializable fields of
  // clazz.
  private void setFields(Class cl)
  {
    if (!isSerializable() || isExternalizable())
      {
	fields = NO_FIELDS;
	return;
      }

    try
      {
	Field serialPersistentFields =
	  cl.getDeclaredField("serialPersistentFields");
	serialPersistentFields.setAccessible(true);
	int modifiers = serialPersistentFields.getModifiers();

	if (Modifier.isStatic(modifiers)
	    && Modifier.isFinal(modifiers)
	    && Modifier.isPrivate(modifiers))
	  {
	    fields = getSerialPersistentFields(cl);
	    if (fields != null)
	      {
		Arrays.sort (fields);
		calculateOffsets();
		return;
	      }
	  }
      }
    catch (NoSuchFieldException ignore)
      {
      }
    catch (IllegalAccessException ignore)
      {
      }

    int num_good_fields = 0;
    Field[] all_fields = cl.getDeclaredFields();

    int modifiers;
    // set non-serializable fields to null in all_fields
    for (int i = 0; i < all_fields.length; i++)
      {
	modifiers = all_fields[i].getModifiers();
	if (Modifier.isTransient(modifiers)
	    || Modifier.isStatic(modifiers))
	  all_fields[i] = null;
	else
	  num_good_fields++;
      }

    // make a copy of serializable (non-null) fields
    fields = new ObjectStreamField[ num_good_fields ];
    for (int from = 0, to = 0; from < all_fields.length; from++)
      if (all_fields[from] != null)
	{
	  Field f = all_fields[from];
	  fields[to] = new ObjectStreamField(f.getName(), f.getType());
	  to++;
	}

    Arrays.sort(fields);
    calculateOffsets();
  }

  // Returns the serial version UID defined by class, or if that
  // isn't present, calculates value of serial version UID.
  private long getClassUID(Class cl)
  {
    try
      {
	// Use getDeclaredField rather than getField, since serialVersionUID
	// may not be public AND we only want the serialVersionUID of this
	// class, not a superclass or interface.
	Field suid = cl.getDeclaredField("serialVersionUID");
	suid.setAccessible(true);
	int modifiers = suid.getModifiers();

	if (Modifier.isStatic(modifiers)
	    && Modifier.isFinal(modifiers)
	    && suid.getType() == Long.TYPE)
	  return suid.getLong(null);
      }
    catch (NoSuchFieldException ignore)
      {
      }
    catch (IllegalAccessException ignore)
      {
      }

    // cl didn't define serialVersionUID, so we have to compute it
    try
      {
	MessageDigest md;
	try 
	  {
	    md = MessageDigest.getInstance("SHA");
	  }
	catch (NoSuchAlgorithmException e)
	  {
	    // If a provider already provides SHA, use it; otherwise, use this.
	    Gnu gnuProvider = new Gnu();
	    Security.addProvider(gnuProvider);
	    md = MessageDigest.getInstance("SHA");
	  }

	DigestOutputStream digest_out =
	  new DigestOutputStream(nullOutputStream, md);
	DataOutputStream data_out = new DataOutputStream(digest_out);

	data_out.writeUTF(cl.getName());

	int modifiers = cl.getModifiers();
	// just look at interesting bits
	modifiers = modifiers & (Modifier.ABSTRACT | Modifier.FINAL
				 | Modifier.INTERFACE | Modifier.PUBLIC);
	data_out.writeInt(modifiers);

	// Pretend that an array has no interfaces, because when array
	// serialization was defined (JDK 1.1), arrays didn't have it.
	if (! cl.isArray())
	  {
	    Class[] interfaces = cl.getInterfaces();
	    Arrays.sort(interfaces, interfaceComparator);
	    for (int i = 0; i < interfaces.length; i++)
	      data_out.writeUTF(interfaces[i].getName());
	  }

	Field field;
	Field[] fields = cl.getDeclaredFields();
	Arrays.sort(fields, memberComparator);
	for (int i = 0; i < fields.length; i++)
	  {
	    field = fields[i];
	    modifiers = field.getModifiers();
	    if (Modifier.isPrivate(modifiers)
		&& (Modifier.isStatic(modifiers)
		    || Modifier.isTransient(modifiers)))
	      continue;

	    data_out.writeUTF(field.getName());
	    data_out.writeInt(modifiers);
	    data_out.writeUTF(TypeSignature.getEncodingOfClass (field.getType()));
	  }

	// write class initializer method if present
	if (VMObjectStreamClass.hasClassInitializer(cl))
	  {
	    data_out.writeUTF("<clinit>");
	    data_out.writeInt(Modifier.STATIC);
	    data_out.writeUTF("()V");
	  }

	Constructor constructor;
	Constructor[] constructors = cl.getDeclaredConstructors();
	Arrays.sort (constructors, memberComparator);
	for (int i = 0; i < constructors.length; i++)
	  {
	    constructor = constructors[i];
	    modifiers = constructor.getModifiers();
	    if (Modifier.isPrivate(modifiers))
	      continue;

	    data_out.writeUTF("<init>");
	    data_out.writeInt(modifiers);

	    // the replacement of '/' with '.' was needed to make computed
	    // SUID's agree with those computed by JDK
	    data_out.writeUTF 
	      (TypeSignature.getEncodingOfConstructor(constructor).replace('/','.'));
	  }

	Method method;
	Method[] methods = cl.getDeclaredMethods();
	Arrays.sort(methods, memberComparator);
	for (int i = 0; i < methods.length; i++)
	  {
	    method = methods[i];
	    modifiers = method.getModifiers();
	    if (Modifier.isPrivate(modifiers))
	      continue;

	    data_out.writeUTF(method.getName());
	    data_out.writeInt(modifiers);

	    // the replacement of '/' with '.' was needed to make computed
	    // SUID's agree with those computed by JDK
	    data_out.writeUTF
	      (TypeSignature.getEncodingOfMethod(method).replace('/', '.'));
	  }

	data_out.close();
	byte[] sha = md.digest();
	long result = 0;
	int len = sha.length < 8 ? sha.length : 8;
	for (int i = 0; i < len; i++)
	  result += (long) (sha[i] & 0xFF) << (8 * i);

	return result;
      }
    catch (NoSuchAlgorithmException e)
      {
	throw new RuntimeException
	  ("The SHA algorithm was not found to use in computing the Serial Version UID for class "
	   + cl.getName(), e);
      }
    catch (IOException ioe)
      {
	throw new RuntimeException(ioe);
      }
  }

  /**
   * Returns the value of CLAZZ's private static final field named
   * `serialPersistentFields'. It performs some sanity checks before
   * returning the real array. Besides, the returned array is a clean
   * copy of the original. So it can be modified.
   *
   * @param clazz Class to retrieve 'serialPersistentFields' from.
   * @return The content of 'serialPersistentFields'.
   */
  private ObjectStreamField[] getSerialPersistentFields(Class clazz) 
    throws NoSuchFieldException, IllegalAccessException
  {
    ObjectStreamField[] fieldsArray = null;
    ObjectStreamField[] o;

    // Use getDeclaredField rather than getField for the same reason
    // as above in getDefinedSUID.
    Field f = clazz.getDeclaredField("serialPersistentFields");
    f.setAccessible(true);

    int modifiers = f.getModifiers();
    if (!(Modifier.isStatic(modifiers) &&
	  Modifier.isFinal(modifiers) &&
	  Modifier.isPrivate(modifiers)))
      return null;
    
    o = (ObjectStreamField[]) f.get(null);
    
    if (o == null)
      return null;

    fieldsArray = new ObjectStreamField[ o.length ];
    System.arraycopy(o, 0, fieldsArray, 0, o.length);
    
    return fieldsArray;
  }

  public static final ObjectStreamField[] NO_FIELDS = {};

  private static Hashtable classLookupTable = new Hashtable();
  private static final NullOutputStream nullOutputStream = new NullOutputStream();
  private static final Comparator interfaceComparator = new InterfaceComparator();
  private static final Comparator memberComparator = new MemberComparator();
  private static final
    Class[] writeMethodArgTypes = { java.io.ObjectOutputStream.class };

  private ObjectStreamClass superClass;
  private Class clazz;
  private String name;
  private long uid;
  private byte flags;

  // this field is package protected so that ObjectInputStream and
  // ObjectOutputStream can access it directly
  ObjectStreamField[] fields;

  // these are accessed by ObjectIn/OutputStream
  int primFieldSize = -1;  // -1 if not yet calculated
  int objectFieldCount;

  boolean isProxyClass = false;

  // This is probably not necessary because this class is special cased already
  // but it will avoid showing up as a discrepancy when comparing SUIDs.
  private static final long serialVersionUID = -6120832682080437368L;

}


// interfaces are compared only by name
class InterfaceComparator implements Comparator
{
  public int compare(Object o1, Object o2)
  {
    return ((Class) o1).getName().compareTo(((Class) o2).getName());
  }
}


// Members (Methods and Constructors) are compared first by name,
// conflicts are resolved by comparing type signatures
class MemberComparator implements Comparator
{
  public int compare(Object o1, Object o2)
  {
    Member m1 = (Member) o1;
    Member m2 = (Member) o2;

    int comp = m1.getName().compareTo(m2.getName());

    if (comp == 0)
      return TypeSignature.getEncodingOfMember(m1).
	compareTo(TypeSignature.getEncodingOfMember(m2));
    else
      return comp;
  }
}
