/* DataFlavor.java -- A type of data to transfer via the clipboard.
   Copyright (C) 1999, 2001 Free Software Foundation, Inc.

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


package java.awt.datatransfer;

import java.io.ByteArrayInputStream;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.IOException;
import java.io.ObjectOutput;
import java.io.ObjectInput;
import java.io.Reader;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;

/**
  * This class represents a particular data format used for transferring
  * data via the clipboard.
  *
  * @author Aaron M. Renn (arenn@urbanophile.com)
  */
public class DataFlavor implements java.io.Externalizable, Cloneable
{

// FIXME: Serialization: Need to write methods for.

/*
 * Static Variables
 */

/**
  * This is the data flavor used for tranferring plain text.  The MIME
  * type is "text/plain; charset=unicode".  The representation class
  * is <code>java.io.InputStream</code>.
  *
  * @deprecated The charset unicode is platform specific and InputStream
  * deals with bytes not chars. Use <code>getRederForText()</code>.
  */
public static final DataFlavor plainTextFlavor;

/**
  * This is the data flavor used for transferring Java strings.  The
  * MIME type is "application/x-java-serialized-object" and the 
  * representation class is <code>java.lang.String</code>.
  */
public static final DataFlavor stringFlavor;

/**
  * This is a data flavor used for transferring lists of files.  The
  * representation type is a <code>java.util.List</code>, with each element of 
  * the list being a <code>java.io.File</code>.
  */
public static final DataFlavor javaFileListFlavor;

/**
  * This is the MIME type used for transferring a serialized object.
  * The representation class is the type of object be deserialized.
  */
public static final String javaSerializedObjectMimeType =
  "application/x-java-serialized-object";

/**
  * This is the MIME type used to transfer a Java object reference within
  * the same JVM.  The representation class is the class of the object
  * being transferred.
  */
public static final String javaJVMLocalObjectMimeType =
  "application/x-java-jvm-local-object";

/**
  * This is the MIME type used to transfer a link to a remote object.
  * The representation class is the type of object being linked to.
  */
public static final String javaRemoteObjectMimeType =
  "application/x-java-remote-object";

static
{
  plainTextFlavor
      = new DataFlavor(java.io.InputStream.class,
		       "text/plain; charset=unicode",
		       "plain unicode text");

  stringFlavor
      = new DataFlavor(java.lang.String.class,
		       "Java Unicode String");

  javaFileListFlavor
      = new DataFlavor(java.util.List.class,
		       "Java File List");

  // javaFileListFlavor.mimeType = "application/x-java-file-list";
}

/*************************************************************************/

/*
 * Instance Variables
 */

// The MIME type for this flavor
private final String mimeType;

// The representation class for this flavor
private final Class representationClass;

// The human readable name of this flavor
private String humanPresentableName;

/*************************************************************************/

/*
 * Static Methods
 */

/**
  * This method attempts to load the named class.  The following class
  * loaders are searched in order: the bootstrap class loader, the
  * system class loader, the context class loader (if it exists), and
  * the specified fallback class loader.
  *
  * @param className The name of the class to load.
  * @param classLoader The class loader to use if all others fail, which
  * may be <code>null</code>.
  *
  * @exception ClassNotFoundException If the class cannot be loaded.
  */
protected static final Class
tryToLoadClass(String className, ClassLoader classLoader)
               throws ClassNotFoundException
{
  try
    {
      return(Class.forName(className));
    }
  catch(Exception e) { ; }
  // Commented out for Java 1.1
  /*
  try
    {
      return(className.getClass().getClassLoader().findClass(className));
    }
  catch(Exception e) { ; }

  try
    {
      return(ClassLoader.getSystemClassLoader().findClass(className));
    }
  catch(Exception e) { ; }
  */

  // FIXME: What is the context class loader?
  /*
  try
    {
    }
  catch(Exception e) { ; }
  */

  if (classLoader != null)
    return(classLoader.loadClass(className));
  else
    throw new ClassNotFoundException(className);
}

/*************************************************************************/

/*
 * Constructors
 */

/**
  * Empty public constructor needed for externalization.
  * Should not be used for normal instantiation.
  */
public
DataFlavor()
{
    mimeType = null;
    representationClass = null;
    humanPresentableName = null;
}

/*************************************************************************/

/**
  * Private constructor.
  */
private
DataFlavor(Class representationClass,
	   String mimeType,
	   String humanPresentableName)
{
    this.representationClass = representationClass;
    this.mimeType = mimeType;
    if (humanPresentableName != null)
	this.humanPresentableName = humanPresentableName;
    else
	this.humanPresentableName = mimeType;
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>DataFlavor</code>.  The class
  * and human readable name are specified, the MIME type will be
  * "application/x-java-serialized-object". If the human readable name
  * is not specified (<code>null</code>) then the human readable name
  * will be the same as the MIME type.
  *
  * @param representationClass The representation class for this object.
  * @param humanPresentableName The display name of the object.
  */
public
DataFlavor(Class representationClass, String humanPresentableName)
{
    this(representationClass,
       "application/x-java-serialized-object"
       + "; class="
       + representationClass.getName(),
       humanPresentableName);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>DataFlavor</code> with the
  * specified MIME type and description.  If the MIME type has a
  * "class=<rep class>" parameter then the representation class will
  * be the class name specified. Otherwise the class defaults to
  * <code>java.io.InputStream</code>. If the human readable name
  * is not specified (<code>null</code>) then the human readable name
  * will be the same as the MIME type.
  *
  * @param mimeType The MIME type for this flavor.
  * @param humanPresentableName The display name of this flavor.
  * @param classLoader The class loader for finding classes if the default
  * class loaders do not work.
  *
  * @exception IllegalArgumentException If the representation class
  * specified cannot be loaded.
  */
public
DataFlavor(String mimeType, String humanPresentableName, 
           ClassLoader classLoader) throws ClassNotFoundException
{
  this(getRepresentationClassFromMime(mimeType, classLoader),
       mimeType, humanPresentableName);
}

private static Class
getRepresentationClassFromMime(String mimeString, ClassLoader classLoader)
{
  String classname = getParameter("class", mimeString);
  if (classname != null)
    {
      try
        {
          return tryToLoadClass(classname, classLoader);
        }
      catch(Exception e)
        {
          throw new IllegalArgumentException("classname: " + e.getMessage());
        }
    }
  else
    {
      return java.io.InputStream.class;
    }
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>DataFlavor</code> with the
  * specified MIME type and description.  If the MIME type has a
  * "class=<rep class>" parameter then the representation class will
  * be the class name specified. Otherwise the class defaults to
  * <code>java.io.InputStream</code>. If the human readable name
  * is not specified (<code>null</code>) then the human readable name
  * will be the same as the MIME type. This is the same as calling
  * <code>new DataFlavor(mimeType, humanPresentableName, null)</code>.
  *
  * @param mimeType The MIME type for this flavor.
  * @param humanPresentableName The display name of this flavor.
  * @param classLoader The class loader for finding classes.
  *
  * @exception IllegalArgumentException If the representation class
  * specified cannot be loaded.
  */
public
DataFlavor(String mimeType, String humanPresentableName)
           throws ClassNotFoundException
{
  this(mimeType, humanPresentableName, null);
}

/*************************************************************************/

/**
  * Initializes a new instance of <code>DataFlavor</code> with the specified
  * MIME type.  This type can have a "class=" parameter to specify the
  * representation class, and then the class must exist or an exception will
  * be thrown. If there is no "class=" parameter then the representation class
  * will be <code>java.io.InputStream</code>. This is the same as calling
  * <code>new DataFlavor(mimeType, null)</code>.
  *
  * @param mimeType The MIME type for this flavor.
  *
  * @exception IllegalArgumentException If a class is not specified in
  * the MIME type.
  * @exception ClassNotFoundException If the class cannot be loaded.
  */
public
DataFlavor(String mimeType) throws ClassNotFoundException
{
  this(mimeType, null);
}

/*************************************************************************/

/**
  * Returns the MIME type of this flavor.
  *
  * @return The MIME type for this flavor.
  */
public String
getMimeType()
{
  return(mimeType);
}

/*************************************************************************/

/**
  * Returns the representation class for this flavor.
  *
  * @return The representation class for this flavor.
  */
public Class
getRepresentationClass()
{
  return(representationClass);
}

/*************************************************************************/

/**
  * Returns the human presentable name for this flavor.
  *
  * @return The human presentable name for this flavor.
  */
public String
getHumanPresentableName()
{
  return(humanPresentableName);
} 

/*************************************************************************/

/**
  * Returns the primary MIME type for this flavor.
  *
  * @return The primary MIME type for this flavor.
  */
public String
getPrimaryType()
{
  int idx = mimeType.indexOf("/");
  if (idx == -1)
    return(mimeType);

  return(mimeType.substring(0, idx));
}

/*************************************************************************/

/**
  * Returns the MIME subtype for this flavor.
  *
  * @return The MIME subtype for this flavor.
  */
public String
getSubType()
{
  int idx = mimeType.indexOf("/");
  if (idx == -1)
    return("");

  String subtype = mimeType.substring(idx + 1);

  idx = subtype.indexOf(" ");
  if (idx == -1)
    return(subtype);
  else
    return(subtype.substring(0, idx));
}

/*************************************************************************/

/**
  * Returns the value of the named MIME type parameter, or <code>null</code>
  * if the parameter does not exist. Given the parameter name and the mime
  * string.
  *
  * @param paramName The name of the parameter.
  * @param mimeString The mime string from where the name should be found.
  *
  * @return The value of the parameter or null.
  */
private static String
getParameter(String paramName, String mimeString)
{
  int idx = mimeString.indexOf(paramName + "=");
  if (idx == -1)
    return(null);

  String value = mimeString.substring(idx + paramName.length() + 2);

  idx = value.indexOf(" ");
  if (idx == -1)
    return(value);
  else
    return(value.substring(0, idx));
}

/*************************************************************************/
/**
  * Returns the value of the named MIME type parameter, or <code>null</code>
  * if the parameter does not exist.
  *
  * @param paramName The name of the paramter.
  *
  * @return The value of the parameter.
  */
public String
getParameter(String paramName)
{
  return getParameter(paramName, mimeType);
}

/*************************************************************************/

/**
  * Sets the human presentable name to the specified value.
  *
  * @param humanPresentableName The new display name.
  */
public void
setHumanPresentableName(String humanPresentableName)
{
  this.humanPresentableName = humanPresentableName;
}

/*************************************************************************/

/**
  * Tests the MIME type of this object for equality against the specified
  * MIME type.
  *
  * @param mimeType The MIME type to test against.
  *
  * @return <code>true</code> if the MIME type is equal to this object's
  * MIME type, <code>false</code> otherwise.
  */
public boolean
isMimeTypeEqual(String mimeType)
{
  // FIXME: Need to handle default attributes and parameters

  return(this.mimeType.equals(mimeType));
}

/*************************************************************************/

/**
  * Tests the MIME type of this object for equality against the specified
  * data flavor's MIME type
  *
  * @param flavor The flavor to test against.
  *
  * @return <code>true</code> if the flavor's MIME type is equal to this 
  * object's MIME type, <code>false</code> otherwise.
  */
public boolean
isMimeTypeEqual(DataFlavor flavor)
{
  return(isMimeTypeEqual(flavor.getMimeType()));
}

/*************************************************************************/

/**
  * Tests whether or not this flavor represents a serialized object.
  *
  * @return <code>true</code> if this flavor represents a serialized
  * object, <code>false</code> otherwise.
  */
public boolean
isMimeTypeSerializedObject()
{
  return(mimeType.startsWith(javaSerializedObjectMimeType));
}

/*************************************************************************/

/**
  * Tests whether or not this flavor has a representation class of
  * <code>java.io.InputStream</code>.
  *
  * @param <code>true</code> if the representation class of this flavor
  * is <code>java.io.InputStream</code>, <code>false</code> otherwise.
  */
public boolean
isRepresentationClassInputStream()
{
  return(representationClass.getName().equals("java.io.InputStream"));
}

/*************************************************************************/

/**
  * Tests whether the representation class for this flavor is
  * serializable.
  *
  * @param <code>true</code> if the representation class is serializable,
  * <code>false</code> otherwise.
  */
public boolean
isRepresentationClassSerializable()
{
  Class[] interfaces = representationClass.getInterfaces();

  int i = 0;
  while (i < interfaces.length)
    {
      if (interfaces[i].getName().equals("java.io.Serializable"))
        return(true);
      ++i;
    }

  return(false);
}

/*************************************************************************/

/**
  * Tests whether the representation class for his flavor is remote.
  *
  * @return <code>true</code> if the representation class is remote,
  * <code>false</code> otherwise.
  */
public boolean
isRepresentationClassRemote()
{
  // FIXME: Implement
  throw new RuntimeException("Not implemented");
}

/*************************************************************************/

/**
  * Tests whether or not this flavor represents a serialized object.
  *
  * @return <code>true</code> if this flavor represents a serialized
  * object, <code>false</code> otherwise.
  */
public boolean
isFlavorSerializedObjectType()
{
  // FIXME: What is the diff between this and isMimeTypeSerializedObject?
  return(mimeType.startsWith(javaSerializedObjectMimeType));
}

/*************************************************************************/

/**
  * Tests whether or not this flavor represents a remote object.
  *
  * @return <code>true</code> if this flavor represents a remote object,
  * <code>false</code> otherwise.
  */
public boolean
isFlavorRemoteObjectType()
{
  return(mimeType.startsWith(javaRemoteObjectMimeType));
}

/*************************************************************************/

/**
  * Tests whether or not this flavor represents a list of files.
  *
  * @return <code>true</code> if this flavor represents a list of files,
  * <code>false</code> otherwise.
  */
public boolean
isFlavorJavaFileListType()
{
  if (this.mimeType.equals(javaFileListFlavor.mimeType) &&
      this.representationClass.equals(javaFileListFlavor.representationClass))
    return(true);

  return(false);
}

/*************************************************************************/

/**
  * Returns a copy of this object.
  *
  * @return A copy of this object.
  */
public Object
clone()
{
  try
    {
      return(super.clone());
    }
  catch(Exception e)
    {
      return(null);
    }
}

/*************************************************************************/

/**
  * This method test the specified <code>DataFlavor</code> for equality
  * against this object.  This will be true if the MIME type and
  * representation type are the equal.
  *
  * @param flavor The <code>DataFlavor</code> to test against.
  *
  * @return <code>true</code> if the flavor is equal to this object,
  * <code>false</code> otherwise.
  */
public boolean
equals(DataFlavor flavor)
{
  if (flavor == null)
    return(false);

  if (!this.mimeType.toLowerCase().equals(flavor.mimeType.toLowerCase()))
    return(false);

  if (!this.representationClass.equals(flavor.representationClass))
    return(false);

  return(true);
}

/*************************************************************************/

/**
  * This method test the specified <code>Object</code> for equality
  * against this object.  This will be true if the following conditions
  * are met:
  * <p>
  * <ul>
  * <li>The object is not <code>null</code>.
  * <li>The object is an instance of <code>DataFlavor</code>.
  * <li>The object's MIME type and representation class are equal to
  * this object's.
  * </ul>
  *
  * @param obj The <code>Object</code> to test against.
  *
  * @return <code>true</code> if the flavor is equal to this object,
  * <code>false</code> otherwise.
  */
public boolean
equals(Object obj)
{
  if (obj == null)
    return(false);

  if (!(obj instanceof DataFlavor))
    return(false);

  return(equals((DataFlavor)obj));
}

/*************************************************************************/

/**
  * Tests whether or not the specified string is equal to the MIME type
  * of this object.
  *
  * @param str The string to test against.
  *
  * @return <code>true</code> if the string is equal to this object's MIME
  * type, <code>false</code> otherwise.
  *
  * @deprecated Not compatible with <code>hashCode()</code>.
  *             Use <code>isMimeTypeEqual()</code>
  */
public boolean
equals(String str)
{
  return(isMimeTypeEqual(str));
}

/*************************************************************************/

/**
  * Returns the hash code for this data flavor.
  * The hash code is based on the (lower case) mime type and the
  * representation class.
  */
public int
hashCode()
{
  return(mimeType.toLowerCase().hashCode()^representationClass.hashCode());
}

/*************************************************************************/

/**
  * Returns <code>true</code> when the given <code>DataFlavor</code>
  * matches this one.
  */
public boolean
match(DataFlavor dataFlavor)
{
  // XXX - How is this different from equals?
  return(equals(dataFlavor));
}

/*************************************************************************/

/**
  * This method exists for backward compatibility.  It simply returns
  * the same name/value pair passed in.
  *
  * @param name The parameter name.
  * @param value The parameter value.
  *
  * @return The name/value pair.
  *
  * @deprecated
  */
protected String
normalizeMimeTypeParameter(String name, String value)
{
  return(name + "=" + value);
}

/*************************************************************************/

/**
  * This method exists for backward compatibility.  It simply returns
  * the MIME type string unchanged.
  *
  * @param type The MIME type.
  * 
  * @return The MIME type.
  *
  * @deprecated
  */
protected String
normalizeMimeType(String type)
{
  return(type);
}

/*************************************************************************/

/**
  * Serialize this class.
  *
  * @param stream The <code>ObjectOutput</code> stream to serialize to.
  */
public void
writeExternal(ObjectOutput stream) throws IOException
{
  // FIXME: Implement me
}

/*************************************************************************/

/**
  * De-serialize this class.
  *
  * @param stream The <code>ObjectInput</code> stream to deserialize from.
  */
public void
readExternal(ObjectInput stream) throws IOException, ClassNotFoundException
{
  // FIXME: Implement me
}

/*************************************************************************/

/**
  * Returns a string representation of this DataFlavor. Including the
  * representation class name, MIME type and human presentable name.
  */
public String
toString()
{
  return("DataFlavor[representationClass="
         + representationClass.getName()
         + ",mimeType="
         + mimeType
         + "humanPresentableName="
         + humanPresentableName);
}

/*************************************************************************/

/**
  * XXX - Currently returns <code>plainTextFlavor</code>.
  */
public static final DataFlavor
getTextPlainUnicodeFlavor()
{
  return(plainTextFlavor);
}

/*************************************************************************/

/**
  * XXX - Currently returns <code>java.io.InputStream</code>.
  *
  * @since 1.3
  */
public static final Class
getDefaultRepresentationClass()
{
  return(java.io.InputStream.class);
}
/*************************************************************************/

/**
  * XXX - Currently returns <code>java.io.InputStream</code>.
  */
public static final String
getDefaultRepresentationClassAsString()
{
  return(getDefaultRepresentationClass().getName());
}

/*************************************************************************/

/**
  * Selects the best supported text flavor on this implementation.
  * Returns <code>null</code> when none of the given flavors is liked.
  *
  * The <code>DataFlavor</code> returned the first data flavor in the
  * array that has either a representation class which is (a subclass of)
  * <code>Reader</code> or <code>String</code>, or has a representation
  * class which is (a subclass of) <code>InputStream</code> and has a
  * primary MIME type of "text" and has an supported encoding.
  */
public static final DataFlavor
selectBestTextFlavor(DataFlavor[] availableFlavors)
{
  for(int i=0; i<availableFlavors.length; i++)
    {
      DataFlavor df = availableFlavors[i];
      Class c = df.representationClass;

      // A Reader or String is good.
      if ((Reader.class.isAssignableFrom(c))
	  || (String.class.isAssignableFrom(c)))
	{
	  return df;
	}

      // A InputStream is good if the mime primary type is "text"
      if ((InputStream.class.isAssignableFrom(c))
	  && ("text".equals(df.getPrimaryType())))
        {
          String encoding = availableFlavors[i].getParameter("charset");
          if (encoding == null)
            encoding = "us-ascii";
          Reader r = null;
          try
            {
              // Try to construct a dummy reader with the found encoding
              r = new InputStreamReader
                    (new ByteArrayInputStream(new byte[0]), encoding);
            }
          catch(UnsupportedEncodingException uee) { /* ignore */ }
          if (r != null)
            return df;
        }
    }

  // Nothing found
  return(null);
}

/*************************************************************************/

/**
  * Creates a <code>Reader</code> for a given <code>Transferable</code>.
  *
  * If the representation class is a (subclass of) <code>Reader</code>
  * then an instance of the representation class is returned. If the
  * representatation class is a <code>String</code> then a
  * <code>StringReader</code> is returned. And if the representation class
  * is a (subclass of) <code>InputStream</code> and the primary MIME type
  * is "text" then a <code>InputStreamReader</code> for the correct charset
  * encoding is returned.
  *
  * @param transferable The <code>Transferable</code> for which a text
  *                     <code>Reader</code> is requested.
  * @exception UnsupportedFlavorException when the transferable doesn't
  * support this <code>DataFlavor</code>. Or if the representable class
  * isn't a (subclass of) <code>Reader</code>, <code>String</code>,
  * <code>InputStream</code> and/or the primary MIME type isn't "text".
  * @exception IOException when any IOException occurs.
  * @exception UnsupportedEncodingException if the "charset" isn't supported
  * on this platform.
  */
public Reader
getReaderForText(Transferable transferable) throws UnsupportedFlavorException,
                                                   IOException,
                                                   UnsupportedEncodingException
{
    if (!transferable.isDataFlavorSupported(this))
        throw new UnsupportedFlavorException(this);

    if (Reader.class.isAssignableFrom(representationClass))
        return((Reader)transferable.getTransferData(this));

    if (String.class.isAssignableFrom(representationClass))
        return(new StringReader((String)transferable.getTransferData(this)));

    if (InputStream.class.isAssignableFrom(representationClass)
        && "text".equals(getPrimaryType()))
      {
        InputStream in = (InputStream)transferable.getTransferData(this);
        String encoding = getParameter("charset");
        if (encoding == null)
            encoding = "us-ascii";
        return(new InputStreamReader(in, encoding));
      }

    throw new UnsupportedFlavorException(this);
}

} // class DataFlavor

