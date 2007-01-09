/* DataFlavor.java -- A type of data to transfer via the clipboard.
   Copyright (C) 1999, 2001, 2004, 2005, 2006 Free Software Foundation, Inc.

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


package java.awt.datatransfer;

import java.io.ByteArrayInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.io.InputStreamReader;
import java.io.ObjectInput;
import java.io.ObjectOutput;
import java.io.OptionalDataException;
import java.io.Reader;
import java.io.Serializable;
import java.io.StringReader;
import java.io.UnsupportedEncodingException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.charset.Charset;
import java.rmi.Remote;

/**
 * This class represents a particular data format used for transferring
 * data via the clipboard.
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
public class DataFlavor implements java.io.Externalizable, Cloneable
{
  static final long serialVersionUID = 8367026044764648243L;

  // FIXME: Serialization: Need to write methods for.

  /**
   * This is the data flavor used for tranferring plain text.  The MIME
   * type is "text/plain; charset=unicode".  The representation class
   * is <code>java.io.InputStream</code>.
   *
   * @deprecated The charset unicode is platform specific and InputStream
   * deals with bytes not chars. Use <code>getRederForText()</code>.
   */
  public static final DataFlavor plainTextFlavor = 
    new DataFlavor("text/plain; charset=unicode; class=java.io.InputStream",
                   "plain unicode text");

  /**
   * This is the data flavor used for transferring Java strings.  The
   * MIME type is "application/x-java-serialized-object" and the 
   * representation class is <code>java.lang.String</code>.
   */
  public static final DataFlavor stringFlavor = 
    new DataFlavor(java.lang.String.class, "Java Unicode String");

  /**
   * This is a data flavor used for transferring lists of files.  The
   * representation type is a <code>java.util.List</code>, with each 
   * element of the list being a <code>java.io.File</code>.
   */
  public static final DataFlavor javaFileListFlavor = 
    new DataFlavor("application/x-java-file-list; class=java.util.List",
                   "Java File List");

  /**
   * This is an image flavor used for transferring images.  The
   * representation type is a <code>java.awt.Image</code>.
   */
  public static final DataFlavor imageFlavor = 
    new DataFlavor(java.awt.Image.class, "Java Image");

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
    "application/x-java-jvm-local-objectref";

  /**
   * This is the MIME type used to transfer a link to a remote object.
   * The representation class is the type of object being linked to.
   */
  public static final String javaRemoteObjectMimeType =
    "application/x-java-remote-object";

  /*
   * Instance Variables
   */
  
  // The MIME type for this flavor
  private MimeType mimeType;
  
  // The representation class for this flavor
  private Class<?> representationClass;
  
  // The human readable name of this flavor
  private String humanPresentableName;

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
  protected static final Class<?> tryToLoadClass(String className,
						 ClassLoader classLoader)
    throws ClassNotFoundException
  {
    // Bootstrap
    try
      {
        return Class.forName(className);
      }
    catch(ClassNotFoundException cnfe)
      {
	// Ignored.
      }
  
    // System
    try
      {
	ClassLoader loader = ClassLoader.getSystemClassLoader();
        return Class.forName(className, true, loader);
      }
    catch(ClassNotFoundException cnfe)
      {
	// Ignored.
      }
 
    // Context
    try
      {
	ClassLoader loader = Thread.currentThread().getContextClassLoader();
        return Class.forName(className, true, loader);
      }
    catch(ClassNotFoundException cnfe)
      {
	// Ignored.
      }
 
    if (classLoader != null)
      return Class.forName(className, true, classLoader);

    throw new ClassNotFoundException(className);
  }
  
  /**
   * XXX - Currently returns <code>plainTextFlavor</code>.
   */
  public static final DataFlavor getTextPlainUnicodeFlavor()
  {
    return plainTextFlavor;
  }
  
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
    for(int i = 0; i < availableFlavors.length; i++)
      {
        DataFlavor df = availableFlavors[i];
        Class c = df.representationClass;
  
        // A Reader or String is good.
        if ((Reader.class.isAssignableFrom(c))
           || (String.class.isAssignableFrom(c)))
      return df;
  
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
    return null;
  }


  /*
   * Constructors
   */
  
  /**
   * Empty public constructor needed for externalization.
   * Should not be used for normal instantiation.
   */
  public DataFlavor()
  {
    // Used for deserialization only, nothing to do here. 
  }

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
  public DataFlavor(Class<?> representationClass, String humanPresentableName)
  {
    if (representationClass == null)
      throw new NullPointerException("representationClass must not be null");
    try
      {
        mimeType = new MimeType(javaSerializedObjectMimeType);
      }
    catch (MimeTypeParseException ex)
      {
        // Must not happen as we use a constant string.
        assert false;
      }
    if (humanPresentableName == null)
      humanPresentableName = javaSerializedObjectMimeType;
    this.humanPresentableName = humanPresentableName;
    this.representationClass = representationClass;
  }

  /**
   * Initializes a new instance of <code>DataFlavor</code> with the
   * specified MIME type and description.  If the MIME type has a
   * "class=&lt;rep class&gt;" parameter then the representation class will
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
   * @exception ClassNotFoundException If the class is not loaded.
   */
  public DataFlavor(String mimeType, String humanPresentableName, 
                   ClassLoader classLoader)
    throws ClassNotFoundException
  {
    init(mimeType, humanPresentableName, classLoader);
  }

  /**
   * Initializes a new instance of <code>DataFlavor</code> with the
   * specified MIME type and description.  If the MIME type has a
   * "class=&lt;rep class&gt;" parameter then the representation class will
   * be the class name specified. Otherwise the class defaults to
   * <code>java.io.InputStream</code>. If the human readable name
   * is not specified (<code>null</code>) then the human readable name
   * will be the same as the MIME type. This is the same as calling
   * <code>new DataFlavor(mimeType, humanPresentableName, null)</code>.
   *
   * @param mimeType The MIME type for this flavor.
   * @param humanPresentableName The display name of this flavor.
   *
   * @exception IllegalArgumentException If the representation class
   * specified cannot be loaded.
   */
  public DataFlavor(String mimeType, String humanPresentableName)
  {
    try
      {
        init(mimeType, humanPresentableName, getClass().getClassLoader());
      }
    catch (ClassNotFoundException ex)
      {
        IllegalArgumentException iae =
          new IllegalArgumentException("Class not found: " + ex.getMessage());
        iae.initCause(ex);
        throw iae;
      }
  }

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
  public DataFlavor(String mimeType) throws ClassNotFoundException
  {
    init(mimeType, null, getClass().getClassLoader());
  }

  /**
   * Called by various constructors to initialize this object.
   *
   * @param mime the mime string
   * @param humanPresentableName the human presentable name
   * @param loader the class loader to use for loading the representation
   *        class
   */
  private void init(String mime, String humanPresentableName,
                    ClassLoader loader)
    throws ClassNotFoundException
  {
    if (mime == null)
      throw new NullPointerException("The mime type must not be null");
    try
      {
        mimeType = new MimeType(mime);
      }
    catch (MimeTypeParseException ex)
      {
        IllegalArgumentException iae =
          new IllegalArgumentException("Invalid mime type");
        iae.initCause(ex);
        throw iae;
      }
    String className = mimeType.getParameter("class");
    if (className == null)
      {
        if (mimeType.getBaseType().equals(javaSerializedObjectMimeType))
          throw new IllegalArgumentException("Serialized object type must have"
                                        + " a representation class parameter");
        else
          representationClass = java.io.InputStream.class;
      }
    else
      representationClass = tryToLoadClass(className, loader);
    mimeType.addParameter("class", representationClass.getName());

    if (humanPresentableName == null)
      {
        humanPresentableName = mimeType.getParameter("humanPresentableName");
        if (humanPresentableName == null)
          humanPresentableName = mimeType.getBaseType();
      }
    this.humanPresentableName = humanPresentableName;
  }

  /**
   * Returns the MIME type of this flavor.
   *
   * @return The MIME type for this flavor.
   */
  public String getMimeType()
  {
    return(mimeType.toString());
  }

  /**
   * Returns the representation class for this flavor.
   *
   * @return The representation class for this flavor.
   */
  public Class<?> getRepresentationClass()
  {
    return(representationClass);
  }

  /**
   * Returns the human presentable name for this flavor.
   *
   * @return The human presentable name for this flavor.
   */
  public String getHumanPresentableName()
  {
    return(humanPresentableName);
  } 

  /**
   * Returns the primary MIME type for this flavor.
   *
   * @return The primary MIME type for this flavor.
   */
  public String getPrimaryType()
  {
    return(mimeType.getPrimaryType());
  }

  /**
   * Returns the MIME subtype for this flavor.
   *
   * @return The MIME subtype for this flavor.
   */
  public String getSubType()
  {
    return mimeType.getSubType();
  }

  /**
   * Returns the value of the named MIME type parameter, or <code>null</code>
   * if the parameter does not exist.
   *
   * @param paramName The name of the paramter.
   *
   * @return The value of the parameter.
   */
  public String getParameter(String paramName)
  {
    if ("humanPresentableName".equals(paramName))
      return getHumanPresentableName();
  
    return mimeType.getParameter(paramName);
  }

  /**
   * Sets the human presentable name to the specified value.
   *
   * @param humanPresentableName The new display name.
   */
  public void setHumanPresentableName(String humanPresentableName)
  {
    this.humanPresentableName = humanPresentableName;
  }

  /**
   * Tests the MIME type of this object for equality against the specified
   * MIME type. Ignores parameters.
   *
   * @param mimeType The MIME type to test against.
   *
   * @return <code>true</code> if the MIME type is equal to this object's
   * MIME type (ignoring parameters), <code>false</code> otherwise.
   *
   * @exception NullPointerException If mimeType is null.
   */
  public boolean isMimeTypeEqual(String mimeType)
  {
    if (mimeType == null)
      throw new NullPointerException("mimeType must not be null");
    boolean equal = false;
    try
      {
        if (this.mimeType != null)
          {
            MimeType other = new MimeType(mimeType);
            equal = this.mimeType.matches(other);
          }
      }
    catch (MimeTypeParseException ex)
      {
        // Return false in this case.
      }
    return equal;
  }

  /**
   * Tests the MIME type of this object for equality against the specified
   * data flavor's MIME type
   *
   * @param flavor The flavor to test against.
   *
   * @return <code>true</code> if the flavor's MIME type is equal to this 
   * object's MIME type, <code>false</code> otherwise.
   */
  public final boolean isMimeTypeEqual(DataFlavor flavor)
  {
    return isMimeTypeEqual(flavor.getMimeType());
  }

  /**
   * Tests whether or not this flavor represents a serialized object.
   *
   * @return <code>true</code> if this flavor represents a serialized
   * object, <code>false</code> otherwise.
   */
  public boolean isMimeTypeSerializedObject()
  {
    return isMimeTypeEqual(javaSerializedObjectMimeType);
  }

  /**
   * Tests whether or not this flavor has a representation class of
   * <code>java.io.InputStream</code>.
   *
   * @return <code>true</code> if the representation class of this flavor
   * is <code>java.io.InputStream</code>, <code>false</code> otherwise.
   */
  public boolean isRepresentationClassInputStream()
  {
    return InputStream.class.isAssignableFrom(representationClass);
  }

  /**
   * Tests whether the representation class for this flavor is
   * serializable.
   *
   * @return <code>true</code> if the representation class is serializable,
   * <code>false</code> otherwise.
   */
  public boolean isRepresentationClassSerializable()
  {
    return Serializable.class.isAssignableFrom(representationClass);
  }

  /**
   * Tests whether the representation class for his flavor is remote.
   *
   * @return <code>true</code> if the representation class is remote,
   * <code>false</code> otherwise.
   */
  public boolean isRepresentationClassRemote()
  {
    return Remote.class.isAssignableFrom (representationClass);
  }

  /**
   * Tests whether or not this flavor represents a serialized object.
   *
   * @return <code>true</code> if this flavor represents a serialized
   * object, <code>false</code> otherwise.
   */
  public boolean isFlavorSerializedObjectType()
  {
    return isRepresentationClassSerializable()
           && isMimeTypeEqual(javaSerializedObjectMimeType);
  }

  /**
   * Tests whether or not this flavor represents a remote object.
   *
   * @return <code>true</code> if this flavor represents a remote object,
   * <code>false</code> otherwise.
   */
  public boolean isFlavorRemoteObjectType()
  {
    return isRepresentationClassRemote()
           && isRepresentationClassSerializable()
           && isMimeTypeEqual(javaRemoteObjectMimeType);
  }

  /**
   * Tests whether or not this flavor represents a list of files.
   *
   * @return <code>true</code> if this flavor represents a list of files,
   * <code>false</code> otherwise.
   */
  public boolean isFlavorJavaFileListType()
  {
    if (getPrimaryType().equals(javaFileListFlavor.getPrimaryType())
        && getSubType().equals(javaFileListFlavor.getSubType())
        && javaFileListFlavor.representationClass
	   .isAssignableFrom(representationClass))
      return true;
  
    return false ;
  }

  /**
   * Returns a copy of this object.
   *
   * @return A copy of this object.
   *
   * @exception CloneNotSupportedException If the object's class does not support
   * the Cloneable interface. Subclasses that override the clone method can also
   * throw this exception to indicate that an instance cannot be cloned.
   */
  public Object clone () throws CloneNotSupportedException
  {
    // FIXME - This cannot be right.
    try
      {
        return super.clone();
      }
    catch(Exception e)
      {
        return null;
      }
  }

  /**
   * This method test the specified <code>DataFlavor</code> for equality
   * against this object.  This will be true if the MIME type and
   * representation class are the equal. If the primary type is 'text'
   * then also the value of the charset parameter is compared. In such a
   * case when the charset parameter isn't given then the charset is
   * assumed to be equal to the default charset of the platform.  All
   * other parameters are ignored.
   *
   * @param flavor The <code>DataFlavor</code> to test against.
   *
   * @return <code>true</code> if the flavor is equal to this object,
   * <code>false</code> otherwise.
   */
  public boolean equals(DataFlavor flavor)
  {
    if (flavor == null)
      return false;

    String primary = getPrimaryType();
    if (! primary.equals(flavor.getPrimaryType()))
      return false;

    String sub = getSubType();
    if (! sub.equals(flavor.getSubType()))
      return false;

    if (! this.representationClass.equals(flavor.representationClass))
      return false;

    if (primary.equals("text"))
      if (! isRepresentationClassCharBuffer()
          && ! isRepresentationClassReader()
          && representationClass != java.lang.String.class
	  && ! (representationClass.isArray()
	        && representationClass.getComponentType() == Character.TYPE))
	{
	  String charset = getParameter("charset");
	  String otherset = flavor.getParameter("charset");
	  String defaultset = Charset.defaultCharset().name();

	  if (charset == null || charset.equals(defaultset))
            return (otherset == null || otherset.equals(defaultset));

	  return charset.equals(otherset);
	}
  
    return true;
  }

  /**
   * This method test the specified <code>Object</code> for equality
   * against this object.  This will be true if the following conditions
   * are met:
   * <p>
   * <ul>
   * <li>The object is not <code>null</code>.</li>
   * <li>The object is an instance of <code>DataFlavor</code>.</li>
   * <li>The object's MIME type and representation class are equal to
   * this object's.</li>
   * </ul>
   *
   * @param obj The <code>Object</code> to test against.
   *
   * @return <code>true</code> if the flavor is equal to this object,
   * <code>false</code> otherwise.
   */
  public boolean equals(Object obj)
  {
    if (! (obj instanceof DataFlavor))
      return false;
  
    return equals((DataFlavor) obj);
  }

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
  public boolean equals(String str)
  {
    return isMimeTypeEqual(str);
  }

  /**
   * Returns the hash code for this data flavor.
   * The hash code is based on the (lower case) mime type and the
   * representation class.
   */
  public int hashCode()
  {
    return mimeType.toString().hashCode() ^ representationClass.hashCode();
  }

  /**
   * Returns <code>true</code> when the given <code>DataFlavor</code>
   * matches this one.
   */
  public boolean match(DataFlavor dataFlavor)
  {
    // XXX - How is this different from equals?
    return equals(dataFlavor);
  }

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
  protected String normalizeMimeTypeParameter(String name, String value)
  {
    return name + "=" + value;
  }

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
  protected String normalizeMimeType(String type)
  {
    return type;
  }

  /**
   * Serialize this class.
   *
   * @param stream The <code>ObjectOutput</code> stream to serialize to.
   *
   * @exception IOException If an error occurs.
   */
  public void writeExternal(ObjectOutput stream) 
    throws IOException
  {
    if (mimeType != null)
      {
        mimeType.addParameter("humanPresentableName", humanPresentableName);
        stream.writeObject(mimeType);
        mimeType.removeParameter("humanPresentableName");
      }
    else
      stream.writeObject(null);
    stream.writeObject(representationClass);
  }


  /**
   * De-serialize this class.
   *
   * @param stream The <code>ObjectInput</code> stream to deserialize from.
   *
   * @exception IOException If an error ocurs.
   * @exception ClassNotFoundException If the class for an object being restored
   * cannot be found.
   */
  public void readExternal(ObjectInput stream) 
    throws IOException, ClassNotFoundException
  {
    mimeType = (MimeType) stream.readObject();
    String className = null;
    if (mimeType != null)
      {
        humanPresentableName =
          mimeType.getParameter("humanPresentableName");
        mimeType.removeParameter("humanPresentableName");
        className = mimeType.getParameter("class");
        if (className == null)
          throw new IOException("No class in mime type");
      }
    try
      {
        representationClass = (Class) stream.readObject();
      }
    catch (OptionalDataException ex)
      {
        if (ex.eof && ex.length == 0)
          {
            if (className != null)
              representationClass = tryToLoadClass(className,
                                                  getClass().getClassLoader());
          }
        else
          throw ex;
      }
  }

  /**
   * Returns a string representation of this DataFlavor. Including the
   * representation class name, MIME type and human presentable name.
   */
  public String toString()
  {
    return (getClass().getName()
           + "[representationClass=" + getRepresentationClass().getName()
           + ",mimeType=" + getMimeType()
           + ",humanPresentableName=" + getHumanPresentableName()
           + "]");
  }

  /**
   * XXX - Currently returns <code>java.io.InputStream</code>.
   *
   * @since 1.3
   */
  public final Class<?> getDefaultRepresentationClass()
  {
    return java.io.InputStream.class;
  }

  /**
   * XXX - Currently returns <code>java.io.InputStream</code>.
   */
  public final String getDefaultRepresentationClassAsString()
  {
    return getDefaultRepresentationClass().getName();
  }

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
   *
   * @exception IllegalArgumentException If the representation class is not one
   * of the seven listed above or the Transferable has null data.
   * @exception NullPointerException If the Transferable is null.
   * @exception UnsupportedFlavorException when the transferable doesn't
   * support this <code>DataFlavor</code>. Or if the representable class
   * isn't a (subclass of) <code>Reader</code>, <code>String</code>,
   * <code>InputStream</code> and/or the primary MIME type isn't "text".
   * @exception IOException when any IOException occurs.
   * @exception UnsupportedEncodingException if the "charset" isn't supported
   * on this platform.
   */
  public Reader getReaderForText(Transferable transferable)
    throws UnsupportedFlavorException, IOException
  {
      if (!transferable.isDataFlavorSupported(this))
          throw new UnsupportedFlavorException(this);
  
      if (Reader.class.isAssignableFrom(representationClass))
          return (Reader)transferable.getTransferData(this);
  
      if (String.class.isAssignableFrom(representationClass))
          return new StringReader((String)transferable.getTransferData(this));
  
      if (InputStream.class.isAssignableFrom(representationClass)
          && "text".equals(getPrimaryType()))
        {
          InputStream in = (InputStream)transferable.getTransferData(this);
          String encoding = getParameter("charset");
          if (encoding == null)
              encoding = "us-ascii";
          return new InputStreamReader(in, encoding);
        }
  
      throw new UnsupportedFlavorException(this);
  }

  /**
   * Returns whether the representation class for this DataFlavor is
   * @see java.nio.ByteBuffer or a subclass thereof.
   *
   * @since 1.4
   */
  public boolean isRepresentationClassByteBuffer()
  {
    return ByteBuffer.class.isAssignableFrom(representationClass);
  }

  /**
   * Returns whether the representation class for this DataFlavor is
   * @see java.nio.CharBuffer or a subclass thereof.
   *
   * @since 1.4
   */
  public boolean isRepresentationClassCharBuffer()
  {
    return CharBuffer.class.isAssignableFrom(representationClass);
  }

  /**
   * Returns whether the representation class for this DataFlavor is
   * @see java.io.Reader or a subclass thereof.
   *
   * @since 1.4
   */
  public boolean isRepresentationClassReader()
  {
    return Reader.class.isAssignableFrom(representationClass);
  }
  
  /**
   * Returns whether this <code>DataFlavor</code> is a valid text flavor for
   * this implementation of the Java platform. Only flavors equivalent to
   * <code>DataFlavor.stringFlavor</code> and <code>DataFlavor</code>s with
   * a primary MIME type of "text" can be valid text flavors.
   * <p>
   * If this flavor supports the charset parameter, it must be equivalent to
   * <code>DataFlavor.stringFlavor</code>, or its representation must be
   * <code>java.io.Reader</code>, <code>java.lang.String</code>,
   * <code>java.nio.CharBuffer</code>, <code>java.io.InputStream</code> or 
   * <code>java.nio.ByteBuffer</code>,
   * If the representation is <code>java.io.InputStream</code> or 
   * <code>java.nio.ByteBuffer</code>, then this flavor's <code>charset</code> 
   * parameter must be supported by this implementation of the Java platform. 
   * If a charset is not specified, then the platform default charset, which 
   * is always supported, is assumed.
   * <p>
   * If this flavor does not support the charset parameter, its
   * representation must be <code>java.io.InputStream</code>,
   * <code>java.nio.ByteBuffer</code>.
   * <p>
   * See <code>selectBestTextFlavor</code> for a list of text flavors which
   * support the charset parameter.
   *
   * @return <code>true</code> if this <code>DataFlavor</code> is a valid
   *         text flavor as described above; <code>false</code> otherwise
   * @see #selectBestTextFlavor
   * @since 1.4
   */
  public boolean isFlavorTextType() {
    // FIXME: I'm not 100% sure if this implementation does the same like sun's does    
    if(equals(DataFlavor.stringFlavor) || getPrimaryType().equals("text"))
      {
        String charset = getParameter("charset");
        Class c = getRepresentationClass();
        if(charset != null) 
          {            
            if(Reader.class.isAssignableFrom(c) 
                || CharBuffer.class.isAssignableFrom(c) 
                || String.class.isAssignableFrom(c)) 
              {
                return true;
              }
            else if(InputStream.class.isAssignableFrom(c)
                    || ByteBuffer.class.isAssignableFrom(c))
              {
                return Charset.isSupported(charset);
              }
          }
        else if(InputStream.class.isAssignableFrom(c)
            || ByteBuffer.class.isAssignableFrom(c))
          {
            return true;
          }
      }
    return false;
  }
} // class DataFlavor

