/* SystemFlavorMap.java -- Maps between native flavor names and MIME types.
   Copyright (C) 2001, 2004  Free Software Foundation, Inc.

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

import java.awt.Toolkit;
import java.io.File;
import java.io.FileInputStream;
import java.io.IOException;
import java.io.InputStream;
import java.net.URL;
import java.security.AccessController;
import java.security.PrivilegedAction;
import java.util.ArrayList;
import java.util.Collection;
import java.util.Enumeration;
import java.util.HashMap;
import java.util.List;
import java.util.Map;
import java.util.Properties;
import java.util.WeakHashMap;

/**
  * This class maps between native platform type names and DataFlavors.
  *
  * XXX - The current implementation does no mapping at all.
  *
  * @author Mark Wielaard (mark@klomp.org)
  *
  * @since 1.2
  */
public final class SystemFlavorMap implements FlavorMap, FlavorTable
{
  /**
   * The map which maps the thread's <code>ClassLoaders</code> to 
   * <code>SystemFlavorMaps</code>.
   */
  private static final Map systemFlavorMaps = new WeakHashMap();
  
  /**
   * Constant which is used to prefix encode Java MIME types.
   */
  private static final String GNU_JAVA_MIME_PREFIX = "gnu.java:";
  
  /**
   * This map maps native <code>String</code>s to lists of 
   * <code>DataFlavor</code>s
   */
  private HashMap<String,List<DataFlavor>> nativeToFlavorMap =
    new HashMap<String,List<DataFlavor>>();
  
  /**
   * This map maps <code>DataFlavor</code>s to lists of native 
   * <code>String</code>s
   */
  private HashMap<DataFlavor, List<String>> flavorToNativeMap =
    new HashMap<DataFlavor, List<String>>();
  
  /**
   * Private constructor.
   */
  private SystemFlavorMap ()
  {
    AccessController.doPrivileged
    (new PrivilegedAction<Object>()
     {
       public Object run()
       {
         try
           {
             // Load installed flavormap.properties first.
             String sep = File.separator;
             File propsFile =
               new File(System.getProperty("gnu.classpath.home.url")
                        + sep + "accessibility.properties");
             InputStream in = new FileInputStream(propsFile);
             Properties props = new Properties();
             props.load(in);
             in.close();

             String augmented = Toolkit.getProperty("AWT.DnD.flavorMapFileURL",
                                                    null);
             if (augmented != null)
               {
                 URL url = new URL(augmented);
                 in = url.openStream();
                 props.load(in);
               }
             setupMapping(props);
           }
         catch (IOException ex)
           {
             // Can't do anything about it.
           }
         return null;
       }
     });
  }

  /**
   * Sets up the mapping from native to mime types and vice versa as specified
   * in the flavormap.properties file.
   *
   * This is package private to avoid an accessor method.
   *
   * @param props the properties file
   */
  void setupMapping(Properties props)
  {
    Enumeration propNames = props.propertyNames();
    while (propNames.hasMoreElements())
      {
        try
          {
            String nat = (String) propNames.nextElement();
            String mime = (String) props.getProperty(nat);
            // Check valid mime type.
            MimeType type = new MimeType(mime);
            DataFlavor flav = new DataFlavor(mime);
            
            List<DataFlavor> flavs = nativeToFlavorMap.get(nat);
            if (flavs == null)
              {
                flavs = new ArrayList<DataFlavor>();
                nativeToFlavorMap.put(nat, flavs);
              }
            List<String> nats = flavorToNativeMap.get(flav);
            if (nats == null)
              {
                nats = new ArrayList<String>();
                flavorToNativeMap.put(flav, nats);
              }
            flavs.add(flav);
            nats.add(nat);
          }
        catch (ClassNotFoundException ex)
          {
            // Skip.
          }
        catch (MimeTypeParseException ex)
          {
            // Skip.
          }
      }
  }

  /**
   * Maps the specified <code>DataFlavor</code> objects to the native
   * data type name.  The returned <code>Map</code> has keys that are
   * the data flavors and values that are strings.  The returned map
   * may be modified.  This can be useful for implementing nested mappings.
   *
   * @param flavors An array of data flavors to map
   *                or null for all data flavors.
   *
   * @return A <code>Map</code> of native data types to data flavors.
   */
  public Map<DataFlavor, String> getNativesForFlavors (DataFlavor[] flavors)
  {
    return new HashMap<DataFlavor, String>();
  }

  /**
   * Maps the specified native type names to <code>DataFlavor</code>'s.
   * The returned <code>Map</code> has keys that are strings and values
   * that are <code>DataFlavor</code>'s.  The returned map may be
   * modified.  This can be useful for implementing nested mappings.
   *
   * @param natives An array of native types to map
   *                or null for all native types.
   *
   * @return A <code>Map</code> of data flavors to native type names.
   */
  public Map<String, DataFlavor> getFlavorsForNatives (String[] natives)
  { 
    return new HashMap<String, DataFlavor>();
  }

  /**
   * Returns the (System)FlavorMap for the current thread's
   * ClassLoader.
   */
  public static FlavorMap getDefaultFlavorMap ()
  {
    ClassLoader classLoader = Thread.currentThread()
        .getContextClassLoader();
    
    //if ContextClassLoader not set, use system default 
    if (classLoader == null)
      {
        classLoader = ClassLoader.getSystemClassLoader();
      }
    
    synchronized(systemFlavorMaps)
      {
        FlavorMap map = (FlavorMap) 
            systemFlavorMaps.get(classLoader);
        if (map == null) 
          {
            map = new SystemFlavorMap();
            systemFlavorMaps.put(classLoader, map);
          }
        return map;
      }
  }

  /**
   * Encodes a MIME type for use as a <code>String</code> native. The format
   * of an encoded representation of a MIME type is implementation-dependent.
   * The only restrictions are:
   * <ul>
   * <li>The encoded representation is <code>null</code> if and only if the
   * MIME type <code>String</code> is <code>null</code>.</li>
   * <li>The encoded representations for two non-<code>null</code> MIME type
   * <code>String</code>s are equal if and only if these <code>String</code>s
   * are equal according to <code>String.equals(Object)</code>.</li>
   * </ul>
   * <p>
   * The present implementation of this method returns the specified MIME
   * type <code>String</code> prefixed with <code>gnu.java:</code>.
   *
   * @param mime the MIME type to encode
   * @return the encoded <code>String</code>, or <code>null</code> if
   *         mimeType is <code>null</code>
   */
  public static String encodeJavaMIMEType (String mime)
  {
    if (mime != null)
      return GNU_JAVA_MIME_PREFIX + mime;
    else
      return null;
  }

  /**
   * Encodes a <code>DataFlavor</code> for use as a <code>String</code>
   * native. The format of an encoded <code>DataFlavor</code> is 
   * implementation-dependent. The only restrictions are:
   * <ul>
   * <li>The encoded representation is <code>null</code> if and only if the
   * specified <code>DataFlavor</code> is <code>null</code> or its MIME type
   * <code>String</code> is <code>null</code>.</li>
   * <li>The encoded representations for two non-<code>null</code>
   * <code>DataFlavor</code>s with non-<code>null</code> MIME type
   * <code>String</code>s are equal if and only if the MIME type
   * <code>String</code>s of these <code>DataFlavor</code>s are equal
   * according to <code>String.equals(Object)</code>.</li>
   * </ul>
   * <p>
   * The present implementation of this method returns the MIME type
   * <code>String</code> of the specified <code>DataFlavor</code> prefixed
   * with <code>gnu.java:</code>.
   *
   * @param df the <code>DataFlavor</code> to encode
   * @return the encoded <code>String</code>, or <code>null</code> if
   *         flav is <code>null</code> or has a <code>null</code> MIME type
   */
  public static String encodeDataFlavor (DataFlavor df)
  {
    if (df != null)
      {
        return encodeJavaMIMEType(df.getMimeType());
      }
    else
      return null;
  }

  /**
   * Returns true if the native type name can be represented as
   * a java mime type. Returns <code>false</code> if parameter is
   * <code>null</code>.
   */
  public static boolean isJavaMIMEType (String name)
  {
    return (name != null && name.startsWith(GNU_JAVA_MIME_PREFIX));
  }

  /**
   * Decodes a <code>String</code> native for use as a Java MIME type.
   *
   * @param name the <code>String</code> to decode
   * @return the decoded Java MIME type, or <code>null</code> if nat 
   *         is not an encoded <code>String</code> native
   */
  public static String decodeJavaMIMEType (String name)
  {
    if (isJavaMIMEType(name))
      {
        return name.substring(GNU_JAVA_MIME_PREFIX.length());
      }
    else 
      return null;
  }

  /**
   * Returns the data flavor given the native type name
   * or null when no such data flavor exists.
   */
  public static DataFlavor decodeDataFlavor (String name)
    throws ClassNotFoundException
  {
    String javaMIMEType = decodeJavaMIMEType (name);
    
    if (javaMIMEType != null)
      return new DataFlavor (javaMIMEType);
    else
      return null;
  }

  /** 
   * Returns a List of <code>DataFlavors</code> to which the specified 
   * <code>String</code> native can be translated by the data transfer 
   * subsystem. The <code>List</code> will be sorted from best 
   * <code>DataFlavor</code> to worst. That is, the first <code>DataFlavor 
   * </code> will best reflect data in the specified native to a Java 
   * application. 
   * <p>
   * If the specified native is previously unknown to the data transfer 
   * subsystem, and that native has been properly encoded, then invoking 
   * this method will establish a mapping in both directions between the 
   * specified native and a DataFlavor whose MIME type is a decoded 
   * version of the native.
   */ 
  public List<DataFlavor> getFlavorsForNative(String nat)
  {
    List<DataFlavor> ret = new ArrayList<DataFlavor>();
    if (nat == null)
      {
        Collection<List<DataFlavor>> all = nativeToFlavorMap.values();
        for (List<DataFlavor> list : all)
          {
            for (DataFlavor flav : list)
              {
                if (! ret.contains(flav))
                  ret.add(flav);
              }
          }
      }
    else
      {
        List<DataFlavor> list = nativeToFlavorMap.get(nat);
        if (list != null)
          ret.addAll(list);
      }
    return ret;
  }

  public List<String> getNativesForFlavor (DataFlavor flav)
  {
    List<String> ret = new ArrayList<String>();
    if (flav == null)
      {
        Collection<List<String>> all = flavorToNativeMap.values();
        for (List<String> list : all)
          {
            for (String nat : list)
              {
                if (! ret.contains(nat))
                  ret.add(nat);
              }
          }
      }
    else
      {
        List<String> list = flavorToNativeMap.get(flav);
        if (list != null)
          ret.addAll(list);
      }
    return ret;
  }
  
  /**
   * Adds a mapping from a single <code>String</code> native to a single
   * <code>DataFlavor</code>. Unlike <code>getFlavorsForNative</code>, the
   * mapping will only be established in one direction, and the native will
   * not be encoded. To establish a two-way mapping, call
   * <code>addUnencodedNativeForFlavor</code> as well. The new mapping will
   * be of lower priority than any existing mapping.
   * This method has no effect if a mapping from the specified
   * <code>String</code> native to the specified or equal
   * <code>DataFlavor</code> already exists.
   *
   * @param nativeStr the <code>String</code> native key for the mapping
   * @param flavor the <code>DataFlavor</code> value for the mapping
   * @throws NullPointerException if nat or flav is <code>null</code>
   *
   * @see #addUnencodedNativeForFlavor
   * @since 1.4
   */
  public synchronized void addFlavorForUnencodedNative(String nativeStr, 
                                                       DataFlavor flavor)
  {
    if ((nativeStr == null) || (flavor == null))
      throw new NullPointerException();
    List<DataFlavor> flavors = nativeToFlavorMap.get(nativeStr);
    if (flavors == null) 
      {
        flavors = new ArrayList<DataFlavor>();
        nativeToFlavorMap.put(nativeStr, flavors);
      }
    else
      {
        if (! flavors.contains(flavor))
          flavors.add(flavor);
      }
  }
  
  /**
   * Adds a mapping from the specified <code>DataFlavor</code> (and all
   * <code>DataFlavor</code>s equal to the specified <code>DataFlavor</code>)
   * to the specified <code>String</code> native.
   * Unlike <code>getNativesForFlavor</code>, the mapping will only be
   * established in one direction, and the native will not be encoded. To
   * establish a two-way mapping, call
   * <code>addFlavorForUnencodedNative</code> as well. The new mapping will 
   * be of lower priority than any existing mapping.
   * This method has no effect if a mapping from the specified or equal
   * <code>DataFlavor</code> to the specified <code>String</code> native
   * already exists.
   *
   * @param flavor the <code>DataFlavor</code> key for the mapping
   * @param nativeStr the <code>String</code> native value for the mapping
   * @throws NullPointerException if flav or nat is <code>null</code>
   *
   * @see #addFlavorForUnencodedNative
   * @since 1.4
   */
  public synchronized void addUnencodedNativeForFlavor(DataFlavor flavor,
                                                       String nativeStr) 
  {
    if ((nativeStr == null) || (flavor == null))
      throw new NullPointerException();
    List<String> natives = flavorToNativeMap.get(flavor);
    if (natives == null) 
      {
        natives = new ArrayList<String>();
        flavorToNativeMap.put(flavor, natives);
      }
    else
      {
        if (! natives.contains(nativeStr))
          natives.add(nativeStr);
      }
  }
  
  /**
   * Discards the current mappings for the specified <code>DataFlavor</code>
   * and all <code>DataFlavor</code>s equal to the specified
   * <code>DataFlavor</code>, and creates new mappings to the 
   * specified <code>String</code> natives.
   * Unlike <code>getNativesForFlavor</code>, the mappings will only be
   * established in one direction, and the natives will not be encoded. To
   * establish two-way mappings, call <code>setFlavorsForNative</code>
   * as well. The first native in the array will represent the highest
   * priority mapping. Subsequent natives will represent mappings of
   * decreasing priority.
   * <p>
   * If the array contains several elements that reference equal
   * <code>String</code> natives, this method will establish new mappings
   * for the first of those elements and ignore the rest of them.
   * <p> 
   * It is recommended that client code not reset mappings established by the
   * data transfer subsystem. This method should only be used for
   * application-level mappings.
   *
   * @param flavor the <code>DataFlavor</code> key for the mappings
   * @param natives the <code>String</code> native values for the mappings
   * @throws NullPointerException if flav or natives is <code>null</code>
   *         or if natives contains <code>null</code> elements
   *
   * @see #setFlavorsForNative
   * @since 1.4
   */
  public synchronized void setNativesForFlavor(DataFlavor flavor,
                                               String[] natives) 
  {
    if ((natives == null) || (flavor == null))
      throw new NullPointerException();
    
    flavorToNativeMap.remove(flavor);
    for (int i = 0; i < natives.length; i++) 
      {
        addUnencodedNativeForFlavor(flavor, natives[i]);
      }
  }
  
  /**
   * Discards the current mappings for the specified <code>String</code>
   * native, and creates new mappings to the specified
   * <code>DataFlavor</code>s. Unlike <code>getFlavorsForNative</code>, the
   * mappings will only be established in one direction, and the natives need
   * not be encoded. To establish two-way mappings, call
   * <code>setNativesForFlavor</code> as well. The first
   * <code>DataFlavor</code> in the array will represent the highest priority
   * mapping. Subsequent <code>DataFlavor</code>s will represent mappings of
   * decreasing priority.
   * <p>
   * If the array contains several elements that reference equal
   * <code>DataFlavor</code>s, this method will establish new mappings
   * for the first of those elements and ignore the rest of them.
   * <p>
   * It is recommended that client code not reset mappings established by the
   * data transfer subsystem. This method should only be used for
   * application-level mappings.
   *
   * @param nativeStr the <code>String</code> native key for the mappings
   * @param flavors the <code>DataFlavor</code> values for the mappings
   * @throws NullPointerException if nat or flavors is <code>null</code>
   *         or if flavors contains <code>null</code> elements
   *
   * @see #setNativesForFlavor
   * @since 1.4
   */
  public synchronized void setFlavorsForNative(String nativeStr,
                                               DataFlavor[] flavors) 
  {
    if ((nativeStr == null) || (flavors == null))
      throw new NullPointerException();
    
    nativeToFlavorMap.remove(nativeStr);
    for (int i = 0; i < flavors.length; i++) 
      {
        addFlavorForUnencodedNative(nativeStr, flavors[i]);
      }
  }

} // class SystemFlavorMap
