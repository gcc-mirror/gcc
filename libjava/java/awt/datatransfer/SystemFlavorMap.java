/* SystemFlavorMap.java -- Maps between native flavor names and MIME types.
   Copyright (C) 2001 Free Software Foundation, Inc.

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

import java.util.HashMap;
import java.util.Map;

/**
  * This class maps between native platform type names and DataFlavors.
  *
  * XXX - The current implementation does no mapping at all.
  *
  * @author Mark Wielaard (mark@klomp.org)
  */
public final class SystemFlavorMap implements FlavorMap
{

/**
  * The default (instance) flavor map.
  */
private static FlavorMap defaultFlavorMap;

/**
  * Private constructor.
  */
private SystemFlavorMap()
{
}

/*************************************************************************/

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
public Map
getNativesForFlavors(DataFlavor[] flavors)
{
  return(new HashMap());
}

/*************************************************************************/

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
public Map
getFlavorsForNatives(String[] natives)
{
  return(new HashMap());
}

/*************************************************************************/

/**
  * Returns the default (instance) (System)FlavorMap.
  */
public static FlavorMap
getDefaultFlavorMap()
{
  if (defaultFlavorMap == null)
    defaultFlavorMap = new SystemFlavorMap();

  return(defaultFlavorMap);
}

/*************************************************************************/

/**
  * Returns the native type name for the given java mime type.
  */
public static String
encodeJavaMIMEType(String mime)
{
  return null;
}

/*************************************************************************/

/**
  * Returns the native type name for the given data flavor.
  */
public static String
encodeDataFlavor(DataFlavor df)
{
  return null;
}

/*************************************************************************/

/**
  * Returns true if the native type name can be represented as
  * a java mime type.
  */
public static boolean
isJavaMIMEType(String name)
{
  return(false);
}

/*************************************************************************/

/**
  * Returns the java mime type for the given the native type name.
  */
public static String
decodeJavaMIMEType(String name)
{
  return null;
}

/*************************************************************************/

/**
  * Returns the data flavor given the native type name
  * or null when no such data flavor exists.
  */
public static DataFlavor
decodeDataFlavor(String name) throws ClassNotFoundException
{
  String javaMIMEType = decodeJavaMIMEType(name);
  if (javaMIMEType != null)
    return(new DataFlavor(javaMIMEType));
  else
    return(null);
}

} // class SystemFlavorMap

