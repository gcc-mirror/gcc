/* VMString.java -- VM Specific String methods
   Copyright (C) 2003, 2010  Free Software Foundation, Inc.

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

package java.lang;

import java.lang.ref.WeakReference;
import java.util.WeakHashMap;

/*
 * This class is a reference version, mainly for compiling a class library
 * jar.  It is likely that VM implementers replace this with their own
 * version that can communicate effectively with the VM.
 */

/**
 * Code relocated from java.lang.String by
 * @author Dave Grove <groved@us.ibm.com>
 */
final class VMString
{

  /**
   * Holds the references for each intern()'d String. If all references to
   * the string disappear, and the VM properly supports weak references,
   * the String will be GC'd.
   */
  private static final WeakHashMap internTable = new WeakHashMap();

  private VMString() {} // Prohibits instantiation.

  /**
   * Fetches this String from the intern hashtable. If two Strings are
   * considered equal, by the equals() method, then intern() will return the
   * same String instance. ie. if (s1.equals(s2)) then
   * (s1.intern() == s2.intern()). All string literals and string-valued
   * constant expressions are already interned.
   *
   * @param str the String to intern
   * @return the interned String
   */
  static String intern(String str)
  {
    synchronized (internTable)
      {
        WeakReference ref = (WeakReference) internTable.get(str);
        if (ref != null)
          {
            String s = (String) ref.get();
            // If s is null, then no strong references exist to the String;
            // the weak hash map will soon delete the key.
            if (s != null)
              return s;
          }
        internTable.put(str, new WeakReference(str));
      }
    return str;
  }

} // class VMString
