/*
 * java.lang.ClassLoader: part of the Java Class Libraries project.
 * Copyright (C) 1998, 2001, 2002 Free Software Foundation
 *
 * This library is free software; you can redistribute it and/or
 * modify it under the terms of the GNU Library General Public
 * License as published by the Free Software Foundation; either
 * version 2 of the License, or (at your option) any later version.
 *
 * This library is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
 * Library General Public License for more details.
 *
 * You should have received a copy of the GNU Library General Public
 * License along with this library; if not, write to the
 * Free Software Foundation, Inc., 59 Temple Place - Suite 330,
 * Boston, MA  02111-1307, USA.
 */

package java.lang;

import java.util.*;

/**
 * java.lang.VMClassLoader is a package-private helper for VMs to implement
 * on behalf of java.lang.ClassLoader.
 *
 * @author John Keiser
 * @version 1.1.0, Sep 22 1998
 * @since CP1.1
 */

class VMClassLoader {

    /** 
     * Helper to define a class using a string of bytes.
     * 
     * @param name the name to give the class.  null if unknown.
     * @param data the data representing the classfile, in classfile format.
     * @param offset the offset into the data where the classfile starts.
     * @param len the length of the classfile data in the array.
     * @return the class that was defined.
     * @exception ClassFormatError if the byte array is not in proper classfile format.
     */
    final static native Class defineClass(ClassLoader cl, String name, 
					  byte[] data, int offset, int len) 
	throws ClassFormatError;

    /** 
     * Helper to resolve all references to other classes from this class.
     * @param c the class to resolve.
     */
  // Not yet needed for libgcj.
  //    final static native void resolveClass(Class c);

    /** 
     * Helper for java.lang.Integer, Byte, etc. to get the TYPE class
     * at initialization time. 
     *
     * @param type code for the primitive type.
     */
  static native Class getPrimitiveClass(char type);

  /**
   * The system default for assertion status. This is used for all system
   * classes (those with a null ClassLoader), as well as the initial value for
   * every ClassLoader's default assertion status.
   *
   * XXX - Not implemented yet; this requires native help.
   *
   * @return the system-wide default assertion status
   */
  static final boolean defaultAssertionStatus()
  {
    return true;
  }

  /**
   * The system default for package assertion status. This is used for all
   * ClassLoader's packageAssertionStatus defaults. It must be a map of
   * package names to Boolean.TRUE or Boolean.FALSE, with the unnamed package
   * represented as a null key.
   *
   * XXX - Not implemented yet; this requires native help.
   *
   * @return a (read-only) map for the default packageAssertionStatus
   */
  static final Map packageAssertionStatus()
  {
    return new HashMap();
  }

  /**
   * The system default for class assertion status. This is used for all
   * ClassLoader's classAssertionStatus defaults. It must be a map of
   * class names to Boolean.TRUE or Boolean.FALSE
   *
   * XXX - Not implemented yet; this requires native help.
   *
   * @return a (read-only) map for the default classAssertionStatus
   */
  static final Map classAssertionStatus()
  {
    return new HashMap();
  }
}
