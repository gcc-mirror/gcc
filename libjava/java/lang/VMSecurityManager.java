/*
 * java.lang.SecurityManager: part of the Java Class Libraries project.
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

import java.net.*;
import java.util.*;
import java.io.*;

/**
 ** VMSecurityManager is a helper class for SecurityManager the VM must
 ** implement.
 **
 ** @author  John Keiser
 ** @version 1.1.0, 31 May 1998
 **/
class VMSecurityManager
{
  /** Get a list of all the classes currently executing
   ** methods on the Java stack.  getClassContext()[0] is
   ** the currently executing method
   ** <STRONG>Spec Note:</STRONG> does not say whether
   ** the stack will include the getClassContext() call or
   ** the one just before it.
   **
   ** @return an array containing all the methods on classes
   **         on the Java execution stack.
   **/
  static native Class[] getClassContext();

  /** Get the current ClassLoader--the one nearest to the
   ** top of the stack.
   ** @return the current ClassLoader.
   **/
  static ClassLoader currentClassLoader()
  {
    // The docs above are wrong.  See the online docs.
    // FIXME this implementation is a bit wrong too -- the docs say we
    // must also consider ancestors of the system class loader.
    Class[] classStack = getClassContext ();
    for (int i = 0; i < classStack.length; i++)
      {
	ClassLoader loader = classStack[i].getClassLoader();
	if (loader != null)
	  return loader;
      }

    return null;
  }
}
