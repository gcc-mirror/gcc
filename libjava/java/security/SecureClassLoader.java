/* SecureClassLoader.java --- A Secure Class Loader
   Copyright (C) 1999 Free Software Foundation, Inc.

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

package java.security;

/**
   A Secure Class Loader for loading classes with additional 
   support for specifying code source and permissions when
   they are retrieved by the system policy handler.

   @since JDK 1.2

   @author Mark Benvenuto
 */
public class SecureClassLoader extends ClassLoader
{
  protected SecureClassLoader(ClassLoader parent)
  {
    super(parent);
    SecurityManager sm = System.getSecurityManager();
    if(sm != null)
      sm.checkCreateClassLoader();
  }

  protected SecureClassLoader()
  {
    SecurityManager sm = System.getSecurityManager();
    if(sm != null)
      sm.checkCreateClassLoader();
  }

  /** 
     Creates a class using an array of bytes and a 
     CodeSource.

     @param name the name to give the class.  null if unknown.
     @param b the data representing the classfile, in classfile format.
     @param off the offset into the data where the classfile starts.
     @param len the length of the classfile data in the array.
     @param cs the CodeSource for the class or null when unknown.

     @return the class that was defined and optional CodeSource.

     @exception ClassFormatError if the byte array is not in proper classfile format.
   */
  protected final Class defineClass(String name, byte[] b, int off, int len,
				    CodeSource cs)
  {
    // FIXME: Need to cache ProtectionDomains according to 1.3 docs.
    if (cs != null)
      {
	ProtectionDomain protectionDomain
		= new ProtectionDomain(cs, getPermissions(cs));
	return super.defineClass(name, b, off, len, protectionDomain);
      } 
    else
      return super.defineClass(name, b, off, len);
  }

  /**
     Returns a PermissionCollection for the specified CodeSource.
     The default implmentation invokes 
     java.security.Policy.getPermissions.

     This method is called by defineClass that takes a CodeSource
     arguement to build a proper ProtectionDomain for the class
     being defined.

   */
  protected PermissionCollection getPermissions(CodeSource cs)
  {
    Policy policy = Policy.getPolicy();
    return policy.getPermissions(cs);
  }
}
