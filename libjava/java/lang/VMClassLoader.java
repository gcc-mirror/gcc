/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */

package java.lang;

import java.io.*;
import java.net.URL;
import gnu.gcj.util.path.SearchPath;

final class VMClassLoader extends java.lang.ClassLoader
{
  private SearchPath   path;
  private final String path_seperator;
  private final String file_seperator;
  private final char   file_seperator_char;
    
  private VMClassLoader () {	
    path_seperator = System.getProperty ("path.separator", ":");
    file_seperator = System.getProperty ("file.separator", "/");

    file_seperator_char = file_seperator.charAt (0);

    String class_path = System.getProperty ("java.class.path", ".");
    path = new SearchPath (class_path);
  }

  protected Class loadClass(String name,
			    boolean resolve) 
    throws java.lang.ClassNotFoundException, java.lang.LinkageError
  {
    return loadClassInternal (name, resolve, false);
  }
	
    /** I'm a little in doubt here, if this method is 
	actually supposed to throw a LinkageError, or not.  
	The spec, 20.14.3, is a little unclear.  It says:

	`` The general contract of loadClass is that, given the name
	of a class, it either returns the Class object for the class
	or throws a ClassNotFoundException.''

	However, by making LinkageError a checked exception, 
	i.e., mention it directly in the throws clause,
	we'll force caller to consider that case as well.
    **/

  protected Class loadClassInternal(String name,
				    boolean resolve, 
				    boolean fromBootLoader) 
    throws java.lang.ClassNotFoundException, java.lang.LinkageError
  {
    Class clazz;

    /** TODO: call _Jv_VerifyClassName **/
    if (   (name.indexOf ('/') != -1)
	   || (name.charAt (0) == '.')
	   || (name.indexOf (file_seperator) != -1)
	   || (name.indexOf ("..") != -1))
	{
	    throw new IllegalArgumentException (name);
	}

    // already loaded?
    clazz = findLoadedClass (name);

    // we need access to the boot class loader here
    if (clazz == null && !fromBootLoader)
      clazz = findBootClass (name);

    if (clazz == null)
      {
	StringBuffer res = new StringBuffer ();

	// here we do actually replace .'s with /'s because
	// we're going to find something in the file system.
	res.append (name.replace ('.', file_seperator_char));
	res.append (".class");
		
	byte[] data = getResourceAsBytes (res.toString ());

	if (data == null)
	  throw new ClassNotFoundException (name);

	clazz = defineClass (name, data, 0, data.length);
	    
      }

    if (resolve && clazz != null)
      resolveClass (clazz);

    return clazz;
  }

  private native Class findBootClass (String name);

  public InputStream getResourceAsStream(String name) 
  {
    return path.getStream (name);
  }

  public URL getResource(String name) 
  {
    return path.getURL (name);
  }

  public byte[] getResourceAsBytes(String name) 
  {
    return path.getBytes (name);
  }
}
