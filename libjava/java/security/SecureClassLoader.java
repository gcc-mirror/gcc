/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.security;

public class SecureClassLoader extends ClassLoader 
{
    public SecureClassLoader ()
    { 
      this (null);
    }

    public SecureClassLoader (ClassLoader parent)
    { 
      super (parent);
    }
}

