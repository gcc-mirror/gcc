// Modifier.java - Process modifier values.

/* Copyright (C) 1998, 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/**
 * @author Tom Tromey <tromey@cygnus.com>
 * @date October 1, 1998
 */

/* Written using "Java Class Libraries", 2nd edition, ISBN 0-201-31002-3
 * "The Java Language Specification", ISBN 0-201-63451-1
 * plus online API docs for JDK 1.2 beta from http://www.javasoft.com.
 * Status: Believed complete and correct to version 1.2.
 */

package java.lang.reflect;

public class Modifier
{
  public static final int PUBLIC    = 0x001;
  public static final int PRIVATE   = 0x002;
  public static final int PROTECTED = 0x004;
  public static final int STATIC    = 0x008;
  public static final int FINAL     = 0x010;
  public static final int SYNCHRONIZED = 0x020;
  public static final int VOLATILE  = 0x040;
  public static final int TRANSIENT = 0x080;
  public static final int NATIVE    = 0x100;
  public static final int INTERFACE = 0x200;
  public static final int ABSTRACT  = 0x400;
  public static final int STRICT    = 0x800;

  // This is only used by the C++ code, so it is not public.
  static final int ALL_FLAGS = 0x7ff;

  public static boolean isAbstract (int mod)
  {
    return (mod & ABSTRACT) != 0;
  }

  public static boolean isFinal (int mod)
  {
    return (mod & FINAL) != 0;
  }

  public static boolean isInterface (int mod)
  {
    return (mod & INTERFACE) != 0;
  }

  public static boolean isNative (int mod)
  {
    return (mod & NATIVE) != 0;
  }

  public static boolean isPrivate (int mod)
  {
    return (mod & PRIVATE) != 0;
  }

  public static boolean isProtected (int mod)
  {
    return (mod & PROTECTED) != 0;
  }

  public static boolean isPublic (int mod)
  {
    return (mod & PUBLIC) != 0;
  }

  public static boolean isStatic (int mod)
  {
    return (mod & STATIC) != 0;
  }

  public static boolean isStrict (int mod)
  {
    return (mod & STRICT) != 0;
  }

  public static boolean isSynchronized (int mod)
  {
    return (mod & SYNCHRONIZED) != 0;
  }

  public static boolean isTransient (int mod)
  {
    return (mod & TRANSIENT) != 0;
  }

  public static boolean isVolatile (int mod)
  {
    return (mod & VOLATILE) != 0;
  }

  public static String toString (int mod)
  {
    StringBuffer r = new StringBuffer ();
    toString(mod, r);
    return r.toString();
  }

  static void toString (int mod, StringBuffer r)
  {
    if (isPublic (mod))
      r.append("public ");
    if (isProtected (mod))
      r.append("protected ");
    if (isPrivate (mod))
      r.append("private ");
    if (isAbstract (mod))
      r.append("abstract ");
    if (isStatic (mod))
      r.append("static ");
    if (isFinal (mod))
      r.append("final ");
    if (isTransient (mod))
      r.append("transient ");
    if (isVolatile (mod))
      r.append("volatile ");
    if (isNative (mod))
      r.append("native ");
    if (isSynchronized (mod))
      r.append("synchronized ");
    if (isInterface (mod))
      r.append("interface ");
    if (isStrict (mod))
      r.append("strict ");

    // Trim trailing space.
    int l = r.length();
    if (l > 0)
      r.setLength(l - 1);
  }
}
