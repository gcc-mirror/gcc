// Show a value given class name and constant name.

/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Written by Tom Tromey <tromey@redhat.com>.  */

// Use like this to print a `static final' value (integers only, not
// strings yet):
//   java showval java.awt.geom.AffineTransform.TYPE_IDENTITY
// Prints result like:
//   TYPE_IDENTITY = 0
// In conjunction with a keyboard macro you can do a number of
// constants very easily.

import java.lang.reflect.*;

public class showval
{
  public static void main (String[] args)
  {
    int ch = args[0].lastIndexOf ('.');
    String className = args[0].substring (0, ch);
    String constName = args[0].substring (ch + 1);
    try
      {
	Class klass = Class.forName (className);
	Field field = klass.getField (constName);
	System.out.println (constName + " = " + field.getInt (null));
      }
    catch (Throwable _)
      {
	System.out.println (_);
      }
  }
}
