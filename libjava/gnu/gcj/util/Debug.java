/*  Copyright (C) 2004  Free Software Foundation

This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Utility methods that allow an object to be converted to a textual
   representation on an OutputStream.  The intention here is that this
   class be used for debugging, so we provide information about all
   fields, public or otherwise. */

package gnu.gcj.util;

import java.lang.reflect.*;
import java.io.*;
import java.util.*;

class Debug 
{
  private final PrintStream p;
  private final int maxdepth;
  private final int maxArrayLength;
  private final boolean printStaticFields;
  private int depth; 

  Debug(PrintStream writer, int maxdepth, int maxArrayLength, boolean printStaticFields)
  {
    p = writer;
    this.maxdepth = maxdepth;
    this.maxArrayLength = maxArrayLength;
    this.printStaticFields = printStaticFields;
  }

  Debug(PrintStream writer)
  {
    this(writer, 0, 10, false);
  }

  Debug(int maxdepth, boolean printStaticFields)
  {
    this(new PrintStream
	 (new FileOutputStream(FileDescriptor.err), true), 
	 maxdepth, 
	 maxdepth > 0 ? 1000 : 10, printStaticFields);
  }

  Debug(int maxdepth)
  {
    this(maxdepth, false);
  }

  Debug()
  {
    this(0, false);
  }
  
  private final void indent()
  {
    for (int i = 0; i < depth; i++)
      p.print("  ");
  }

  private final java.util.IdentityHashMap h = 
  new java.util.IdentityHashMap();

  private static native Field[] getDeclaredFields(Class c);
  private static native Object getField(Object o, Field f);
  private static native long getAddr(Object o);

  // Return an array containing all the fields of a class and its
  // superclasses.
  private Field[] internalGetFields(Class c)
  {
    HashSet set = new HashSet();
    set.addAll(Arrays.asList(getDeclaredFields(c)));
    Class[] interfaces = c.getInterfaces();
    for (int i = 0; i < interfaces.length; i++)
      set.addAll(Arrays.asList(internalGetFields(interfaces[i])));
    Class superClass = c.getSuperclass();
    if (superClass != null)
      set.addAll(Arrays.asList(internalGetFields(superClass)));
    return (Field[])set.toArray(new Field[set.size()]);
  }

  // FIXME: We could just use getClass() here, but this is a
  // workaround for a C++ bug that is causing getClass() to be
  // miscompiled.
  static private Class getItsClass(Object O)
  {
    return O.getClass();
  }

  // Print a reasonably readable textual representation of an object
  // on our OutputStream.  Objects are only printed once, no matter
  // how many references point to them.
  private void print(Object O)
  {
    int savedDepth = depth;
    h.put(O, O);
    try
      {
	Class C = getItsClass(O);
	p.print(C.getName() + "@");
	p.println(Long.toHexString(getAddr(O)));

	if (C.isArray())
	  {
	    indent(); p.println("{");
	    depth++;
	    indent();
	    C = C.getComponentType();

	    int len = Array.getLength(O);
	    for (int i = 0; i < len; i++)
	      {
		Object thing = Array.get(O, i);
		print0(thing, C);
		p.print(", ");
		if (i > maxArrayLength)
		  {
		    p.print("...");
		    break;
		  }
	      }
	    depth--;
	    p.println();
	    indent(); p.print("}");
	    return;
	  }

	indent(); p.println("{");
	depth++;
	if (C == java.lang.Class.class)
	  {
	    indent();
	    p.println ("class = " + O.toString() + ",");
	  }
	else if (C == java.lang.reflect.Field.class)
	  {
	    indent();
	    p.println ("<field> = \"" + O.toString() + "\",");
	  }
	else if (C == java.lang.String.class)
	  {
	    indent();
	    p.println ("<string> = \"" + O.toString() + "\",");
	  }
	Field[] f = internalGetFields(C);
	for (int i = 0; i < f.length; i++)
	  {
	    Class type = f[i].getType();
	    boolean isStatic = (f[i].getModifiers() & Modifier.STATIC) != 0;
 
	    if (isStatic && ! printStaticFields)
	      continue;

	    indent();
	    if (isStatic)
	      p.print("static ");
	    p.print(type.getName() +" " +f[i].getName() + " = ");
	    Object thing = getField(O, f[i]);
	    print0(thing, type);
	    p.println(",");
	  }
	depth--;
	indent(); p.print("}");
      }
    catch (Throwable t)
      {
	p.print("error: 0x" + Long.toHexString(getAddr(O)) + ";");
	depth = savedDepth;
      }
  }

  private void print0(Object thing, Class C)
  {
    try
      {
	if (thing == null)
	  {
	    p.print("null");
	    return;
	  }
	else if (C == gnu.gcj.RawData.class || 
		 C == gnu.gcj.RawDataManaged.class)
	  {
	  }
	else if (C.isPrimitive())
	  {
	    if (getItsClass(thing) == Character.class)				
	      p.print("'" + thing + "'");
	    else
	      p.print(thing);
	    return;
	  }
	else if (getItsClass(thing) == String.class)
	  {			  
	    p.print("\"" + thing + "\"");
	    return;
	  }
	else if (depth < maxdepth && h.get(thing) == null)
	  {
	    depth++;
	    print(thing);
	    depth--;
	    return;
	  }
      }
    catch (Throwable t)
      {
      }
    
    // The default action: just print the address.
    p.print("0x"+ Long.toHexString(getAddr(thing)));    
  }

  // Print the textual representation of an object on System.err.
  public void write(Object O)
  {
    depth = 0;
    print(O);
    p.flush();
  }
}
