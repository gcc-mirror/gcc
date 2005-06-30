/* RMIC.java --
   Copyright (c) 1996, 1997, 1998, 1999, 2001, 2002, 2003, 2004, 2005
   Free Software Foundation, Inc.

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


package gnu.java.rmi.rmic;

import gnu.java.rmi.server.RMIHashes;

import java.io.File;
import java.io.FileWriter;
import java.io.IOException;
import java.io.PrintWriter;
import java.lang.reflect.Method;
import java.rmi.RemoteException;
import java.util.Arrays;
import java.util.HashSet;
import java.util.Iterator;
import java.util.Set;


public class RMIC
{
  private String[] args;
  private int next;
  private Exception exception;
  private boolean keep = false;
  private boolean need11Stubs = true;
  private boolean need12Stubs = true;
  private boolean compile = true;
  private boolean verbose;
  private String destination;
  private PrintWriter out;
  private TabbedWriter ctrl;
  private Class clazz;
  private String classname;
  private String fullclassname;
  private MethodRef[] remotemethods;
  private String stubname;
  private String skelname;
  private int errorCount = 0;
  private Class mRemoteInterface;

  public RMIC(String[] a)
  {
    args = a;
  }

  public static void main(String[] args)
  {
    RMIC r = new RMIC(args);
    if (r.run() == false)
      {
	Exception e = r.getException();
	if (e != null)
	  e.printStackTrace();
	else
	  System.exit(1);
      }
  }

  public boolean run()
  {
    parseOptions();
    if (next >= args.length)
      error("no class names found");
    for (int i = next; i < args.length; i++)
      {
	try
	  {
	    if (verbose)
	      System.out.println("[Processing class " + args[i] + ".class]");
	    processClass(args[i].replace(File.separatorChar, '.'));
	  }
	catch (Exception e)
	  {
	    exception = e;
	    return (false);
	  }
      }
    return (true);
  }

  private boolean processClass(String classname) throws Exception
  {
    errorCount = 0;
    analyzeClass(classname);
    if (errorCount > 0)
      System.exit(1);
    generateStub();
    if (need11Stubs)
      generateSkel();
    if (compile)
      {
	compile(stubname.replace('.', File.separatorChar) + ".java");
	if (need11Stubs)
	  compile(skelname.replace('.', File.separatorChar) + ".java");
      }
    if (! keep)
      {
	(new File(stubname.replace('.', File.separatorChar) + ".java")).delete();
	if (need11Stubs)
	  (new File(skelname.replace('.', File.separatorChar) + ".java"))
	  .delete();
      }
    return (true);
  }

  private void analyzeClass(String cname) throws Exception
  {
    if (verbose)
      System.out.println("[analyze class " + cname + "]");
    int p = cname.lastIndexOf('.');
    if (p != -1)
      classname = cname.substring(p + 1);
    else
      classname = cname;
    fullclassname = cname;

    HashSet rmeths = new HashSet();
    findClass();

    // get the remote interface
    mRemoteInterface = getRemoteInterface(clazz);
    if (mRemoteInterface == null)
      return;
    if (verbose)
      System.out.println("[implements " + mRemoteInterface.getName() + "]");

    // check if the methods of the remote interface declare RemoteExceptions
    Method[] meths = mRemoteInterface.getDeclaredMethods();
    for (int i = 0; i < meths.length; i++)
      {
	Class[] exceptions = meths[i].getExceptionTypes();
	int index = 0;
	for (; index < exceptions.length; index++)
	  {
	    if (exceptions[index].equals(RemoteException.class))
	      break;
	  }
	if (index < exceptions.length)
	  rmeths.add(meths[i]);
	else
	  logError("Method " + meths[i]
	           + " does not throw a java.rmi.RemoteException");
      }

    // Convert into a MethodRef array and sort them
    remotemethods = new MethodRef[rmeths.size()];
    int c = 0;
    for (Iterator i = rmeths.iterator(); i.hasNext();)
      remotemethods[c++] = new MethodRef((Method) i.next());
    Arrays.sort(remotemethods);
  }

  public Exception getException()
  {
    return (exception);
  }

  private void findClass() throws ClassNotFoundException
  {
    clazz =
      Class.forName(fullclassname, true, ClassLoader.getSystemClassLoader());
  }

  private void generateStub() throws IOException
  {
    stubname = fullclassname + "_Stub";
    String stubclassname = classname + "_Stub";
    ctrl =
      new TabbedWriter(new FileWriter((destination == null ? ""
                                                           : destination
                                                           + File.separator)
                                      + stubname.replace('.',
                                                         File.separatorChar)
                                      + ".java"));
    out = new PrintWriter(ctrl);

    if (verbose)
      System.out.println("[Generating class " + stubname + ".java]");

    out.println("// Stub class generated by rmic - DO NOT EDIT!");
    out.println();
    if (fullclassname != classname)
      {
	String pname =
	  fullclassname.substring(0, fullclassname.lastIndexOf('.'));
	out.println("package " + pname + ";");
	out.println();
      }

    out.print("public final class " + stubclassname);
    ctrl.indent();
    out.println("extends java.rmi.server.RemoteStub");

    // Output interfaces we implement
    out.print("implements ");
    /* Scan implemented interfaces, and only print remote interfaces. */
    Class[] ifaces = clazz.getInterfaces();
    Set remoteIfaces = new HashSet();
    for (int i = 0; i < ifaces.length; i++)
      {
	Class iface = ifaces[i];
	if (java.rmi.Remote.class.isAssignableFrom(iface))
	  remoteIfaces.add(iface);
      }
    Iterator iter = remoteIfaces.iterator();
    while (iter.hasNext())
      {
	/* Print remote interface. */
	Class iface = (Class) iter.next();
	out.print(iface.getName());

	/* Print ", " if more remote interfaces follow. */
	if (iter.hasNext())
	  out.print(", ");
      }
    ctrl.unindent();
    out.print("{");
    ctrl.indent();

    // UID
    if (need12Stubs)
      {
	out.println("private static final long serialVersionUID = 2L;");
	out.println();
      }

    // InterfaceHash - don't know how to calculate this - XXX
    if (need11Stubs)
      {
	out.println("private static final long interfaceHash = "
	            + RMIHashes.getInterfaceHash(clazz) + "L;");
	out.println();
	if (need12Stubs)
	  {
	    out.println("private static boolean useNewInvoke;");
	    out.println();
	  }

	// Operation table
	out.print("private static final java.rmi.server.Operation[] operations = {");

	ctrl.indent();
	for (int i = 0; i < remotemethods.length; i++)
	  {
	    Method m = remotemethods[i].meth;
	    out.print("new java.rmi.server.Operation(\"");
	    out.print(getPrettyName(m.getReturnType()) + " ");
	    out.print(m.getName() + "(");
	    // Output signature
	    Class[] sig = m.getParameterTypes();
	    for (int j = 0; j < sig.length; j++)
	      {
		out.print(getPrettyName(sig[j]));
		if (j + 1 < sig.length)
		  out.print(", ");
	      }
	    out.print(")\")");
	    if (i + 1 < remotemethods.length)
	      out.println(",");
	  }
	ctrl.unindent();
	out.println("};");
	out.println();
      }

    // Set of method references.
    if (need12Stubs)
      {
	for (int i = 0; i < remotemethods.length; i++)
	  {
	    Method m = remotemethods[i].meth;
	    out.println("private static java.lang.reflect.Method $method_"
	                + m.getName() + "_" + i + ";");
	  }

	// Initialize the methods references.
	out.println();
	out.print("static {");
	ctrl.indent();

	out.print("try {");
	ctrl.indent();

	if (need11Stubs)
	  {
	    out.println("java.rmi.server.RemoteRef.class.getMethod(\"invoke\", new java.lang.Class[] { java.rmi.Remote.class, java.lang.reflect.Method.class, java.lang.Object[].class, long.class });");
	    out.println("useNewInvoke = true;");
	  }

	for (int i = 0; i < remotemethods.length; i++)
	  {
	    Method m = remotemethods[i].meth;
	    out.print("$method_" + m.getName() + "_" + i + " = ");
	    out.print(mRemoteInterface.getName() + ".class.getMethod(\""
	              + m.getName() + "\"");
	    out.print(", new java.lang.Class[] {");
	    // Output signature
	    Class[] sig = m.getParameterTypes();
	    for (int j = 0; j < sig.length; j++)
	      {
		out.print(getPrettyName(sig[j]) + ".class");
		if (j + 1 < sig.length)
		  out.print(", ");
	      }
	    out.println("});");
	  }
	ctrl.unindent();
	out.println("}");
	out.print("catch (java.lang.NoSuchMethodException e) {");
	ctrl.indent();
	if (need11Stubs)
	  out.print("useNewInvoke = false;");
	else
	  out.print("throw new java.lang.NoSuchMethodError(\"stub class initialization failed\");");

	ctrl.unindent();
	out.print("}");

	ctrl.unindent();
	out.println("}");
	out.println();
      }

    // Constructors
    if (need11Stubs)
      {
	out.print("public " + stubclassname + "() {");
	ctrl.indent();
	out.print("super();");
	ctrl.unindent();
	out.println("}");
      }

    if (need12Stubs)
      {
	out.print("public " + stubclassname
	          + "(java.rmi.server.RemoteRef ref) {");
	ctrl.indent();
	out.print("super(ref);");
	ctrl.unindent();
	out.println("}");
      }

    // Method implementations
    for (int i = 0; i < remotemethods.length; i++)
      {
	Method m = remotemethods[i].meth;
	Class[] sig = m.getParameterTypes();
	Class returntype = m.getReturnType();
	Class[] except = sortExceptions(m.getExceptionTypes());

	out.println();
	out.print("public " + getPrettyName(returntype) + " " + m.getName()
	          + "(");
	for (int j = 0; j < sig.length; j++)
	  {
	    out.print(getPrettyName(sig[j]));
	    out.print(" $param_" + j);
	    if (j + 1 < sig.length)
	      out.print(", ");
	  }
	out.print(") ");
	out.print("throws ");
	for (int j = 0; j < except.length; j++)
	  {
	    out.print(getPrettyName(except[j]));
	    if (j + 1 < except.length)
	      out.print(", ");
	  }
	out.print(" {");
	ctrl.indent();

	out.print("try {");
	ctrl.indent();

	if (need12Stubs)
	  {
	    if (need11Stubs)
	      {
		out.print("if (useNewInvoke) {");
		ctrl.indent();
	      }
	    if (returntype != Void.TYPE)
	      out.print("java.lang.Object $result = ");
	    out.print("ref.invoke(this, $method_" + m.getName() + "_" + i
	              + ", ");
	    if (sig.length == 0)
	      out.print("null, ");
	    else
	      {
		out.print("new java.lang.Object[] {");
		for (int j = 0; j < sig.length; j++)
		  {
		    if (sig[j] == Boolean.TYPE)
		      out.print("new java.lang.Boolean($param_" + j + ")");
		    else if (sig[j] == Byte.TYPE)
		      out.print("new java.lang.Byte($param_" + j + ")");
		    else if (sig[j] == Character.TYPE)
		      out.print("new java.lang.Character($param_" + j + ")");
		    else if (sig[j] == Short.TYPE)
		      out.print("new java.lang.Short($param_" + j + ")");
		    else if (sig[j] == Integer.TYPE)
		      out.print("new java.lang.Integer($param_" + j + ")");
		    else if (sig[j] == Long.TYPE)
		      out.print("new java.lang.Long($param_" + j + ")");
		    else if (sig[j] == Float.TYPE)
		      out.print("new java.lang.Float($param_" + j + ")");
		    else if (sig[j] == Double.TYPE)
		      out.print("new java.lang.Double($param_" + j + ")");
		    else
		      out.print("$param_" + j);
		    if (j + 1 < sig.length)
		      out.print(", ");
		  }
		out.print("}, ");
	      }
	    out.print(Long.toString(remotemethods[i].hash) + "L");
	    out.print(");");

	    if (returntype != Void.TYPE)
	      {
		out.println();
		out.print("return (");
		if (returntype == Boolean.TYPE)
		  out.print("((java.lang.Boolean)$result).booleanValue()");
		else if (returntype == Byte.TYPE)
		  out.print("((java.lang.Byte)$result).byteValue()");
		else if (returntype == Character.TYPE)
		  out.print("((java.lang.Character)$result).charValue()");
		else if (returntype == Short.TYPE)
		  out.print("((java.lang.Short)$result).shortValue()");
		else if (returntype == Integer.TYPE)
		  out.print("((java.lang.Integer)$result).intValue()");
		else if (returntype == Long.TYPE)
		  out.print("((java.lang.Long)$result).longValue()");
		else if (returntype == Float.TYPE)
		  out.print("((java.lang.Float)$result).floatValue()");
		else if (returntype == Double.TYPE)
		  out.print("((java.lang.Double)$result).doubleValue()");
		else
		  out.print("(" + getPrettyName(returntype) + ")$result");
		out.print(");");
	      }

	    if (need11Stubs)
	      {
		ctrl.unindent();
		out.println("}");
		out.print("else {");
		ctrl.indent();
	      }
	  }

	if (need11Stubs)
	  {
	    out.println("java.rmi.server.RemoteCall call = ref.newCall((java.rmi.server.RemoteObject)this, operations, "
	                + i + ", interfaceHash);");
	    out.print("try {");
	    ctrl.indent();
	    out.print("java.io.ObjectOutput out = call.getOutputStream();");
	    for (int j = 0; j < sig.length; j++)
	      {
		out.println();
		if (sig[j] == Boolean.TYPE)
		  out.print("out.writeBoolean(");
		else if (sig[j] == Byte.TYPE)
		  out.print("out.writeByte(");
		else if (sig[j] == Character.TYPE)
		  out.print("out.writeChar(");
		else if (sig[j] == Short.TYPE)
		  out.print("out.writeShort(");
		else if (sig[j] == Integer.TYPE)
		  out.print("out.writeInt(");
		else if (sig[j] == Long.TYPE)
		  out.print("out.writeLong(");
		else if (sig[j] == Float.TYPE)
		  out.print("out.writeFloat(");
		else if (sig[j] == Double.TYPE)
		  out.print("out.writeDouble(");
		else
		  out.print("out.writeObject(");
		out.print("$param_" + j + ");");
	      }
	    ctrl.unindent();
	    out.println("}");
	    out.print("catch (java.io.IOException e) {");
	    ctrl.indent();
	    out.print("throw new java.rmi.MarshalException(\"error marshalling arguments\", e);");
	    ctrl.unindent();
	    out.println("}");
	    out.println("ref.invoke(call);");
	    if (returntype != Void.TYPE)
	      out.println(getPrettyName(returntype) + " $result;");
	    out.print("try {");
	    ctrl.indent();
	    out.print("java.io.ObjectInput in = call.getInputStream();");
	    boolean needcastcheck = false;
	    if (returntype != Void.TYPE)
	      {
		out.println();
		out.print("$result = ");
		if (returntype == Boolean.TYPE)
		  out.print("in.readBoolean();");
		else if (returntype == Byte.TYPE)
		  out.print("in.readByte();");
		else if (returntype == Character.TYPE)
		  out.print("in.readChar();");
		else if (returntype == Short.TYPE)
		  out.print("in.readShort();");
		else if (returntype == Integer.TYPE)
		  out.print("in.readInt();");
		else if (returntype == Long.TYPE)
		  out.print("in.readLong();");
		else if (returntype == Float.TYPE)
		  out.print("in.readFloat();");
		else if (returntype == Double.TYPE)
		  out.print("in.readDouble();");
		else
		  {
		    if (returntype != Object.class)
		      out.print("(" + getPrettyName(returntype) + ")");
		    else
		      needcastcheck = true;
		    out.print("in.readObject();");
		  }
		out.println();
		out.print("return ($result);");
	      }
	    ctrl.unindent();
	    out.println("}");
	    out.print("catch (java.io.IOException e) {");
	    ctrl.indent();
	    out.print("throw new java.rmi.UnmarshalException(\"error unmarshalling return\", e);");
	    ctrl.unindent();
	    out.println("}");
	    if (needcastcheck)
	      {
		out.print("catch (java.lang.ClassNotFoundException e) {");
		ctrl.indent();
		out.print("throw new java.rmi.UnmarshalException(\"error unmarshalling return\", e);");
		ctrl.unindent();
		out.println("}");
	      }
	    out.print("finally {");
	    ctrl.indent();
	    out.print("ref.done(call);");
	    ctrl.unindent();
	    out.print("}");

	    if (need12Stubs && need11Stubs)
	      {
		ctrl.unindent();
		out.print("}");
	      }
	  }

	ctrl.unindent();
	out.print("}");

	boolean needgeneral = true;
	for (int j = 0; j < except.length; j++)
	  {
	    out.println();
	    out.print("catch (" + getPrettyName(except[j]) + " e) {");
	    ctrl.indent();
	    out.print("throw e;");
	    ctrl.unindent();
	    out.print("}");
	    if (except[j] == Exception.class)
	      needgeneral = false;
	  }
	if (needgeneral)
	  {
	    out.println();
	    out.print("catch (java.lang.Exception e) {");
	    ctrl.indent();
	    out.print("throw new java.rmi.UnexpectedException(\"undeclared checked exception\", e);");
	    ctrl.unindent();
	    out.print("}");
	  }

	ctrl.unindent();
	out.print("}");
	out.println();
      }

    ctrl.unindent();
    out.println("}");

    out.close();
  }

  private void generateSkel() throws IOException
  {
    skelname = fullclassname + "_Skel";
    String skelclassname = classname + "_Skel";
    ctrl =
      new TabbedWriter(new FileWriter((destination == null ? ""
                                                           : destination
                                                           + File.separator)
                                      + skelname.replace('.',
                                                         File.separatorChar)
                                      + ".java"));
    out = new PrintWriter(ctrl);

    if (verbose)
      System.out.println("[Generating class " + skelname + ".java]");

    out.println("// Skel class generated by rmic - DO NOT EDIT!");
    out.println();
    if (fullclassname != classname)
      {
	String pname =
	  fullclassname.substring(0, fullclassname.lastIndexOf('.'));
	out.println("package " + pname + ";");
	out.println();
      }

    out.print("public final class " + skelclassname);
    ctrl.indent();

    // Output interfaces we implement
    out.print("implements java.rmi.server.Skeleton");

    ctrl.unindent();
    out.print("{");
    ctrl.indent();

    // Interface hash - don't know how to calculate this - XXX
    out.println("private static final long interfaceHash = "
                + RMIHashes.getInterfaceHash(clazz) + "L;");
    out.println();

    // Operation table
    out.print("private static final java.rmi.server.Operation[] operations = {");

    ctrl.indent();
    for (int i = 0; i < remotemethods.length; i++)
      {
	Method m = remotemethods[i].meth;
	out.print("new java.rmi.server.Operation(\"");
	out.print(getPrettyName(m.getReturnType()) + " ");
	out.print(m.getName() + "(");
	// Output signature
	Class[] sig = m.getParameterTypes();
	for (int j = 0; j < sig.length; j++)
	  {
	    out.print(getPrettyName(sig[j]));
	    if (j + 1 < sig.length)
	      out.print(", ");
	  }
	out.print("\")");
	if (i + 1 < remotemethods.length)
	  out.println(",");
      }
    ctrl.unindent();
    out.println("};");

    out.println();

    // getOperations method
    out.print("public java.rmi.server.Operation[] getOperations() {");
    ctrl.indent();
    out.print("return ((java.rmi.server.Operation[]) operations.clone());");
    ctrl.unindent();
    out.println("}");

    out.println();

    // Dispatch method
    out.print("public void dispatch(java.rmi.Remote obj, java.rmi.server.RemoteCall call, int opnum, long hash) throws java.lang.Exception {");
    ctrl.indent();

    out.print("if (opnum < 0) {");
    ctrl.indent();

    for (int i = 0; i < remotemethods.length; i++)
      {
	out.print("if (hash == " + Long.toString(remotemethods[i].hash)
	          + "L) {");
	ctrl.indent();
	out.print("opnum = " + i + ";");
	ctrl.unindent();
	out.println("}");
	out.print("else ");
      }
    out.print("{");
    ctrl.indent();
    out.print("throw new java.rmi.server.SkeletonMismatchException(\"interface hash mismatch\");");
    ctrl.unindent();
    out.print("}");

    ctrl.unindent();
    out.println("}");
    out.print("else if (hash != interfaceHash) {");
    ctrl.indent();
    out.print("throw new java.rmi.server.SkeletonMismatchException(\"interface hash mismatch\");");
    ctrl.unindent();
    out.println("}");

    out.println();

    out.println(fullclassname + " server = (" + fullclassname + ")obj;");
    out.println("switch (opnum) {");

    // Method dispatch
    for (int i = 0; i < remotemethods.length; i++)
      {
	Method m = remotemethods[i].meth;
	out.println("case " + i + ":");
	out.print("{");
	ctrl.indent();

	Class[] sig = m.getParameterTypes();
	for (int j = 0; j < sig.length; j++)
	  {
	    out.print(getPrettyName(sig[j]));
	    out.println(" $param_" + j + ";");
	  }

	out.print("try {");
	boolean needcastcheck = false;
	ctrl.indent();
	out.println("java.io.ObjectInput in = call.getInputStream();");
	for (int j = 0; j < sig.length; j++)
	  {
	    out.print("$param_" + j + " = ");
	    if (sig[j] == Boolean.TYPE)
	      out.print("in.readBoolean();");
	    else if (sig[j] == Byte.TYPE)
	      out.print("in.readByte();");
	    else if (sig[j] == Character.TYPE)
	      out.print("in.readChar();");
	    else if (sig[j] == Short.TYPE)
	      out.print("in.readShort();");
	    else if (sig[j] == Integer.TYPE)
	      out.print("in.readInt();");
	    else if (sig[j] == Long.TYPE)
	      out.print("in.readLong();");
	    else if (sig[j] == Float.TYPE)
	      out.print("in.readFloat();");
	    else if (sig[j] == Double.TYPE)
	      out.print("in.readDouble();");
	    else
	      {
		if (sig[j] != Object.class)
		  {
		    out.print("(" + getPrettyName(sig[j]) + ")");
		    needcastcheck = true;
		  }
		out.print("in.readObject();");
	      }
	    out.println();
	  }
	ctrl.unindent();
	out.println("}");
	out.print("catch (java.io.IOException e) {");
	ctrl.indent();
	out.print("throw new java.rmi.UnmarshalException(\"error unmarshalling arguments\", e);");
	ctrl.unindent();
	out.println("}");
	if (needcastcheck)
	  {
	    out.print("catch (java.lang.ClassCastException e) {");
	    ctrl.indent();
	    out.print("throw new java.rmi.UnmarshalException(\"error unmarshalling arguments\", e);");
	    ctrl.unindent();
	    out.println("}");
	  }
	out.print("finally {");
	ctrl.indent();
	out.print("call.releaseInputStream();");
	ctrl.unindent();
	out.println("}");

	Class returntype = m.getReturnType();
	if (returntype != Void.TYPE)
	  out.print(getPrettyName(returntype) + " $result = ");
	out.print("server." + m.getName() + "(");
	for (int j = 0; j < sig.length; j++)
	  {
	    out.print("$param_" + j);
	    if (j + 1 < sig.length)
	      out.print(", ");
	  }
	out.println(");");

	out.print("try {");
	ctrl.indent();
	out.print("java.io.ObjectOutput out = call.getResultStream(true);");
	if (returntype != Void.TYPE)
	  {
	    out.println();
	    if (returntype == Boolean.TYPE)
	      out.print("out.writeBoolean($result);");
	    else if (returntype == Byte.TYPE)
	      out.print("out.writeByte($result);");
	    else if (returntype == Character.TYPE)
	      out.print("out.writeChar($result);");
	    else if (returntype == Short.TYPE)
	      out.print("out.writeShort($result);");
	    else if (returntype == Integer.TYPE)
	      out.print("out.writeInt($result);");
	    else if (returntype == Long.TYPE)
	      out.print("out.writeLong($result);");
	    else if (returntype == Float.TYPE)
	      out.print("out.writeFloat($result);");
	    else if (returntype == Double.TYPE)
	      out.print("out.writeDouble($result);");
	    else
	      out.print("out.writeObject($result);");
	  }
	ctrl.unindent();
	out.println("}");
	out.print("catch (java.io.IOException e) {");
	ctrl.indent();
	out.print("throw new java.rmi.MarshalException(\"error marshalling return\", e);");
	ctrl.unindent();
	out.println("}");
	out.print("break;");

	ctrl.unindent();
	out.println("}");
	out.println();
      }

    out.print("default:");
    ctrl.indent();
    out.print("throw new java.rmi.UnmarshalException(\"invalid method number\");");
    ctrl.unindent();
    out.print("}");

    ctrl.unindent();
    out.print("}");

    ctrl.unindent();
    out.println("}");

    out.close();
  }

  private void compile(String name) throws Exception
  {
    Compiler comp = Compiler.getInstance();
    if (verbose)
      System.out.println("[Compiling class " + name + "]");
    comp.setDestination(destination);
    comp.compile(name);
  }

  private static String getPrettyName(Class cls)
  {
    StringBuffer str = new StringBuffer();
    for (int count = 0;; count++)
      {
	if (! cls.isArray())
	  {
	    str.append(cls.getName());
	    for (; count > 0; count--)
	      str.append("[]");
	    return (str.toString());
	  }
	cls = cls.getComponentType();
      }
  }

/**
 * Sort exceptions so the most general go last.
 */
  private Class[] sortExceptions(Class[] except)
  {
    for (int i = 0; i < except.length; i++)
      {
	for (int j = i + 1; j < except.length; j++)
	  {
	    if (except[i].isAssignableFrom(except[j]))
	      {
		Class tmp = except[i];
		except[i] = except[j];
		except[j] = tmp;
	      }
	  }
      }
    return (except);
  }

/**
 * Process the options until we find the first argument.
 */
  private void parseOptions()
  {
    for (;;)
      {
	if (next >= args.length || args[next].charAt(0) != '-')
	  break;
	String arg = args[next];
	next++;

	// Accept `--' options if they look long enough.
	if (arg.length() > 3 && arg.charAt(0) == '-' && arg.charAt(1) == '-')
	  arg = arg.substring(1);

	if (arg.equals("-keep"))
	  keep = true;
	else if (arg.equals("-keepgenerated"))
	  keep = true;
	else if (arg.equals("-v1.1"))
	  {
	    need11Stubs = true;
	    need12Stubs = false;
	  }
	else if (arg.equals("-vcompat"))
	  {
	    need11Stubs = true;
	    need12Stubs = true;
	  }
	else if (arg.equals("-v1.2"))
	  {
	    need11Stubs = false;
	    need12Stubs = true;
	  }
	else if (arg.equals("-g"))
	  {
	  }
	else if (arg.equals("-depend"))
	  {
	  }
	else if (arg.equals("-nowarn"))
	  {
	  }
	else if (arg.equals("-verbose"))
	  verbose = true;
	else if (arg.equals("-nocompile"))
	  compile = false;
	else if (arg.equals("-classpath"))
	  next++;
	else if (arg.equals("-help"))
	  usage();
	else if (arg.equals("-version"))
	  {
	    System.out.println("rmic (" + System.getProperty("java.vm.name")
	                       + ") " + System.getProperty("java.vm.version"));
	    System.out.println();
	    System.out.println("Copyright 2005 Free Software Foundation, Inc.");
	    System.out.println("This is free software; see the source for copying conditions.  There is NO");
	    System.out.println("warranty; not even for MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.");
	    System.exit(0);
	  }
	else if (arg.equals("-d"))
	  {
	    destination = args[next];
	    next++;
	  }
	else if (arg.charAt(1) == 'J')
	  {
	  }
	else
	  error("unrecognized option `" + arg + "'");
      }
  }

/**
 * Looks for the java.rmi.Remote interface that that is implemented by theClazz.
 * @param theClazz the class to look in
 * @return the Remote interface of theClazz or null if theClazz does not implement a Remote interface
 */
  private Class getRemoteInterface(Class theClazz)
  {
    Class[] interfaces = theClazz.getInterfaces();
    for (int i = 0; i < interfaces.length; i++)
      {
	if (java.rmi.Remote.class.isAssignableFrom(interfaces[i]))
	  return interfaces[i];
      }
    logError("Class " + theClazz.getName()
             + " is not a remote object. It does not implement an interface that is a java.rmi.Remote-interface.");
    return null;
  }

/**
 * Prints an error to System.err and increases the error count.
 * @param theError
 */
  private void logError(String theError)
  {
    errorCount++;
    System.err.println("error:" + theError);
  }

  private static void error(String message)
  {
    System.err.println("rmic: " + message);
    System.err.println("Try `rmic --help' for more information.");
    System.exit(1);
  }

  private static void usage()
  {
    System.out.println("Usage: rmic [OPTION]... CLASS...\n" + "\n"
                       + "	-keep 			Don't delete any intermediate files\n"
                       + "	-keepgenerated 		Same as -keep\n"
                       + "	-v1.1			Java 1.1 style stubs only\n"
                       + "	-vcompat		Java 1.1 & Java 1.2 stubs\n"
                       + "	-v1.2			Java 1.2 style stubs only\n"
                       + "	-g *			Generated debugging information\n"
                       + "	-depend *		Recompile out-of-date files\n"
                       + "	-nowarn	*		Suppress warning messages\n"
                       + "	-nocompile		Don't compile the generated files\n"
                       + "	-verbose 		Output what's going on\n"
                       + "	-classpath <path> *	Use given path as classpath\n"
                       + "	-d <directory> 		Specify where to place generated classes\n"
                       + "	-J<flag> *		Pass flag to Java\n"
                       + "	-help			Print this help, then exit\n"
                       + "	-version		Print version number, then exit\n" + "\n"
                       + "  * Option currently ignored\n"
                       + "Long options can be used with `--option' form as well.");
    System.exit(0);
  }

  static class MethodRef
    implements Comparable
  {
    Method meth;
    String sig;
    long hash;

    MethodRef(Method m)
    {
      meth = m;
      // We match on the name - but what about overloading? - XXX
      sig = m.getName();
      hash = RMIHashes.getMethodHash(m);
    }

    public int compareTo(Object obj)
    {
      MethodRef that = (MethodRef) obj;
      return (this.sig.compareTo(that.sig));
    }
  }
}
