/* NameFinder.java -- Translates addresses to StackTraceElements.
   Copyright (C) 2002 Free Software Foundation, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.runtime;

import gnu.gcj.RawData;

import java.lang.StringBuffer;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.IOException;
import java.io.File;

/**
 * Helper class that translates addresses (represented as longs) to a
 * StackTraceElement array.
 *
 * There are a couple of system properties that can be set to manipulate the
 * result (all default to true):
 * <li>
 * <ul><code>gnu.gcj.runtime.NameFinder.demangle</code>
 *     Whether names should be demangled.</ul>
 * <ul><code>gnu.gcj.runtime.NameFinder.sanitize</code></ul>
 *     Whether calls to initialize exceptions and starting the runtime system
 *     should be removed from the stack trace. Only done when names are
 *     demangled.</ul>
 * <ul><code>gnu.gcj.runtime.NameFinder.remove_unknown</code>
 *     Whether calls to unknown functions (class and method names are unknown)
 *     should be removed from the stack trace. Only done when the stack is
 *     sanitized.</ul>
 * <ul><code>gnu.gcj.runtime.NameFinder.remove_interpreter</code>
 *     Whether runtime interpreter calls (methods in the _Jv_InterpMethod class
 *     and functions starting with 'ffi_') should be removed from the stack
 *     trace. Only done when the stack is sanitized.</ul>
 * <ul><code>gnu.gcj.runtime.NameFinder.use_addr2line</code>
 *     Whether an external process (addr2line or addr2name.awk) should be used
 *     as fallback to convert the addresses to function names when the runtime
 *     is unable to do it through <code>dladdr</code>.</ul>
 * </li>
 *
 * <code>close()</code> should be called to get rid of all resources.
 *
 * This class is used from <code>java.lang.VMThrowable</code>.
 *
 * Currently the <code>lookup(long[])</code> method is not thread safe.
 * It can easily be made thread safe by synchronizing access to all external
 * processes when used.
 *   
 * @author Mark Wielaard (mark@klomp.org)
 */
public class NameFinder
{
  // Set these to false when not needed.
  private static final boolean demangle
	  = Boolean.valueOf(System.getProperty
		("gnu.gcj.runtime.NameFinder.demangle", "true")
	    ).booleanValue();
  private static final boolean sanitize
	  = Boolean.valueOf(System.getProperty
		("gnu.gcj.runtime.NameFinder.sanitize", "true")
	    ).booleanValue();
  private static final boolean remove_unknown
	  = Boolean.valueOf(System.getProperty
		("gnu.gcj.runtime.NameFinder.remove_unknown", "true")
	    ).booleanValue();
  private static final boolean remove_interpreter
	  = Boolean.valueOf(System.getProperty
		("gnu.gcj.runtime.NameFinder.remove_interpreter", "true")
	    ).booleanValue();
  private static final boolean use_addr2line
	  = Boolean.valueOf(System.getProperty
		("gnu.gcj.runtime.NameFinder.use_addr2line", "true")
	    ).booleanValue();

  /**
   * The name of the currently running executable.
   */
  private final String executable;

  /**
   * Process used for demangling names.
   */
  private Process cppfilt;

  private BufferedWriter cppfiltOut;
  private BufferedReader cppfiltIn;

  /**
   * Process used for translating addresses to function/file names.
   */
  private Process addr2line;

  private BufferedWriter addr2lineOut;
  private BufferedReader addr2lineIn;

  /**
   * Flag set if using addr2name.awk instead of addr2line from binutils.
   */
  private boolean usingAddr2name = false;

  /**
   * Creates a new NameFinder. Call close to get rid of any resources
   * created while using the <code>lookup</code> methods.
   */
  public NameFinder()
  {
    executable = getExecutable();
    Runtime runtime = Runtime.getRuntime();
    if (demangle)
    {
      try
	{
	  String[] exec = new String[] {"c++filt", "-s", "java"};
	  cppfilt = runtime.exec(exec);
	  cppfiltIn = new BufferedReader
			(new InputStreamReader(cppfilt.getInputStream()));
	  cppfiltOut = new BufferedWriter
			(new OutputStreamWriter(cppfilt.getOutputStream()));
	}
      catch (IOException ioe)
        {
	  if (cppfilt != null)
	    cppfilt.destroy();
	  cppfilt = null;
	}
    }

    if (use_addr2line)
      {
	try
	  {
	    String[] exec = new String[] {"addr2line", "-f", "-e", executable};
	    addr2line = runtime.exec(exec);
	  }
	catch (IOException ioe)
	  {
	    try
	      {
		String[] exec = new String[] {"addr2name.awk", executable};
		addr2line = runtime.exec(exec);
		usingAddr2name = true;
	      }
	    catch (IOException ioe2) { addr2line = null; }
	  }

	if (addr2line != null)
	  {
	    try
	      {
		addr2lineIn = new BufferedReader
			(new InputStreamReader(addr2line.getInputStream()));
		addr2lineOut = new BufferedWriter
			(new OutputStreamWriter(addr2line.getOutputStream()));
	      }
	    catch (IOException ioe)
	      {  
		addr2line.destroy();
		addr2line = null;
	      }
	  }
      }
  }

  /**
   * Returns the name of the currently running process.
   */
  native private static String getExecutable();

  /**
   * Tries to use dladdr to create the nth StackTraceElement from the given
   * addresses. Returns null on failure.
   */
  native private StackTraceElement dladdrLookup(RawData addrs, int n);

  /**
   * Returns the nth element from the stack as a hex encoded String.
   */
  native private String getAddrAsString(RawData addrs, int n);

  /**
   * Returns the label that is exported for the given method name.
   */
  native private String getExternalLabel(String name);

  /**
   * If nth element of stack is an interpreted frame, return the
   * element representing the method being interpreted.
   */
  native private StackTraceElement lookupInterp(RawData addrs, int n);

  /**
   * Creates the nth StackTraceElement from the given native stacktrace.
   */
  private StackTraceElement lookup(RawData addrs, int n)
  {
    StackTraceElement result;

    result = lookupInterp(addrs, n);
    if (result == null)
      result = dladdrLookup(addrs, n);
    if (result == null)
      {
	String name = null;
	String file = null;

	String hex = getAddrAsString(addrs, n);
	
	if (addr2line != null)
	  {
	    try
	      {
		addr2lineOut.write(hex);
		addr2lineOut.newLine();
		addr2lineOut.flush();
		name = addr2lineIn.readLine();
		file = addr2lineIn.readLine();

                // addr2line uses symbolic debugging information instead
                // of the actually exported labels as addr2name.awk does.
                // This name might need some modification, depending on 
                // the system, to make it a label like that returned 
                // by addr2name.awk or dladdr.
                if (! usingAddr2name)
                  if (name != null && ! "??".equals (name))
                    name = getExternalLabel (name);
	      }
	    catch (IOException ioe) { addr2line = null; }
	  }

	if (name == null || "??".equals(name))
	  name = hex;

	result = createStackTraceElement(name, file);
      }

    return result;
  }

  /**
   * Given an Throwable and a native stacktrace returns an array of
   * StackTraceElement containing class, method, file and linenumbers.
   */
  public StackTraceElement[] lookup(Throwable t, RawData addrs, int length)
  {
    StackTraceElement[] elements = new StackTraceElement[length];
    for (int i=0; i < length; i++)
      elements[i] = lookup(addrs, i);

    if (demangle && sanitize)
      return sanitizeStack(elements, t);
    else
      return elements;
  }

  
  /**
   * Removes calls to initialize exceptions and the runtime system from
   * the stack trace including stack frames of which nothing usefull is known.
   * Throw away the top of the stack till we find the constructor(s)
   * of this Throwable or at least the contructors of java.lang.Throwable
   * or the actual fillInStackTrace call.
   * Also throw away from the top everything before and including a runtime
   * _Jv_Throw call.
   */
  private static StackTraceElement[] sanitizeStack(StackTraceElement[] elements,
						   Throwable t)
  {
    StackTraceElement[] stack;

    String className = t.getClass().getName();
    String consName;
    int lastDot = className.lastIndexOf('.');
    if (lastDot == -1)
      consName = className + '(';
    else
      consName = className.substring(lastDot + 1) + '(';

    int unknown = 0;
    int interpreter = 0;
    int last_throw = -1;
    int length = elements.length;
    int end = length-1;
    for (int i = 0; i < length; i++)
      {
	String CName = elements[i].getClassName();
	String MName = elements[i].getMethodName();
	if ((CName == null && MName != null && MName.startsWith("_Jv_Throw"))
	  ||
	   (CName != null
	    && (CName.equals(className)
		|| CName.equals("java.lang.Throwable")
		|| CName.equals("java.lang.VMThrowable"))
	    && MName != null
	    && (MName.startsWith(consName)
		|| MName.startsWith("Throwable(")
		|| MName.startsWith("fillInStackTrace("))))
	  {
	    last_throw = i;
	    // Reset counting of unknown and interpreter frames.
	    unknown = 0;
	    interpreter = 0;
	  }
	else if (remove_unknown && CName == null 
		 && (MName == null || MName.startsWith("0x")))
	  unknown++;
	else if (remove_interpreter
		 && ((CName == null
		      && MName != null && MName.startsWith("ffi_"))
		     || (CName != null && CName.equals("_Jv_InterpMethod"))))
	  interpreter++;
	else if ("main(java.lang.String[])".equals(MName))
	  {
	    end = i;
	    break;
	  }
      }
    int begin = last_throw+1;

    // Now filter out everything at the start and the end that is not part
    // of the "normal" user program including any elements that are interpreter
    // calls or have no usefull information whatsoever.
    // Unless that means we filter out all info.
    int nr_elements = end-begin-unknown-interpreter+1;
    if ((begin > 0 || end < length-1 || unknown > 0 || interpreter > 0)
	&& nr_elements > 0)
      {
	stack = new StackTraceElement[nr_elements];
	int pos =0;
	for (int i=begin; i<=end; i++)
	  {
	    String MName = elements[i].getMethodName();
	    String CName = elements[i].getClassName();
	    if (remove_unknown && CName == null 
		 && (MName == null || MName.startsWith("0x")))
	      ; // Skip unknown frame
	    else if (remove_interpreter
		     && ((CName == null
			 && MName != null && MName.startsWith("ffi_"))
			|| (CName != null && CName.equals("_Jv_InterpMethod"))))
	      ; // Skip interpreter runtime frame
	    else
	      {
		stack[pos] = elements[i];
		pos++;
	      }
	  }
      }
    else
      stack = elements;

    return stack;
  }

  /**
   * Creates a StackTraceElement given a string and a filename.
   * Splits the given string into the class and method part.
   * The string name will be a demangled to a fully qualified java method
   * string. The string file will be decomposed into a file name and possibly
   * a line number. The name should never be null, but the file may be if it
   * is unknown.
   */
  private StackTraceElement createStackTraceElement(String name, String file)
  {
    if (!demangle)
      return new StackTraceElement(file, -1, null, name, false);

    String s = demangleName(name);
    String methodName = s;
    String className = null;
    int bracket = s.indexOf('(');
    if (bracket > 0)
      {
	int dot = s.lastIndexOf('.', bracket);
	if (dot > 0)
	  {
	    className = s.substring(0, dot);
	    methodName = s.substring(dot+1, s.length());
	  }
      }

    String fileName = file;
    int line = -1;
    if (fileName != null)
      {
	int colon = file.lastIndexOf(':');
	if (colon > 0)
	  {
	    fileName = file.substring(0, colon);
	    try
	      {
		line = Integer.parseInt(file.substring(colon+1, file.length()));
	      }
	    catch (NumberFormatException nfe) { /* ignore */ }
	  }

	if (line == 0)
	  line =-1;

	if ("".equals(fileName) || "??".equals(fileName))
	  fileName = null;
	else if (fileName != null)
	  {
	    try
	      {
		fileName = new File(fileName).getCanonicalPath();
	      }
	    catch (IOException ioe) { /* ignore */ }
	  }
      }

    return new StackTraceElement(fileName, line, className, methodName, false);
  }

  /**
   * Demangles the given String if possible. Returns the demangled String or
   * the original string if demangling is impossible.
   */
  private String demangleName(String s)
  {
    if (cppfilt != null)
    {
      try
	{
	  cppfiltOut.write(s);
	  cppfiltOut.newLine();
	  cppfiltOut.flush();
	  return cppfiltIn.readLine();
	}
      catch (IOException ioe) { cppfilt.destroy(); cppfilt = null; }
    }

    return s;
  }

  /**
   * Returns human readable method name and aguments given a method type
   * signature as known to the interpreter and a classname.
   */
  public static String demangleInterpreterMethod(String m, String cn)
  {
    int index = 0;
    int length = m.length();
    StringBuffer sb = new StringBuffer(length);

    // Figure out the real method name
    if (m.startsWith("<init>"))
      {
	String className;
	int i = cn.lastIndexOf('.');
	if (i < 0)
	  className = cn;
	else
	  className = cn.substring(i + 1);
	sb.append(className);
	index += 7;
      }
    else
      {
	int i = m.indexOf('(');
	if (i > 0)
	  {
	    sb.append(m.substring(0,i));
	    index += i + 1;
	  }
      }

    sb.append('(');

    // Demangle the type arguments
    int arrayDepth = 0;
    char c = (index < length) ? m.charAt(index) : ')';
    while (c != ')')
      {
	String type;
	switch(c)
	{
          case 'B':
            type = "byte";
	    break;
          case 'C':
            type = "char";
	    break;
          case 'D':
            type = "double";
	    break;
          case 'F':
            type = "float";
	    break;
          case 'I':
            type = "int";
	    break;
          case 'J':
            type = "long";
	    break;
          case 'S':
            type = "short";
	    break;
          case 'Z':
            type = "boolean";
	    break;
          case 'L':
	    int i = m.indexOf(';', index);
	    if (i > 0)
	      {
		type = m.substring(index+1, i);
		index = i;
	      }
	    else
	      type = "<unknown ref>";
	    break;
          case '[':
	    type = "";
	    arrayDepth++;
	    break;
          default:
	    type = "<unknown " + c + '>';
	}
	sb.append(type);

	// Handle arrays
	if (c != '[' && arrayDepth > 0)
	  while (arrayDepth > 0)
	    {
	      sb.append("[]");
	      arrayDepth--;
	    }

	index++;
	char nc = (index < length) ? m.charAt(index) : ')';
	if (c != '[' && nc  != ')')
	  sb.append(", ");
	c = nc;
      }

    // Stop. We are not interested in the return type.
    sb.append(')');
    return sb.toString();
  }

  /**
   * Releases all resources used by this NameFinder.
   */
  public void close()
  {
    if (cppfilt != null)
      cppfilt.destroy();

    if (addr2line != null)
      addr2line.destroy();
  }

  /**
   * Calls close to get rid of all resources.
   */
  protected void finalize()
  {
    close();
  }
}
