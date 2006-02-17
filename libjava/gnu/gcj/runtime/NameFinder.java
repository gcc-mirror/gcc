/* NameFinder.java -- Translates addresses to StackTraceElements.
   Copyright (C) 2002, 2004 Free Software Foundation, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.runtime;

import gnu.classpath.Configuration;
import gnu.gcj.RawData;

import java.lang.StringBuffer;

import java.io.BufferedReader;
import java.io.BufferedWriter;
import java.io.InputStreamReader;
import java.io.OutputStreamWriter;
import java.io.IOException;
import java.io.File;
import java.util.Iterator;
import java.util.HashMap;


/**
 * Lookup addresses (represented as longs) to find source & line number info.
 *
 * The following system property is available (defaults to true):
 * <li>
 * <ul><code>gnu.gcj.runtime.NameFinder.use_addr2line</code>
 *     Whether an external process, addr2line, should be used to look up
 *     source file and line number info. Throwable.printStackTrace() will
 *     be faster if this property is set to 'false'.
 * </ul>
 * <ul><code>gnu.gcj.runtime.NameFinder.remove_unknown</code>
 *     Whether calls to unknown functions (class and method names are unknown)
 *     should be removed from the stack trace. </ul>
 * </li>
 *
 * <code>close()</code> should be called to get rid of all resources.
 *
 * This class is used from <code>java.lang.VMThrowable</code>.
 *
 * @author Mark Wielaard (mark@klomp.org)
 */
public class NameFinder
{
  /**
   * The name of the binary to look up.
   */
  private String binaryFile;
  private String sourceFile;
  private int lineNum;
  private HashMap procs = new HashMap();

  private static final boolean use_addr2line
          = Boolean.valueOf(System.getProperty
                ("gnu.gcj.runtime.NameFinder.use_addr2line", "true")
            ).booleanValue();

  private static final boolean remove_unknown
	  = Boolean.valueOf(System.getProperty
		("gnu.gcj.runtime.NameFinder.remove_unknown", "true")
	    ).booleanValue();

  // Return true if non-Java frames should be removed from stack
  // traces.
  static final boolean removeUnknown()
  {
    return remove_unknown;
  }

  class Addr2Line
  {
    Process proc;
    BufferedWriter out;
    BufferedReader in;

    Addr2Line(String binaryFile)
    {
      try
      {
	String[] exec = new String[] {"addr2line", "-e", binaryFile};
	Runtime runtime = Runtime.getRuntime();
	proc = runtime.exec(exec);
      }
      catch (IOException ioe)
      {
      }

      if (proc != null)
      {
	in = new BufferedReader(new InputStreamReader(proc.getInputStream()));
	out = new BufferedWriter(new OutputStreamWriter(proc.getOutputStream()));
      }
    }
    
    void close()
    {
      try
      {
	if (in != null)
	  in.close();
	if (out != null)
	  out.close();
      }
      catch (IOException x) {}
      if (proc != null)
	proc.destroy();
    }
  }

  /**
   * Create a new NameFinder to lookup names in binaryFile. Call close to get rid of any 
   * resources created while using the <code>lookup</code> methods.
   */
  public NameFinder()
  {
  }

  /**
   * Returns the source file name if lookup() was successful. If the source file could not be 
   * determined, the binary name will be returned instead.
   */
  public String getSourceFile()
  {
    String file;
    if (sourceFile != null)
      file = sourceFile;
    else
      file = binaryFile;
    
    return file.substring(file.lastIndexOf(File.separator) + 1, file.length());
  }

  /**
   * If lookup() was successful, returns the line number of addr. If the line number could not
   * be determined, -1 is returned.
   */  
  public int getLineNum()
  {
    return lineNum;
  }
  
  public void lookup (String file, long addr)
  {
    binaryFile = file;
    sourceFile = null;
    lineNum = -1;
    
    if (! use_addr2line)
      return;
    Addr2Line addr2line = (Addr2Line) procs.get(file);
    if (addr2line == null)
      {
      addr2line = new Addr2Line(file);
      procs.put(file, addr2line);
      }
    
    if (addr2line.proc == null)      
      return;
    
    String hexAddr = "0x" + Long.toHexString(addr);
    String name;

    try
      {
      addr2line.out.write(hexAddr);
      addr2line.out.newLine();
      addr2line.out.flush();
      String result = addr2line.in.readLine();

      if (result.indexOf("??") == -1)
	{
	  int split = result.lastIndexOf(':');
	  sourceFile = result.substring(0, split);
	  String lineNumStr = result.substring(split + 1, result.length());
	  lineNum = Integer.parseInt (lineNumStr);
	}
      }
    catch (IOException ioe)
      {
      addr2line = null;
      }
    catch (NumberFormatException x)
      {
      }
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
    Iterator itr = procs.values().iterator();
    while (itr.hasNext())
      {
        Addr2Line proc = (Addr2Line) itr.next();
        proc.close();
      }
  }
}
