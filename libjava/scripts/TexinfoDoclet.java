/* Copyright (C) 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

import java.io.*;
import com.sun.javadoc.*;

public class TexinfoDoclet
{
  static PrintStream outfile;
  
  public static int optionLength(String option) 
  {
    if (option.equals("-outfile")) 
      return 2;
    return 0;
  }

  private static String replace (String s, String text, String replacement)
  {
    int i = s.indexOf (text);
    while (i != -1)
      {
	s = s.substring(0, i) + replacement + s.substring(i+text.length());
	i = s.indexOf (text);
      }
    
    return s;
  }
  
  private static String texify (String s)
  {
    if (s.indexOf('<') == -1)
      return s;
    
    s = replace (s, "<code>", "@code{");
    s = replace (s, "</code>", "}");
    s = replace (s, "<ol>", "\n@itemize @bullet\n");
    s = replace (s, "</ol>", "\n@end itemize\n");
    s = replace (s, "<ul>", "\n@itemize @bullet\n");
    s = replace (s, "</ul>", "\n@end itemize\n");
    s = replace (s, "<li>", "\n@item\n");
    s = replace (s, "</li>", "\n");
    s = replace (s, "<p>", "\n\n");
    
    s = replace (s, "<CODE>", "@code{");
    s = replace (s, "</CODE>", "}");
    s = replace (s, "<OL>", "\n@itemize @bullet\n");
    s = replace (s, "</OL>", "\n@end itemize\n");
    s = replace (s, "<UL>", "\n@itemize @bullet\n");
    s = replace (s, "</UL>", "\n@end itemize\n");
    s = replace (s, "<LI>", "\n@item\n");
    s = replace (s, "</LI>", "\n");
    s = replace (s, "<P>", "\n\n");
    
    return s;
  }
  
  private static void emitMethod (ClassDoc c, MethodDoc m)
  {
    outfile.print ("@deftypemethod " + c.typeName()
		   + " {" + m.modifiers()
		   + " " + m.returnType().typeName()
		   + "} " + m.name());
    
    outfile.print (" (");
    Parameter p[] = m.parameters();
    boolean first = true;
    
    for (int i = 0; i < p.length; i++)
      {
	if (!first)
	  outfile.print (", ");
	outfile.print (p[i].typeName() 
		       + "@w{ }@var{"
		       + p[i].name()
		       + "}");
	first = false;
      }
    outfile.print (") ");
    
    ClassDoc exceptions[] = m.thrownExceptions();
    if (exceptions.length > 0)
      {
	outfile.print ("@*throws ");
	first = true;
	for (int i = 0; i < exceptions.length; i++)
	  {
	    if (!first)
	      outfile.print (", ");
	    outfile.print (exceptions[i].typeName());
	    first = false;
	  }
      }
    outfile.println ("");
    
    outfile.println (texify (m.commentText()));
    
    outfile.println ("@end deftypemethod");
  }
  
  private static void emitClass (ClassDoc c)
  {
    MethodDoc[] methods = c.methods();
    for (int i = 0; i < methods.length; i++)
      {
	emitMethod (c, methods[i]);
      }
  }
  
  public static boolean start (RootDoc root)
  {
    String options[][] = root.options ();
    
    for (int i = 0; i < options.length; i++)
      {
	try 
	  {
	    if (options[i][0].equals ("-outfile"))
	      {
		outfile = new PrintStream (new FileOutputStream (options[i][1]));
	      }
	  } catch (java.io.IOException e) {
	    System.err.println ("Can't write to file " + options[i][1]);
	    return false;
	  }
      }
    
    ClassDoc[] classes = root.classes();
    for (int i = 0; i < classes.length; i++)
      {
	emitClass (classes[i]);
      }
    return true;
  }
}
