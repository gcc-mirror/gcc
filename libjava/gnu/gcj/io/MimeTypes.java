/* Copyright (C) 2000  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.io;

import java.util.*;
import java.io.*;

public class MimeTypes
{
  private static Hashtable mime_types;
  
  public static void fillFromFile (Hashtable table, String fname) 
    throws IOException
  {
    LineNumberReader reader = 
      new LineNumberReader (new FileReader (fname));
    
    while (reader.ready ())
      {
	StringTokenizer tokenizer = 
	  new StringTokenizer (reader.readLine ());
	
	try 
	  {
	    String t = tokenizer.nextToken ();
	    
	    if (! t.startsWith ("#"))
	      {
		while (true)
		  {
		    // Read the next extension
		    String e = tokenizer.nextToken ();
		    if ((e != null) && (! e.startsWith ("#")))
		      table.put (e, t);
		    else
		      break;
		  }
	      }
	  } catch (NoSuchElementException ex) {
	    // Do nothing.
	  }
      }
  }

  // This is the primary interface to this class.
  public static String getMimeTypeFromExtension (String extension)
  {
    if (mime_types == null)
      {
	mime_types = new Hashtable ();
	
	// First populate the hash table with the default mime type
	// mappings.
	int i = DefaultMimeTypes.types.length;
	while (i > 1)
	  {
	    mime_types.put (DefaultMimeTypes.types[i - 2],
			    DefaultMimeTypes.types[i - 1]);
	    i = i - 2;
	  }
	
	// Now read mime types from /etc/mime.types if it exists.
	// This should override the default values.
	try {
	  fillFromFile (mime_types, "/etc/mime.types");
	} catch (IOException ex) {
	  // Do nothing.
	}
	
	// Now read mime types from ~/.mime.types.  
	// FIXME: We can't currently parse this file.
      }

    String type = (String) mime_types.get (extension);
    if (type == null)
      return ("application/octet-stream");
    else
      return (type);
  }
}
