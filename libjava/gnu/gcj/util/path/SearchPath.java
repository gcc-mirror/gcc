// SearchPath.java -- generic search path utility

/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

/* Author: Kresten Krab Thorup <krab@gnu.org>  */


package gnu.gcj.util.path;

import java.util.*;
import java.util.zip.*;
import java.io.*;
import java.net.*;

final public class SearchPath {

    final static String path_seperator
      = System.getProperty ("path.separator");
    final static char   path_seperator_char
      = path_seperator.charAt (0);
    final static String file_seperator
      = System.getProperty ("file.separator");
    final static char   file_seperator_char
      = file_seperator.charAt (0);

    private Vector path;

    /**
     *  Constructs a SearchPath object, given a system path.
     *  The system path is expected to be seperated by the string 
     *  defined by the <code>path.seperator</code> property.  
     *  (<code>":"</code> on unix, <code>;</code> on Windows, etc.).
     *  The path may contain names of directories, or names of
     *  .zip or .jar files.  Elements that are neither of these
     *  are ignored.
     * @param sys_path the search path
     */

    SearchPath (String sys_path)
    {
	StringTokenizer st = new StringTokenizer (sys_path, path_seperator);
	init (st);
    }

    /**
     *  Constructs a SearchPath object, given a Vector of 
     *  <code>String</code>, <code>File</code> or <code>URL</code>
     *  objects.  
     *  The path may contain names of directories, or names of
     *  .zip or .jar files.  Elements that are neither of these
     *  are ignored.
     * @param p the vector of search path elements
     */

    SearchPath (Vector p)
    {
	init (p.elements ());
    }

    public URL getURL (String element)
    {
	URL result;

	Enumeration e = path.elements ();
	while (e.hasMoreElements ())
	    {
		PathEntry ent = (PathEntry) e.nextElement ();

		result = ent.getURL (element);
		
		if (result != null) 
		    {
			return result;
		    }
	    }

	return null;
    }
    

    public InputStream getStream (String element)
    {
	InputStream result;

	Enumeration e = path.elements ();
	while (e.hasMoreElements ())
	    {
		PathEntry ent = (PathEntry) e.nextElement ();

		result = ent.getStream (element);
		
		if (result != null) 
		    {
			return result;
		    }
	    }

	return null;
    }
    

    public byte[] getBytes (String element)
    {
	byte[] result;

	Enumeration e = path.elements ();
	while (e.hasMoreElements ())
	    {
		PathEntry ent = (PathEntry) e.nextElement ();
		result = ent.getBytes (element);		
		if (result != null) 
		    {
			// System.out.println ("loading " + ent 
			//		    + "(" + element + ")");
			return result;
		    }
	    }

	return null;
    }
    


    private void init (Enumeration st)
    {
	path = new Vector ();
	while (st.hasMoreElements ()) 
	    {  
		Object e = st.nextElement ();

		String elem;
		File efile;

		if (e instanceof URL)
		    {
			path.addElement (new URLPathEntry ((URL) e));
			continue;
		    }

		if (e instanceof File)
		    {
			efile = (File) e; 
			elem = efile.getPath ();
		    }

		else if (e instanceof String)
		    {
			elem = (String) e;
			efile   = new File (elem); 
		    }

		else
		    throw new IllegalArgumentException ();

		// make sure it is absolute, so we won't get 
		// trouble if the cwd is changed...
		if (! efile.isAbsolute ())
		    efile = new File (efile.getAbsolutePath ());

		if (efile.isDirectory ())
		    {
			try {
			    path.addElement(new DirectoryPathEntry (efile));
			} catch (IOException x) {
			    /* ignore for now */
			}
		    }

		else if (efile.isFile ())
		    {
			int ext = elem.lastIndexOf ('.');
			if (ext == -1)
			    continue;

			if (!elem.substring(ext+1).equalsIgnoreCase("zip"))
			    continue;

			ZipPathEntry zpe = null;
			try {
			    zpe = new ZipPathEntry (efile);
			} catch (ZipException zx) {
			    System.err.println ("SearchPath::ZipException");
			    zpe = null;
			} catch (MalformedURLException mx) {
			    System.err.println ("SearchPath::URLException");
			    zpe = null;
			} catch (IOException iox) {
			    System.err.println ("SearchPath::IOException");
			    zpe = null;
			}
			if (zpe != null) path.addElement (zpe);
		    }
	    }
	
    }
    
    
}

