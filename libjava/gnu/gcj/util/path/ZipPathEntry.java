// ZipPathEntry.java -- search path element for directories

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


final class ZipPathEntry extends PathEntry {
    final ZipFile zip;
    final URL     file;

    public String toString () { return zip.getName (); }

    ZipPathEntry (File f) 
	throws MalformedURLException, ZipException, IOException
    {
	file = new URL ("file", "", f.getPath ());
	zip  = new ZipFile (f);
	zip.readDirectory ();
    }

    /* 
       The url for a zip-file resource is,
	   
       <code>file:///path/file.zip#name</code>
	   
       Then, it is URLConnection's problem to handle that.
    */

    URL getURL (String f) {

	ZipEntry ent = zip.getEntry (f);

	try {
	    if (ent != null)
		return new URL (file, "#"+f);
	    else
		return null;
	} catch (IOException x) {
	    return null;
	}
    }

    InputStream getStream (String f) {

	ZipEntry ent = zip.getEntry (f);

	try {
	    if (ent != null)
		return zip.getInputStream (ent);
	    else
		return null;
	} catch (IOException x) {
	    return null;
	}
    }

    byte[] getBytes (String f) {
	ZipEntry ent = zip.getEntry (f);

	try {
	    if (ent != null)
		return readbytes (zip.getInputStream (ent),
				  (int) ent.getSize ());
	    else
		return null;
	} catch (IOException x) {
	    return null;
	}
	    
    }
}

