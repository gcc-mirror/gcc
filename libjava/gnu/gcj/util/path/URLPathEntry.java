// URLPathEntry.java -- search path element for URL's

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

final class URLPathEntry extends PathEntry {
    final URL base;

    URLPathEntry (URL f) {
	base = f;
    }

    public String toString () { return base.toString (); }

    URL getURL (String file) {

	try {
	    URL res = new URL (base, file);
	    InputStream is = res.openStream (); // exc if not found
	    is.close ();
	    return res;
	} catch (java.io.IOException x) {
	    return null;
	}
    }

    InputStream getStream (String file) {

	try {
	    URL res = new URL (base, file);
	    return res.openStream ();
	} catch (java.io.IOException x) {
	    return null;
	}

    }

    byte[] getBytes (String file) {

	try {
	    URL res = new URL (base, file);
	    URLConnection conn = res.openConnection ();
	    int len = conn.getContentLength ();
	    if (len == -1) return null;
	    return readbytes (conn.getInputStream (), len);
	} catch (java.io.IOException x) {
	    return null;
	}

    }

}

