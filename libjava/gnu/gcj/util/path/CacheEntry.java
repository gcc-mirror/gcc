// CacheEntry.java -- directory cache

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


final class CacheEntry {
    String   dir;
    String[] files;
    long     time;
    
    CacheEntry (String d)
    {
	dir = d;
	files = new File(dir).list();
	time = System.currentTimeMillis ();
    }
    
    void touch ()
    {
	time = System.currentTimeMillis ();
    }
    
    final long EXPIRATION_TIME_MS = 1000;
    
    boolean is_old () {
	return (System.currentTimeMillis () - time) > EXPIRATION_TIME_MS;
    }
    
    public int hashCode () { return dir.hashCode(); }
    boolean contains (String file) { 
	if (files == null)
	    return false;
	
	int index = file.lastIndexOf(SearchPath.file_seperator_char);
	String f;
	
	if (index == -1)
	    f = file;
	else
	    f = file.substring (index+1);
	
	for (int i = 0; i < files.length; i++)
	    {
		if (f.equals (files[i])) return true;
	    }
	
	return false;
    }
}

