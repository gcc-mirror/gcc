// DirectoryPathEntry.java -- search path element for directories

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

final class DirectoryPathEntry extends PathEntry
{
  final File dir;
  final String base_canon;

  public String toString () { return base_canon; }

  DirectoryPathEntry (File f)
    throws java.io.IOException
  {
    if (!f.isAbsolute ())
      throw new IllegalArgumentException ();

    dir = f; 
    base_canon = dir.getCanonicalPath ();
  }

  /*
   *  We maintain a cache of files, so that we 
   *  can avoid many calls to stat(), which are
   *  very expensive.
   *
   *  seen_cache contains (as keys) the directories 
   *  which we have visited so far.  The values are 
   *  instances of CacheEntry, containing a time stamp,
   *  and a list of files in that directory.
   *
   */

  private Hashtable seen_cache = new Hashtable ();

  private boolean in_cache (File f)
  {
    String rel_dir = f.getParent ();
    CacheEntry ent;

    if (rel_dir == null)
      throw new IllegalArgumentException ();

    ent = (CacheEntry) seen_cache.get (rel_dir);
    if (ent == null)
      {
	ent = new CacheEntry (rel_dir);
	seen_cache.put (rel_dir, ent);
      }

    if (ent.contains (f.getPath ()))
      {
	return true;
      }

    if ( ent.is_old () )
      {
	if (f.exists ())
	  {
	    seen_cache.remove (rel_dir);
	    return true;
	  }
	else
	  {
	    ent.touch ();
	  }
      }

    return false;
  }

  URL getURL (String file) {
    try {
      File f = new File((new File (dir, file).getCanonicalPath ()));
	    
      if (! f.getCanonicalPath ().startsWith (base_canon))
	throw new IllegalArgumentException (file);

		
      if (in_cache (f))
	return new URL ("file", "", f.getPath ());
      else
	return null;

    } catch (IOException x) {
      return null;
    }
  }

  InputStream getStream (String file) {
    try {			
      File f = new File((new File (dir, file)).getCanonicalPath ());

      if (! f.getCanonicalPath ().startsWith (base_canon))
	throw new IllegalArgumentException (file);

      if (in_cache (f))
	return new FileInputStream (f);
      else
	return null;
    } catch (IOException x) {
      return null;
    }
  }

  byte[] getBytes (String file) {
    File f = new File (dir, file);

    try {			
      if (in_cache (f))
	return readbytes (new FileInputStream (f),
			  (int) f.length ());
      else
	return null;
    } catch (IOException x) {
      return null;
    }
  }

}

