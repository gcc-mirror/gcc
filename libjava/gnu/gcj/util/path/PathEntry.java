// PathEntry.java -- abstract element of search paths

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

abstract class PathEntry {
  abstract URL getURL (String file);
  abstract InputStream getStream (String file);
  abstract byte[] getBytes (String file);

  /**
   *  Utility routine like InputStream.read(byte[], 0, len), but will
   *  read fully, even if all the data is not available at once.  
   */
  protected static byte[] readbytes (InputStream is, int length)
  {
    try {

      byte[] data = new byte[length];
      int read; 
      int off = 0;
	    
      while (off != length)
	{
	  read = is.read (data, off, (int) (length-off));

	  if (read == -1) 
	    return null;

	  off += read;
	}
	    
      return data;
    } catch (IOException x) {
      return null;
    }
  }

}


