// SimpleSHSStream.java

/* Copyright (C) 2000  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.io;
import java.io.Serializable;
import java.io.*;
import java.lang.reflect.*;

public class SimpleSHSStream extends java.io.DataOutputStream
{
  int counter;
 
  final int SHS_BLOCKSIZE = 64;
  final int SHS_DIGESTSIZE = 20;

  byte buf[];
  byte shs_info[];

  native static byte [] shsFinal (byte info[]);
  native static void shsUpdate (byte info[], byte buf[], int count);
  native static byte [] shsInit ();

  private void update (byte b)
  {
    buf [counter++] = b;
    if (counter % SHS_BLOCKSIZE == 0)
      {
	counter = 0;
	shsUpdate (shs_info, buf, SHS_BLOCKSIZE);
      }
  }    
  
  public void write (int b) throws IOException
  {
    update ((byte)b);
    super.write (b);
  }

  public void write (byte[] b, int off, int len) throws IOException
  {
    for (int i = 0; i < len; i++)
      write (b[i+off]);
  }

  public byte[] digest()
  {
    shsUpdate (shs_info, buf, counter);
    return shsFinal (shs_info);
  }

  public SimpleSHSStream (OutputStream out)
  {
    super (out);
    buf = new byte[SHS_BLOCKSIZE];
    shs_info = shsInit ();
    counter = 0;
  }
}

