/* Copyright (C) 1999  Cygnus Solutions

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.zip;
import java.io.*;

/** JUST AN INCOMPLETE STUB! */

public class DeflaterOutputStream extends FilterOutputStream
{
  protected byte[] buf;

  protected Deflater def;

  public DeflaterOutputStream(OutputStream out)
  {
    this(out, null, 512);
  }

  public DeflaterOutputStream(OutputStream out, Deflater defl)
  {
    this(out, defl, 512);
  }

  public DeflaterOutputStream(OutputStream out, Deflater defl, int bufsize)
  {
    super(out);
    buf = new byte[bufsize];
    def = defl;
  }

  public void finish () throws IOException
  {
  }

  public void close () throws IOException
  {
    finish();
    out.close();
  }
}
