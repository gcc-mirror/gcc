/* Copyright (C) 1999  Red Hat, Inc.

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package java.util.zip;

/**
 * @author Per Bothner
 * @date April 6, 1999.
 */

/*
 * Written using on-line Java Platform 1.2 API Specification, as well
 * as "The Java Class Libraries", 2nd edition (Addison-Wesley, 1998).
 * The actual Adler32 algorithm is taken from RFC 1950.
 * Status:  Believed complete and correct.
 */

public class Adler32 implements Checksum
{
  private static int BASE = 65521; /* largest prime smaller than 65536 */

  int s1;
  int s2;

  public Adler32 ()
  {
    reset();
  }

  public void reset () { s1 = 1;  s2 = 0; }

  public void update (int bval)
  {
    s1 = (s1 + (bval & 0xFF)) % BASE;
    s2 = (s1 + s2) % BASE;
  }

  public void update (byte[] buffer)
  {
    update(buffer, 0, buffer.length);
  }

  public void update (byte[] buf, int off, int len)
  {
    int s1 = this.s1;
    int s2 = this.s2;
    while (len > 0)
      {
	// We can defer the modulo operation.
	int n = 4000;
	if (n > len)
	  n = len;
	len -= n;
	while (--n >= 0)
	  {
	    s1 = s1 + (buf[off++] & 0xFF);
	    s2 = s2 + s1;
	  }
	s1 %= BASE;
	s2 %= BASE;
      }
    this.s1 = s1;
    this.s2 = s2;
  }

  public long getValue()
  {
    return ((long) s2 << 16) + s1;
  }
}


























