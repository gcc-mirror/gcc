/* BlockOutputStream.java --
   Copyright (C) 2003, 2004  Free Software Foundation, Inc.

This file is part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2, or (at your option)
any later version.
 
GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; see the file COPYING.  If not, write to the
Free Software Foundation, Inc., 59 Temple Place, Suite 330, Boston, MA
02111-1307 USA.

Linking this library statically or dynamically with other modules is
making a combined work based on this library.  Thus, the terms and
conditions of the GNU General Public License cover the whole
combination.

As a special exception, the copyright holders of this library give you
permission to link this library with independent modules to produce an
executable, regardless of the license terms of these independent
modules, and to copy and distribute the resulting executable under
terms of your choice, provided that you also meet, for each linked
independent module, the terms and conditions of the license of that
module.  An independent module is a module which is not derived from
or based on this library.  If you modify this library, you may extend
this exception to your version of the library, but you are not
obligated to do so.  If you do not wish to do so, delete this
exception statement from your version. */


package gnu.java.net.protocol.ftp;

import java.io.IOException;
import java.io.OutputStream;

/**
 * A DTP output stream that implements the FTP block transfer mode.
 *
 * @author Chris Burdess (dog@gnu.org)
 */
class BlockOutputStream
  extends DTPOutputStream
{

  static final byte RECORD = -128;      // 0x80
  static final byte EOF = 64;   // 0x40

  BlockOutputStream(DTP dtp, OutputStream out)
  {
    super(dtp, out);
  }

  public void write(int c)
    throws IOException
  {
    if (transferComplete)
      {
        return;
      }
    byte[] buf = new byte[]
      {
        RECORD,                 /* record descriptor */
        0x00, 0x01,             /* one byte */
        (byte) c                /* the byte */
      };
    out.write(buf, 0, 4);
  }

  public void write(byte[] b)
    throws IOException
  {
    write(b, 0, b.length);
  }

  public void write(byte[] b, int off, int len)
    throws IOException
  {
    if (transferComplete)
      {
        return;
      }
    byte[] buf = new byte[len + 3];
    buf[0] = RECORD;            /* record descriptor */
    buf[1] = (byte) ((len & 0x00ff) >> 8);      /* high byte of bytecount */
    buf[2] = (byte) (len & 0xff00);     /* low byte of bytecount */
    System.arraycopy(b, off, buf, 3, len);
    out.write(buf, 0, len);
  }

  public void close()
    throws IOException
  {
    byte[] buf = new byte[]
      {
        EOF,                    /* eof descriptor */
        0x00, 0x00              /* no bytes */
      };
    out.write(buf, 0, 3);
    super.close();
  }

}

