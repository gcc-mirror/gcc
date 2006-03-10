/* JCESecurityParameters.java -- JCE-based security parameters.
   Copyright (C) 2006  Free Software Foundation, Inc.

This file is a part of GNU Classpath.

GNU Classpath is free software; you can redistribute it and/or modify
it under the terms of the GNU General Public License as published by
the Free Software Foundation; either version 2 of the License, or (at
your option) any later version.

GNU Classpath is distributed in the hope that it will be useful, but
WITHOUT ANY WARRANTY; without even the implied warranty of
MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the GNU
General Public License for more details.

You should have received a copy of the GNU General Public License
along with GNU Classpath; if not, write to the Free Software
Foundation, Inc., 51 Franklin St, Fifth Floor, Boston, MA 02110-1301
USA

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
exception statement from your version.  */


package gnu.javax.net.ssl.provider;

import java.io.ByteArrayOutputStream;

import java.util.Arrays;
import java.util.zip.DataFormatException;
import java.util.zip.Deflater;
import java.util.zip.Inflater;

import javax.crypto.BadPaddingException;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.Mac;

import javax.net.ssl.SSLException;

class JCESecurityParameters implements SecurityParameters
{

  // Fields.
  // -------------------------------------------------------------------------

  private Cipher inCipher, outCipher;
  private Mac inMac, outMac;
  private Inflater inflater;
  private Deflater deflater;
  private int fragmentLength;
  private long inSequence, outSequence;
  private ProtocolVersion version;

  // Constructors.
  // -------------------------------------------------------------------------

  JCESecurityParameters ()
  {
    fragmentLength = 16384;
    inSequence = 0L;
    outSequence = 0L;
  }

  // Instance methods.
  // -------------------------------------------------------------------------

  public void reset()
  {
    inCipher = null;
    outCipher = null;
    inMac = null;
    outMac = null;
    deflater = null;
    inflater = null;
  }

  public void setInCipher (Object inCipher)
  {
    this.inCipher = (Cipher) inCipher;
  }

  public void setOutCipher (Object outCipher)
  {
    this.outCipher = (Cipher) outCipher;
  }

  public void setInMac (Object inMac)
  {
    this.inMac = (Mac) inMac;
    inSequence = 0L;
  }

  public void setOutMac (Object outMac)
  {
    this.outMac = (Mac) outMac;
    outSequence = 0L;
  }

  public void setDeflating (boolean deflate)
  {
    if (deflate)
      {
        if (deflater == null)
          deflater = new Deflater();
      }
    else
      deflater = null;
  }

  public void setInflating (boolean inflate)
  {
    if (inflate)
      {
        if (inflater == null)
          inflater = new Inflater();
      }
    else
      inflater = null;
  }

  public int getFragmentLength()
  {
    return fragmentLength;
  }

  public void setFragmentLength (int fragmentLength)
  {
    this.fragmentLength = fragmentLength;
  }

  public ProtocolVersion getVersion()
  {
    return version;
  }

  public void setVersion (ProtocolVersion version)
  {
    this.version = version;
  }

  public synchronized byte[] decrypt (byte[] fragment, ProtocolVersion version,
                                      ContentType type)
    throws MacException, OverflowException, SSLException
  {
    boolean badpad = false;
    if (inCipher != null)
      {
        // We imagine that the JCE would be used in cases where hardware
        // acceleration is available, since it isn't really that useful for
        // pure Java crypto. We decrypt (and encrypt, below) in one go
        // to minimize (potential) calls to native methods.
        try
          {
            fragment = inCipher.doFinal (fragment);
          }
        catch (BadPaddingException bpe)
          {
            badpad = true;
          }
        catch (IllegalBlockSizeException ibse)
          {
            badpad = true;
          }
      }

    if (inMac != null)
      {
        int macLen = inMac.getMacLength();
        int fragLen = fragment.length - macLen;
        byte[] mac = Util.trim (fragment, fragLen, macLen);
        fragment = Util.trim (fragment, fragLen);
        inMac.update ((byte) (inSequence >>> 56));
        inMac.update ((byte) (inSequence >>> 48));
        inMac.update ((byte) (inSequence >>> 40));
        inMac.update ((byte) (inSequence >>> 32));
        inMac.update ((byte) (inSequence >>> 24));
        inMac.update ((byte) (inSequence >>> 16));
        inMac.update ((byte) (inSequence >>>  8));
        inMac.update ((byte)  inSequence);
        inMac.update ((byte) type.getValue());
        if (version != ProtocolVersion.SSL_3)
          {
            inMac.update ((byte) version.getMajor());
            inMac.update ((byte) version.getMinor());
          }
        inMac.update ((byte) (fragLen >>> 8));
        inMac.update ((byte)  fragLen);
        inMac.update (fragment);
        if (!Arrays.equals (mac, inMac.doFinal()) || badpad)
          throw new MacException();
      }

    if (inflater != null)
      {
        byte[] buf = new byte[1024];
        ByteArrayOutputStream bout = new ByteArrayOutputStream (fragment.length << 1);
        inflater.setInput (fragment);
        int len;
        try
          {
            while ((len = inflater.inflate (buf)) > 0)
              {
                bout.write (buf, 0, len);
                if (bout.size() > fragmentLength + 1024)
                  throw new OverflowException ("inflated data too large");
              }
          }
        catch (DataFormatException dfe)
          {
            throw new SSLException (String.valueOf (dfe));
          }
        fragment = bout.toByteArray();
        inflater.reset();
      }

    inSequence++;
    return fragment;
  }

  public synchronized byte[] encrypt (byte[] fragment, int off, int len,
                                      ContentType type)
    throws OverflowException, SSLException
  {
    if (deflater != null)
      {
        byte[] buf = new byte[1024];
        ByteArrayOutputStream bout = new ByteArrayOutputStream (len >>> 1);
        deflater.setInput (fragment, off, len);
        deflater.finish();
        len = 0;
        while ((len = deflater.deflate (buf)) > 0)
          bout.write (buf, 0, len);
        // This should technically never happen for zlib.
        if (bout.size() > fragmentLength + 1024)
          throw new OverflowException ("deflated data too large");
        fragment = bout.toByteArray();
        off = 0;
        len = fragment.length;
        deflater.reset();
      }

    if (outMac != null)
      {
        outMac.update ((byte) (inSequence >>> 56));
        outMac.update ((byte) (inSequence >>> 48));
        outMac.update ((byte) (inSequence >>> 40));
        outMac.update ((byte) (inSequence >>> 32));
        outMac.update ((byte) (inSequence >>> 24));
        outMac.update ((byte) (inSequence >>> 16));
        outMac.update ((byte) (inSequence >>>  8));
        outMac.update ((byte)  inSequence);
        outMac.update ((byte) type.getValue());
        if (version != ProtocolVersion.SSL_3)
          {
            outMac.update ((byte) version.getMajor());
            outMac.update ((byte) version.getMinor());
          }
        outMac.update ((byte) (len >>> 8));
        outMac.update ((byte)  len);
        outMac.update (fragment, off, len);
        fragment = Util.concat (fragment, outMac.doFinal());
        off = 0;
        len = fragment.length;
      }

    if (outCipher != null)
      {
        try
          {
            fragment = outCipher.doFinal (fragment, off, len);
          }
        catch (BadPaddingException shouldNeverHappen)
          {
            // This is nonsensical. Don't even pretend that we can handle this.
            throw new RuntimeException ("bad padding thrown while encrypting");
          }
        catch (IllegalBlockSizeException ibse)
          {
            // Ditto.
            throw new RuntimeException ("illegal block size thrown while encrypting");
          }
        off = 0;
        len = fragment.length;
      }

    outSequence++;
    if (off == 0 && len == fragment.length)
      return fragment;
    else
      return Util.trim (fragment, off, len);
  }
}
