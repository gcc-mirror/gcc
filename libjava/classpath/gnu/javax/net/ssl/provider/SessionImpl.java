/* SessionImpl.java --
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

import gnu.javax.crypto.key.GnuPBEKey;
import gnu.javax.net.ssl.Session;
import java.io.IOException;
import java.io.Serializable;

import java.security.InvalidKeyException;
import java.security.NoSuchAlgorithmException;
import java.security.SecureRandom;
import javax.crypto.Cipher;
import javax.crypto.IllegalBlockSizeException;
import javax.crypto.NoSuchPaddingException;
import javax.crypto.SealedObject;
import javax.net.ssl.SSLException;

public class SessionImpl extends Session
{
  static final long serialVersionUID = 8932976607588442485L;
  CipherSuite suite;
  ProtocolVersion version;
  byte[] privateDataSalt;
  SealedObject sealedPrivateData;
  MaxFragmentLength maxLength;
  
  transient PrivateData privateData;
  
  public SessionImpl()
  {
    super();
    privateData = new PrivateData();
  }
  
  SecureRandom random ()
  {
    return random;
  }
  
  public String getProtocol()
  {
    return version.toString();
  }
  
  public void prepare(char[] passwd) throws SSLException
  {
    try
      {
        privateDataSalt = new byte[32];
        random.nextBytes(privateDataSalt);
        GnuPBEKey key = new GnuPBEKey(passwd, privateDataSalt, 1000);
        Cipher cipher = Cipher.getInstance("PBEWithHMacSHA256AndAES/OFB/PKCS7Padding");
        cipher.init(Cipher.ENCRYPT_MODE, key);
        sealedPrivateData = new SealedObject(privateData, cipher);
      }
    catch (IllegalBlockSizeException ibse)
      {
        throw new SSLException(ibse);
      }
    catch (InvalidKeyException ike)
      {
        throw new SSLException(ike);
      }
    catch (IOException ioe)
      {
        throw new SSLException(ioe);
      }
    catch (NoSuchAlgorithmException nsae)
      {
        throw new SSLException(nsae);
      }
    catch (NoSuchPaddingException nspe)
      {
        throw new SSLException(nspe);
      }
  }
  
  public void repair(char[] passwd) throws SSLException
  {
    try
      {
        GnuPBEKey key = new GnuPBEKey(passwd, privateDataSalt, 1000);
        privateData = (PrivateData) sealedPrivateData.getObject(key);
      }
    catch (ClassNotFoundException cnfe)
      {
        throw new SSLException(cnfe);
      }
    catch (InvalidKeyException ike)
      {
        throw new SSLException(ike);
      }
    catch (IOException ioe)
      {
        throw new SSLException(ioe);
      }
    catch (NoSuchAlgorithmException nsae)
      {
        throw new SSLException(nsae);
      }
  }
  
  public SealedObject privateData() throws SSLException
  {
    if (privateData == null)
      throw new SSLException("this session has not been prepared");
    return sealedPrivateData;
  }
  
  public void setPrivateData(SealedObject so) throws SSLException
  {
    this.sealedPrivateData = so;
  }

  void setApplicationBufferSize(int size)
  {
    applicationBufferSize = size;
  }
  
  void setRandom(SecureRandom random)
  {
    this.random = random;
  }
  
  void setTruncatedMac(boolean truncatedMac)
  {
    this.truncatedMac = truncatedMac;
  }
  
  void setId(Session.ID id)
  {
    this.sessionId = id;
  }
  
  void setLocalCertificates(java.security.cert.Certificate[] chain)
  {
    this.localCerts = chain;
  }
  
  void setPeerCertificates(java.security.cert.Certificate[] chain)
  {
    this.peerCerts = chain;
  }
  
  void setPeerVerified(boolean peerVerified)
  {
    this.peerVerified = peerVerified;
  }
  
  static class PrivateData implements Serializable
  {
    static final long serialVersionUID = -8040597659545984581L;
    byte[] masterSecret;
  }
}
