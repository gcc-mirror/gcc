/* DummySignature.java
   Copyright (C) 1999 Free Software Foundation, Inc.

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

As a special exception, if you link this library with other files to
produce an executable, this library does not by itself cause the
resulting executable to be covered by the GNU General Public License.
This exception does not however invalidate any other reasons why the
executable file might be covered by the GNU General Public License. */

package java.security;

final class DummySignature extends Signature
{
  private SignatureSpi sigSpi = null;

  public DummySignature(SignatureSpi sigSpi, String algorithm)
  {
    super(algorithm);
    this.sigSpi = sigSpi;
  }

  protected void engineInitVerify(PublicKey publicKey)
    throws InvalidKeyException
  {
    sigSpi.engineInitVerify(publicKey);
  }

  protected void engineInitSign(PrivateKey privateKey)
    throws InvalidKeyException
  {
    sigSpi.engineInitSign(privateKey);
  }

  protected void engineUpdate(byte b) throws SignatureException
  {
    sigSpi.engineUpdate(b);
  }

  protected void engineUpdate(byte[]b, int off, int len)
    throws SignatureException
  {
    sigSpi.engineUpdate(b, off, len);
  }

  protected byte[] engineSign() throws SignatureException
  {
    return sigSpi.engineSign();
  }

  protected boolean engineVerify(byte[]sigBytes) throws SignatureException
  {
    return sigSpi.engineVerify(sigBytes);
  }

  protected void engineSetParameter(String param, Object value)
    throws InvalidParameterException
  {
    sigSpi.engineSetParameter(param, value);
  }

  protected Object engineGetParameter(String param)
    throws InvalidParameterException
  {
    return sigSpi.engineGetParameter(param);
  }
}
