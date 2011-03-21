/* SecurityContext.java --
   Copyright (C) 2003, 2006 Free Software Foundation, Inc.

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


package gnu.javax.crypto.sasl.srp;

/**
 * A package-private placeholder for an SRP security context.
 */
class SecurityContext
{
  private String mdName;
  private byte[] sid;
  private byte[] K;
  private byte[] cIV;
  private byte[] sIV;
  private boolean replayDetection;
  private int inCounter;
  private int outCounter;
  private IALG inMac;
  private IALG outMac;
  private CALG inCipher;
  private CALG outCipher;

  SecurityContext(final String mdName, final byte[] sid, final byte[] K,
                  final byte[] cIV, final byte[] sIV,
                  final boolean replayDetection, final int inCounter,
                  final int outCounter, final IALG inMac, final IALG outMac,
                  final CALG inCipher, final CALG outCipher)
  {
    super();

    this.mdName = mdName;
    this.sid = sid;
    this.K = K;
    this.cIV = cIV;
    this.sIV = sIV;
    this.replayDetection = replayDetection;
    this.inCounter = inCounter;
    this.outCounter = outCounter;
    this.inMac = inMac;
    this.outMac = outMac;
    this.inCipher = inCipher;
    this.outCipher = outCipher;
  }

  String getMdName()
  {
    return mdName;
  }

  byte[] getSID()
  {
    return sid;
  }

  byte[] getK()
  {
    return K;
  }

  byte[] getClientIV()
  {
    return cIV;
  }

  byte[] getServerIV()
  {
    return sIV;
  }

  boolean hasReplayDetection()
  {
    return replayDetection;
  }

  int getInCounter()
  {
    return inCounter;
  }

  int getOutCounter()
  {
    return outCounter;
  }

  IALG getInMac()
  {
    return inMac;
  }

  IALG getOutMac()
  {
    return outMac;
  }

  CALG getInCipher()
  {
    return inCipher;
  }

  CALG getOutCipher()
  {
    return outCipher;
  }
}
