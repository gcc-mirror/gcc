/* java.util.zip.ZipConstants
   Copyright (C) 2001 Free Software Foundation, Inc.

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

package java.util.zip;

interface ZipConstants
{
    /* The local file header */
    public final static int LOCHDR = 30;
    public final static int LOCSIG = 'P'|('K'<<8)|(3<<16)|(4<<24);

    public final static int LOCVER =  4;
    public final static int LOCFLG =  6;
    public final static int LOCHOW =  8;
    public final static int LOCTIM = 10;
    public final static int LOCCRC = 14;
    public final static int LOCSIZ = 18;
    public final static int LOCLEN = 22;
    public final static int LOCNAM = 26;
    public final static int LOCEXT = 28;

    /* The Data descriptor */
    public final static int EXTSIG = 'P'|('K'<<8)|(7<<16)|(8<<24);
    public final static int EXTHDR = 16;

    public final static int EXTCRC =  4;
    public final static int EXTSIZ =  8;
    public final static int EXTLEN = 12;

    /* The central directory file header */
    public final static int CENSIG = 'P'|('K'<<8)|(1<<16)|(2<<24);
    public final static int CENHDR = 46;

    public final static int CENVEM =  4;
    public final static int CENVER =  6;
    public final static int CENFLG =  8;
    public final static int CENHOW = 10;
    public final static int CENTIM = 12;
    public final static int CENCRC = 16;
    public final static int CENSIZ = 20;
    public final static int CENLEN = 24;
    public final static int CENNAM = 28;
    public final static int CENEXT = 30;
    public final static int CENCOM = 32;
    public final static int CENDSK = 34;
    public final static int CENATT = 36;
    public final static int CENATX = 38;
    public final static int CENOFF = 42;

    /* The entries in the end of central directory */
    public final static int ENDSIG = 'P'|('K'<<8)|(5<<16)|(6<<24);
    public final static int ENDHDR = 22;

    /* The following two fields are missing in SUN JDK */
    final static int ENDNRD =  4;
    final static int ENDDCD =  6;
    public final static int ENDSUB =  8;
    public final static int ENDTOT = 10;
    public final static int ENDSIZ = 12;
    public final static int ENDOFF = 16;
    public final static int ENDCOM = 20;
}

