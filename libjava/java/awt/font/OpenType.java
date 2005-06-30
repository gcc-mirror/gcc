/* OpenType.java
   Copyright (C) 2003 Free Software Foundation, Inc.

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
Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA
02110-1301 USA.

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


package java.awt.font;

/**
 * @author Michael Koch
 */
public interface OpenType
{
  int TAG_ACNT = 1633906292;
  int TAG_AVAR = 1635148146;
  int TAG_BASE = 1111577413;
  int TAG_BDAT = 1650745716;
  int TAG_BLOC = 1651273571;
  int TAG_BSLN = 1651731566;
  int TAG_CFF = 1128678944;
  int TAG_CMAP = 1668112752;
  int TAG_CVAR = 1668702578;
  int TAG_CVT = 1668707360;
  int TAG_DSIG = 1146308935;
  int TAG_EBDT = 1161970772;
  int TAG_EBLC = 1161972803;
  int TAG_EBSC = 1161974595;
  int TAG_FDSC = 1717859171;
  int TAG_FEAT = 1717920116;
  int TAG_FMTX = 1718449272;
  int TAG_FPGM = 1718642541;
  int TAG_FVAR = 1719034226;
  int TAG_GASP = 1734439792;
  int TAG_GDEF = 1195656518;
  int TAG_GLYF = 1735162214;
  int TAG_GPOS = 1196445523;
  int TAG_GSUB = 1196643650;
  int TAG_GVAR = 1735811442;
  int TAG_HDMX = 1751412088;
  int TAG_HEAD = 1751474532;
  int TAG_HHEA = 1751672161;
  int TAG_HMTX = 1752003704;
  int TAG_JSTF = 1246975046;
  int TAG_JUST = 1786082164;
  int TAG_KERN = 1801810542;
  int TAG_LCAR = 1818452338;
  int TAG_LOCA = 1819239265;
  int TAG_LTSH = 1280594760;
  int TAG_MAXP = 1835104368;
  int TAG_MMFX = 1296909912;
  int TAG_MMSD = 1296913220;
  int TAG_MORT = 1836020340;
  int TAG_NAME = 1851878757;
  int TAG_OPBD = 1836020340;
  int TAG_OS2 = 1330851634;
  int TAG_PCLT = 1346587732;
  int TAG_POST = 1886352244;
  int TAG_PREP = 1886545264;
  int TAG_PROP = 1886547824;
  int TAG_TRAK = 1953653099;
  int TAG_TYP1 = 1954115633;
  int TAG_VDMX = 1447316824;
  int TAG_VHEA = 1986553185;
  int TAG_VMTX = 1986884728;
  
  byte[] getFontTable (int sfntTag);
  
  byte[] getFontTable (int sfntTag, int offset, int count);
  
  byte[] getFontTable (String strSfntTag);
  
  byte[] getFontTable (String strSfntTag, int offset, int count);
  
  int getFontTableSize (int sfntTag);
  
  int getFontTableSize (String strSfntTag);
  
  int getVersion ();
}
