/* gnu.classpath.tools.doclets.xmldoclet.doctranslet.DocTransletOptions
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
02111-1307 USA. */

package gnu.classpath.tools.doclets.xmldoclet.doctranslet;

/**
 *  Value class for carrying command line options which need to be
 *  passed through to the stylesheets.
 */
public class DocTransletOptions
{
   public boolean nonavbar;
   public boolean noindex;
   public boolean notree;
   public boolean nocomment;
   public boolean nohelp;
   public boolean splitindex;
   public boolean linksource;
   public boolean nodeprecatedlist;
   public boolean uses;
   public String  windowtitle = "";
   public String  helpfile = "";
   public String  stylesheetfile = "";
   public String  header = "";
   public String  footer = "";
   public String  bottom = "";
   public String  doctitle = "";
}
