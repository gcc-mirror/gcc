/* gnu.classpath.tools.doclets.xmldoclet.TargetContext
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

package gnu.classpath.tools.doclets.xmldoclet;

import java.io.File;
import java.io.InputStream;
import java.io.OutputStream;
import java.io.FileInputStream;
import java.io.FileOutputStream;
import java.io.IOException;

import java.net.URL;

import java.util.List;
import java.util.Iterator;
import java.util.Map;
import java.util.HashMap;

import gnu.classpath.tools.doclets.xmldoclet.doctranslet.DocTranslet;

/**
 *  Stores any XSL transformation and postprocessing-specific
 *  information given by the user on the doclet command line.
 *
 *  @author Julian Scheid
 */
public class TargetContext {

   /**
    *  The DocTranslet to use for processing doclet output.
    */
   private DocTranslet docTranslet;

   /**
    *  Directory to write final output to.
    */
   private File targetDirectory;

   /**
    *  Directory where XSLT output will be written to. If an XSLT
    *  sheet was specified, but no postprocessing driver was given,
    *  this is the target directory specified by the user. Otherwise,
    *  this is a temporary directory.
    */
   private File xsltTargetDirectory;


   public TargetContext(DocTranslet docTranslet, File targetDirectory) {
      this.docTranslet = docTranslet;
      this.targetDirectory = targetDirectory;
   }

   public File getTargetDirectory() {
      return targetDirectory;
   }

   public void setTargetDirectory(File targetDirectory) {
      this.targetDirectory = targetDirectory;
   }

   public DocTranslet getDocTranslet() {
      return docTranslet;
   }

   public void setDocTranslet(DocTranslet docTranslet) {
      this.docTranslet = docTranslet;
   }
}
