/* MimeTypeMapper.java -- A class for mapping file names to MIME types
   Copyright (C) 1998, 2006 Free Software Foundation, Inc.

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

package java.net;

import java.util.Hashtable;
import gnu.gcj.io.MimeTypes;


/**
 * This non-public class is used to implement the FileNameMap interface
 * which defines a mechanism for mapping filenames to MIME types.
 *
 * @version 0.5
 *
 * @author Aaron M. Renn (arenn@urbanophile.com)
 */
class MimeTypeMapper implements FileNameMap
{
  /**
   * This array of strings is used to identify a MIME type based on a file
   * extension.  This is list is based on the Apache mime.types file.
   */
  protected static final String[][] mime_strings =
    {
      { "application/mac-binhex40", "hqx" },
      { "application/mac-compactpro", "cpt" },
      { "application/msword", "doc" },
      { "application/octet-stream", "bin" },
      { "application/octet-stream", "dms" },
      { "application/octet-stream", "lha" },
      { "application/octet-stream", "lzh" },
      { "application/octet-stream", "exe" },
      { "application/octet-stream", "class" },
      { "application/oda", "oda" },
      { "application/pdf", "pdf" },
      { "application/postscript", "ai" },
      { "application/postscript", "eps" },
      { "application/postscript", "ps" },
      { "application/powerpoint", "ppt" },
      { "application/rtf", "rtf" },
      { "application/x-bcpio", "bcpio" },
      { "application/x-cdlink", "vcd" },
      { "application/x-compress", "Z" },
      { "application/x-cpio", "cpio" },
      { "application/x-csh", "csh" },
      { "application/x-director", "dcr" },
      { "application/x-director", "dir" },
      { "application/x-director", "dxr" },
      { "application/x-dvi", "dvi" },
      { "application/x-gtar", "gtar" },
      { "application/x-gzip", "gz" },
      { "application/x-hdf", "hdf" },
      { "application/x-httpd-cgi", "cgi" },
      { "application/x-koan", "skp" },
      { "application/x-koan", "skd" },
      { "application/x-koan", "skt" },
      { "application/x-koan", "skm" },
      { "application/x-latex", "latex" },
      { "application/x-mif", "mif" },
      { "application/x-netcdf", "nc" },
      { "application/x-netcdf", "cdf" },
      { "application/x-sh", "sh" },
      { "application/x-shar", "shar" },
      { "application/x-stuffit", "sit" },
      { "application/x-sv4cpio", "sv4cpio" },
      { "application/x-sv4crc", "sv4crc" },
      { "application/x-tar", "tar" },
      { "application/x-tcl", "tcl" },
      { "application/x-tex", "tex" },
      { "application/x-texinfo", "texinfo" },
      { "application/x-texinfo", "texi" },
      { "application/x-troff", "t" },
      { "application/x-troff", "tr" },
      { "application/x-troff", "roff" },
      { "application/x-troff-man", "man" },
      { "application/x-troff-me", "me" },
      { "application/x-troff-ms", "ms" },
      { "application/x-ustar", "ustar" },
      { "application/x-wais-source", "src" },
      { "application/zip", "zip" },
      { "audio/basic", "au" },
      { "audio/basic", "snd" },
      { "audio/mpeg", "mpga" },
      { "audio/mpeg", "mp2" },
      { "audio/mpeg", "mp3" },
      { "audio/x-aiff", "aif" },
      { "audio/x-aiff", "aiff" },
      { "audio/x-aiff", "aifc" },
      { "audio/x-pn-realaudio", "ram" },
      { "audio/x-pn-realaudio-plugin", "rpm" },
      { "audio/x-realaudio", "ra" },
      { "audio/x-wav", "wav" },
      { "chemical/x-pdb", "pdb" },
      { "chemical/x-pdb", "xyz" },
      { "image/gif", "gif" },
      { "image/ief", "ief" },
      { "image/jpeg", "jpeg" },
      { "image/jpeg", "jpg" },
      { "image/jpeg", "jpe" },
      { "image/png", "png" },
      { "image/tiff", "tiff" },
      { "image/tiff", "tif" },
      { "image/x-cmu-raster", "ras" },
      { "image/x-portable-anymap", "pnm" },
      { "image/x-portable-bitmap", "pbm" },
      { "image/x-portable-graymap", "pgm" },
      { "image/x-portable-pixmap", "ppm" },
      { "image/x-rgb", "rgb" },
      { "image/x-xbitmap", "xbm" },
      { "image/x-xpixmap", "xpm" },
      { "image/x-xwindowdump", "xwd" },
      { "text/html", "html" },
      { "text/html", "htm" },
      { "text/plain", "txt" },
      { "text/richtext", "rtx" },
      { "text/tab-separated-values", "tsv" },
      { "text/x-setext", "etx" },
      { "text/x-sgml", "sgml" },
      { "text/x-sgml", "sgm" },
      { "video/mpeg", "mpeg" },
      { "video/mpeg", "mpg" },
      { "video/mpeg", "mpe" },
      { "video/quicktime", "qt" },
      { "video/quicktime", "mov" },
      { "video/x-msvideo", "avi" },
      { "video/x-sgi-movie", "movie" },
      { "x-conference/x-cooltalk", "ice" },
      { "x-world/x-vrml", "wrl" },
      { "x-world/x-vrml", "vrml" }
    };

  /**
   * The MIME types above are put into this Hashtable for faster lookup.
   */
  private static Hashtable mime_types = new Hashtable(150);

  // Static initializer to load MIME types into Hashtable
  static
  {
    for (int i = 0; i < mime_strings.length; i++)
      mime_types.put(mime_strings[i][1], mime_strings[i][0]);
  }

  /**
   * Create a new <code>MimeTypeMapper</code> object.
   */
  public MimeTypeMapper()
  {
    // Do nothing here.
  }

  /**
   * The method returns the MIME type of the filename passed as an argument.
   * The value returned is based on the extension of the filename.  The
   * default content type returned if this method cannot determine the
   * actual content type is "application/octet-stream"
   *
   * @param filename The name of the file to return the MIME type for
   *
   * @return The MIME type
   */
  public String getContentTypeFor(String filename)
  {
    int index = filename.lastIndexOf(".");
    if (index != -1)
      {
	if (index == filename.length())
	  return "application/octet-stream";
	else
	  filename = filename.substring(index + 1);
      }

    String type = (String) mime_types.get(filename);
    if (type == null)
      return MimeTypes.getMimeTypeFromExtension(filename);
    else
      return type;
  }
}
