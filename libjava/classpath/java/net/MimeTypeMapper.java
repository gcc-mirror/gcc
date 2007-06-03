/* MimeTypeMapper.java -- A class for mapping file names to MIME types
   Copyright (C) 1998 Free Software Foundation, Inc.

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

import gnu.classpath.SystemProperties;

import java.io.FileReader;
import java.io.IOException;
import java.io.LineNumberReader;
import java.util.Hashtable;
import java.util.Iterator;
import java.util.Map;
import java.util.NoSuchElementException;
import java.util.StringTokenizer;
import java.util.TreeMap;


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
       { "ai", "application/postscript" }
     , { "aif", "audio/x-aiff" }
     , { "aifc", "audio/x-aiff" }
     , { "aiff", "audio/x-aiff" }
     , { "asc", "text/plain" }
     , { "au", "audio/basic" }
     , { "avi", "video/x-msvideo" }
     , { "bcpio", "application/x-bcpio" }
     , { "bin", "application/octet-stream" }
     , { "bmp", "image/bmp" }
     , { "bz2", "application/x-bzip2" }
     , { "cdf", "application/x-netcdf" }
     , { "chrt", "application/x-kchart" }
     , { "class", "application/octet-stream" }
     , { "cpio", "application/x-cpio" }
     , { "cpt", "application/mac-compactpro" }
     , { "csh", "application/x-csh" }
     , { "css", "text/css" }
     , { "dcr", "application/x-director" }
     , { "dir", "application/x-director" }
     , { "djv", "image/vnd.djvu" }
     , { "djvu", "image/vnd.djvu" }
     , { "dll", "application/octet-stream" }
     , { "dms", "application/octet-stream" }
     , { "doc", "application/msword" }
     , { "dvi", "application/x-dvi" }
     , { "dxr", "application/x-director" }
     , { "eps", "application/postscript" }
     , { "etx", "text/x-setext" }
     , { "exe", "application/octet-stream" }
     , { "ez", "application/andrew-inset" }
     , { "gif", "image/gif" }
     , { "gtar", "application/x-gtar" }
     , { "gz", "application/x-gzip" }
     , { "hdf", "application/x-hdf" }
     , { "hqx", "application/mac-binhex40" }
     , { "htm", "text/html" }
     , { "html", "text/html" }
     , { "ice", "x-conference/x-cooltalk" }
     , { "ief", "image/ief" }
     , { "iges", "model/iges" }
     , { "igs", "model/iges" }
     , { "img", "application/octet-stream" }
     , { "iso", "application/octet-stream" }
     , { "jpe", "image/jpeg" }
     , { "jpeg", "image/jpeg" }
     , { "jpg", "image/jpeg" }
     , { "js", "application/x-javascript" }
     , { "kar", "audio/midi" }
     , { "kil", "application/x-killustrator" }
     , { "kpr", "application/x-kpresenter" }
     , { "kpt", "application/x-kpresenter" }
     , { "ksp", "application/x-kspread" }
     , { "kwd", "application/x-kword" }
     , { "kwt", "application/x-kword" }
     , { "latex", "application/x-latex" }
     , { "lha", "application/octet-stream" }
     , { "lzh", "application/octet-stream" }
     , { "m3u", "audio/x-mpegurl" }
     , { "man", "application/x-troff-man" }
     , { "me", "application/x-troff-me" }
     , { "mesh", "model/mesh" }
     , { "mid", "audio/midi" }
     , { "midi", "audio/midi" }
     , { "mif", "application/vnd.mif" }
     , { "mov", "video/quicktime" }
     , { "movie", "video/x-sgi-movie" }
     , { "mp2", "audio/mpeg" }
     , { "mp3", "audio/mpeg" }
     , { "mpe", "video/mpeg" }
     , { "mpeg", "video/mpeg" }
     , { "mpg", "video/mpeg" }
     , { "mpga", "audio/mpeg" }
     , { "ms", "application/x-troff-ms" }
     , { "msh", "model/mesh" }
     , { "mxu", "video/vnd.mpegurl" }
     , { "nc", "application/x-netcdf" }
     , { "ogg", "application/ogg" }
     , { "pbm", "image/x-portable-bitmap" }
     , { "pdb", "chemical/x-pdb" }
     , { "pdf", "application/pdf" }
     , { "pgm", "image/x-portable-graymap" }
     , { "pgn", "application/x-chess-pgn" }
     , { "png", "image/png" }
     , { "pnm", "image/x-portable-anymap" }
     , { "ppm", "image/x-portable-pixmap" }
     , { "ppt", "application/vnd.ms-powerpoint" }
     , { "ps", "application/postscript" }
     , { "qt", "video/quicktime" }
     , { "ra", "audio/x-realaudio" }
     , { "ram", "audio/x-pn-realaudio" }
     , { "ras", "image/x-cmu-raster" }
     , { "rgb", "image/x-rgb" }
     , { "rm", "audio/x-pn-realaudio" }
     , { "roff", "application/x-troff" }
     , { "rpm", "application/x-rpm" }
     , { "rtf", "text/rtf" }
     , { "rtx", "text/richtext" }
     , { "sgm", "text/sgml" }
     , { "sgml", "text/sgml" }
     , { "sh", "application/x-sh" }
     , { "shar", "application/x-shar" }
     , { "silo", "model/mesh" }
     , { "sit", "application/x-stuffit" }
     , { "skd", "application/x-koan" }
     , { "skm", "application/x-koan" }
     , { "skp", "application/x-koan" }
     , { "skt", "application/x-koan" }
     , { "smi", "application/smil" }
     , { "smil", "application/smil" }
     , { "snd", "audio/basic" }
     , { "so", "application/octet-stream" }
     , { "spl", "application/x-futuresplash" }
     , { "src", "application/x-wais-source" }
     , { "stc", "application/vnd.sun.xml.calc.template" }
     , { "std", "application/vnd.sun.xml.draw.template" }
     , { "sti", "application/vnd.sun.xml.impress.template" }
     , { "stw", "application/vnd.sun.xml.writer.template" }
     , { "sv4cpio", "application/x-sv4cpio" }
     , { "sv4crc", "application/x-sv4crc" }
     , { "swf", "application/x-shockwave-flash" }
     , { "sxc", "application/vnd.sun.xml.calc" }
     , { "sxd", "application/vnd.sun.xml.draw" }
     , { "sxg", "application/vnd.sun.xml.writer.global" }
     , { "sxi", "application/vnd.sun.xml.impress" }
     , { "sxm", "application/vnd.sun.xml.math" }
     , { "sxw", "application/vnd.sun.xml.writer" }
     , { "t", "application/x-troff" }
     , { "tar", "application/x-tar" }
     , { "tcl", "application/x-tcl" }
     , { "tex", "application/x-tex" }
     , { "texi", "application/x-texinfo" }
     , { "texinfo", "application/x-texinfo" }
     , { "tgz", "application/x-gzip" }
     , { "tif", "image/tiff" }
     , { "tiff", "image/tiff" }
     , { "torrent", "application/x-bittorrent" }
     , { "tr", "application/x-troff" }
     , { "tsv", "text/tab-separated-values" }
     , { "txt", "text/plain" }
     , { "ustar", "application/x-ustar" }
     , { "vcd", "application/x-cdlink" }
     , { "vrml", "model/vrml" }
     , { "wav", "audio/x-wav" }
     , { "wbmp", "image/vnd.wap.wbmp" }
     , { "wbxml", "application/vnd.wap.wbxml" }
     , { "wml", "text/vnd.wap.wml" }
     , { "wmlc", "application/vnd.wap.wmlc" }
     , { "wmls", "text/vnd.wap.wmlscript" }
     , { "wmlsc", "application/vnd.wap.wmlscriptc" }
     , { "wrl", "model/vrml" }
     , { "xbm", "image/x-xbitmap" }
     , { "xht", "application/xhtml+xml" }
     , { "xhtml", "application/xhtml+xml" }
     , { "xls", "application/vnd.ms-excel" }
     , { "xml", "text/xml" }
     , { "xpm", "image/x-xpixmap" }
     , { "xsl", "text/xml" }
     , { "xwd", "image/x-xwindowdump" }
     , { "xyz", "chemical/x-xyz" }
     , { "zip", "application/zip" }
    };

  /**
   * The MIME types above are put into this Hashtable for faster lookup.
   */
  private Hashtable<String, String> mime_types
    = new Hashtable<String, String>(150);

  /**
   * Create a new <code>MimeTypeMapper</code> object.
   */
  public MimeTypeMapper()
  {
    for (int i = 0; i < mime_strings.length; i++)
      mime_types.put(mime_strings[i][0], mime_strings[i][1]);

    // Now read from the system mime database, if it exists.  Entries found
    // here override our internal ones.
    try
      {
        // On Linux this usually means /etc/mime.types.
        String file
          = SystemProperties.getProperty("gnu.classpath.mime.types.file");
        if (file != null)
          fillFromFile(mime_types, file);
      }
    catch (IOException ignore)
      {
      }
  }

  public static void fillFromFile (Map<String, String> table, String fname) 
    throws IOException
  {
    LineNumberReader reader = 
      new LineNumberReader (new FileReader (fname));

    while (reader.ready ())
      {
        StringTokenizer tokenizer = 
          new StringTokenizer (reader.readLine ());

        try
          {
            String t = tokenizer.nextToken ();

            if (! t.startsWith ("#"))
              {
                while (true)
                  {
                    // Read the next extension
                    String e = tokenizer.nextToken ();
                    if ((e != null) && (! e.startsWith ("#")))
                      table.put (e, t);
                    else
                      break;
                  }
              }
          }
        catch (NoSuchElementException ex)
          {
            // Do nothing.
          }
      }
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
      return "application/octet-stream";
    else
      return type;
  }

  /**
   * Run this class as a program to create a new mime_strings table.
   */
  public static void main(String[] args) throws IOException
  {
    TreeMap<String, String> map = new TreeMap<String, String>();
    // It is fine to hard-code the name here.  This is only ever
    // used by maintainers, who can hack it if they need to re-run
    // it.
    fillFromFile(map, "/etc/mime.types");
    Iterator<String> it = map.keySet().iterator();
    boolean first = true;
    while (it.hasNext())
      {
        String key = it.next();
        String value = map.get(key);
        // Put the "," first since it is easier to make correct syntax this way.
        System.out.println("      " + (first ? "  " : ", ") 
                           + "{ \"" + key + "\", \"" + value + "\" }");
        first = false;
      }
  }
}
