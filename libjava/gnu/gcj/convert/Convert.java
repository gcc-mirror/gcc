/* Copyright (C) 1999  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;
import java.io.*;

public class Convert
{
  static void error (String message)
  {
    System.err.print("convert: ");
    System.err.println(message);
    System.err.println("Usage:  convert [--from srcEncoding] [--to dstEncoding]");
    System.err.println("  [inputfile [outputfile]]");
    System.exit(-1);
  }

  static void missing (String arg)
  {
    error("missing arg after `" + arg + "' option");
  }

  public static void main (String[] args)
  {
    String inName = "-";
    String outName = "-";
    String inEncodingName = null;
    String outEncodingName = "JavaSrc";
    int seenNames = 0;
    boolean reverse = false;

    for (int i = 0;  i < args.length;  i++)
      {
	String arg = args[i];
	if (arg.length() == 0)
	  error("zero-length argument");
	if (arg.charAt(0) == '-')
	  {
	    if (arg.equals("-encoding") || arg.equals("--encoding")
		|| args.equals("-from") || arg.equals("--from"))
	      {
		if (++i == args.length) missing(arg);
		inEncodingName = args[i];
	      }
	    else if (arg.equals("-to") || arg.equals("--to"))
	      {
		if (++i == args.length) missing(arg);
		outEncodingName = args[i];
	      }
	    else if (arg.equals("-i"))
	      {
		if (++i == args.length) missing(arg);
		inName = args[i];
	      }
	    else if (arg.equals("-o"))
	      {
		if (++i == args.length) missing(arg);
		outName = args[i];
	      }
	    else if (arg.equals("-reverse") || arg.equals("--reverse"))
	      {
		reverse = true;
	      }
	    else if (arg.equals("-"))
	      {
		switch (seenNames)
		  {
		  case 0:
		    inName = "-";
		    seenNames++;
		    break;
		  case 1:
		    outName = "-";
		    seenNames++;
		    break;
		  default:
		    error("too many `-' arguments");
		  }
	      }
	    else
	      error("unrecognized argument `" + arg + "'");
	  }
	else
	  {
	    switch (seenNames)
	      {
	      case 0:
		inName = arg;
		seenNames++;
		break;
	      case 1:
		outName = arg;
		seenNames++;
		break;
	      default:
		error("too many filename arguments");
	      }
	  }
      }

    if (reverse)
      {
	String tmp = inEncodingName;
	inEncodingName = outEncodingName;
	outEncodingName = tmp;
      }

    try
      {
	BytesToUnicode inDecoder
	  = inEncodingName == null ? BytesToUnicode.getDefaultDecoder()
	  : BytesToUnicode.getDecoder(inEncodingName);
	UnicodeToBytes outEncoder
	  = outEncodingName == null ? UnicodeToBytes.getDefaultEncoder()
	  : UnicodeToBytes.getEncoder(outEncodingName);
	InputStream inStream = inName == "-" ? System.in
	  : new FileInputStream(inName);
	OutputStream outStream;
	if (outName == "-")
	  outStream = System.out;
	else
	  outStream = new FileOutputStream(outName);
	InputStreamReader in
	  = new InputStreamReader(inStream, inEncodingName);
	OutputStreamWriter out
	  = new OutputStreamWriter(outStream, outEncodingName);
	char[] buffer = new char[2048];
	for (;;)
	  {
	    int count = in.read(buffer);
	    if (count < 0)
	      break;
	    out.write(buffer, 0, count);
	  }

	in.close();
	out.close();
      }
    catch (java.io.IOException ex)
      {
	System.err.print("convert exception: ");
	System.err.println(ex);
	System.exit(-1);
      }
  }
}
