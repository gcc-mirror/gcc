/* Copyright (C) 1999, 2000, 2001  Free Software Foundation

   This file is part of libgcj.

This software is copyrighted work licensed under the terms of the
Libgcj License.  Please consult the file "LIBGCJ_LICENSE" for
details.  */

package gnu.gcj.convert;

public abstract class BytesToUnicode extends IOConverter
{
  /** Buffer to read bytes from.
   * The characters inbuffer[inpos] ... inbuffer[inlength-1] are available. */
  public byte[] inbuffer;
  /** Starting index in buffer to read bytes from. */
  public int inpos;
  /** End of valid bytes in buffer. */
  public int inlength;

  // The name of the default encoding.
  static String defaultEncoding;

  /* These keep a small cache of decoders for reuse.  The array holds
     the actual decoders.  The currCachePos is the next value we are
     going to replace in the cache.  We don't just throw the data away
     if the cache is full, because if the cache filled up with stuff
     we don't need then the cache would be worthless.  We instead
     circulate through the cache the implement kind of an LRU
     algorithm. */
  private static final int CACHE_SIZE = 4;  // A power of 2 for speed
  private static BytesToUnicode[] decoderCache
    = new BytesToUnicode[CACHE_SIZE];
  private static int currCachePos = 0;

  public abstract String getName();

  public static BytesToUnicode getDefaultDecoder()
  {
    try
      {
	synchronized (BytesToUnicode.class)
	  {
	    if (defaultEncoding == null)
	      {
		String encoding
		  = canonicalize (System.getProperty("file.encoding",
						     "8859_1"));
		String className = "gnu.gcj.convert.Input_" + encoding;
		try
		  {
		    Class defaultDecodingClass = Class.forName(className);
		    defaultEncoding = encoding;
		  }
		catch (ClassNotFoundException ex)
		  {
		    throw new NoClassDefFoundError("missing default encoding "
						   + encoding + " (class "
						   + className
						   + " not found)");
		  }
	      }
	  }
	return getDecoder (defaultEncoding);
      }
    catch (Throwable ex)
      {
	return new Input_8859_1();
      }
  }

  /** Get a byte-stream->char-stream converter given an encoding name. */
  public static BytesToUnicode getDecoder (String encoding)
    throws java.io.UnsupportedEncodingException
  {
    /* First hunt in our cache to see if we have a decoder that is
       already allocated. */
    synchronized (BytesToUnicode.class)
      {
	int i;
	for (i = 0; i < decoderCache.length; ++i)
	  {
	    if (decoderCache[i] != null
		&& encoding.equals(decoderCache[i].getName ()))
	      {
		BytesToUnicode rv = decoderCache[i];
		decoderCache[i] = null;
		return rv;
	    }
	  }
      }

    // It's not in the cache, so now we have to do real work.
    String className = "gnu.gcj.convert.Input_" + canonicalize (encoding);
    Class decodingClass;
    try 
      { 
	decodingClass = Class.forName(className); 
	return (BytesToUnicode) decodingClass.newInstance();
      } 
    catch (Throwable ex) 
      { 
	try
	  {
	    // We pass the original name to iconv and let it handle
	    // its own aliasing.
	    return new Input_iconv (encoding);
	  }
	catch (Throwable _)
	  {
	    throw new java.io.UnsupportedEncodingException(encoding
							   + " (" + ex + ')');
	  }
      }
  }

  /** Make input bytes available to the conversion.
   * @param buffer source of input bytes
   * @param pos index of first available byte
   * @param length one more than index of last available byte
   */
  public final void setInput(byte[] buffer, int pos, int length)
  {
    inbuffer = buffer;
    inpos = pos;
    inlength = length;
  }

  /** Convert bytes to chars.
   * Input bytes are taken from this.inbuffer.  The available input
   * bytes start at inbuffer[inpos], and end at inbuffer[inlength-1].
   * @param outbuffer buffer for the converted character
   * @param outpos position in buffer to start putting converted characters
   * @param count the maximum number of characters to convert
   * @return number of chars placed in outbuffer.
   * Also, this.inpos is incremented by the number of bytes consumed.
   *
   * (Note the asymmetry in that the input upper bound is inbuffer[inlength-1],
   * while the output upper bound is outbuffer[outpos+count-1].  The
   * justification is that inlength is like the count field of a
   * BufferedInputStream, while the count parameter is like the
   * length parameter of a read request.)  The count parameter is
   * also defined to be <= outbuffer.length - outpos (per the specification
   * of the length parameter for a read request).
   */
  public abstract int read (char[] outbuffer, int outpos, int count);

  /** Indicate that the converter is resuable.
   * This class keeps track of converters on a per-encoding basis.
   * When done with an encoder you may call this method to indicate
   * that it can be reused later.
   */
  public void done ()
  {
    synchronized (BytesToUnicode.class)
      {
	this.inbuffer = null;
	this.inpos = 0;
	this.inlength = 0;

	decoderCache[currCachePos] = this;
	currCachePos = (currCachePos + 1) % CACHE_SIZE;
      }
  }
}
