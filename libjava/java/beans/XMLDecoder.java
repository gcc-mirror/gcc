/* java.beans.XMLDecoder --
   Copyright (C) 2004, 2005  Free Software Foundation, Inc.

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


package java.beans;

import gnu.java.beans.decoder.DefaultExceptionListener;
import gnu.java.beans.decoder.PersistenceParser;

import java.io.IOException;
import java.io.InputStream;
import java.util.Iterator;
import java.util.NoSuchElementException;

/**
 * The XMLDecoder reads XML data that is structured according to
 * <a href="http://java.sun.com/products/jfc/tsc/articles/persistence3/javabeans.dtd">this</a> DTD
 * and creates objects according to the content. Usually such data is generated using the
 * {@link XMLEncoder} class.
 * <p>
 * An example XML document might look like this:
 * <code>
 * &lt;java&gt;
 * 	&lt;string&gt;Hello World&lt;/string&gt;
 * 	&lt;int&gt;200&lt;/int&gt;
 * &lt;/java&gt;
 * </code>
 * <p>To read the <code>String</code> and the <code>Integer</code> instance the following can be used (assume
 * the XML data can be obtained from the InputStream):</p>
 * <code>
 * XMLDecoder decoder = new XMLDecoder(inputStreamContainingXMLData);
 * String message = (String) decoder.readObject();
 * Integer number = (Integer) decoder.readObject();
 * </code>
 * <p>Besides this basic functionality the <code>XMLDecoder</code> has some more features that might come
 * handy in certain situations:</p>
 * <p>An owner object can be set using the <code>setOwner</code> method which can then be accessed when
 * decoding. This feature is only useful if the XML data is aware of the owner object. Such data may
 * look like this (assume that the owner object is a JFrame instance):</p>
 * <code> 
 * &lt;java&gt;
 * 	&lt;void method="getOwner"&gt;
 * 		&lt;void method="setVisible"&gt;
 * 			&lt;boolean&gt;true&lt;boolean&gt;
 * 		&lt;/void&gt;
 *  &lt;/void&gt;
 * &lt;/java&gt;
 * </code>
 * This accesses the <code>JFrame</code> and makes it visible using the <code>setVisible</code> method.
 * <p>Please note that changing the owner <b>after</b> the having read the first object has no effect,
 * because all object have been decoded then.</p>
 * <p>If the <code>XMLDecoder</code> is created with no {@link ExceptionListener} instance a default one
 * is used that prints an error message to <code>System.err</code> whenever a recoverable exception
 * is thrown. Recovarable exceptions occur when the XML data cannot be interpreted correctly (e.g
 * unknown classes or methods, invocation on null, ...). In general be very careful when the
 * <code>XMLDecoder</code> provoked such exceptions because the resulting object(s) may be in an
 * undesirable state.</p>
 * <p>Note that changing the ExceptionListener instance after <code>readObject</code> has been called
 * once has no effect because the decoding is completed then.</p>
 * <p>At last one can provide a specific <code>ClassLoader</code> which is then used when <code>Class</code>
 * objects are accessed. See {@link java.lang.Class#forName(String, boolean, ClassLoader)} for details
 * on this.</p>
 * <p>Note: If the <code>InputStream</code> instance given to any of the constructors is <code>null</code>
 * the resulting <code>XMLDecoder</code> will be silently (without any exception) useless. Each call
 * to <code>readObject</code> will return <code>null</code> and never throws an
 * <code>ArrayIndexOutOfBoundsException</code>.</p>
 *  
 * @author Robert Schuster
 * @since 1.4
 * @status updated to 1.5
 */
public class XMLDecoder
{
	private Object owner;

	private ExceptionListener exceptionListener;

	private InputStream inputStream;

	private boolean isStreamClosed;

	private ClassLoader classLoader;

	private Iterator iterator;

	/** Creates a XMLDecoder instance that parses the XML data of the given input stream.
	 * Using this constructor no special ClassLoader, a default ExceptionListener
	 * and no owner object is used.
	 * 
	 * @param in InputStream to read XML data from.
	 */
	public XMLDecoder(InputStream in)
	{
		this(in, null);
	}

	/** Creates a XMLDecoder instance that parses the XML data of the given input stream.
	 * Using this constructor no special ClassLoader and a default ExceptionListener
	 * is used.
	 * 
	 * @param in InputStream to read XML data from.
	 * @param owner Owner object which can be accessed and modified while parsing.
	 */
	public XMLDecoder(InputStream in, Object owner)
	{
		this(in, owner, null);
	}

	/** Creates a XMLDecoder instance that parses the XML data of the given input stream.
	 * If the ExceptionListener argument is null a default implementation is used.
	 * 
	 * @param in InputStream to read XML data from.
	 * @param owner Owner object which can be accessed and modified while parsing.
	 * @param exceptionListener ExceptionListener instance to which exception notifications are send.
	 */
	public XMLDecoder(
		InputStream in,
		Object owner,
		ExceptionListener exceptionListener)
	{
		this(
			in,
			owner,
			exceptionListener,
			Thread.currentThread().getContextClassLoader());
	}

	/** Creates a XMLDecoder instance that parses the XML data of the given input stream.
	 * If the ExceptionListener argument is null a default implementation is used.
	 * 
	 * @param in InputStream to read XML data from.
	 * @param owner Owner object which can be accessed and modified while parsing.
	 * @param exceptionListener ExceptionListener instance to which exception notifications are send.
	 * @param cl ClassLoader instance that is used for calls to <code>Class.forName(String, boolean, ClassLoader)</code>
	 * @since 1.5
	 */
	public XMLDecoder(
		InputStream in,
		Object owner,
		ExceptionListener listener,
		ClassLoader cl)
	{
		// initially here was a check for the validity of the InputStream argument but some
		// great engineers decided that this API should silently discard this and behave rather
		// odd: readObject will always return null ...
		inputStream = in;

		setExceptionListener(listener);

		// validity of this object is checked in Class.forName() and therefore may be null
		classLoader = cl;

		this.owner = owner;
	}

	/** Closes the stream associated with this decoder. This should be done after having read all 
	 * decoded objects.
	 * <p>See the description of the {@link #readObject()} for the effect caused by <code>close</code>.</p> 
	 */
	public void close()
	{
		if (isStreamClosed)
		{
			return;
		}

		try
		{
			inputStream.close();
			isStreamClosed = true;
		}
		catch (IOException e)
		{
			// bad style forced by original API design ... 
		}
	}

	/** Returns the ExceptionListener instance associated with this decoder.
	 * <p>See the description of {@link XMLDecoder} class for more information on the ExceptionListener.</p>
	 * 
	 * @return Current ExceptionListener of the decoder.
	 */
	public ExceptionListener getExceptionListener()
	{
		return exceptionListener;
	}

	/** Returns the owner object of the decoder. This method is usually called
	 * from within the parsed XML data.
	 * <p>See the description of {@link XMLDecoder} class for more information on the owner object.</p>
	 * 
	 * @return The owner object of this decoder.
	 */
	public Object getOwner()
	{
		return owner;
	}

	/** Returns the next available decoded object.
	 * <p>Note that the actual decoding takes place when the method is called for the first time.</p>
	 * <p>If the <code>close</code> method was already called a <code>NoSuchElementException</code>
	 * is thrown.</p>
	 * <p>If the InputStream instance used in the constructors was <code>null</code> this method
	 * will always return <code>null</code> itself.</p>
	 * 
	 * @return The next object in a sequence decoded from XML data.
	 * @throws ArrayIndexOutOfBoundsException When no more objects are available.
	 */
	public Object readObject() throws ArrayIndexOutOfBoundsException
	{
		// note: the RI does it this way ...
		if(inputStream == null) {
			return null;
		}
		
		// note: the original API documentation says nothing on what to do
		// when the stream was closed before readObject is called but it actually
		// throws a NoSuchElementException - this behaviour is imitated here
		if (isStreamClosed)
		{
			throw new NoSuchElementException("Cannot read any objects - XMLDecoder was already closed.");
		}

		// creates the PersistenceParser (doing the parsing and decoding) and returns its
		// Iterator on first invocation
		if (iterator == null)
		{
			iterator =
				new PersistenceParser(
					inputStream,
					exceptionListener,
					classLoader,
					this)
					.iterator();
		}

		// note: done according to the official documentation
		if (!iterator.hasNext())
		{
			throw new ArrayIndexOutOfBoundsException("No more objects available from this XMLDecoder.");
		}

		// returns just the next object if there was no problem
		return iterator.next();
	}

	/** Sets the ExceptionListener instance to which notifications of exceptions are send
	 * while parsing the XML data.
	 * <p>See the description of {@link XMLDecoder} class for more information on the ExceptionListener.</p>
	 *
	 * @param listener
	 */
	public void setExceptionListener(ExceptionListener listener)
	{
		// uses a default implementation when null 
		if (listener == null)
		{
			listener = new DefaultExceptionListener();
		}
		exceptionListener = listener;
	}

	/** Sets the owner object which can be accessed from the parsed XML data.
	 * <p>See the description of {@link XMLDecoder} class for more information on the owner object.</p>
	 * 
	 * @param newOwner
	 */
	public void setOwner(Object newOwner)
	{
		owner = newOwner;
	}

}
