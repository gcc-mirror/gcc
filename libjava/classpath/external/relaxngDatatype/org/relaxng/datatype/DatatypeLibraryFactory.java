package org.relaxng.datatype;

/**
 * Factory class for the DatatypeLibrary class.
 * 
 * <p>
 * The datatype library should provide the implementation of
 * this interface if it wants to be found by the schema processors.
 * The implementor also have to place a file in your jar file.
 * See the reference datatype library implementation for detail.
 * 
 * @author <a href="mailto:jjc@jclark.com">James Clark</a>
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
public interface DatatypeLibraryFactory
{
	/**
	 * Creates a new instance of a DatatypeLibrary that supports 
	 * the specified namespace URI.
	 * 
	 * @return
	 *		<code>null</code> if the specified namespace URI is not
	 *		supported.	
	 */
	DatatypeLibrary createDatatypeLibrary( String namespaceURI );
}
