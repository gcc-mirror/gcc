package org.relaxng.datatype;

/**
 * Creates a user-defined type by adding parameters to
 * the pre-defined type.
 * 
 * @author <a href="mailto:jjc@jclark.com">James Clark</a>
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
public interface DatatypeBuilder {
	
	/**
	 * Adds a new parameter.
	 *
	 * @param name
	 *		The name of the parameter to be added.
	 * @param strValue
	 *		The raw value of the parameter. Caller may not normalize
	 *		this value because any white space is potentially significant.
	 * @param context
	 *		The context information which can be used by the callee to
	 *		acquire additional information. This context object is
	 *		valid only during this method call. The callee may not
	 *		keep a reference to this object.
	 * @exception	DatatypeException
	 *		When the given parameter is inappropriate for some reason.
	 *		The callee is responsible to recover from this error.
	 *		That is, the object should behave as if no such error
	 *		was occured.
	 */
	void addParameter( String name, String strValue, ValidationContext context )
		throws DatatypeException;
	
	/**
	 * Derives a new Datatype from a Datatype by parameters that
	 * were already set through the addParameter method.
	 * 
	 * @exception DatatypeException
	 *		DatatypeException must be thrown if the derivation is
	 *		somehow invalid. For example, a required parameter is missing,
	 *		etc. The exception should contain a diagnosis message
	 *		if possible.
	 */
	Datatype createDatatype() throws DatatypeException;
}
