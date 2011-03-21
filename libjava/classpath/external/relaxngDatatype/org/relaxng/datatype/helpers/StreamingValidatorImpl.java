package org.relaxng.datatype.helpers;

import org.relaxng.datatype.*;

/**
 * Dummy implementation of {@link DatatypeStreamingValidator}.
 *
 * <p>
 * This implementation can be used as a quick hack when the performance
 * of streaming validation is not important. And this implementation
 * also shows you how to implement the DatatypeStreamingValidator interface.
 *
 * <p>
 * Typical usage would be:
 * <PRE><XMP>
 * class MyDatatype implements Datatype {
 *     ....
 *     public DatatypeStreamingValidator createStreamingValidator( ValidationContext context ) {
 *         return new StreamingValidatorImpl(this,context);
 *     }
 *     ....
 * }
 * </XMP></PRE>
 *
 * @author <a href="mailto:kohsuke.kawaguchi@sun.com">Kohsuke KAWAGUCHI</a>
 */
public final class StreamingValidatorImpl implements DatatypeStreamingValidator {

        /** This buffer accumulates characters. */
        private final StringBuffer buffer = new StringBuffer();

        /** Datatype obejct that creates this streaming validator. */
        private final Datatype baseType;

        /** The current context. */
        private final ValidationContext context;

        public void addCharacters( char[] buf, int start, int len ) {
                // append characters to the current buffer.
                buffer.append(buf,start,len);
        }

        public boolean isValid() {
                return baseType.isValid(buffer.toString(),context);
        }

        public void checkValid() throws DatatypeException {
                baseType.checkValid(buffer.toString(),context);
        }

        public StreamingValidatorImpl( Datatype baseType, ValidationContext context ) {
                this.baseType = baseType;
                this.context = context;
        }
}
