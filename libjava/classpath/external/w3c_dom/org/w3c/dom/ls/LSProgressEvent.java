/*
 * Copyright (c) 2004 World Wide Web Consortium,
 *
 * (Massachusetts Institute of Technology, European Research Consortium for
 * Informatics and Mathematics, Keio University). All Rights Reserved. This
 * work is distributed under the W3C(r) Software License [1] in the hope that
 * it will be useful, but WITHOUT ANY WARRANTY; without even the implied
 * warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
 *
 * [1] http://www.w3.org/Consortium/Legal/2002/copyright-software-20021231
 */

package org.w3c.dom.ls;

import org.w3c.dom.events.Event;

/**
 *  This interface represents a progress event object that notifies the
 * application about progress as a document is parsed. It extends the
 * <code>Event</code> interface defined in [<a href='http://www.w3.org/TR/2003/NOTE-DOM-Level-3-Events-20031107'>DOM Level 3 Events</a>]
 * .
 * <p> The units used for the attributes <code>position</code> and
 * <code>totalSize</code> are not specified and can be implementation and
 * input dependent.
 * <p>See also the <a href='http://www.w3.org/TR/2004/REC-DOM-Level-3-LS-20040407'>Document Object Model (DOM) Level 3 Load
and Save Specification</a>.
 */
public interface LSProgressEvent extends Event {
    /**
     * The input source that is being parsed.
     */
    public LSInput getInput();

    /**
     * The current position in the input source, including all external
     * entities and other resources that have been read.
     */
    public int getPosition();

    /**
     * The total size of the document including all external resources, this
     * number might change as a document is being parsed if references to
     * more external resources are seen. A value of <code>0</code> is
     * returned if the total size cannot be determined or estimated.
     */
    public int getTotalSize();

}
