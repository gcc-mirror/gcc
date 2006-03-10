======================================================================
           README FILE FOR DATATYPE INTERFACES FOR RELAX NG
======================================================================



RELAX NG supports multiple datatype vocabularies. To achive this, an
interface between datatype vocabularies and schema processors is 
necessary. This interface is intended to be a standard Java interface
for this purpose.


----------------------------------------------------------------------
LICENSE
----------------------------------------------------------------------

See copying.txt.

Note: this license is the BSD license.



----------------------------------------------------------------------
FOR DEVELOPER
----------------------------------------------------------------------

If you are planning to implement a datatype library, A sample datatype
library implementation by James Clark is available at [1], which
comes with documentation and source code.

If you are planning to implement a schema processor, then don't forget
to check out org.relaxng.datatype.helpers.DatatypeLibraryLoader, as 
this allows you to dynamically locate datatype implementations.


----------------------------------------------------------------------
LINKS
----------------------------------------------------------------------

* OASIS RELAX NG TC
   http://www.oasis-open.org/committees/relax-ng/
* RELAX home page
   http://www.xml.gr.jp/relax/


----------------------------------------------------------------------
REFERENCES
----------------------------------------------------------------------
[1] Sample datatype library implementation by James Clark
    http://www.thaiopensource.com/relaxng/datatype-sample.zip

Document written by Kohsuke Kawaguchi (kohsuke.kawaguchi@sun.com)
======================================================================
END OF README
