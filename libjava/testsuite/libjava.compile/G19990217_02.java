/*-------------------------------------------------------------------------*/
/* File name : G19990217_02                                                */
/*           :                                                             */
/* Cause     :                                                             */
/*           :                                                             */
/* Message   : Can't access class `foo.bar'.                               */
/*           : Only public classes and interfaces in other packages can be */
/*           : accessed.                                                   */
/*           : public class G19990217_02 extends bar {                     */
/*           :                                   ^                         */
/*-------------------------------------------------------------------------*/
package foo;
public class G19990217_02 extends bar {
}
class bar {
}
