@interface Object (Deprecated)

/* The following methods were deprecated in GCC 4.6.0 and will be
   removed in the next GCC release.
*/
+ (int)streamVersion: (TypedStream*)aStream; /* __attribute__ ((deprecated)) */

- read: (TypedStream*)aStream; /* __attribute__ ((deprecated)) */
- write: (TypedStream*)aStream; /* __attribute__ ((deprecated)) */
- awake; /* __attribute__ ((deprecated)) */

@end

