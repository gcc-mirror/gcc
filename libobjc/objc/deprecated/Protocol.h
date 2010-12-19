/* The following methods were deprecated in GCC 4.6.0 and will be
   removed in the next GCC release.  */
@interface Protocol (Deprecated)
/* Obtaining attributes intrinsic to the protocol */
- (const char *)name;

/* Testing protocol conformance */
- (BOOL) conformsTo: (Protocol *)aProtocolObject;

/* Looking up information specific to a protocol */
- (struct objc_method_description *) descriptionForInstanceMethod:(SEL)aSel;
- (struct objc_method_description *) descriptionForClassMethod:(SEL)aSel;
@end
