/* The following methods were deprecated in GCC 4.6.0 and will be
   removed in the next GCC release.  */
@interface Object (Deprecated)
/* Initializing classes and instances */
+ initialize;
- init;

/* Creating, freeing, and copying instances */
+ new;
+ alloc;
- free;
- copy;
- shallowCopy;
- deepen;
- deepCopy;

/* Identifying classes */
- (Class)superClass;
- (MetaClass)metaClass;
- (const char *)name;

/* Identifying and comparing objects */
- self;
- (unsigned int)hash;
- (int)compare:(id)anotherObject;

/* Testing object type */
- (BOOL)isMetaClass;
- (BOOL)isClass;
- (BOOL)isInstance;

/* Testing inheritance relationships */
- (BOOL)isKindOf:(Class)aClassObject;
- (BOOL)isMemberOf:(Class)aClassObject;
- (BOOL)isKindOfClassNamed:(const char *)aClassName;
- (BOOL)isMemberOfClassNamed:(const char *)aClassName;

/* Testing class functionality */
+ (BOOL)instancesRespondTo:(SEL)aSel;
- (BOOL)respondsTo:(SEL)aSel;

/* Testing protocol conformance */
- (BOOL)conformsTo:(Protocol*)aProtocol;

/* Introspection */
+ (IMP)instanceMethodFor:(SEL)aSel;
- (IMP)methodFor:(SEL)aSel;
+ (struct objc_method_description *)descriptionForInstanceMethod:(SEL)aSel;
- (struct objc_method_description *)descriptionForMethod:(SEL)aSel;

/* Sending messages determined at run time */
- perform:(SEL)aSel;
- perform:(SEL)aSel with:anObject;
- perform:(SEL)aSel with:anObject1 with:anObject2;

/* Forwarding */
- (retval_t)forward:(SEL)aSel :(arglist_t)argFrame;
- (retval_t)performv:(SEL)aSel :(arglist_t)argFrame;

/* Posing */
+ poseAs:(Class)aClassObject;
- (Class)transmuteClassTo:(Class)aClassObject;

/* Enforcing intentions */
- subclassResponsibility:(SEL)aSel;
- notImplemented:(SEL)aSel;
- shouldNotImplement:(SEL)aSel;

/* Error handling */
- doesNotRecognize:(SEL)aSel;
- error:(const char *)aString, ...;

/* Archiving */
+ (int)version;
+ setVersion:(int)aVersion;

+ (int)streamVersion: (TypedStream*)aStream; /* __attribute__ ((deprecated)) */

- read: (TypedStream*)aStream; /* __attribute__ ((deprecated)) */
- write: (TypedStream*)aStream; /* __attribute__ ((deprecated)) */
- awake; /* __attribute__ ((deprecated)) */

@end

