/* This struct used to be public, but is now private to the runtime.  */

/*
** Definition of a selector.  Selectors themselves are not unique, but
** the sel_id is a unique identifier.
*/
struct objc_selector
{
  void *sel_id;
  const char *sel_types;
};

inline static BOOL
sel_eq (SEL s1, SEL s2)
{
  if (s1 == 0 || s2 == 0)
    return s1 == s2;
  else
    return s1->sel_id == s2->sel_id;
}
