#ifndef _EXTRA_PARTS_VIEWS_H_
#define _EXTRA_PARTS_VIEWS_H_

#include "parts-test-extra-parts.h"

class ExtraPartsViews : public ExtraParts {
 public:
  ExtraPartsViews ();
  virtual ~ExtraPartsViews ();

  virtual void ToolkitInitialized ();
};

#endif /* _EXTRA_PARTS_VIEWS_H_ */
