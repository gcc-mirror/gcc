#include "parts-test-extra-parts-views.h"

ExtraPartsViews::ExtraPartsViews ()
  : ExtraParts () {
}

ExtraPartsViews::~ExtraPartsViews () {}

void
ExtraPartsViews::ToolkitInitialized ()
{
  /* Do something */
  int sum = 0;
  for (int i = 0; i < 10; ++i)
    sum += i;
}
