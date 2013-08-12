#include "parts-test-main.h"
#include "parts-test-extra-parts-views.h"

MainParts::MainParts () {}

MainParts::~MainParts ()
{
  for (int i = static_cast<int>(main_extra_parts_.size()) - 1; i >= 0; --i)
    delete main_extra_parts_[i];
  main_extra_parts_.clear();
}

void
MainParts::AddParts (ExtraParts *parts)
{
  main_extra_parts_.push_back (parts);
}


void
MainParts::PreEarlyInitialization (void) 
{
  for (int i = 0; i < main_extra_parts_.size(); ++i)
    main_extra_parts_[i]->PreEarlyInitialization ();
}


int
main (int argc, char **argv)
{
  MainParts *main_parts = new MainParts ();

  main_parts->AddParts (new ExtraPartsViews ());
  main_parts->PreEarlyInitialization ();

  return 0;
}
