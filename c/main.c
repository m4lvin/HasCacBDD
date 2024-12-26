#include "BDDNodeC.h"

int main(void) {

  XBDDManager* mgr = XBDDManager_new(1048576);
  printf("%p\n", mgr);

  BDD b = XBDDManager_BddOne(mgr);
  printf("&v = %p\n", &b);

  return 0;
}
