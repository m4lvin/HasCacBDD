#include <BDDNode.h>

extern "C" {

BDD* BDD_new();
const XManager* BDD_checkSameManager(const BDD* this_ptr, const BDD* other);
int BDD_Variable(BDD* this_ptr);
void BDD_Then(BDD* ret_ptr, const BDD* this_ptr);
void BDD_Else(BDD* ret_ptr, const BDD* this_ptr);
void BDD_Exist(BDD* ret_ptr, BDD* this_ptr, const BDD* cube);
void BDD_Universal(BDD* ret_ptr, BDD* this_ptr, const BDD* cube);
void BDD_Restrict(BDD* ret_ptr, const BDD* this_ptr, const BDD* other);
void BDD_Compose(BDD* ret_ptr, const BDD* this_ptr, int v, const BDD* other);
void BDD_Permute(BDD* ret_ptr, const BDD* this_ptr, const vector<int>* permu);
void BDD_AndExist(BDD* ret_ptr, BDD* this_ptr, const BDD* other, const BDD* cube);
bool BDD_IsComp(BDD* this_ptr);
const XManager* BDD_manager(const BDD* this_ptr);

// In C we can not overload operators, so we use functions instead.
void BDD_Operator_Not       (BDD* ret_ptr, const BDD* this_ptr); // !

bool BDD_Operator_Equal    (const BDD* this_ptr, const BDD* other); // ==
void BDD_Operator_Or        (BDD* ret_ptr, const BDD* this_ptr, const BDD* other); // +
void BDD_Operator_And       (BDD* ret_ptr, const BDD* this_ptr, const BDD* other); // *
void BDD_Operator_Xor       (BDD* ret_ptr, const BDD* this_ptr, const BDD* other); // ^
void BDD_Operator_LessEqual (BDD* ret_ptr, const BDD* this_ptr, const BDD* other); // <=
void BDD_Operator_Nor       (BDD* ret_ptr, const BDD* this_ptr, const BDD* other); // %
void BDD_Operator_Nand      (BDD* ret_ptr, const BDD* this_ptr, const BDD* other); // |
void BDD_Operator_XNor      (BDD* ret_ptr, const BDD* this_ptr, const BDD* other); // &


XBDDManager* XBDDManager_new(int varCount);
void XBDDManager_BddOne(BDD* ret_ptr, XBDDManager* this_ptr);
void XBDDManager_BddZero(BDD* ret_ptr, XBDDManager* this_ptr);
void XBDDManager_BddVar(BDD* ret_ptr, XBDDManager* this_ptr, int varIndex);
void XBDDManager_Ite(BDD* ret_ptr, XBDDManager* this_ptr, const BDD* f, const BDD* g, const BDD* h);

void XBDDManager_ShowInfo(XBDDManager* this_ptr);

const XManager* XBDDManager_manager(const XBDDManager* this_ptr);
}
