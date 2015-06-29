#include <BDDNode.h>

extern "C" {

BDD* BDD_new();
BDD* BDD_new_XManagerPDD(XManager* m, DD vNode);
const XManager* BDD_checkSameManager(const BDD* this_ptr, const BDD* other);
int BDD_Variable(BDD* this_ptr);
BDD BDD_Then(const BDD* this_ptr);
BDD BDD_Else(const BDD* this_ptr);
BDD BDD_Exist(BDD* this_ptr, const BDD* cube);
BDD BDD_Universal(BDD* this_ptr, const BDD* cube);
BDD BDD_Restrict(const BDD* this_ptr, const BDD* other);
BDD BDD_Compose(const BDD* this_ptr, int v, const BDD* other);
BDD BDD_Permute(const BDD* this_ptr, const vector<int>* permu);
BDD BDD_AndExist(BDD* this_ptr, const BDD* other, const BDD* cube);
bool BDD_IsComp(BDD* this_ptr);
void BDD_Support(BDD* this_ptr, vector<int>* vars);
const XManager* BDD_manager(const BDD* this_ptr);

// In C we can not overload operators, so we use functions instead.
BDD BDD_Operator_Not       (const BDD* this_ptr); // !
bool BDD_Operator_Equal    (const BDD* this_ptr, const BDD* other); // ==
BDD BDD_Operator_Or        (const BDD* this_ptr, const BDD* other); // +
BDD BDD_Operator_And       (const BDD* this_ptr, const BDD* other); // *
BDD BDD_Operator_Xor       (const BDD* this_ptr, const BDD* other); // ^
BDD BDD_Operator_LessEqual (const BDD* this_ptr, const BDD* other); // <=
BDD BDD_Operator_Nor       (const BDD* this_ptr, const BDD* other); // %
BDD BDD_Operator_Nand      (const BDD* this_ptr, const BDD* other); // |
BDD BDD_Operator_XNor      (const BDD* this_ptr, const BDD* other); // &

DD BDD_Node(const BDD* this_ptr);

XBDDManager* XBDDManager_new(int varCount);
BDD XBDDManager_BddOne(XBDDManager* this_ptr);
BDD XBDDManager_BddZero(XBDDManager* this_ptr);
BDD XBDDManager_BddVar(XBDDManager* this_ptr, int varIndex);
BDD XBDDManager_Ite(XBDDManager* this_ptr, const BDD* f, const BDD* g, const BDD* h);

void XBDDManager_ShowInfo(XBDDManager* this_ptr, double vtime);

const XManager* XBDDManager_manager(const XBDDManager* this_ptr);
}
