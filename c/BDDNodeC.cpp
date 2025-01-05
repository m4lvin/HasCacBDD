#include "BDDNodeC.h"

#include "BDDNode.h"
#include <stdio.h>

BDD* BDD_new() { return new BDD(); }

int BDD_Variable(BDD* this_ptr) { return this_ptr->Variable(); }

BDD* BDD_Then(const BDD* this_ptr) { BDD* b = new BDD(); *b = (this_ptr->Then()); return b; }

BDD* BDD_Else(const BDD* this_ptr) { BDD* b = new BDD(); *b = (this_ptr->Else()); return b; }

BDD* BDD_Exist(BDD* this_ptr, const BDD* cube) { BDD* b = new BDD(); *b = (this_ptr->Exist(*cube)); return b; }

BDD* BDD_Universal(BDD* this_ptr, const BDD* cube) { BDD* b = new BDD(); *b = (this_ptr->Universal(*cube)); return b; }

BDD* BDD_Restrict(const BDD* this_ptr, const BDD* other) { BDD* b = new BDD(); *b = (this_ptr->Restrict(*other)); return b; }

BDD* BDD_Compose(const BDD* this_ptr, int v, const BDD* other) { BDD* b = new BDD(); *b = (this_ptr->Compose(v, *other)); return b; }

BDD* BDD_Permute(const BDD* this_ptr, const vector<int>* permu) { BDD* b = new BDD(); *b = (this_ptr->Permute(*permu)); return b; }

BDD* BDD_AndExist(BDD* this_ptr, const BDD* other, const BDD* cube) { BDD* b = new BDD(); *b = (this_ptr->AndExist(*other, *cube)); return b; }

bool BDD_IsComp(BDD* this_ptr) { bool b = this_ptr->IsComp(); return b; }

const XManager* BDD_manager(const BDD* this_ptr) { return this_ptr->manager(); }

XBDDManager* XBDDManager_new(int varCount) { return new XBDDManager(varCount); }

BDD* XBDDManager_BddOne(XBDDManager* this_ptr) { BDD* b = new BDD(); *b = (this_ptr->BddOne()); return b; }

BDD* XBDDManager_BddZero(XBDDManager* this_ptr) { BDD* b = new BDD(); *b = (this_ptr->BddZero()); return b; }

BDD* XBDDManager_BddVar(XBDDManager* this_ptr, int varIndex) { BDD* b = new BDD(); *b = (this_ptr->BddVar(varIndex)); return b; }

BDD* XBDDManager_Ite(XBDDManager* this_ptr, const BDD* f, const BDD* g, const BDD* h) { BDD* b = new BDD(); *b = (this_ptr->Ite(*f, *g, *h)); return b; }

const XManager* XBDDManager_manager(const XBDDManager* this_ptr) { return this_ptr->manager(); }

BDD* BDD_Operator_Not(const BDD *this_ptr) { BDD* b = new BDD(); *b = ((!*this_ptr)); return b; }

bool BDD_Operator_Equal(const BDD* this_ptr, const BDD* other) { bool b = (*this_ptr == *other); return b; }

BDD* BDD_Operator_Or(const BDD* this_ptr, const BDD* other) { BDD* b = new BDD(); *b = ((*this_ptr + *other)); return b; }

BDD* BDD_Operator_And(const BDD* this_ptr, const BDD* other) { BDD* b = new BDD(); *b = ((*this_ptr * *other)); return b; }

BDD* BDD_Operator_Xor(const BDD* this_ptr, const BDD* other) { BDD* b = new BDD(); *b = ((*this_ptr ^ *other)); return b; }

BDD* BDD_Operator_LessEqual(const BDD* this_ptr, const BDD* other) { BDD* b = new BDD(); *b = ((*this_ptr <= *other)); return b; }

BDD* BDD_Operator_Nor(const BDD* this_ptr, const BDD* other) { BDD* b = new BDD(); *b = (*this_ptr % *other); return b; }

BDD* BDD_Operator_Nand(const BDD* this_ptr, const BDD* other) { BDD* b = new BDD(); *b = (*this_ptr | *other); return b; }

BDD* BDD_Operator_XNor(const BDD* this_ptr, const BDD* other) { BDD* b = new BDD(); *b = (*this_ptr & *other); return b; }

void XBDDManager_ShowInfo(XBDDManager* this_ptr) { this_ptr->ShowInfo(); }
