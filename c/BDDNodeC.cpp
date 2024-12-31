#include "BDDNodeC.h"

#include "BDDNode.h"
#include <stdio.h>

BDD* BDD_new() { return new BDD(); }

int BDD_Variable(BDD* this_ptr) { return this_ptr->Variable(); }

// TODO change BDD to void returns!

void BDD_Then(BDD* ret_ptr, const BDD* this_ptr) { *ret_ptr = this_ptr->Then(); }

void BDD_Else(BDD* ret_ptr, const BDD* this_ptr) { *ret_ptr = this_ptr->Else(); }

void BDD_Exist(BDD* ret_ptr, BDD* this_ptr, const BDD* cube) { *ret_ptr = this_ptr->Exist(*cube); }

void BDD_Universal(BDD* ret_ptr, BDD* this_ptr, const BDD* cube) { *ret_ptr = this_ptr->Universal(*cube); }

void BDD_Restrict(BDD* ret_ptr, const BDD* this_ptr, const BDD* other) { *ret_ptr = this_ptr->Restrict(*other); }

void BDD_Compose(BDD* ret_ptr, const BDD* this_ptr, int v, const BDD* other) { *ret_ptr = this_ptr->Compose(v, *other); }

void BDD_Permute(BDD* ret_ptr, const BDD* this_ptr, const vector<int>* permu) { *ret_ptr = this_ptr->Permute(*permu); }

void BDD_AndExist(BDD* ret_ptr, BDD* this_ptr, const BDD* other, const BDD* cube) { *ret_ptr = this_ptr->AndExist(*other, *cube); }

bool BDD_IsComp(BDD* this_ptr) { bool b = this_ptr->IsComp(); return b; }

const XManager* BDD_manager(const BDD* this_ptr) { return this_ptr->manager(); }

XBDDManager* XBDDManager_new(int varCount) { return new XBDDManager(varCount); }

void XBDDManager_BddOne(BDD* ret_ptr, XBDDManager* this_ptr) { printf("\nDEBUG BddNodeC.cpp: this_ptr = %p\n\n", this_ptr); *ret_ptr = this_ptr->BddOne(); }

void XBDDManager_BddZero(BDD* ret_ptr, XBDDManager* this_ptr) { *ret_ptr = this_ptr->BddZero(); }

void XBDDManager_BddVar(BDD* ret_ptr, XBDDManager* this_ptr, int varIndex) { *ret_ptr = this_ptr->BddVar(varIndex); }

void XBDDManager_Ite(BDD* ret_ptr, XBDDManager* this_ptr, const BDD* f, const BDD* g, const BDD* h) { *ret_ptr = this_ptr->Ite(*f, *g, *h); }

const XManager* XBDDManager_manager(const XBDDManager* this_ptr) { return this_ptr->manager(); }

void BDD_Operator_Not(BDD* ret_ptr, const BDD *this_ptr) { *ret_ptr = (!*this_ptr); }

bool BDD_Operator_Equal(const BDD* this_ptr, const BDD* other) { bool b = (*this_ptr == *other); return b; }

void BDD_Operator_Or(BDD* ret_ptr, const BDD* this_ptr, const BDD* other) { *ret_ptr = (*this_ptr + *other); }

void BDD_Operator_And(BDD* ret_ptr, const BDD* this_ptr, const BDD* other) { *ret_ptr = (*this_ptr * *other); }

void BDD_Operator_Xor(BDD* ret_ptr, const BDD* this_ptr, const BDD* other) { *ret_ptr = (*this_ptr ^ *other); }

void BDD_Operator_LessEqual(BDD* ret_ptr, const BDD* this_ptr, const BDD* other) { *ret_ptr = (*this_ptr <= *other); }

void BDD_Operator_Nor(BDD* ret_ptr, const BDD* this_ptr, const BDD* other) { *ret_ptr = (*this_ptr % *other); }

void BDD_Operator_Nand(BDD* ret_ptr, const BDD* this_ptr, const BDD* other) { *ret_ptr = (*this_ptr | *other); }

void BDD_Operator_XNor(BDD* ret_ptr, const BDD* this_ptr, const BDD* other) { *ret_ptr = (*this_ptr & *other); }

void XBDDManager_ShowInfo(XBDDManager* this_ptr) { this_ptr->ShowInfo(); }
