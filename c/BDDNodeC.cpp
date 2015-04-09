#include "BDDNodeC.h"

#include "BDDNode.h"
#include <stdio.h>

BDD* BDD_new_XManagerPDD(XManager* m, DD vNode) { return new BDD(m, vNode); }

BDD* BDD_new_BDDP(const BDD* right) { return new BDD(*right); }

BDD* BDD_new() { return new BDD(); }

void BDD_delete(BDD* this_ptr) { delete this_ptr; }

int BDD_Variable(BDD* this_ptr) { return this_ptr->Variable(); }

BDD BDD_Then(const BDD* this_ptr) { return this_ptr->Then(); }

BDD BDD_Else(const BDD* this_ptr) { return this_ptr->Else(); }

BDD BDD_Exist(BDD* this_ptr, const BDD* cube) { return this_ptr->Exist(*cube); }

BDD BDD_Universal(BDD* this_ptr, const BDD* cube) { return this_ptr->Universal(*cube); }

BDD BDD_Restrict(const BDD* this_ptr, const BDD* other) { return this_ptr->Restrict(*other); }

BDD BDD_Compose(const BDD* this_ptr, int v, const BDD* other) { return this_ptr->Compose(v, *other); }

BDD BDD_Permute(const BDD* this_ptr, const vector<int>* permu) { return this_ptr->Permute(*permu); }

BDD BDD_AndExist(BDD* this_ptr, const BDD* other, const BDD* cube) { return this_ptr->AndExist(*other, *cube); }

bool BDD_IsComp(BDD* this_ptr) { bool b = this_ptr->IsComp(); return b; }

void BDD_Support(BDD* this_ptr, vector<int>* vars) { this_ptr->Support(*vars); }

const XManager* BDD_manager(const BDD* this_ptr) { return this_ptr->manager(); }

XBDDManager* XBDDManager_new(int varCount) { return new XBDDManager(varCount); }

void XBDDManager_delete(XBDDManager* this_ptr) { delete this_ptr; }

void XBDDManager_Clear(XBDDManager* this_ptr, int varCnt) { this_ptr->Clear(varCnt); }

BDD XBDDManager_BddOne(XBDDManager* this_ptr) { return (BDD) this_ptr->BddOne(); }

BDD XBDDManager_BddZero(XBDDManager* this_ptr) { return (BDD) this_ptr->BddZero(); }

BDD XBDDManager_BddVar(XBDDManager* this_ptr, int varIndex) { BDD bar = this_ptr->BddVar(varIndex); return bar; }

BDD XBDDManager_Ite(XBDDManager* this_ptr, const BDD* f, const BDD* g, const BDD* h) { return this_ptr->Ite(*f, *g, *h); }

const XManager* XBDDManager_manager(const XBDDManager* this_ptr) { return this_ptr->manager(); }

void XBDDManager_ShowInfo(XBDDManager* this_ptr, double vtime) { this_ptr->ShowInfo(vtime); }

BDD BDD_Operator_Not(const BDD* this_ptr) { BDD bar = (! *this_ptr); return bar;  }

bool BDD_Operator_Equal(const BDD* this_ptr, const BDD* other) { bool b = (*this_ptr == *other); return b; }

BDD BDD_Operator_Or(const BDD* this_ptr, const BDD* other) { BDD bar = (*this_ptr + *other); return bar; }

BDD BDD_Operator_And(const BDD* this_ptr, const BDD* other) { return(BDD) (*this_ptr * *other); }

BDD BDD_Operator_Xor(const BDD* this_ptr, const BDD* other) { return(BDD) (*this_ptr ^ *other); }

BDD BDD_Operator_LessThan(const BDD* this_ptr, const BDD* other) { return(BDD) (*this_ptr < *other); }

BDD BDD_Operator_MoreThan(const BDD* this_ptr, const BDD* other) { return(BDD) (*this_ptr > *other); }

BDD BDD_Operator_LessEqual(const BDD* this_ptr, const BDD* other) { return(BDD) (*this_ptr <= *other); }

BDD BDD_Operator_MoreEqual(const BDD* this_ptr, const BDD* other) { return(BDD) (*this_ptr >= *other); }

BDD BDD_Operator_Nor(const BDD* this_ptr, const BDD* other) { return (BDD) (*this_ptr % *other); }

BDD BDD_Operator_Nand(const BDD* this_ptr, const BDD* other) { return(BDD) (*this_ptr | *other); }

BDD BDD_Operator_XNor(const BDD* this_ptr, const BDD* other) { return(BDD) (*this_ptr & *other); }

// void BDD_Show(const BDD* this_ptr) {
//   XManager* mgr = this_ptr->manager();
//   this_ptr->Node() // this gives us a DD ...
//   mgr->PrintNode( ...); // ... but here we want to put in a DdNode
// }
