#include <iostream>
#include <stdlib.h>
#include <algorithm>
#include <vector>
using namespace std;

#define UNDEF -1
#define TRUE 1
#define FALSE 0


/*
Millores:

1- Propagació eficient de les variables amb OcurrList, on guardarem per a cada variable i la seva negada 
amb quines altres variables comparteixen una o mes clausules per propagarlas si fem una decisió (readClauses i propagateGivesConflict,indexOfNextLitToPropagate..)
DONE

2-Afegir un heuristic per triar el millor literal sobre el qual decidir (getNextDecisionLiteral)
    Algunes idees:
                    1) Tenir un contador de aparicions de una variable a les clausules i escollir en cada cas la variable indefinida amb el contador mes altres
                    2) Semblant a l'anterior, pero amb un contador per a la variable en positiu (c+) i un altre per la negativa (c-). Si surt moltes vegades en positiu
                    farem la decisio en negatiu i viceversa, ja que es com es provocaran mes conflictes
                    3) MILLOR >> Tenir un contador del nombre de vegades que una variable ha aparegut en un conflicte, i cada N iteracions (conflictes) dividim el index de 
                    conflicte /2, per tenir mes en compte els conflictes recients

*/

uint numVars;
uint numClauses;
vector<vector<int> > clauses;
vector<int> model;
vector<int> modelStack;
uint indexOfNextLitToPropagate;
uint decisionLevel;

// per a cada literal guardem a quines clausules es troba
vector<vector<int> > positiveLiteralOcurrences;
vector<vector<int> > negativeLiteralOcurrences;
vector<int> numConflicts;
int TotalConflicts;
int TotalDecisions;

void writeOcurrenceLists(){
  cout << "literals positius: " << endl;
  for (uint i=1;i<positiveLiteralOcurrences.size();++i)
  {
    cout << i << ": ";
    for (uint j=0;j<positiveLiteralOcurrences[i].size();++j)
      cout << positiveLiteralOcurrences[i][j] << ", ";
    cout << endl;
  }

  cout << "literals negatius: " << endl;
  for (uint i=1;i<negativeLiteralOcurrences.size();++i)
  {
    cout << (i*-1) << ": ";
    for (uint j=0;j<negativeLiteralOcurrences[i].size();++j)
      cout << negativeLiteralOcurrences[i][j] << ", ";
    cout << endl;
  }
  
}

void readClauses(){

  // Skip comments
  char c = cin.get();
  while (c == 'c') {
    while (c != '\n') c = cin.get();
    c = cin.get();
  }  

  // Read "cnf numVars numClauses"
  string aux;
  cin >> aux >> numVars >> numClauses;
  clauses.resize(numClauses);

  positiveLiteralOcurrences.resize(numVars+1);
  negativeLiteralOcurrences.resize(numVars+1);
  numConflicts.resize(numVars+1,1);
  TotalConflicts=0;
  TotalDecisions=0;

  // Read clauses
  for (uint i = 0; i < numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0) {
        clauses[i].push_back(lit);
        if(lit > 0) positiveLiteralOcurrences[lit].push_back(i);
        else negativeLiteralOcurrences[-lit].push_back(i); 

        // considerem com a conflicte les aparicions per fer primer les q surten mes
        ++numConflicts[abs(lit)];  
    }
  }    
}

  // Read clauses
  /*writeOcurrenceLists();
  cout << "start" << endl;
  for (uint i = 0; i < numClauses; ++i) {
    int lit;
    while (cin >> lit and lit != 0) {
      clauses[i].push_back(lit);
    }

    for (uint j=0;j<clauses[i].size();++j) {
      lit= clauses[i][j];
      int pos = 0;

      while (pos < clauses[i].size()){

        if(clauses[i][pos]!=lit){
          if(lit > 0) positiveLiteralOcurrences[lit].push_back(clauses[i][pos]);
          else negativeLiteralOcurrences[-lit].push_back(clauses[i][pos]);
        }
        ++pos;
      }
    }
    cout << "Clausula " << i << endl;
    writeOcurrenceLists();
    cout << endl;
  }*/ 




int currentValueInModel(int lit){
  if (lit >= 0) return model[lit];
  else {
    if (model[-lit] == UNDEF) return UNDEF;
    else return 1 - model[-lit];
  }
}


void setLiteralToTrue(int lit){
  modelStack.push_back(lit);
  if (lit > 0) model[lit] = TRUE;
  else model[-lit] = FALSE;   
}

void update_conflicts( int clauseID ){

  for(uint i = 0; i < clauses[clauseID].size();++i){
    int value = clauses[clauseID][i];
    value=abs(value);

    numConflicts[value]+=1;
  }
  if(TotalConflicts %(numClauses) == 0)
    //si numConflicts[i] es 0 peta ???
    for(uint i=0;i<numConflicts.size();++i) if(numConflicts[i]>1) numConflicts[i]/=2;


}
bool propagateGivesConflict ( ) {
  while ( indexOfNextLitToPropagate < modelStack.size() ) {

    vector<int> *propags;
    int value = modelStack[indexOfNextLitToPropagate];

    // aixo va al reves ya que si el literal esta en negatiu on es poden crear conflictes es on surt en positiu

    if (value > 0) propags = &negativeLiteralOcurrences[value];
    else propags = &positiveLiteralOcurrences[-value];

    ++indexOfNextLitToPropagate;

    for (uint i = 0; i < propags->size(); ++i) {
      bool someLitTrue = false;
      int numUndefs = 0;
      int lastLitUndef = 0;
      int clauseID=(*propags)[i];

      for (uint k = 0; not someLitTrue and k < clauses[clauseID].size(); ++k){
        int val = currentValueInModel(clauses[clauseID][k]);
        if (val == TRUE) someLitTrue = true;
        else if (val == UNDEF){ ++numUndefs; lastLitUndef = clauses[clauseID][k]; }
      }
      if (not someLitTrue and numUndefs == 0){
        ++TotalConflicts;
        update_conflicts(clauseID);
        return true; // conflict! all lits false
      }
      else if (not someLitTrue and numUndefs == 1) setLiteralToTrue(lastLitUndef);  
    }    
  }
  return false;
}   


void backtrack(){
  uint i = modelStack.size() -1;
  int lit = 0;
  while (modelStack[i] != 0){ // 0 is the DL mark
    lit = modelStack[i];
    model[abs(lit)] = UNDEF;
    modelStack.pop_back();
    --i;
  }
  // at this point, lit is the last decision
  modelStack.pop_back(); // remove the DL mark
  --decisionLevel;
  indexOfNextLitToPropagate = modelStack.size();
  setLiteralToTrue(-lit);  // reverse last decision
}


// Heuristic for finding the next decision literal:
int getNextDecisionLiteral(){
  int decision=0;
  int conflict_level=0;
  for (uint i = 1; i <= numVars; ++i) // stupid heuristic:
    if (model[i] == UNDEF && conflict_level < numConflicts[i]){
      conflict_level = numConflicts[i];
      decision=i;
    }
  if(decision!=0) ++TotalDecisions;  // returns first UNDEF var, positively
  return decision; // reurns 0 when all literals are defined
}


void checkmodel(){
  for (uint i = 0; i < numClauses; ++i){
    bool someTrue = false;
    for (uint j = 0; not someTrue and j < clauses[i].size(); ++j)
      someTrue = (currentValueInModel(clauses[i][j]) == TRUE);

    if (not someTrue) {
      cout << "Error in model, clause is not satisfied:";
      
      for (uint j = 0; j < clauses[i].size(); ++j)
  cout << clauses[i][j] << " ";

      cout << endl;
      exit(1);
    }
  }  
}




int main(){ 
  readClauses(); // reads numVars, numClauses and clauses
  model.resize(numVars+1,UNDEF);

  indexOfNextLitToPropagate = 0;  
  decisionLevel = 0;
  
  // Take care of initial unit clauses, if any
  for (uint i = 0; i < numClauses; ++i)
    if (clauses[i].size() == 1) {
      int lit = clauses[i][0];
      int val = currentValueInModel(lit);
      if (val == FALSE) {cout << "UNSATISFIABLE" << endl; return 10;}
      else if (val == UNDEF) setLiteralToTrue(lit);
    }
  
  // DPLL algorithm
  while (true) {
    while ( propagateGivesConflict() ) {
      if ( decisionLevel == 0) { cout << "UNSATISFIABLE" << endl; cout << "c " << TotalDecisions << " decisions" << endl; return 10; }
      backtrack();
    }
    int decisionLit = getNextDecisionLiteral();
    if (decisionLit == 0) { checkmodel(); cout << "SATISFIABLE" << endl; cout << "c " << TotalDecisions << " decisions" << endl; return 20; }
    // start new decision level:
    modelStack.push_back(0);  // push mark indicating new DL
    ++indexOfNextLitToPropagate;
    ++decisionLevel;
    setLiteralToTrue(decisionLit);    // now push decisionLit on top of the mark
  }
}

