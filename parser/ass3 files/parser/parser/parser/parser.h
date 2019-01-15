#include "buffer.h"
#include "token.h"




#define ELSE 0
#define FALSE 1
#define IF 2
#define PLATYPUS 3
#define READ 4
#define REPEAT 5
#define THEN 6
#define TRUE 7
#define WHILE 8
#define WRITE  9

#define NO_ATTR 1
#define KWT_SIZE 10

Token lookahead;
extern Buffer * str_LTBL;
extern Token malar_next_token(void);
extern char * kw_table[KWT_SIZE];
extern line;



int synerrno=0;


void match(int pr_token_code, int pr_token_attribute);
void syn_eh(int sync_token_code);
gen_incode(char *str);
void program(void);
void optStatements();
void statements();
void statement();
void statements_p();
void assignmentStatement();
void assignmentExpression();
void selectionStatement();
void iterationStatement();
void preCondition();
void inputStatement();
void variableList();
void variableIdentifier();
void variableList_p();
void outputStatement();
void outputList();
void OptvariableList();
void arithmeticExpression();
void unaryArithExp();
void primaryArithExp();
void addArithExp();
void addArithExp_p();
void multiArithExp();
void multiArithExp_p();
void stringExpression();
void primaryStringExp();
void stringExpression_p();
void conditionalExpression();
void logicalORexpression();
void logicalORexpression_p();
void logicalANDexpression();
void logicalANDexpression_p();
void relationExpression();
void primaryArelationExp_p();
void primaryArelationExp();
void primarySrelationExp_p();
void primarySrelationExp();
