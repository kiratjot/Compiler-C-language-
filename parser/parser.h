/*
* File Name: parser.h
* Compiler: MS Visual Studio 2015 Community
* Author: Gurkirat Singh 040876315, Shubham Ujinwal 040885893
* Course: CST 8152  Compilers
* Lab Section: 12
* Assignment: 3
* Date: 6 December 2018
* Professor: Sv. Ranev
* Purpose: Declaration required for the Parser implementation
* Function list:parser();
*				match();
*				syn_printe();
*				syn_eh(int);
*				gen_incode();
*				program();
*				optStatements();
*				statements();
*				statement();
*				statements_p();
*				assignmentStatement();
*				assignmentExpression();
*				selectionStatement();
*				iterationStatement();
*				preCondition();
*				inputStatement();
*				variableList();
*				variableIdentifier();
*				variableList_p();
*				outputStatement();
*				outputList();
*				arithmeticExpression();
*				unaryArithExp();
*				primaryArithExp();
*				addArithExp();
*				addArithExp_p();
*				multiArithExp();
*				multiArithExp_p();
*				stringExpression();
*				primaryStringExp();
*				stringExpression_p();
*				conditionalExpression();
*				logicalORexpression();
*				logicalORexpression_p();
*				logicalANDexpression();
*				logicalANDexpression_p();
*				relationExpression();
*				primaryArelationExp_p();
*				primaryArelationExp();
*				primarySrelationExp_p();
*				primarySrelationExp();
*
*/


#include "buffer.h"
#include "token.h"

/*Constants for the keyword*/
#define NO_ATTR -1
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
#define KWT_SIZE 10


/*Global variables */
int synerrno;							/*declaring counter for run time errors*/
Token lookahead;						/*For iterating through the tokens*/
extern Buffer * str_LTBL;				/*String literal table*/
extern Token malar_next_token(void);	/*scanner function to get the next token*/
extern char * kw_table[KWT_SIZE];		/*Keyword table*/
extern line;							/*variable to count the number of lines in the code*/



										/* function declarations */
void parser(void);
void match(int pr_token_code, int pr_token_attribute);
void syn_printe();
void syn_eh(int sync_token_code);
void gen_incode(char *str);
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
