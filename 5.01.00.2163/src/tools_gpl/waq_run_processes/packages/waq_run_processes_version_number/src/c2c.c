#include <stdlib.h>
#include <stdio.h>
#include <string.h>

#if defined(WIN32) || defined (WIN64)
#  include <io.h>
#  include <wtypes.h>
#endif

#include "version_number.h"
#define CAT(a, b) a ## b
#define FUNC_CAT(a, b) CAT(a, b)

extern  char * FUNC_CAT( version_getFileVersionString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getFullVersionString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getCompanyString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getVersionNumberString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getProgramNameString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getShortProgramNameString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getSvnRevisionString_, MOD_NAME)(void);
extern  char * FUNC_CAT( version_getFeatureNumberString_, MOD_NAME)(void);

/*==========================================================================*/
void FUNC_CAT( getFileVersionString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getFileVersionString_, MOD_NAME)();
  strcpy(str, str1);
}

/*==========================================================================*/
void FUNC_CAT( getFullVersionString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getFullVersionString_, MOD_NAME)();
  strcpy(str, str1);
}

/*==========================================================================*/
void FUNC_CAT( getCompanyString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getCompanyString_, MOD_NAME)();
  strcpy(str, str1);
}

/*==========================================================================*/
void FUNC_CAT( getVersionNumberString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getVersionNumberString_, MOD_NAME)();
  strcpy(str, str1);
}
/*==========================================================================*/
void FUNC_CAT( getProgramNameString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getProgramNameString_, MOD_NAME)();
  strcpy(str, str1);
}
/*==========================================================================*/
void FUNC_CAT( getShortProgramNameString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getShortProgramNameString_, MOD_NAME)();
  strcpy(str, str1);
}

/*==========================================================================*/
void FUNC_CAT( getSvnRevisionString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getSvnRevisionString_, MOD_NAME)();
  strcpy(str, str1);
}
/*==========================================================================*/
void FUNC_CAT( getFeatureNumberString_, MOD_NAME)( char * str )
{
  const char * str1;
  str1 = FUNC_CAT( version_getFeatureNumberString_, MOD_NAME)();
  strcpy(str, str1);
}
