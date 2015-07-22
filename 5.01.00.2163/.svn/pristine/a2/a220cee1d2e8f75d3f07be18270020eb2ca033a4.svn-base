/*
 *  equal.c  -  ODS string comparison supporting wildcards
 *
 *  Copyright (C) 1994 Delft Hydraulics
 *
 *  Marc Kool
 */

/*
 *  $Author: Markus $
 *  $Date: 12/27/00 2:53p $
 *  $Source: /u/cvsroot/gpp/libsrc/ods/equal.c,v $
 *
 */

/*   Date:       11 Jul 1993                                          */
/*   Time:       14:07                                                */
/*   Program:    strequal.c                                           */
/*   Version:    1.00                                                 */
/*   Programmer: Andr‚ Hendriks                                       */
/*   (c) Copyright 1993 Delft Hydraulics                              */
/*   Previous version(s):                                             */
/*   0.00 -- 11 Jul 1993 -- 14:07 -- Operating System: DOS            */
/*   Project:    Open Data Structuur                                  */
/*   Module:     strequal                                             */
/*   Function:   compare two strings, one containing the contents     */
/*               of a database field (nul terminated), the other a    */
/*               mask (also null terminated), which may contain       */
/*               a wildcard (*).                                      */
/*   Comment:                                                         */
/*   Note that ASCII char|0x20 (lowercase conversion) produces valid  */
/*   results in range 0x20-0x7F . Outside this range unwanted         */
/*   character mapping will occur. EAV.                               */
/*   Reference:                                                       */
/*   Review:                                                          */

#include <stdio.h>

#define ASCI_LOW 0x20

int equal ( char * mask, char * field)
   {
   char *m, *s;

   for (m=mask,s=field; *m && *s && *m!='*' ;m++,s++)
      if ((*m|ASCI_LOW)!=(*s|ASCI_LOW))
         return 0;

   if (*m=='*') /* strings gelijk tot aan wildcard in mask: OK */
      return 1;

   if (*m || *s) /* lengtes field en mask ongelijk: geen match */
      return 0;

   return 1;
   }

#ifdef TESTDRIVER

main ()
{
   printf ("equal 'test' 'test' %d\n",equal("test","test"));
   printf ("equal 'TEST' 'test' %d\n",equal("TEST","test"));
   printf ("equal 'best' 'test' %d\n",equal("best","test"));
   printf ("equal 'testje' 'test' %d\n",equal("testje","test"));
   printf ("equal 'test' 'testje' %d\n",equal("test","testje"));
   printf ("equal 'test*' 'test' %d\n",equal("test","test"));
   printf ("equal 'te*' 'test' %d\n",equal("te*","test"));
   printf ("equal '*' 'test' %d\n",equal("*","test"));
}

#endif
