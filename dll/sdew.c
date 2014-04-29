/********************************************************************************
*  sdew.c
*  -------------------
*  Begin: 2006/09/01
*  Last revision: $Date: 2007-05-29 20:14:55 $ $Author: areeves $
*  Version number: $Revision: 1.2 $
*  Project: Simple Delphi Expat Wrapper
*  Website: http://www.naadsm.org/opensource/delphi
*  Author: Shaun Case <Shaun.Case@colostate.edu>
*  Revision History:
*  $Log: sdew.c,v $
*  Revision 1.2  2007-05-29 20:14:55  areeves
*  Updated SDEW with new functions for:
*    -- Creating a parser from a buffer rather than a file
*    -- Handling element attributes
*
*  Revision 1.1  2007/01/10 22:20:59  areeves
*  Initial commit of the Simple Delphi Expat Wrapper.
*
*  Revision 1.1  2006/10/19 14:05:05  shaun
*  --  Moved to a separate C_DLLs module
*
*  Revision 1.3  2006/09/19 20:07:15  shaun
*  Revision History logging added to file.
*
*  --------------------------------------------------
*  Copyright (C) 2006 - 2007 Animal Population Health Institute, Colorado State University
*
*  This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
*  Public License as published by the Free Software Foundation; either version 2 of the License, or
*  (at your option) any later version.
*
*********************************************************************************/

#include <windows.h>
#include <stdio.h>
#include <stdlib.h>
#include <scew/scew.h>

#include "sdew.h"

/*******************************************************************************
 *******************************************************************************
 **                                                                           **
 **  NOTE:  All pointers passed as parameters to these functions,             **
 **         as well as, those returned by them are the C type void * .        **
 **         However, in all cases they represent pointers into scew           **
 **         structures used by the scew library, unless otherwise noted,      **
 **         such as in the of char *.  This was done in order to provide a    **
 **         smooth interface for such languages as Delphi, which do not have  **
 **         the ability to recognize these pointers, except as a void *.      **
 **                                                                           **
 *******************************************************************************
 *******************************************************************************/


/*******************************************************************************
 *  Function:  Create_Scew_Parser_From_File
 *
 *  This function returns a void * representing a pointer to a valid
 *  scew_element structure, which is the next child element of the _parent
 *  parameter, or if _parent is NULL, then the next sibling of the _element
 *  parameter.
 *
 *  Parameters:
 *    Filename:  A char * to a NULL terminated character array, which represents
 *               the filename of the XML file to open, and with which to
 *               initialize the parser created in this function.  This filename
 *               may also contain a path if necessary, otherwise the function
 *               will attempt to open a file by the name of the filename in the
 *               current directory of the currently running process.
 *
 *  Returns:
 *    void *:  Representing a pointer to a valid scew_parser structure, which
 *             meets the constraints of the input parameters, or NULL on error.
 *
 ******************************************************************************/
DLLIMPORT void *Create_Scew_Parser_From_File(char *Filename)
{
   scew_parser *parser;
   void *ret_val = NULL;

   parser = scew_parser_create ();

   if (parser != NULL)
   {
      if (scew_parser_load_file (parser, Filename) > 0 )
         ret_val = (void *)parser;
      else
      {
         scew_parser_free( parser );
         parser = NULL;
      }
   }

   return ret_val;
}


/*******************************************************************************
 *  Function:  Create_Scew_Parser_From_String
 *
 *  This function returns a void * representing a pointer to a valid
 *  scew_element structure, which is the next child element of the _parent
 *  parameter, or if _parent is NULL, then the next sibling of the _element
 *  parameter.
 *
 *  Parameters:
 *    str:       A char * to a NULL terminated character array, which represents
 *               the XML string with which to initialize the parser created in
 *               this function.
 *
 *    strLen:    The length of str.
 *
 *  Returns:
 *    void *:  Representing a pointer to a valid scew_parser structure, which
 *             meets the constraints of the input parameters, or NULL on error.
 *
 ******************************************************************************/
DLLIMPORT void *Create_Scew_Parser_From_String( char * str, int strLen )
{
   scew_parser *parser;
   void *ret_val = NULL;

   parser = scew_parser_create();

   if( NULL != parser )
   {
      if( 0 < scew_parser_load_buffer( parser, str, strLen ) )
         ret_val = (void *)parser;
      else
      {
         scew_parser_free( parser );
         parser = NULL;
      }
   }

   return ret_val;  
}

/*******************************************************************************
 *  Function:  Free_Parser
 *
 *  This function frees the memory used by a scew parser.
 *
 *  Parameters:
 *    _parser:  void * which represents a pointer to a scew_parser.  This
 *              pointer must have been created previously using the function
 *              Create_Load_Scew_Parser() .
 *
 *  Returns:  Nothing
 *
 ******************************************************************************/
DLLIMPORT void Free_Parser(void *_parser)
{
   scew_parser_free( (scew_parser *)_parser );
}


/*******************************************************************************
 *  Function:  Tree_RootScew
 *
 *  This function returns a pointer to the root
 *  tree of the parser passed as the parameter
 *  _parser.
 *
 *  Parameters:
 *    _parser:  void * which represents a pointer to a scew_parser.  This
 *              pointer must have been created previously using the function
 *              Create_Load_Scew_Parser() .
 *
 *  Returns:
 *    void *, representing a pointer to a scew_element.  On error, a NULL
 *            pointer value is returned.
 *
 ******************************************************************************/
DLLIMPORT void *Tree_Root_Scew(void *_parser)
{
   void *ret_val = NULL;

   if ( _parser != NULL )
   {
      scew_tree *temp = scew_parser_tree((scew_parser *)_parser);

      if ( temp != NULL )
         ret_val = (void *)scew_tree_root ( temp );
   };

   return ret_val;
}


/*******************************************************************************
 *  Function:  Free_RootTree
 *
 *  This function frees the memory used by the input parameter _rootTree, which
 *  represents a valid pointer to a scew_tree structure.
 *
 *  Parameters:
 *    _rootTree:  A void * representing a pointer to a valid scew_tree.  The
 *                memory used by this structure will be freed by this function.
 *
 *  Returns:
 *            NOTHING.
 *
 ******************************************************************************/
DLLIMPORT void  Free_RootTree( void *_rootTree )
{
   if ( _rootTree != NULL )
     scew_tree_free( _rootTree );

}


/*******************************************************************************
 *  TestParser
 *
 *  This is a test function to verify that the library is properly opening and
 *  reading xml files.
 ******************************************************************************/
DLLIMPORT void TestParser( char *Filename )
{

   scew_parser *parser;
   scew_element *params;
   int elementCount = 0;
   int i;
   char Buffer[1024];

   parser = scew_parser_create ();

   if (parser != NULL)
   {
      scew_parser_load_file (parser, Filename);
      params = scew_tree_root (scew_parser_tree (parser));

      elementCount = scew_element_count (params);

      sprintf(Buffer, "Root has %u children\n", elementCount);
      MessageBox( 0, Buffer, "Debugging", MB_ICONINFORMATION );

      for (i = 0; i < elementCount; i++)
      {
          scew_element *newParams = scew_element_by_index (params, i);
          sprintf(Buffer, "This child has %u subchildren\n", scew_element_count( newParams ));
          MessageBox( 0, Buffer, "Debugging", MB_ICONINFORMATION );
          sprintf(Buffer, "Child %i name='%s'\n", i, scew_element_name (scew_element_by_index (params, i)));
          MessageBox( 0, Buffer, "Debugging", MB_ICONINFORMATION );
      }
      scew_parser_free( parser );
   }
}


/*******************************************************************************
 *  Function:  Element_Count_Scew
 *
 *  This function returns an integer value representing the numerical count of
 *  the children elements subordinate to the parameter passed.
 *
 *  Parameters:
 *    _element:  void * which represents a pointer to a scew_element.  This
 *              pointer must have been created previously by some other function
 *              in this library.
 *
 *  Returns:
 *    int, representing a count of the number of child elements of the element
 *         passed in.  On error, a -1 is returned.
 *
 ******************************************************************************/
DLLIMPORT int Element_Count_Scew(void *_element)
{
   int ret_val = -1;

   if ( _element != NULL )
   {
      ret_val = scew_element_count ((scew_element *)_element);
   };

   return ret_val;
}

/*******************************************************************************
 *  Function:  Element_By_Name
 *
 *  This function returns a void * representing a pointer to a valid
 *  scew_element structure, which is a child element of the _element parameter
 *  named the same as the _name parameter.
 *
 *  Parameters:
 *    _element:  void * which represents a pointer to a scew_element.  This
 *              pointer must have been created previously by some other function
 *              in this library.
 *    _name:    A pointer to a NULL terminated charater array representing the
 *              name of the element to search for as a child of the _element
 *              parameter.
 *
 *  Returns:
 *    void *:  Representing a pointer to a valid scew_element structure, which
 *             meets the constraints of the input parameters, or NULL on error.
 *
 ******************************************************************************/
DLLIMPORT void *Element_By_Name( void *_element, char *_name)
{
   void * ret_val = NULL;

   if (( _element != NULL ) && ( _name != NULL ))
   {
      ret_val = scew_element_by_name( (scew_element *)_element, _name );
   }

   return ret_val;
}

/*******************************************************************************
 *  Function:  Element_Next
 *
 *  This function returns a void * representing a pointer to a valid
 *  scew_element structure, which is the next child element of the _parent
 *  parameter, or if _parent is NULL, then the next sibling of the _element
 *  parameter.
 *
 *  Parameters:
 *    _parent:  void * which represents a pointer to a scew_element.  This
 *              pointer must have been created previously by some other function
 *              in this library.
 *    _element:  void * which represents a pointer to a scew_element.  This
 *              pointer must have been created previously by some other function
 *              in this library.
 *
 *  Returns:
 *    void *:  Representing a pointer to a valid scew_element structure, which
 *             meets the constraints of the input parameters, or NULL on error.
 *
 ******************************************************************************/
DLLIMPORT void *Element_Next( void *_parent, void *_element )
{
   void *ret_val = NULL;

   ret_val = scew_element_next( (scew_element *)_parent, (scew_element *)_element );
   return ret_val;
}


/*******************************************************************************
 *  Function:  Element_Contents
 *
 *  This function returns a char * representing a pointer to the contents of the
 *  element specified in the input parameter, _element.
 *
 *  Parameters:
 *    _element:  A void * representing a valid pointer to a scew_element
 *               structure.  This pointer must have been created previously by
 *               some other function in this library.
 *
 *  Returns:
 *    char *:  Representing a pointer to a NULL terminated character array,
 *             which contains the contents of the element given in the input
 *             parameter, _element.  A NULL value is returned on error.
 *
 ******************************************************************************/
DLLIMPORT char *Element_Contents( void *_element )
{
   char *ret_val = NULL;
   if ( _element != NULL)
   {
      ret_val = (char *)scew_element_contents( (scew_element *)_element );
   }
   return ret_val;
}


/*******************************************************************************
 *  Function:  Element_Name
 *
 *  This function returns a char * representing a pointer to the name of the
 *  element specified in the input parameter, _element.
 *
 *  Parameters:
 *    _element:  A void * representing a valid pointer to a scew_element
 *               structure.  This pointer must have been created previously by
 *               some other function in this library.
 *
 *  Returns:
 *    char *:  Representing a pointer to a NULL terminated character array,
 *             which contains the name of the element given in the input
 *             parameter, _element.  A NULL value is returned on error.
 *
 ******************************************************************************/
DLLIMPORT char *Element_Name( void *_element )
{
   char *ret_val = NULL;
   if ( _element != NULL )
   {
      ret_val = (char *)scew_element_name( (scew_element *)_element );
   }

   return ret_val;
}


/*******************************************************************************
 *  Function:  Element_Name_By_Index
 *
 *  This function returns a char * representing a pointer to the name of the
 *  element specified as the Index(th) child of the input parameter _element.
 *
 *  Parameters:
 *    _element:  A void * representing a valid pointer to a scew_element
 *               structure.  This pointer must have been created previously by
 *               some other function in this library.
 *    Index:     An integer value, zero based, representing the index of the
 *               child element of _element.
 *
 *  Returns:
 *    char *:  Representing a pointer to a NULL terminated character array,
 *             which contains the name of the element established by the Index(th)
 *             child element of _element.  A NULL value is returned on error.
 *
 ******************************************************************************/
DLLIMPORT char *Element_Name_By_Index(void *_element, int Index)
{
   char *ret_val = NULL;
   if (( _element != NULL ) && ( Index >=0 ))
   {
      scew_element *temp = scew_element_by_index((scew_element *)_element, Index);

      if ( temp != NULL )
         ret_val = (char *)scew_element_name( temp );
   };

   return ret_val;
}


/*******************************************************************************
 *  Function:  Element_By_Index
 *
 *  This function returns a void * representing a pointer to a valid
 *  scew_element structure, which is the Index(th) child element of the input
 *  parameter _element, or a NULL pointer on error.
 *
 *  Parameters:
 *    _element:  A void * representing a valid pointer to a scew_element
 *               structure.  This pointer must have been created previously by
 *               some other function in this library.
 *    Index:     An integer value, zero based, representing the index of the
 *               child element of _element.
 *
 *  Returns:
 *    void *:  Representing a pointer to a scew_element structure, which is the
 *             Index(th) child element of _element.
 *             A NULL value is returned on error.
 *
 ******************************************************************************/
DLLIMPORT void *Element_By_Index( void *_element, int Index )
{
   void *ret_val = NULL;

   if (( _element != NULL ) && (Index >= 0 ))
   {
      ret_val = scew_element_by_index (_element, Index);
   };

   return ret_val;
}


/*******************************************************************************
 *  Function:  Element_Attribute
 *
 *  This function returns a char * representing a pointer to a NULL terminated
 *  character array, which contains the value associated with the attribute,
 *  specified by the _attr input parameter, of the element specified by the
 *  _element input parameter.
 *
 *  Parameters:
 *    _element:  A void * representing a valid pointer to a scew_element
 *               structure.  This pointer must have been created previously by
 *               some other function in this library.
 *    _attr:     A char * pointing to a NULL terminated character array, which
 *               contains the name of the attribute in _element, for which to
 *               find a value.
 *
 *  Returns:
 *    char *:  Representing a pointer to a NULL terminated character array, which
 *             contains the value of the attribute of _element, if it exists.
 *             A NULL Pointer is returned on error.
 *
 ******************************************************************************/
DLLIMPORT char *Element_Attribute( void *_element, char *_attr )
{
   char *ret_val = NULL;

   if ( ( _element != NULL ) && ( _attr != NULL ) )
   {
      scew_attribute *temp = scew_attribute_by_name( _element, _attr );

      if ( temp != NULL )
         ret_val = (char *)scew_attribute_value( temp );
   };

   return ret_val;
}


/*******************************************************************************
 *  Function:  Attribute_Count_Scew
 *
 *  This function returns an integer value representing the numerical count of
 *  the atrributes the parameter passed.
 *
 *  Parameters:
 *    _element:  void * which represents a pointer to a scew_element.  This
 *              pointer must have been created previously by some other function
 *              in this library.
 *
 *  Returns:
 *    int, representing a count of the number of attributes of the element
 *         passed in.  On error, a -1 is returned.
 *
 ******************************************************************************/
DLLIMPORT int Attribute_Count_Scew( void *_element )
{
   int ret_val = -1;

   if ( _element != NULL )
   {
      ret_val = scew_attribute_count ((scew_element *)_element);
   };

   return ret_val;  
}


DLLIMPORT char *Attribute_Name( void *_attribute )
{
  char* ret_val = NULL;
  if( _attribute != NULL )
  {
    ret_val = (char*)scew_attribute_name( (scew_attribute*)_attribute );     
  }
  return ret_val;  
}


DLLIMPORT char *Attribute_Value( void *_attribute )
{
  char* ret_val = NULL;
  if( _attribute != NULL )
  {
    ret_val = (char*)scew_attribute_value( (scew_attribute*)_attribute );     
  }
  return ret_val;   
}

DLLIMPORT void *Attribute_Next( void *_parent, void *_attribute )
{
  return scew_attribute_next( (scew_element*)_parent, (scew_attribute*)_attribute ); 
}


DLLIMPORT void *Attribute_By_Index( void *_element, int Index )
{
  return scew_attribute_by_index( (scew_element*)_element, Index );
}


DLLIMPORT void *Attribute_By_Name( void *_element, char *_name)
{
  return scew_attribute_by_name( (scew_element*)_element, _name );  
}


DLLIMPORT char *Attribute_Name_By_Index( void *_element, int Index )
{
  scew_attribute* attr;
  
  attr = scew_attribute_by_index( (scew_element*)_element, Index );
  
  if( NULL != attr )
    return (char*)scew_attribute_name( attr );
  else
    return ""; 
}



BOOL APIENTRY DllMain (HINSTANCE hInst     /* Library instance handle. */ ,
                       DWORD reason        /* Reason this function is being called. */ ,
                       LPVOID reserved     /* Not used. */ )
{
    switch (reason)
    {
      case DLL_PROCESS_ATTACH:
        break;

      case DLL_PROCESS_DETACH:
        break;

      case DLL_THREAD_ATTACH:
        break;

      case DLL_THREAD_DETACH:
        break;
    }

    /* Returns TRUE on success, FALSE on failure */
    return TRUE;
}

