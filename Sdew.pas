unit Sdew; //Dephi interface into the scew/expat libraries for XML parsing.

(*
Sdew.pas
--------
Begin: 2006/09/01
Last revision: $Date: 2012-10-23 22:22:05 $ $Author: areeves $
Version number: $Revision: 1.6 $
Project: Simple Delphi Expat Wrapper
Website: http://www.naadsm.org/opensource/delphi
Author: Shaun Case <Shaun.Case@colostate.edu>
Author: Shaun Case <Aaron.Reeves@colostate.edu>
--------------------------------------------------
Copyright (C) 2006 - 2012 Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface

  uses
    Classes,
    Windows
  ;

  //------------------------------------------------------------------------------
  //  Class: TSdew
  //
  //  Purpose:  TSdew is used as a Delphi interface into the scew C library, via
  //    an intermediate step.
  //    (The scew C library is a wrapper for the expat XML non-validating
  //     parser.)
  //    This object requires the presence of the Windows DLL, sdew.dll.
  //    The sdew.dll is a Windows DLL used to provide a wrapper to the scew
  //    C library so that Delphi may access it more effectively.
  //    TSdew provides basic XML parsing functionality to include element
  //    attributes.
  //
  //------------------------------------------------------------------------------
    type TSdew = class( TObject )
      protected
        _Filename: PChar;     // Filename of the xml file to parse
        _Parser: Pointer;     // Pointer used by scew libraries
        _RootTree: Pointer;   // Pointer used by scew libraries
        _LibLoaded: boolean;  // Flag to identify if the DLL has been loaded successfully.
        _FileLoaded: boolean; //Flag to identify if the XML parser was properly created.

        _dllHandle: THandle; // Windows handle to the sdew.dll
        _minVersion: string; // Version string

        //  These functions represent DLL functions provided by the
        //  DLL sdew.dll   They are accessed through their
        //  public equivalents in this object (see below).
        _CreateScewParserFromFile: function( filename: pchar):Pointer; cdecl;
        _CreateScewParserFromString: function( str: pchar; strLen: integer ): pointer; cdecl;
        _FreeParser: procedure( parser: Pointer);cdecl;

        _TreeRootScew: function(parser: Pointer):Pointer; cdecl;
        //_CreateTree: function():Pointer; cdecl; // Currently unused.
        //_TreeAddRoot: function( tree:Pointer; name:PChar ):Pointer; cdecl; // Currently unused.
        _FreeTree: procedure( rootTree:Pointer); cdecl;

        _ElementCount: function(element: Pointer): Integer; cdecl;
        _ElementNameByIndex: function(element: Pointer; Index: Integer):PChar; cdecl;
        _ElementByIndex: function(element: Pointer; Index: Integer):Pointer; cdecl;
        _ElementName: function( element:Pointer):PChar; cdecl;
        _ElementContents: function( element:Pointer):PChar; cdecl;
        _ElementByName: function( element:Pointer; name:PChar ):Pointer; cdecl;
        _ElementNext: function( parent:Pointer; element:Pointer):Pointer; cdecl;
        _ElementAttribute: function( element:Pointer; name:PChar):PChar; cdecl;
        _ElementAdd: function( element:Pointer; name:PChar ):Pointer; cdecl;

        _TestParser: procedure(filename:pchar ); cdecl;

        _AttributeCount: function(element: pointer ): integer; cdecl;
        _AttributeNameByIndex: function( element: pointer; index: integer ): pchar; cdecl;
        _AttributeByIndex: function( element: pointer; index: integer ): pointer; cdecl;
        _AttributeName: function( attribute: pointer ): pchar; cdecl;
        _AttributeValue: function( attribute: pointer ): pchar; cdecl;
        _AttributeNext: function( element: pointer; attribute: pointer ): pointer; cdecl;
        _AttributeByName: function( element: pointer; name: pchar ): pointer; cdecl;

        procedure Initialize();

        function isLibLoaded():Boolean;

      public
        //  Constructor:  Filename is the name of the XML file.
        constructor createFromFile( Filename: PChar );
        constructor createFromString( str: string );

        //  Destructor:
        destructor destroy(); override;

        procedure debug();

        //------------------------------------------------------------------------------------
        // Public accessible functions of sdew.dll.
        // In  most cases these are just wrappers to the existing functions.
        //------------------------------------------------------------------------------------
        // Tree handling
        //--------------
        function GetRootTree(): Pointer;
        //function CreateTree():Pointer; // Not yet implemented
        //function AddTreeRoot( tree:Pointer; name:PChar ):Pointer; // Not yet implemented

        // Element handling
        //-----------------
        function GetElementCount( element: Pointer ): Integer;
        function GetElementNameByIndex( element: Pointer; Index: Integer ): String;
        function GetElementByIndex( element: Pointer; Index: Integer ): Pointer;
        function GetElementName( element: Pointer): String;
        function GetElementContents( element: Pointer ): String; overload;
        function GetElementByName( element:Pointer; name:PChar ): Pointer;
        function GetNextElement( parent:Pointer; element:Pointer ): Pointer;
        function GetElementAttribute( element:Pointer; name:PChar ): String;

        function getElementContents( element: pointer; name: pchar ): string; overload;
        function getElementContents( element: pointer; index: integer ): string; overload;

        //function AddElement( element:Pointer; name:PChar ):Pointer;  // Not yet implemented


        // Attribute handling
        //-------------------
        function GetAttributeCount(element: Pointer ): integer;
        function getAttributeNameByIndex( element: pointer; index: integer ): string;
        function getAttributeByIndex( element: pointer; index: integer ): pointer;
        function getAttributeName( attribute: pointer ): string;
        function getAttributeValue( attribute: pointer ): string; overload;
        function getNextAttribute( element: pointer; attribute: pointer ): pointer;
        function getAttributeByName( element: pointer; name: pchar ): pointer;

        function getAttributeValue( element: pointer; name: pchar ): string; overload;
        function getAttributeValue( element: pointer; index: integer ): string; overload;
        //------------------------------------------------------------------------------------

        // Other properties
        //-----------------
        property LibLoaded:Boolean read isLibLoaded;
      end
    ;


implementation
  uses
    SysUtils,

    MyStrUtils,
    DebugWindow
  ;

  // Debug output switch
  const UseDebug:boolean = true;

  

   procedure TSdew.debug();
      //-----------------------------
      // Subroutine debugElement
      //-----------------------------
      procedure debugElement( element: pointer; indent: integer );
        var
          nElements: integer;
          nAttrs: integer;
          i: integer;

          spaces: string;
          str: string;

          attr: pointer;
        begin
          spaces := '';

          for i := 0 to indent do
            spaces := spaces + '  '
          ;

          nElements := getElementCount( element );
          nAttrs := getAttributeCount( element );

          str :=
            spaces + getElementName( element )
            + ': attrs ' + intToStr( nAttrs )
            + ', elements ' + intToStr( nElements )
          ;

          if( 0 = nElements ) then
            str := str + ', contents ' + trim( getElementContents( element ) )
          ;

          dbcout( str, true );

          for i := 0 to nAttrs - 1 do
            begin
              attr := getAttributeByIndex( element, i );
              dbcout( spaces + '  ATTR: ' + getAttributeName( attr ) + ': ' + getAttributeValue( attr ), true );
            end
          ;

          for i := 0 to nElements - 1 do
            debugElement( getElementByIndex( element, i ), indent + 1 )
          ;
        end
      ;
      //-----------------------------
      begin
        debugElement( getRootTree(), 0 );
      end
    ;

//*****************************************************************************
// CLASS:  TSdew Implementation
//*****************************************************************************
  // --------------------------------------------------------------------------
  // TSdew: Creation/initialization/destruction
  //   params:
  //     Filename -  PChar :  Contains the name of the XML file to parse
  // --------------------------------------------------------------------------
  constructor TSdew.createFromFile( Filename: PChar );
    begin
       inherited create();

       _LibLoaded := false;
       _FileLoaded := false;
       _Filename := Filename;
       _RootTree := nil;
       _Parser := nil;

       Initialize();

       if ( _LibLoaded ) then
         begin
           if ( length( Filename ) > 0 ) then
             begin
               _Parser := _CreateScewParserFromFile( Filename );

               if ( _Parser <> nil ) then       //Check to be sure the load succeeded
                 begin
                   _RootTree := _TreeRootScew( _Parser );
                   _FileLoaded := true;
                 end
               else
                 begin
                   _FreeParser( _Parser );
                   _Parser := nil;
                 end
               ;
             end
           ;
         end
       ;
    end
  ;


  constructor TSdew.createFromString( str: string );
    begin
       inherited create();

       _LibLoaded := false;
       _FileLoaded := false;
       _Filename := '';
       _RootTree := nil;
       _Parser := nil;

       Initialize();

       if ( _LibLoaded ) then
         begin
           if ( length( str ) > 0 ) then
             begin
               _Parser := _CreateScewParserFromString( pchar( str ), length( str ) );

               if ( _Parser <> nil ) then       //Check to be sure the load succeeded
                 begin
                   _RootTree := _TreeRootScew( _Parser );
                   _FileLoaded := true;
                 end
               else
                 begin
                   _FreeParser( _Parser );
                   _Parser := nil;
                 end
               ;
             end
           ;
         end
       else
        dbcout( 'scew library could not be loaded.', true )
       ;
    end
  ;


  destructor TSdew.destroy();
     begin
        if ( _Parser <> nil ) then
          _FreeParser( _Parser )
        ;
          
        if ( _RootTree <> nil ) then
          _FreeTree( _RootTree )
        ;

        inherited destroy();
     end
  ;


  function TSdew.isLibLoaded():Boolean;
    begin
      result := _LibLoaded;
    end
  ;

  //----------------------------------------------------------------------------
  //  Initialize
  //
  //  This function loads the sdew.dll library, and gets the addresses
  //  of associated functions
  //----------------------------------------------------------------------------
  procedure TSdew.Initialize();
    begin
      _dllHandle := 0;
      try
        _dllHandle := loadLibrary( 'sdew.dll' );
        dbcout('loadLibrary successful', UseDebug );
      except
        dbcout('loadLibrary of sdew.dll failed!', UseDebug );
        //exit;
      end;


      if( _dllHandle >= 32 ) then // library was successfully loaded.  Assign function pointers now.
        begin
          dbcout( 'Library was successfully loaded', UseDebug );
          dbcout( 'Attempting to set function pointers', UseDebug );

          _CreateScewParserFromFile := GetProcAddress( _dllHandle, 'Create_Scew_Parser_From_File' );
          _CreateScewParserFromString := GetProcAddress( _dllHandle, 'Create_Scew_Parser_From_String' );

          _FreeParser := GetProcAddress( _dllHandle, 'Free_Parser' );

          _TreeRootScew := GetProcAddress( _dllHandle, 'Tree_Root_Scew' );
          _FreeTree := GetProcAddress( _dllHandle, 'Free_RootTree' );

          _TestParser := GetProcAddress( _dllHandle, 'TestParser' );

          _ElementCount := GetProcAddress( _dllHandle, 'Element_Count_Scew' );
          _ElementNameByIndex := GetProcAddress( _dllHandle, 'Element_Name_By_Index' );
          _ElementByIndex := GetProcAddress( _dllHandle, 'Element_By_Index' );
          _ElementName := GetProcAddress( _dllHandle, 'Element_Name');
          _ElementContents := GetProcAddress( _dllHandle, 'Element_Contents');
          _ElementByName := GetProcAddress( _dllHandle, 'Element_By_Name');
          _ElementNext := GetProcAddress( _dllHandle, 'Element_Next');
          _ElementAttribute := GetProcAddress( _dllHandle, 'Element_Attribute');

          _AttributeCount := GetProcAddress( _dllHandle, 'Attribute_Count_Scew' );
          _AttributeNameByIndex := GetProcAddress( _dllHandle, 'Attribute_Name_By_Index' );
          _AttributeByIndex := GetProcAddress( _dllHandle, 'Attribute_By_Index' );
          _AttributeName := GetProcAddress( _dllHandle, 'Attribute_Name' );
          _AttributeValue := GetProcAddress( _dllHandle, 'Attribute_Value' );
          _AttributeNext := GetProcAddress( _dllHandle, 'Attribute_Next' );
          _AttributeByName := GetProcAddress( _dllHandle, 'Attribute_By_Name' );


          // Make sure that the functions were all able to be loaded.
          if
            Assigned( _CreateScewParserFromFile ) and Assigned( _CreateScewParserFromString ) and Assigned( _FreeParser )
          and
            Assigned( _TreeRootScew ) {and Assigned( _TreeAddRoot )}
          and
            {Assigned( _CreateTree ) and} Assigned( _FreeTree )
          and
            Assigned( _TestParser )
          and
              Assigned( _ElementCount ) and Assigned( _ElementNameByIndex )
            and
               Assigned( _ElementByIndex ) and Assigned( _ElementByName )
            and
              Assigned( _ElementName ) and Assigned( _ElementContents )
            and
               Assigned( _ElementNext ) and Assigned( _ElementAttribute )
            {and
              Assigned( _ElementAdd )}
          and
              Assigned( _AttributeCount ) 
            and
              Assigned( _AttributeNameByIndex ) and Assigned( _AttributeByIndex )
            and
              Assigned( _AttributeName ) and Assigned( _AttributeValue )
            and
              Assigned( _AttributeNext ) and Assigned( _attributeByName )
          then
            _LibLoaded := true
          ;
        end
      ;
    end
  ;



  //----------------------------------------------------------------------------
  //  GetRootTree
  //
  //  Returns the root tree of the xml file.
  //  All elements of the file fall under this tree
  //
  //  The member variable "_RootTree" is set in the constructor function.
  //----------------------------------------------------------------------------
  function TSdew.GetRootTree():Pointer;
    begin
      result := _RootTree;
    end
  ;

  //--------------------------------------------------------------------------
  //  GetElementAttribute
  //
  //  This function returns a string value containing the value assigned
  //  to the attribute, (name), of the element pointed to by the element
  //  parameter.
  //  Example:
  //    If the current element points to: <mytag size="large"/>  and name is
  //    "size", the return value would be "large".
  //
  //  Valid results:  A string of length zero or greater.  A zero length
  //                  string indicates a failure condition of some kind.
  //--------------------------------------------------------------------------
  function TSdew.GetElementAttribute( element:Pointer; name:PChar ):String;
   var
     ret_val:String;
     temp:PChar;
   begin
     ret_val := '';
       
     if ( _FileLoaded ) then
       begin
         temp := _ElementAttribute( element, PChar(name));
         if ( temp <> nil ) then
           ret_val := String( temp )
         ;
       end
     ;

     result := ret_val;
   end
  ;


  //---------------------------------------------------------------------------
  //  GetNextElement
  //
  //  This function returns a pointer to the next element in the file.
  //
  //  NOTE:  This function has two basic functionalities:
  //         1)  If element is nil, or invalid, and parent is valid, it will
  //             return the first child element of parent.
  //         2)  If element is valid, it will return the next sibling of that
  //             element.
  //
  //         To iterate through elements, be sure to pass the last element
  //         returned by this function as the element parameter.  The function
  //         here and the function in the scew libraries maintains NO state
  //         information.
  //
  //  Valid results:  Returns a pointer to the next element or nil on error.
  //
  //---------------------------------------------------------------------------
  function TSdew.GetNextElement( parent:Pointer; element:Pointer):Pointer;
   var
     ret_val:Pointer;
   begin
     ret_val := nil;

     if ( _FileLoaded ) then
       ret_val := _ElementNext( parent, element )
     ;

     result := ret_val;
   end
  ;


  //---------------------------------------------------------------------------
  //  GetElementByName
  //
  //  This function returns a pointer to an element, whose tag value is the
  //  name passed to this function.  This is handy for random access within
  //  the XML file.  However, if multiple elements are used in the file, all
  //  with the same name, for whatever reason, this function will only return
  //  the first occurance and subsequent attempts will result in undefined
  //  side-effects.  If you must iterate through multiple elements, all sharing
  //  the same name, then use the GetElementByIndex() function.
  //
  //  NOTE:  As with some other functions, this will only return the element
  //         which is a child to the element pointer passed in.  If you need to
  //         find a sibling by this name, you must know its parent's element
  //         pointer.
  //
  //  Results:  Returns a pointer to the element, or nil on error.
  //
  //---------------------------------------------------------------------------
  function TSdew.GetElementByName( element:Pointer; name:PChar ):Pointer;
   var
     ret_val:Pointer;
   begin
     ret_val := nil;

     if ( _FileLoaded ) then
       ret_val := _ElementByName( element, PChar(name) )
     ;

     result:= ret_val;
   end
  ;

  //---------------------------------------------------------------------------
  //  GetElementContents
  //
  //  This function returns a string containing the data stored between the
  //  begin and end tags of this element.
  //  Example:
  //    If element points to:
  //                         <FirstName>
  //                           Shaun
  //                         </FirstName>
  //
  //    Then, this function will return "Shaun"
  //
  //  Results:  Returns a string of length zero or greater.  A zero length
  //            string indicates that this element had no contents or there
  //            was some kind of error.
  //---------------------------------------------------------------------------
  function TSdew.GetElementContents( element: Pointer ):String;
   var
     ret_val:String;
     temp:PChar;
   begin
     ret_val := '';

     if ( _FileLoaded ) then
       begin
         temp := _ElementContents(element);

         if ( temp <> nil ) then
           ret_val := String( temp )
         ;
       end
     ;

     result := ret_val;
   end
  ;


  function TSdew.getElementContents( element: pointer; name: pchar ): string;
    var
      ret_val: string;
      childElement: pointer;
    begin
      ret_val := '';

      childElement := self.GetElementByName( element, name );

      if( nil <> childElement ) then
        ret_val := self.GetElementContents( childElement )
      ;

      result := ret_val;
    end
  ;


  function TSdew.getElementContents( element: pointer; index: integer ): string;
    var
      ret_val: string;
      childElement: pointer;
    begin
      ret_val := '';

      childElement := self.GetElementByIndex( element, index );

      if( nil <> childElement ) then
        ret_val := self.GetElementContents( childElement )
      ;

      result := ret_val;
    end
  ;


  //---------------------------------------------------------------------------
  //  GetElementName
  //
  //  This function returns the actual name of the element pointed to by the
  //  element parameter.  This is useful, if you are iterating through elements
  //  by use of the GetElementNext or GetElementByIndex methods, and you want
  //  to know the name of the element you have been returned.
  //
  //  Example:
  //    If the element pointer parameter points to:
  //
  //    <ShoppingList>blah blah blah</ShoppingList>
  //
  //    Then, this function will return the string, "ShoppingList"
  //
  //  Results:  Returns a string of length zero or greater.  A zero length
  //            string indicates that this element had no name or there
  //            was some kind of error.
  //
  //---------------------------------------------------------------------------
  function TSdew.GetElementName( element: Pointer ):String;
   var
     ret_val:String;
     temp:PChar;
   begin
     ret_val := '';

     if ( _FileLoaded ) then
       begin
         temp := _ElementName( element );

         if ( temp <> nil ) then
           ret_val := String( temp )
         ;
       end;

     result := ret_val;
   end
  ;


  //---------------------------------------------------------------------------
  //  GetElementCount
  //
  //  This function returns the number of child elements directly subordinate
  //  to the element pointer parameter.  It will NOT return a count
  //  all-inclusive of all elements subordinate to the subordinates of this
  //  element.  It ONLY scans one level beneath the the element pointer.
  //
  //  Example:
  //    If the element pointer points to:
  //      <ShoppingList>
  //        <AppleJuice Quantity="3"/>
  //        <Lettuce Quantity="1"/>
  //        <Milk Quantity="2">
  //          <Units>Quart</Units>
  //        </Milk>
  //      </ShoppingList>
  //
  //    Then, this function will return (3) as an integer value, representing
  //    the three children of this element.  Using this result, the funciton
  //    GetElementByIndex() may be used to iterate through these.
  //
  //  NOTE:  All index parameters of these functions are zero based array
  //         indexes.  Consequently, the 0th element is the first one.
  //         A return value from this function HOWEVER, of 0 means that there
  //         are NO children of this element.  (Don't confuse the two meanings)
  //
  //    Results:  A Positive integer greater than or equal to 0 on success.
  //              A -1 return value indicates an error.
  //
  //---------------------------------------------------------------------------
  function TSdew.GetElementCount( element: Pointer ):Integer;
   var
     ret_val:Integer;
   begin
     ret_val := -1;

     if ( _FileLoaded ) then
       ret_val := _ElementCount( element )
     ;

     result := ret_val;
   end
  ;


  //---------------------------------------------------------------------------
  //  GetElementNameByIndex
  //
  //  This function is similar to GetElementName().  It returns the name of the
  //  element referenced, in this case, by the child index of the element
  //  parameter.  The difference from GetElementName() is that this function
  //  returns the names of child elements of 'element'.
  //
  //  NOTE:  All index parameters of these functions are zero based array
  //         indexes.  Consequently, the 0th element is the first one.
  //
  //  Results:  Returns a string of length zero or greater.  A zero length
  //            string indicates that this element had no name or there
  //            was some kind of error, such as an invalid index value.
  //
  //---------------------------------------------------------------------------
  function TSdew.GetElementNameByIndex( element: Pointer; Index: Integer ):String;
   var
     ret_val: String;
     temp:PChar;
   begin
     ret_val := '';

     if ( _FileLoaded ) then
       begin
         temp := _ElementNameByIndex( element, Index );

         if ( temp <> nil ) then
           ret_val := String( temp )
         ;
       end;

     result := ret_val;
   end
  ;


  //---------------------------------------------------------------------------
  //  GetElementByIndex
  //
  //  This function returns a pointer to the index(th) child element of the
  //  element parameter.
  //
  //  NOTE:  All index parameters of these functions are zero based array
  //         indexes.  Consequently, the 0th element is the first one.
  //
  //  Results:  Returns a pointer to the element, or nil on error.
  //
  //---------------------------------------------------------------------------
  function TSdew.GetElementByIndex( element: Pointer; Index: Integer ):Pointer;
   var
     ret_val:Pointer;
   begin
     ret_val := nil;

     if ( _FileLoaded ) then
       ret_val := _ElementByIndex( element, Index )
     ;

     result := ret_val;
   end
  ;

(*
  function TSdew.CreateTree():Pointer;
   var
    ret_val: Pointer;
   begin
     ret_val := nil;

     ret_val := _CreateTree();

     result := ret_val;
   end
  ;
*)

(*
  function TSdew.AddTreeRoot( tree:Pointer; name: PChar ):Pointer;
   var
     ret_val:Pointer;
   begin
     ret_val := nil;

     if (( tree <> nil ) and ( name <> nil )) then
       begin
         ret_val := _TreeAddRoot( tree, name );
       end
     ;

     result := ret_val;
   end
  ;
*)

  (*
  function TSdew.AddElement( element:Pointer; name:PChar ):Pointer;
   begin
   end;
  *)


  //---------------------------------------------------------------------------
  //  GetAttributeCount
  //
  //  This function returns the number of child attributes
  //  of the element pointer parameter.
  //
  //  Example:
  //    If the element pointer points to:
  //      <ShoppingList store = "Safeway" day = "Thursday">
  //        <AppleJuice Quantity="3"/>
  //        <Lettuce Quantity="1"/>
  //        <Milk Quantity="2">
  //          <Units>Quart</Units>
  //        </Milk>
  //      </ShoppingList>
  //
  //    Then, this function will return (2) as an integer value, representing
  //    the two attributes of this element.  Using this result, the function
  //    GetAttributeByIndex() may be used to iterate through these.
  //
  //  NOTE:  All index parameters of these functions are zero based array
  //         indexes.  Consequently, the 0th element is the first one.
  //         A return value from this function HOWEVER, of 0 means that there
  //         are NO children of this element.  (Don't confuse the two meanings)
  //
  //    Results:  A Positive integer greater than or equal to 0 on success.
  //              A -1 return value indicates an error.
  //
  //---------------------------------------------------------------------------
  function TSdew.GetAttributeCount( element: Pointer ):Integer;
   var
     ret_val:Integer;
   begin
     ret_val := -1;

     if ( _FileLoaded ) then
       ret_val := _AttributeCount( element )
     ;

     result := ret_val;
   end
  ;


 //---------------------------------------------------------------------------
  //  GetAttributeName
  //
  //  This function returns the actual name of the attribute pointed to by the
  //  attribute parameter.  This is useful, if you are iterating through attributes
  //  by use of the GetAttributeNext or GetAttributeByIndex methods, and you want
  //  to know the name of the attribute you have been returned.
  //
  //  Example:
  //    If the attribute pointer parameter points to:
  //
  //    <ShoppingList store="K-Mart">
  //
  //    Then, this function will return the string, "store"
  //
  //  Results:  Returns a string of length zero or greater.  A zero length
  //            string indicates that there was some kind of error.
  //
  //---------------------------------------------------------------------------
  function TSdew.getAttributeName( attribute: pointer ): string;
   var
     ret_val:String;
     temp:PChar;
   begin
     ret_val := '';

     if ( _FileLoaded ) then
       begin
         temp := _AttributeName( attribute );

         if ( temp <> nil ) then
           ret_val := String( temp )
         ;
       end;

     result := ret_val;
   end
  ;


  //---------------------------------------------------------------------------
  //  GetAttributeNameByIndex
  //
  //  This function is similar to GetAttributeName().  It returns the name of the
  //  attribute referenced, in this case, by the child index of the element
  //  parameter.  The difference from GetAttributeName() is that this function
  //  returns the names of attributes of 'element'.
  //
  //  NOTE:  All index parameters of these functions are zero based array
  //         indexes.  Consequently, the 0th element is the first one.
  //
  //  Results:  Returns a string of length zero or greater.  A zero length
  //            string indicates that this attribute had no name or there
  //            was some kind of error, such as an invalid index value.
  //
  //---------------------------------------------------------------------------
  function TSdew.getAttributeNameByIndex( element: pointer; index: integer ): string;
   var
     ret_val: String;
     temp:PChar;
   begin
     ret_val := '';

     if ( _FileLoaded ) then
       begin
         temp := _AttributeNameByIndex( element, Index );

         if ( temp <> nil ) then
           ret_val := String( temp )
         ;
       end;

     result := ret_val;
   end
  ;


  //---------------------------------------------------------------------------
  //  GetElementByIndex
  //
  //  This function returns a pointer to the index(th) child element of the
  //  element parameter.
  //
  //  NOTE:  All index parameters of these functions are zero based array
  //         indexes.  Consequently, the 0th element is the first one.
  //
  //  Results:  Returns a pointer to the element, or nil on error.
  //
  //---------------------------------------------------------------------------
  function TSdew.getAttributeByIndex( element: pointer; index: integer ): pointer;
   var
     ret_val:Pointer;
   begin
     ret_val := nil;

     if ( _FileLoaded ) then
       ret_val := _AttributeByIndex( element, Index )
     ;

     result := ret_val;
   end
  ;


  //---------------------------------------------------------------------------
  //  GetAttributeContents
  //
  //  This function returns a string containing attribute data.
  //  Example:
  //    If attribute points to:
  //                         <ShoppingList store="Safeway">
  //
  //    Then, this function will return "Safeway"
  //
  //  Results:  Returns a string of length zero or greater.  A zero length
  //            string indicates that this attribute had no contents or there
  //            was some kind of error.
  //---------------------------------------------------------------------------
  function TSdew.getAttributeValue( attribute: pointer ): string;
   var
     ret_val:String;
     temp:PChar;
   begin
     ret_val := '';

     if ( _FileLoaded ) then
       begin
         temp := _AttributeValue( attribute);

         if ( temp <> nil ) then
           ret_val := String( temp )
         ;
       end
     ;

     result := ret_val;
   end
  ;


  function TSdew.getAttributeValue( element: pointer; name: pchar ): string;
    var
      attr: pointer;
      ret_val: string;
    begin
      ret_val := '';

      attr := getAttributeByName( element, name );

      if( nil <> attr ) then
        ret_val := getAttributeValue( attr )
      ;

      result := ret_val;
    end
  ;


  function TSdew.getAttributeValue( element: pointer; index: integer ): string;
    var
      attr: pointer;
      ret_val: string;
    begin
      ret_val := '';

      attr := getAttributeByIndex( element, index );

      if( nil <> attr ) then
        ret_val := getAttributeValue( attr )
      ;

      result := ret_val;
    end
  ;


 //---------------------------------------------------------------------------
  //  GetAttributeByName
  //
  //  This function returns a pointer to an attribute of an element, whose
  //  name is passed to this function.  This is handy for random access within
  //  the XML file.  However, if an element has sever attributes, all
  //  with the same name, for whatever reason, this function will only return
  //  the first occurrence and subsequent attempts will result in undefined
  //  side-effects.  If you must iterate through multiple attributes, all sharing
  //  the same name, then use the GetAttributeByIndex() function.
  //
  //  Results:  Returns a pointer to the element, or nil on error.
  //
  //---------------------------------------------------------------------------
  function TSdew.getAttributeByName( element: pointer; name: pchar ): pointer;
   var
     ret_val:Pointer;
   begin
     ret_val := nil;

     if ( _FileLoaded ) then
       ret_val := _AttributeByName( element, PChar(name) )
     ;

     result:= ret_val;
   end
  ;


  //---------------------------------------------------------------------------
  //  GetNextAttribute
  //
  //  This function returns a pointer to the next attribute of an element.
  //
  //  NOTE:  This function has two basic functionalities:
  //         1)  If attribute is nil, or invalid, and element is valid, it will
  //             return the first attribute of element.
  //         2)  If attribute is valid, it will return the next sibling of that
  //             attribute.
  //
  //         To iterate through attributes, be sure to pass the last attribute
  //         returned by this function as the attribute parameter.  The function
  //         here and the function in the scew libraries maintains NO state
  //         information.
  //
  //  Valid results:  Returns a pointer to the next attribute or nil on error.
  //
  //---------------------------------------------------------------------------
  function TSdew.getNextAttribute( element: pointer; attribute: pointer ): pointer;
   var
     ret_val:Pointer;
   begin
     ret_val := nil;

     if ( _FileLoaded ) then
       ret_val := _AttributeNext( element, attribute )
     ;

     result := ret_val;
   end
  ;


        
// END Implementation section

end.
