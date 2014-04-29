unit Loc;
(*
Loc.pas/dcu
-------------------
Begin: 2006/09/01
Last revision: $Date: 2007-01-10 22:20:59 $ $Author: areeves $
Version number: $Revision: 1.1 $
Project: APHI General Purpose Delphi Library, XML datafile functions
Website: http://www.naadsm.org/opensource/delphi
Author: Shaun Case <Shaun.Case@colostate.edu>
Revision History:
$Log: Loc.pas,v $
Revision 1.1  2007-01-10 22:20:59  areeves
Initial commit of the Simple Delphi Expat Wrapper.

Revision 1.1  2006/10/19 14:33:22  shaun
*** empty log message ***

Revision 1.2  2006/09/19 22:00:08  shaun
Adding Comments

--------------------------------------------------
Copyright (C) 2005 - 2006 Animal Population Health Institute, Colorado State University

This program is free software; you can redistribute it and/or modify it under the terms of the GNU General
Public License as published by the Free Software Foundation; either version 2 of the License, or
(at your option) any later version.
*)

interface
  
type TLoc = class(TObject)
  public
    constructor create();overload;
    constructor create( Latitude: double; Longitude: double );overload;

    function GetLatitude():double;
    function GetLongitude():double;

    procedure SetLatitude( Latitude: double );
    procedure SetLongitude( Longitude: double );

  protected
  _latitude: double;
  _longitude: double;
end;

implementation

  constructor TLoc.create();
    begin
      _latitude := 0.0;
      _longitude := 0.0;
    end;

  constructor TLoc.create(Latitude: double; Longitude: double );
    begin
      _latitude := Latitude;
      _longitude := Longitude;
    end;

  function TLoc.GetLatitude():double;
    begin
      result := _latitude;
    end;

  function TLoc.GetLongitude():double;
    begin
      result := _longitude;
    end;

  procedure TLoc.SetLatitude( Latitude: double );
    begin
      _latitude := Latitude;
    end;

  procedure TLoc.SetLongitude( Longitude: double );
    begin
      _longitude := Longitude;
    end;

end.
