with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with Ada.Unchecked_Deallocation;
package arpm_rpm_depends is
    type rpm_depend is tagged limited private;
    type rpm_depend_access is access all rpm_depend;

    type rpm_depend_array is array (Positive range <>) of rpm_depend;
    type rpm_depends_access is access all rpm_depend_array;
    procedure Set_Flags( This : in out rpm_depend; Flags : in Long_Long_Integer );
    procedure Set_Name(This : in out rpm_depend; Name : in Unbounded_String);
    procedure Set_Version(This : in out rpm_depend; Version : in Unbounded_String);
    function Name(This: in out rpm_depend) return Unbounded_String;
    function Version(This: in out rpm_depend) return Unbounded_String;
    procedure Free_Depends(This : in out rpm_depends_access);



    private

    type rpm_depend is tagged limited record
        Name : Unbounded_String;
        Version : Unbounded_String;
        Flags : Long_Long_Integer;
    end record;
    procedure Free is new Ada.Unchecked_Deallocation
        (Object => rpm_depend_array,
        Name   => rpm_depends_access);

end arpm_rpm_depends;

