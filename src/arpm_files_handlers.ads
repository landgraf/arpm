with Ada.Strings.Unbounded; use Ada.Strings.Unbounded;
with ARPM_RPM_types; use ARPM_RPM_types;


package ARpm_Files_Handlers is

      protected Files is
      --   Onject to syncronize Files


      procedure Put (FileName : Unbounded_String);
      --  Put file to process


      entry Get (FileName : out Unbounded_String);
      --  Get file to process


      entry Finish;
      --  Call after last file

   private
      F : ARPM_Vector_Container;
      E : Boolean := False;
   end Files;

   protected Workers is
   --  Syncronoze workers
   --  to Free memoty after last worker is finished

      procedure Increase;
      --  Increase count of workers

      procedure Decrease;
      --  Decrease count of workes

      entry Is_Empty;
      --  Barier to determinate if all workers are finished

      function Is_Empty return Boolean;
   private
      Count : Integer := 0;
      Started : Boolean := False;
   end Workers;

end ARpm_Files_Handlers;
