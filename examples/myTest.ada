program noDeclare
-- global variable
    declare
        globalVariable2  := -25;
        globalVariable1 := 25.0;
        globalVariable3 := 1+2+1;
        arri : integer [5];
        boolGlobal: boolean := false;
        boolTrue : boolean := true;
-- procedures block
    procedure procedure1 (a: integer; b: integer) 
        -- procedure inner block
        -- procedure inner block declare
        declare
            pro1LocalVariable1 := -25.0;
            pro1LocalVariable2 : string;
            pro1LocalVariable3 : float;
            pro1LocalVariable4  := 4;
            pro1LocalVariable5 : integer;
            pro1LocalVariable6 := "ff";

            pro1ConstantLocalVariable1 : constant := "Ge""Ay" ;
            pro1ConstantLocalVariable2 : constant  := 0.26E+1-5;
            pro1ConstantLocalVariable3 : constant  := -3.0 + pro1ConstantLocalVariable2;
            pro1ConstantLocalVariable4 : constant  := pro1ConstantLocalVariable2;
            
            arri : integer [5];
            arrb : boolean [5];
            arrs : string [5];
            arrf : float [5];
        begin
            --println (5+6);
            pro1LocalVariable5 := 6.0;
            print ("test");
            arri[pro1LocalVariable5] := 5  + ( -arri[2]) ; 
            print pro1ConstantLocalVariable1;
           
            return  pro1ConstantLocalVariable1;
        end;
    end procedure1;

    procedure function2 return integer
        begin
        return;
        end;
    end function2;
    procedure function1(a:string;b:integer) return integer
        -- procedure inner block
        -- procedure inner block declare
        declare
            fun1LocalVariable1 := -25;
            fun1ConstantLocalVariable1 : constant := "Gey";
        begin
            
            print ("test");
            -- test if 
            if ((4>5)/= false)and(4<4) then
                declare
                    ifVariable1 := 5;
                begin 
                    print fun1LocalVariable1;
                    println globalVariable2;
                    
                end;    
               
            else
                println ("s");
            end if;

            if ((4>5)/= false)and(4<4) then
             read arri;
            end if;
            
            while (boolGlobal) loop
                print 4;
            end loop;

            for (ac in -12..6)loop
                declare
                    testFor:=6;
                begin
                    for(svx in 0 .. 6)loop
                        declare
                        begin
                            print svx;
                        end;
                    end loop;
                    
                end;
            end loop;
            print "ac";
            read arri;
           
            globalVariable1 := 55;
            globalVariable2 := (-25+69-8869*45/96);
            
        end;
    end function1;

-- main block
    begin
        -- print  ();
        println (5+6);
        globalVariable1 := -(366+5);
        globalVariable1 := 5;
        if ((4>5)/= false)and(4<4) then
             read arri;
        end if;
        read globalVariable1;
        while (boolGlobal) loop
            begin
            print 4;
            end;
        end loop;
        globalVariable1 := (function1("2",4) + arri[6] );
        arri[0] := function1("2",4);
        arri[function2] := 4;
        procedure1(function1("1",5),function2);
        return (function1("2",4) > arri[6]); 
        read arri;
        boolGlobal := (not(true)) or (false and not boolTrue);
        globalVariable1 := -0.26E+1-0.26E+1;
    end;
end noDeclare 