%{
    open Minirisc

    let convert_label label = 
        if label = "main" then 0 else (int_of_string (String.sub label 1 ((String.length label) - 1)))
    
    let blk_counter = ref 0

    let increment_blk_counter () = 
        blk_counter := !blk_counter + 1

    let ht = Hashtbl.create 1000

    let add_to_ht label = 
        Hashtbl.add ht label !blk_counter;
        increment_blk_counter ()
    
    let get_label_map () = 
        Hashtbl.fold (fun key value mapping -> LabelMap.add key value mapping) ht LabelMap.empty
%}

%token <string> LABEL
%token <int> INT
%token EOF NOP COPY NOT ADD ADDI SUB SUBI MULT MULTI AND LESS ANDI RHS STORE LOAD LOADI JUMP CJUMP

%start <program> prog

%%

prog:
    | bl = block_list ; EOF {Program (Array.of_list (List.rev bl), get_label_map ())}
block_list:
    | bl = block_list ; b = block {b :: bl}
    | b = block {[b]}
block:
    l = label ; t = instruction_list {add_to_ht l; (List.rev t)}
instruction_list:
    | il = instruction_list ; i = instruction {i::il}
    | i = instruction {[i]}
instruction:
    | code = brop; r1 = register ; r2 = register ; RHS ; r3 = register {Simple(Rtor(code, r1, r2, r3))}
    | code = biop; r1 = register ; n = INT ; RHS ; r2 = register {Simple(Rtoi(code, r1, n, r2))}
    | code = urop; r1 = register ; RHS ; r2 = register {Simple(Rury(code, r1, r2))}
    | LOAD ; r1 = register ; RHS ; r2 = register {Simple(Load(r1, r2))}
    | LOADI ; n = INT ; RHS ; r1 = register {Simple(LoadI(n, r1))}
    | STORE ; r1 = register ; RHS ; r2 = register {Simple(Store(r1, r2))}
    | JUMP ; l = label {Jump l}
    | CJUMP ; r1 = register ; jumpfrom = label ; jumpto = label {Cjump(r1, jumpfrom, jumpto)}
    | NOP ; {Simple(Nop)}
register:
    | r = INT {Id r}
label:
    | l = LABEL {Label (convert_label l)}
%inline brop:
    | ADD {Add}
    | SUB {Sub}
    | MULT {Mult}
    | AND {And}
    | LESS {Less}
%inline biop:
    | ADDI {AddI}
    | SUBI {SubI}
    | MULTI {MultI}
    | ANDI {AndI}
%inline urop:
    | NOT {Not}
    | COPY {Copy}