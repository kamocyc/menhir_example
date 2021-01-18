%{
open Util

let make_object a = Object a
let make_assoc k v = (k, v)
%}

%token <float> FLOAT
%token <string> ID
%token NULL
%token LEFT_BRACE
%token RIGHT_BRACE
%token COLON
%token COMMA
%token EOF

%start<Util.value> prog
%%

prog:
  obj EOF { $1 }

obj:
  LEFT_BRACE assocs RIGHT_BRACE { make_object $2 }

assocs:
| separated_list(COMMA, assoc) { $1 }

assoc:
  ID COLON jvalue { make_assoc $1 $3 }

jvalue:
| obj{ $1 }
| float_value { $1 }
| null_value { $1 }

float_value:
  FLOAT { Float $1 }
  
null_value:
  NULL { Null }

  
  