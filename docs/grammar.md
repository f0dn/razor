### KW_RETURN
return

### KW_DECL
decl

### KW_IF
if

### KW_FUNC
func

### KW_FOR
for

### KW_MAC
mac

### KW_USE
use

### KW_EXIT
exit

### IDENT
[IDENT_START](#ident_start) [IDENT_CHAR](#ident_char)*

### IDENT_START
[a-zA-Z_]

### IDENT_CHAR
[a-zA-Z0-9_]

### STRING
" .* "

### INTEGER
[0-9]+

### PATH
< .* >

### PROGRAM
[STAT](#stat)*

### STAT
[ASM](#asm) \
| [ASSIGN](#assign) \
| [ASSIGN_AT](#assign_at) \
| [COMMENT](#comment) \
| [DECL](#decl) \
| [EXIT](#exit) \
| [EXPR](#expr) ; \
| [FOR](#for) \
| [FUNC](#func) \
| [IF](#if) \
| [RETURN](#return) \
| [USE](#use)

### ASM
\` .* \`

### ASSIGN
[IDENT](#ident) = [EXPR](#expr) ;

### ASSIGN_AT
@ [EXPR](#expr) = [EXPR](#expr) ;

### COMMENT
// .*

### DECL
[KW_DECL](#kw_decl) [IDENT](#ident) = [EXPR](#expr) ;

### EXIT
[KW_EXIT](#kw_exit) [EXPR](#expr) ;

### EXPR
[ASM](#asm) \
| [BIN_OP](#bin_op) \
| [CALL](#call) \
| [&] [IDENT](#ident) \
| [INTEGER](#integer) \
| [STRING](#string) \
| { [STAT](#stat)* }

### FOR
[KW_FOR](#kw_for) [DECL](#decl) [EXPR](#expr) ; [ASSIGN](#assign) { [STAT](#stat)* }

### FUNC
[KW_FUNC](#kw_func) [IDENT](#ident) ( [IDENT](#ident) ) { [STAT](#stat)* }

### IF
[KW_IF](#kw_if) [EXPR](#expr) { [STAT](#stat)* }

### RETURN
[KW_RETURN](#kw_return) [EXPR](#expr) ;

### USE
[KW_USE](#kw_use) [PATH](#path) . [IDENT](#ident) ;

### CALL
[IDENT](#ident) ( [EXPR](#expr) )

### MACRO
[KW_MAC](#kw_mac) [IDENT](#ident) ( [MACRO_ARG](#macro_arg)* ) { [MACRO_BODY](#macro_body)* # }

### MACRO_ARG
{ . [IDENT](#ident) } \
| . \
| ( [MACRO_ARG](#macro_arg)* ) [?*+]

### MACRO_BODY
[. [MACRO_REPEAT](#macro_repeat) [MACRO_VAR](#macro_var)]

### MACRO_CALL
[IDENT](#ident) # ( .* )

### MACRO_REPEAT
( # [MACRO_BODY](#macro_body)* # )

### MACRO_USE
\# [USE](#use) ;

### MACRO_VAR
\# [IDENT](#ident)

### BIN_OP
[EXPR](#expr) [OP](#op) [EXPR](#expr)

### OP
| Operator | Precedence |
|----------|------------|
| \|\|     | 0          |
| &&       | 0          |
| ==       | 1          |
| >        | 1          |
| <        | 1          |
| +        | 2          |
| -        | 2          |
| *        | 3          |
| /        | 3          |
| %        | 3          |
| @        | 4          |
| !        | 4          |
| (  )     | 5          |
