// V Prelude: The Initial Environment

in system __name "system";

// simple builtin types

type anytype __name "anytype";
type any __name "any";
type module __name "module";
type symbol __name "symbol";
type continuation __name "continuation";
type function __name "function";
type closure __name "closure";
type vector __name "vector";
type tuple __name "tuple";
type void_type __name "void_type";
type unknown_type __name "unknown_type";
type nil_type __name "nil_type";
type object_pointer __name "object_pointer";
type object __name "object" = { };
type list __name "list";
type ref __name "ref";
type value __name "value";
type set __name "set";

type int8 __name "int8";
type int16 __name "int16";
type int32 __name "int32";
type int64 __name "int64";
type int __name "int" = int32;
type uint8 __name "uint8";
type uint16 __name "uint16";
type uint32 __name "uint32";
type uint64 __name "uint64";
type uint __name "uint" = uint32;
type anyint __name "anyint" = 
  int8 | int16 | int32 | int64 |
  uint8 | uint16 | uint32 | uint64;

type float32 __name "float32";
type float64 __name "float64";
type float128 __name "float128";
type float __name "float" = float64;
type anyfloat __name "anyfloat" = 
  float32 | float64 | float128;

type complex32 __name "complex32";
type complex64 __name "complex64";
type complex128 __name "complex128";
type complex __name "complex" = complex64;
type anycomplex __name "anycomplex" = 
  complex32 | complex64 | complex128;

type anynum __name "anynum" = anyint | anyfloat | anycomplex;
type char __name "char" = uint8;
type size __name "size" = uint32;
type true __name "true"; type false __name "false"; type bool __name "bool" = true | false;
type string __name "string";
type enum_element __name "enum_element" = int;

// global data

nil __name "nil" : 0;
_ __name "unknown" : 0;
void __name "void" : 0;
empty_tuple __name "empty_tuple" : 0;
empty_list __name "empty_list" : ();

// global initialization

__main __name "__main__" : 0;

//   public builtin functions
reply: #reply __name "reply";

// meta functions
anytype::class(a:anytype)		   : #__meta_apply self a;

// overloadable primitive operators

__init(a:any)				   : a; 
new(a:anytype) __name "new_object"	   : { o: #__new a; o.__init; o };
operator(a:any, #".", b:symbol)		   : #__operator a #"." b;
operator(a:any, #"*.", b:symbol)	   : #__operator a #"." b;
operator(a:anynum, #"*", b:anynum)	   : #__operator a #"*" b;
operator(a:anynum, #"/", b:anynum)	   : #__operator a #"/" b;
operator(a:anynum, #"%", b:anynum)	   : #__operator a #"%" b;
operator(a:anynum, #"+", b:anynum)	   : #__operator a #"+" b;
operator(a:anynum, #"-", b:anynum)	   : #__operator a #"-" b;
operator(a:int, #"<<", b:int)		   : #__operator a #"<<" b;
operator(a:int, #">>", b:int)		   : #__operator a #">>" b;
operator(a:anynum, #"<", b:anynum)	   : #__operator a #"<" b;
operator(a:anynum, #"<=", b:anynum)	   : #__operator a #"<=" b;
operator(a:anynum, #">", b:anynum)	   : #__operator a #">" b;
operator(a:anynum, #">=", b:anynum)	   : #__operator a #">=" b;
operator(a:anynum, #"==", b:anynum)	   : #__operator a #"==" b;
operator(a:anynum, #"!=", b:anynum)	   : #__operator a #"!=" b;
operator(a:int, #"&", b:int)		   : #__operator a #"&" b;
operator(a:int, #"^", b:int)		   : #__operator a #"^" b;
operator(a:int, #"|", b:int)		   : #__operator a #"|" b;
operator(a:any, #"&&", b:any)		   : #__operator a #"&&" b;
operator(a:any, #"||", b:any)		   : #__operator a #"||" b;
operator(a:ref, #"=", b:any)		   : #__operator a #"=" b;
operator(a:any, #"=", b:any)		   : b;
operator(a:value, #"=", b:value)	   : #__operator a #"=" b;
operator(a:anynum, #"*=", b:anynum)	   : #__operator a #"*" b;
operator(a:anynum, #"/=", b:anynum)	   : #__operator a #"/" b;
operator(a:anynum, #"%=", b:anynum)	   : #__operator a #"%" b;
operator(a:anynum, #"+=", b:anynum)	   : #__operator a #"+" b;
operator(a:anynum, #"-=", b:anynum)	   : #__operator a #"-" b;
operator(a:anynum, #"<<=", b:int)	   : #__operator a #"<<" b;
operator(a:anynum, #">>=", b:int)	   : #__operator a #">>" b;
operator(a:anynum, #"&=", b:int)	   : #__operator a #"&" b;
operator(a:anynum, #"|=", b:int)	   : #__operator a #"|" b;
operator(a:anynum, #"^=", b:int)	   : #__operator a #"^" b;
operator(a:any, #"->", b:symbol)	   : #__operator (#__operator #"*" a) #"." b;
operator(a:any, #"->*", b:symbol)	   : #__operator (#__operator #"*" a) #"." b; 
operator(a:anynum, #"^^", b:anynum)	   : #__operator a #"^^" b;
operator(#"++", a:anynum)		   : a + 1;
operator(#"--", a:anynum)		   : a - 1;
operator(#"+", a:anynum)		   : #__operator #"+" a;
operator(#"-", a:anynum)		   : #__operator #"-" a;
operator(#"~", a:anynum)		   : #__operator #"~" a;
operator(#"!", a:any)			   : #__operator #"!" a;
operator(#"*", a:any)			   : #__operator #"*" a;
operator(#"&", var a:any)		   : #__operator #"&" a;
operator(a:anynum, #"|", b:anynum)	   : #__operator a #"|" b;
operator(#"(", a:symbol, b:any)		   : #__operator #"(" a b;
operator(a:anynum, #"++")		   : a + 1;
operator(a:anynum, #"--")		   : a - 1;

// tuples indexing

tuple::self(a:anyint) : #__index_object self a;

// vector indexing

vector::self(a:anyint) : #__index_object self a;

// generic printing

print a : #__primitive #println a;
