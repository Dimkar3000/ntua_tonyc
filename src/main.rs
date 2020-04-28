use inkwell::context::Context;
use libtonyc::ast::*;
use libtonyc::codegen::CodeGen;
use libtonyc::parser::TokenKind;
use std::error::Error;

fn main() -> Result<(), Box<dyn Error>> {
    let context = Context::create();
    let module = context.create_module("main");
    let mut codegen = CodeGen::new(&context, module);

    // let mut ast = AstRoot::new(
    //     "
    //     def main():
    //     int i

    //     for i:=0; i<100; i :=i +1 :
    //     puti(i)
    //     putc('\\n')
    //     if i > 20:
    //     puts(\"happend\n\")
    //     exit
    //     end
    //     end
    //     end
    //     "
    // );

    // let mut ast = AstRoot::new(
    //     "def main ():
    //      def char[] reverse (char[] s):
    //          char[] t
    //         int i, l
    //         l := strlen(s)
    //         t := new char[l+1]
    //         for i := 0; i < l; i := i+1: t[i] := s[l-i-1] end
    //         t[i] := '\\x00'
    //         return t
    //      end
    //      puts(reverse(\"\\n!dlrow olleH\"))
    //      end
    //     ",
    // );

    // let mut ast = AstRoot::new(
    //     "def main():
    //         int j
    //         list[int] i
    //         i := 1 # 2 # nil
    //         j := head(i)
    //         puti(j)
    //         i := tail(i)
    //         j := head(i)
    //         puti(j)
    //     end
    //     "
    // );

    // let mut ast = AstRoot::new(
    //     "def main():
    //         list[int] j
    //         int i
    //         def list[int] test():
    //             list[int] j
    //             j := 5 # j
    //             j := 10 # j
    //             return j
    //         end
    //         j := test()
    //         i := head(j)
    //         puti(head(j))
    //     end",
    // );

    // let mut ast = AstRoot::new(
    //     "def main ():
    //         def list[int] qsort (list[int] l):

    //             def list[int] qsort_aux (list[int] l, rest):
    //                 int pivot, x
    //                 list[int] lt, ge
    //                 if nil?(l):
    //                     return rest
    //                 end
    //                 pivot := head(l)
    //                 l := tail(l)
    //                 for lt := nil, ge := nil; not nil?(l); l := tail(l):
    //                         x := head(l)
    //                     if x < pivot: lt := x # lt else: ge := x # ge end
    //                 end
    //                 return qsort_aux(lt, pivot # qsort_aux(ge, rest))
    //             end
    //         return qsort_aux(l, nil)
    //         end
    //         def writeList (char[] msg; list[int] l):
    //             bool more
    //             puts(msg)
    //             for more := false; not nil?(l); l := tail(l), more := true:
    //             if more: puts(\", \") end
    //             puti(head(l))
    //         end
    //      puts(\"\\n\")
    //      end
    //      int seed, i
    //      list[int] l
    //      seed := 65
    //      for i := 0, l := nil; i < 16; i := i+1:
    //      seed := (seed * 137 + 220 + i) mod 101
    //      l := seed # l
    //      end
    //      writeList(\"Initial list: \", l)
    //      l := qsort(l)
    //      writeList(\"Sorted list: \", l)
    //      end
    //     ",
    // );

    // let mut ast = AstRoot::new(
    //     "
    // def main():
    //     def int fib(int n,p):
    //         int a, b, j, i
    //         if n = 0: return 0
    //         elif n = 1: return 1
    //         else :
    //         a := 0
    //         b := 1
    //         for i:= 2, j:= 2; i<n; i:=i+1:
    //         puti(j)
    //         putc('\\n')
    //         a := b
    //         b := j
    //         j := (a mod p + b mod p) mod p
    //         end
    //         end
    //         return j
    //     end
    //     puts(\"result:\\n\")
    //     puti(fib(1000,1000))

    let mut ast = AstRoot::new(
        "
    def main():
        def int slow_fib(int n,p):
            puti(n)
            putc('\\n')
            if n = 0: return 0 
            elsif n = 1: return 1 
            end
            return (slow_fib(n-1,p) mod p + slow_fib(n-2,p) mod p) mod p
            
        end
        puts(\"result:\\n\")
        puti(slow_fib(100,1000))
    end",
    );

    // end",
    // );

    // let mut ast = AstRoot::new(
    //     "def main ():

    //      def hanoi (int rings; char[] source, target, auxiliary):

    //      def move (char[] source, target):
    //      puts(\"Moving from \") puts(source) puts(\" to \") puts(target) puts(\".\n\")
    //      end

    //      if rings >= 1:
    //      hanoi(rings-1, source, auxiliary, target)
    //      move(source, target)
    //      hanoi(rings-1, auxiliary, target, source)
    //      end
    //      end

    //      int NumberOfRings

    //      puts(\"Rings: \")
    //      NumberOfRings := geti()
    //      hanoi(NumberOfRings, \"left\", \"right\", \"middle\")
    //      end
    //     ",
    // );

    // let mut ast = AstRoot::new(
    //     "def main ():

    //      def bsort (int n; int[] x):

    //      def swap (ref int x, y):
    //      int t
    //      t := x
    //      x := y
    //      y := t
    //      end

    //      int i
    //      bool changed

    //      for changed := true; changed; skip:
    //      changed := false
    //      for i := 0; i < n-1; i := i+1:
    //      if x[i] > x[i+1]:
    //      swap(x[i], x[i+1])

    //      changed := true
    //      end
    //      end
    //      end
    //      end

    //      def writeArray (char[] msg; int n; int[] x):
    //      int i

    //      puts(msg)
    //      for i := 0; i < n; i := i+1:
    //      if i > 0: puts(\", \") end
    //      puti(x[i])
    //      end
    //      puts(\"\\n\")
    //      end

    //      int seed, i
    //      int[] x

    //      x := new int[16]
    //      seed := 65
    //      for i := 0; i < 16; i := i+1:
    //      seed := (seed * 137 + 220 + i) mod 101
    //      x[i] := seed
    //      end
    //      writeArray(\"Initial array: \", 16, x)
    //      bsort(16, x)
    //      writeArray(\"Sorted array: \", 16, x)
    //      end
    // ",
    // );

    // let main = codegen.module.add_function("main",codegen.context.i16_type().fn_type(&[],false),None);
    // let basic_block = codegen.context
    // .append_basic_block(main,"entry");
    // codegen.builder.position_at_end(basic_block);
    // codegen.get_atom(&Atomic::CString("Hello".to_owned()));
    // codegen.builder.build_return(None);
    let a = codegen.compile(&mut ast);
    if a.is_err() {
        println!("{}", a.unwrap_err());
    }
    // drop(codegen);
    Ok(())
}
