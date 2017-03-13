#Student Info
Qili Sui
Programming language: Standard ML
*****Important Note****** 

(1)Because of the llvm version gcc on mac, I chose to use Linux machine to run my compiler. 
So, the invokeAssemblerLinker() of my compiler doesn't work for llvm gcc on mac.
Please test and grade my code on a machine that have "real" gcc installed. Thanks much. - Qili

(2) Test files are in input/smlfunc. input directory is at the top level of the repository.
    aseembly file and excutables are generated in input/smlfunc.

********END OF NOTE******



#What is working:
1.Implemented succesfully - print the numbers between 1 and 100(test02.sml, test02.s, test02)

2.Implemented succesfully - calculate and print the factorial of numbers between 1 and 100(test01.sml, test01.s, test01)

3.Implemented successfuly - Implement literal strings that can be printed directly but not used within expressions.(test04.sml, test04.s, test04)

4.Implemented succesfully - Implement function/method/procedure calls for your programming language(See test01.sml - Recursively Call Function Factorial())



#What is not working:
1.Implemented partially - (test03.sml, test03.s, test03)print the numbers (in order) between 1 and 100 that are divisible by neither 3 nor 5
        
        working part:  

        (1)The abstract syntax tree for % (modulo) 

            Successfully parsed file "./input/smlfunc/test03.sml".
            Result is Program(List(Function(mod,List(n),Block(List(If(Prim(=,Prim(%,Var(n),CstI(3)),CstI(0)),Block(List(Return(Var(n)))),Block(List(Return(CstI(0))))))))),Block(List(Asgn(i,CstI(1)), While(Prim(<,Var(i),CstI(101)),Block(List(Block(List(Print(Call(mod,List(Var(i)))))), Asgn(i,Prim(+,Var(i),CstI(1))))))))).
            Index is 565.
            Variables: i -> (i), n -> (n)
            Compiling:
            Wrote to ./input/smlfunc/test03.s
            Running assembler: 
        (2) Code generation partially working. See test03.s file.

2.Impelmention Failed - allocated in the activation record / stack frame
