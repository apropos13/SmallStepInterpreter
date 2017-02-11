
 /* 
README: 
This project was compiled using a Scala compiler;
 Scala 2.12.1 (Java HotSpot(TM) 64-Bit Server VM, Java 1.8.0_121)

To execute the program type: scalac SmallStepInterpreter.scala && scala SmallStepInterpreter
on the command line 

Assignment #4
Program: SmallStepInterpreter.scala (for the while language)
Authors: Panos Karagiannis (ID: 1309484) and Kostas Zambetakis (ID:1567380)
On this homework, we worked together entirely and no individual work was done.

*/


object SmallStepInterpreter {
   /* Implementation choices: 
   * We represent values as integers and variables as strings
   * State is a mutable hashmap that maps variables to values
   */
   
   
   type Var = String 
   type Val = Int
   type State = collection.mutable.Map[Var, Val]

   //helper functions to change state 
   def get_val(m: State, x:Var)= m(x)
   def set_val(m: State, x:Var, v:Val)= m.update(x,v)

   /* Implementation choices: 
   * We represent Aexp,Bexp and Commands as traits which correspond
   * to Java Interfaces and provide a good alternative for enums.
   * A sealed trait may not be directly extended unless
   * the inheriting class is in the same source file. That allows the compiler to
   * warn us of exhaustive match cases. Moreover, we choose to use 
   * final classes since they should never be extended.
   */

   //syntax definition of Arithmetic Expressions
   sealed trait Aexp
   final case class Num(a: Int) extends Aexp {override def toString:String = a.toString}
   final case class Variable(b: Var) extends Aexp {override def toString:String = b}
   final case class Add(left: Aexp, right: Aexp) extends Aexp {override def toString:String = left+"+"+right}
   final case class Subtr(left: Aexp, right: Aexp) extends Aexp {override def toString:String = left+"-"+right}
   final case class Mult(left: Aexp, right: Aexp) extends Aexp {override def toString:String = left+"*"+right}

   //Aexp->State->Int
   def eval_aexp(expr : Aexp, s : State): Int= expr match {
   	case Num(n) => n
   	case Variable(y) => get_val(s,y)
   	case Add(left, right) => eval_aexp(left, s) + eval_aexp(right,s) 
   	case Subtr(left,right) => eval_aexp(left,s)- eval_aexp(right,s)
   	case Mult(left,right)=> eval_aexp(left,s)* eval_aexp(right,s)
   }

   //syntax definition of Boolean expression
   sealed trait Bexp
   final case class TT() extends Bexp {override def toString:String = "true"}
   final case class FF() extends Bexp {override def toString:String = "false"}
   final case class Not(b: Bexp) extends Bexp {override def toString:String = "not "+ b}
   final case class Equal(left: Aexp, right: Aexp) extends Bexp {override def toString:String = left+"=="+right}
   final case class Less(left: Aexp, right: Aexp) extends Bexp {override def toString:String = left+"<"+right}
   final case class And(left: Bexp, right: Bexp) extends Bexp {override def toString:String = left+" && "+right}
   final case class Or(left: Bexp, right: Bexp) extends Bexp {override def toString:String = left+" || "+right}

   //Bexp->State->Boolean
   def eval_bexp(expr: Bexp, s:State):Boolean= expr match {
   	case TT() => true
   	case FF() => false
   	case Not(b) => ! eval_bexp(b,s)
   	case Equal(left, right) => eval_aexp(left,s)==eval_aexp(right,s)
   	case Less(left,right) => eval_aexp(left,s)<eval_aexp(right,s)
   	case And(left,right) => eval_bexp(left,s)&&eval_bexp(right,s)
   	case Or(left,right) => eval_bexp(left,s)||eval_bexp(right,s)

   }

   //syntax rule for commands
   sealed trait Command
   final case class Skip() extends Command
   final case class Assign(left: Var , right: Aexp) extends Command {override def toString:String = left+":="+right}
   final case class Sequence(left: Command, right:Command) extends Command {override def toString:String = left+";"+right}
   final case class IfThenElse(b:Bexp, c1:Command, c2:Command) extends Command {override def toString:String = "if "+b +" then "+ c1 + " else "+c2}
   final case class WhileLoop(b:Bexp, c:Command) extends Command {override def toString:String = "while "+b +" do "+ c }

   /*SMALL STEP INTERPRETER HELPER: 
   This interpreter is based on an recursive one-step reduction function 
   which we define by 'reduce'. This function returns Option[Command]. In case of 
   terminal commands it returns None. Otherwise it returns the Some(Command),
   after performing a one-step reduction while altering the corresponding state(parameter s).
   The state need not be returned since it is a mutable structure
   */
   // Command ->State -> Option[Command] 

   
   def reduce(expr:Command, s:State):Option[Command]= expr match {
   	case Skip() => None
   	case Assign(x,v)=>  set_val(s,x,eval_aexp( v,s) ); Some( Skip() ) 
   	case IfThenElse(b,c1,c2)=>  if (eval_bexp(b,s)) {Some(c1)} else {Some(c2)} 
   	case Sequence(c0,c1)=> (reduce(c0,s)) match {
   		case Some(c0_prime)=> Some( Sequence(c0_prime, c1))
   		case None=>println("<"+ c1+ " --- " +printState(s)+"> \n"); reduce(c1,s)
   	}
   	case WhileLoop(b,c)=> if (eval_bexp(b,s)) {Some(Sequence(c, WhileLoop(b,c)) )} else {Some(Skip())}

   }
   

   //used for visually nice printed states
   def printState(s:State):String= {
      val lines = s.mkString("{ ", " , ", " }")
      "State="+ lines 
   }


   //finally, repeatidly call reduce
   def eval(expr:Command, s:State):Unit= {
      println("<"+ expr.toString()+ " --- " +printState(s)+"> \n") //print nicely
      reduce(expr, s)  match {
         case None =>None
         case Some(c)=>  eval(c,s)
   }
}



   //test cases in main
   def main(args: Array[String]) {

   	val state_1 = collection.mutable.Map[String, Int]("a" -> 1,"b"->10, "c"->(-100))
   	val empty_state=collection.mutable.Map[String, Int]()

   	set_val(state_1,"x",9)

   	//a = a+1
      println("---------TESTING: c=4---------")
      var add_new_var=Assign("q", Num(4))
      eval(add_new_var,state_1)

   	var ass=Assign("x",Add(Variable("x"),Num(1)))
      
   	var ass2=Assign("x",Subtr(Variable("x"),Num(10)))
   	var ifff= IfThenElse(Less(Variable("x"),Num(5)), ass, ass2 )

      var ass_a=Assign("a",Add(Variable("a"),Num(1)))
   	var while_test= WhileLoop(Less(Variable("a"),Num(4)), ass_a)
   	var seq= Sequence(Assign("x",Num(3)), ifff )

      println("---------TESTING: x=3; if x<5 then x=x+1 else x=x+10---------")
   	eval(seq, empty_state)

      println("---------TESTING: while(a<4) do a=a+1---------")
   	eval(while_test,state_1)

      val state_2=  collection.mutable.Map[String, Int]("y" -> 0)

      println("---------TESTING: t:=12;if not false && t==12 then t:=10*t else Skip() ---------")
      var complicated_expr= Sequence(Assign("t", Num(12)) ,
         IfThenElse(And( Not(FF()),Equal(Variable("t"),Num(12)) ) ,
            Assign("t", Mult(Num(10),Variable("t"))) ,
            Skip()))
      eval(complicated_expr, state_2)

      println("---------TESTING GCD: <while (not y<2) && (not x<2) do if (y<x || y==x) then x:=x-y else y:=y-x ---------")
      val state_3= collection.mutable.Map[String, Int]("y" -> 5, "x"->11)
      //the gcd of 5,11 is 1 (returned in x)
      val while_gcd= WhileLoop( And( Not(Less(Variable("y"), Num(2))), Not(Less(Variable("x"), Num(2)))   ),
         IfThenElse(Or(Less(Variable("y"), Variable("x")), Equal(Variable("y"), Variable("x"))) ,
            Assign("x", Subtr(Variable("x"),Variable("y"))) ,
            Assign("y", Subtr(Variable("y"),Variable("x"))) )
         )

      eval(while_gcd,state_3)
      

   }
}