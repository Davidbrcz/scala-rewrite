
import collection.immutable.HashMap

trait IntExpr;

final case class Add(l : IntExpr,r : IntExpr) extends IntExpr;
final case class Multiply(l : IntExpr,r : IntExpr) extends IntExpr;
final case class VarRef(name : String) extends IntExpr;
final case class MyRandom(low : Int, up : Int) extends IntExpr;
final case class MyInt(i : Int) extends IntExpr;

object GLOBAL{
  type Strategy = Function2[IntExpr,HashMap[String,Int],Option[IntExpr]]
}

object Identity extends GLOBAL.Strategy {
  def apply(expr : IntExpr,mapping :HashMap[String,Int]) = {
    Some(expr)
  }
}

object HandleRef extends GLOBAL.Strategy {
  def apply (expr : IntExpr,mapping :HashMap[String,Int]) = {
    expr match{
      case VarRef(name) => Some(MyInt(mapping(name)))
      case _ => None
    }
  }
}

object Sequence {
  def apply
    (s1 : GLOBAL.Strategy,s2 : GLOBAL.Strategy) 
    (expr : IntExpr,mapping :HashMap[String,Int])  = {
    s1(expr,mapping) match {
      case Some(ret) => s2(ret,mapping)
      case None => None
    }
  }
}


object Choice {
  def apply(s1 : GLOBAL.Strategy,s2 : GLOBAL.Strategy)
    (expr : IntExpr,mapping :HashMap[String,Int]) = {
    s1(expr,mapping) match {
      case attempt1 : Some[IntExpr] => attempt1
      case None => s2(expr,mapping) match {
        case attempt2 : Some[IntExpr] => attempt2
        case None => None
      }
    }
  }
}

object Not{
  def apply(s1 : GLOBAL.Strategy)
    (expr : IntExpr,mapping :HashMap[String,Int]) = {
    s1(expr,mapping) match {
      case Some(_) => None
      case None => Some(expr)
    }
  }
}

object Try{
  def apply(s1 : GLOBAL.Strategy)
    (expr : IntExpr,mapping : HashMap[String,Int]) =
    Choice(s1, Identity)(expr,mapping)
}

object Repeat {
  def apply(s1 : GLOBAL.Strategy)
    (expr : IntExpr,mapping :HashMap[String,Int]) : Option[IntExpr] ={    
    Try(Sequence(s1, Repeat(s1) _ ) _ )(expr,mapping)
  }
}



object All {
  //TODO : do the visiting ???
  def apply(s1 : GLOBAL.Strategy)
    (expr : IntExpr,mapping :HashMap[String,Int]) = {
    expr match {
      case Add(l,r) => {
        All(s1)(l,mapping)
    }
  }
}

object One {
  //TODO : do the visiting too and stop on first success ???
  def apply(s1 : GLOBAL.Strategy)
    (expr : IntExpr,mapping :HashMap[String,Int]) = ???
}



object BottomUp {
  def apply(s1 : GLOBAL.Strategy)
    (expr : IntExpr, mapping :HashMap[String,Int]) : Option[IntExpr] = {
    Sequence(All(BottomUp(s1) _) _,s1)(expr,mapping)
  }
}
object TopDown {
  def apply(s1 : GLOBAL.Strategy)
    (expr : IntExpr,mapping :HashMap[String,Int]) : Option[IntExpr] = {
    Sequence(s1, All(TopDown(s1) _) _)(expr,mapping)
  }
}
object OnceBottomUp {
  def apply(s1 : GLOBAL.Strategy)
    (expr : IntExpr,mapping :HashMap[String,Int]) : Option[IntExpr] = {
    Choice(One(OnceBottomUp(s1) _) _,s1)(expr,mapping)
  }
}
object OnceTopDown {
  def apply(s1 : GLOBAL.Strategy)
    (expr : IntExpr,mapping :HashMap[String,Int]) : Option[IntExpr] = {
    Choice(s1, One(OnceTopDown(s1) _) _)(expr,mapping)
  }
}
object Innermost {
  def apply(s1 : GLOBAL.Strategy)
    (expr : IntExpr,mapping :HashMap[String,Int]) =
    Repeat(OnceBottomUp(s1) _ )(expr,mapping)
}
object Outermost {
  def apply(s1 : GLOBAL.Strategy)
    (expr : IntExpr,mapping :HashMap[String,Int]) =
    Repeat(OnceTopDown(s1) _)(expr,mapping)
}

object Main{
  def main(args: Array[String]) = {

    val expr = Add(Multiply(VarRef("a"),MyInt(5)),VarRef("b"))
    val mapping = HashMap("a" -> 2,"b" ->4)
    println(expr)
    val ret = Outermost(HandleRef)(expr,mapping)
    println(ret)
  }
}
