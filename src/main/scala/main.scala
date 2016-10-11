
import collection.immutable.HashMap

trait IntExpr;

final case class Add(l : IntExpr,r : IntExpr) extends IntExpr;
final case class Multiply(l : IntExpr,r : IntExpr) extends IntExpr;
final case class VarRef(name : String) extends IntExpr;
final case class MyRandom(low : Int, up : Int) extends IntExpr;
final case class MyInt(i : Int) extends IntExpr;

object GLOBAL{
  type Strategy[T,-V] = Function2[T,V,Option[T]]
}

final case class Identity[T,V]() extends GLOBAL.Strategy[T,V] {
  override def apply(expr : T,mapping :V) = {
    Some(expr)
  }
}

case object HandleRef extends GLOBAL.Strategy[IntExpr,HashMap[String,Int]] {
  override def apply (expr : IntExpr,mapping :HashMap[String,Int]) = {
    expr match{
      case VarRef(name) => Some(MyInt(mapping(name)))
      case _ => None
    }
  }
}

case object EvalAdd extends GLOBAL.Strategy[IntExpr,HashMap[String,Int]] {
  override def apply (expr : IntExpr,mapping :HashMap[String,Int]) = {
    expr match {
      case Add(l,r) =>
      (l,r) match {
        case (MyInt(lv),MyInt(rv)) => Some(MyInt(lv+rv))
        case _ => None
      }
      case _ => None
    }
  }
}

case object EvalMultiply extends GLOBAL.Strategy[IntExpr,HashMap[String,Int]] {
  override def apply (expr : IntExpr,mapping :HashMap[String,Int]) = {
    expr match{
      case Multiply(l,r) =>
        (l,r) match {
          case (MyInt(lv),MyInt(rv)) => Some(MyInt(lv*rv))
          case _ => None
        }
      case _ => None
    }    
  }
}

object Sequence {
  def apply[T,V]
    (s1 : GLOBAL.Strategy[T,V],s2 : GLOBAL.Strategy[T,V]) 
    (expr : T,mapping :V)  = {
    s1(expr,mapping) match {
      case Some(ret) => s2(ret,mapping)
      case None => None
    }
  }
}


object Choice {
  def apply[T,V](s1 : GLOBAL.Strategy[T,V],s2 : GLOBAL.Strategy[T,V])
    (expr : T,mapping :V) = {
    s1(expr,mapping) match {
      case attempt1 : Some[T] => attempt1
      case None => s2(expr,mapping) match {
        case attempt2 : Some[T] => attempt2
        case None => None
      }
    }
  }
}

object Not{
  def apply[T,V](s1 : GLOBAL.Strategy[T,V])
    (expr : T,mapping :V) = {
    s1(expr,mapping) match {
      case Some(_) => None
      case None => Some(expr)
    }
  }
}

object Try{
  def apply[T,V](s1 : GLOBAL.Strategy[T,V])
    (expr : T,mapping : V) = {
    val id = (new Identity[T,V]).apply(_,_)
    Choice(s1, id)(expr,mapping)
  }
}

object Repeat {
  def apply[T,V](s1 : GLOBAL.Strategy[T,V])
    (expr : T ,mapping :V) : Option[T] ={    
    Try(Sequence(s1, Repeat(s1) _ ) _ )(expr,mapping)
  }
}

trait AllT[T,V]{
  def apply(s1 : GLOBAL.Strategy[T,V])
    (expr : T,mapping :V) : Option[T]
}

object AllInt  extends AllT[IntExpr,HashMap[String,Int]] {
  override def apply(s1 : GLOBAL.Strategy[IntExpr,HashMap[String,Int]])
    (expr : IntExpr,mapping :HashMap[String,Int]) : Option[IntExpr]={

    expr match {
      case Add(l,r) => {
        val new_l = s1(l,mapping).getOrElse(return None)
        val new_r = s1(r,mapping).getOrElse(return None)
        Some(Add(new_l,new_r))
      } // end case Add

      case Multiply(l,r) => {
        val new_l = s1(l,mapping).getOrElse(return None)
        val new_r = s1(r,mapping).getOrElse(return None)
        Some(Multiply(new_l,new_r))
      } // end case Multiply

      case _ => Some(expr)
    }
  }
}

trait OneT[T,V]{
  def apply(s1 : GLOBAL.Strategy[T,V])
    (expr : T,mapping :V) : Option[T]
}

object  OneInt extends OneT[IntExpr,HashMap[String,Int]] {
  override def apply(s1 : GLOBAL.Strategy[IntExpr,HashMap[String,Int]])
    (expr : IntExpr,mapping :HashMap[String,Int]):Option[IntExpr] = {
    expr match {
      case Add(l,r) => {
        lazy val l_transform = s1(l,mapping)
        lazy val r_transform = s1(r,mapping)
          (l_transform,r_transform) match {
          case (Some(new_l),_) => Some(Add(new_l,r))
          case (None,Some(new_r)) => Some(Add(l,new_r))
          case _ => None
        }
      } // end case Add

      case Multiply(l,r) => {
        lazy val l_transform = s1(l,mapping)
        lazy val r_transform = s1(r,mapping)
          (l_transform,r_transform) match {
          case (Some(new_l),_) => Some(Multiply(new_l,r))
          case (None,Some(new_r)) => Some(Multiply(l,new_r))
          case _ => None
        }

      } // end case Multiply

      case _ => None
    }
  }
}



object BottomUp {
  def apply[T,V](s1 : GLOBAL.Strategy[T,V])
    (expr : T , mapping :V)(implicit theAll : AllT[T,V]) : Option[T] = {
    Sequence(theAll(BottomUp(s1) _) _,s1)(expr,mapping)
  }
}
object TopDown {
  def apply[T,V](s1 : GLOBAL.Strategy[T,V])
    (expr : T ,mapping :V)(implicit theAll : AllT[T,V]) : Option[T] = {
    Sequence(s1, theAll(TopDown(s1) _) _)(expr,mapping)
  }
}
object OnceBottomUp {
  def apply[T,V](s1 : GLOBAL.Strategy[T,V])
    (expr : T,mapping :V)(implicit theOne : OneT[T,V]) : Option[T] = {
    Choice(theOne(OnceBottomUp(s1) _) _,s1)(expr,mapping)
  }
}
object OnceTopDown {
  def apply[T,V](s1 : GLOBAL.Strategy[T,V])
    (expr : T,mapping :V)(implicit theOne : OneT[T,V]) : Option[T] = {
    Choice(s1, theOne(OnceTopDown(s1) _) _)(expr,mapping)
  }
}
object Innermost {
  def apply[T,V](s1 : GLOBAL.Strategy[T,V])
    (expr : T,mapping :V)(implicit theOne : OneT[T,V]) =
    Repeat(OnceBottomUp(s1) _ )(expr,mapping)
}
object Outermost {
  def apply[T,V](s1 : GLOBAL.Strategy[T,V])
    (expr : T,mapping :V)(implicit theOne : OneT[T,V]) =
    Repeat(OnceTopDown(s1) _)(expr,mapping)
}

object Main{
  def main(args: Array[String]) = {

    val expr =
      Add(
        MyInt(5),
        Multiply(
          Add(
            Multiply(
              //VarRef("a"),
              MyInt(2),
              MyInt(5)
            ),
            //VarRef("b")
            MyInt(4)
          ),
          MyInt(5)
        )
      )
    val mapping = HashMap("a" -> 2,"b" ->4)
    println(expr)
    implicit val oi = OneInt
    implicit val al = AllInt
    
    val fct = Sequence(Outermost(HandleRef),    
      BottomUp(Sequence(Outermost(EvalMultiply),Outermost(EvalAdd)))
    ) _ ;
    val ret = fct(expr,mapping)

    ret match {
      case Some(f) => println(f)
      case None => println("faillure 1")
    }

    val exprStr = MultiplyS(5,AddS(VarRefString("z"),VarRefString("zz")))
    val mappingStr = HashMap("z" -> "PIPO","zz" ->"TEST")
    println(exprStr)
    implicit val oistr = OneStr
    implicit val alstr = AllStr

    
    val fct2 = Sequence(Outermost(HandleRefS),    
      BottomUp(Sequence(Outermost(EvalMultiplyS),Outermost(EvalAddS)))
    )_

    val ret2 = fct2(exprStr,mappingStr)
    ret2 match {
      case Some(f) => println(f)
      case None => println("faillure 2")
    }
  }
}
