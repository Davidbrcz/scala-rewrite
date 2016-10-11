import collection.immutable.HashMap

trait StringExpr;
final case class AddS(l : StringExpr,r : StringExpr) extends StringExpr;
final case class MultiplyS(l : Int,r : StringExpr) extends StringExpr;
final case class VarRefString(name : String) extends StringExpr;
final case class StringRef(s : String) extends StringExpr;

case object HandleRefS extends GLOBAL.Strategy[StringExpr,String] {
  override def apply (expr : StringExpr,mapping :HashMap[String,String]) = {
    expr match{
      case VarRefString(name) => Some(StringRef(mapping(name)))
      case _ => None
    }
  }
}

case object EvalAddS extends GLOBAL.Strategy[StringExpr,String] {
  override def apply (expr : StringExpr,mapping :HashMap[String,String]) = {
    expr match {
      case AddS(l,r) =>
      (l,r) match {
        case (StringRef(lv),StringRef(rv)) => Some(StringRef(lv+rv))
        case _ => None
      }
      case _ => None
    }
  }
}

case object EvalMultiplyS extends GLOBAL.Strategy[StringExpr,String] {
  override def  apply (expr : StringExpr,mapping :HashMap[String,String]) = {
    expr match{
      case MultiplyS(n,o) =>
        o match {
          case StringRef(s) => Some(StringRef(s*n))
          case _ => None
        }
      case _ => None
    }
  }
}

object AllStr  extends AllT[StringExpr,String] {
  override def apply(s1 : GLOBAL.Strategy[StringExpr,String])
    (expr : StringExpr,mapping :HashMap[String,String]) : Option[StringExpr]={

    expr match {
      case AddS(l,r) => {
        val new_l = s1(l,mapping).getOrElse(return None)
        val new_r = s1(r,mapping).getOrElse(return None)
        Some(AddS(new_l,new_r))
      } // end case Add

      case MultiplyS(l,r) => {
        //val new_l = s1(l,mapping).getOrElse(return None)
        val new_r = s1(r,mapping).getOrElse(return None)
        Some(MultiplyS(l,new_r))
      } // end case Multiply

      case _ => Some(expr)
    }
  }
}

object  OneStr extends OneT[StringExpr,String] {
  override def apply(s1 : GLOBAL.Strategy[StringExpr,String])
    (expr : StringExpr,mapping :HashMap[String,String]):Option[StringExpr] = {
    expr match {
      case AddS(l,r) => {
        lazy val l_transform = s1(l,mapping)
        lazy val r_transform = s1(r,mapping)
          (l_transform,r_transform) match {
          case (Some(new_l),_) => Some(AddS(new_l,r))
          case (None,Some(new_r)) => Some(AddS(l,new_r))
          case _ => None
        }
      } // end case Add

      case MultiplyS(l,r) => {
        lazy val l_transform : Option[StringExpr] = None
        lazy val r_transform = s1(r,mapping)
          (l_transform,r_transform) match {
          case (None,Some(new_r)) => Some(MultiplyS(l,new_r))
          case _ => None
        }

      } // end case Multiply

      case _ => None
    }
  }
}
