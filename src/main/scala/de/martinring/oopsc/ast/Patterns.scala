//package de.martinring.oopsc.ast
//
///**
// * Extractor objects for pattern matching
// */
//object Patterns {
//  object + {
//    def unapply(bin: Binary): Option[(Expression, Expression)] =
//      if (bin.operator == "+") Some(bin.left, bin.right)
//      else None
//  }
//
//  object - {
//    def unapply(bin: Binary): Option[(Expression, Expression)] =
//      if (bin.operator == "-") Some(bin.left, bin.right)
//      else None
//  }
//
//  object Minus {
//    def unapply(un: Unary): Option[(Expression)] =
//      if (un.operator == "-") Some(un.operand)
//      else None
//  }
//
//  object * {
//    def unapply(bin: Binary): Option[(Expression, Expression)] =
//      if (bin.operator == "*") Some(bin.left, bin.right)
//      else None
//  }
//
//  object / {
//    def unapply(bin: Binary): Option[(Expression, Expression)] =
//      if (bin.operator == "/") Some(bin.left, bin.right)
//      else None
//  }
//
//  object % {
//    def unapply(bin: Binary): Option[(Expression, Expression)] =
//      if (bin.operator == "MOD") Some(bin.left, bin.right)
//      else None
//  }
//
//  object & {
//    def unapply(bin: Binary): Option[(Expression, Expression)] =
//      if (bin.operator == "AND") Some(bin.left, bin.right)
//      else None
//  }
//
//  object | {
//    def unapply(bin: Binary): Option[(Expression, Expression)] =
//      if (bin.operator == "OR") Some(bin.left, bin.right)
//      else None
//  }
//
//  object ! {
//    def unapply(un: Unary): Option[(Expression)] =
//      if (un.operator == "NOT") Some(un.operand)
//      else None
//  }
//
//  object Bool {
//    def unapply(l: Literal): Option[Boolean] =
//      if (l.typed == Class.boolType.name) Some(l.value != 0)
//      else None
//  }
//  object Int {
//    def unapply(l: Literal): Option[Int] =
//      if (l.typed == Class.intType.name) Some(l.value)
//      else None
//  }
//}
