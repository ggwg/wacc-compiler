package com.wacc.expressions

import com.wacc.primitives.{Digit, IntegerSign}

class IntegerLiter(sign: Option[IntegerSign], val digits: List[Digit])
    extends Expression {}
