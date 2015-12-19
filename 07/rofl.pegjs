
AssignmentList
  = in_as:(Assignment _)* {
    var out_as = [];
    for (var i = 0; i < in_as.length; i += 1) {
      out_as.push(in_as[i][0]);
    }
    return out_as;
  }

WireName
  = [a-z]+ {
    return {
      type: 'wire',
      value: text()
    };
  }

Integer "integer"
  = [0-9]+ {
    return {
      type: 'int',
      value: parseInt(text(), 10)
    };
  }

Terminal
  = WireName / Integer

Assignment
  = expr:Expression _ "->" _ v:WireName {
    return {
      type: 'assignment',
      left: expr,
      right: v
    };
  }

Expression
  = (left:Terminal _ op:BinOp _ right:Terminal) {
    return {
      type: 'binop',
      op: op,
      left: left,
      right: right
    };
  }
  / (op:UnOp _ arg:Terminal) {
    return {
      type: 'unop',
      op: op,
      arg: arg
    };
  }
  / Terminal

BinOp
  = "AND" / "OR" / "LSHIFT" / "RSHIFT"
  
UnOp
  = "NOT"

_ "whitespace"
  = [ \t\n\r]*
