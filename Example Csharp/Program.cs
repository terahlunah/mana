using System.Diagnostics;
using Mana;

var m = new Mana.Mana();

// Mana calling Native

Value Add1(Env<Value> env, List<Value> args) =>
    args switch
    {
        [Value.Num a, Value.Num b] => Value.NewNum(a.AsNum + b.AsNum),
        _ => throw new ArgumentException("Arguments must be numbers")
    };

var Add2 = (double a, double b) => a + b;

m.setValue("add1", Value.NewFuncClosure(Add1));
m.setValue("add2", m.toValue<Func<double, double, double>>(Add2));

m.setValue("x", Value.NewNum(2));
m.setValue("y", Value.NewNum(3));

var res1 = m.run("add1 x y");
var res2 = m.run("add2 x y");

Debug.Assert(res1.AsNum == 5);
Debug.Assert(res2.AsNum == 5);

// Native calling Mana

m.run("let addM = {|x, y| x + y}");

var resM = m.call("addM", [Value.NewNum(2), Value.NewNum(3)]);

Debug.Assert(resM.AsNum == 5);